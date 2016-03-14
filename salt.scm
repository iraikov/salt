 
;;
;; Hybrid dynamical systems modeling.
;;
;; Copyright 2015-2016 Ivan Raikov.
;;
;; This implementation follows the work of Tom Short and his Julia
;; Sims.jl library, which is in turn based on David Broman's MKL
;; simulator and the work of George Giorgidze and Henrik Nilsson in
;; functional hybrid modeling.
;;
;; Following Sims.jl, a nodal formulation is used based on David
;; Broman's thesis:
;; 
;;   David Broman. Meta-Languages and Semantics for Equation-Based
;;   Modeling and Simulation. PhD thesis, Thesis No 1333. Department of
;;   Computer and Information Science, Linköping University, Sweden,
;;   2010.
;;   http://www.bromans.com/david/publ/thesis-2010-david-broman.pdf
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;


(module salt 

	(

         parse elaborate simcreate codegen-ODE codegen-ODE/ML codegen-ODE/C 
         math-constant-env math-binop-env math-unop-env 
         model-quantities model-units
         verbose add-trace clear-trace

         constant
         empty-env env-lookup extend-env-with-binding env->list
         gen-binding binding-key binding-value

         astdecls? make-astdecls astdecls-decls

         equation-set-model
         equation-set-definitions
         equation-set-discrete-definitions
         equation-set-parameters
         equation-set-fields
         equation-set-externals
         equation-set-externalevs
         equation-set-equations
         equation-set-initial
         equation-set-conditions
         equation-set-pos-responses
         equation-set-neg-responses
         equation-set-functions
         equation-set-nodemap
         equation-set-regimemap
         equation-set-dimenv

         codegen-primop codegen-expr codegen-const-expr codegen-ODE
         name/ML binding->ML stmt->ML value->ML codegen-ODE/ML

         model-time
	 )

	(import scheme chicken)
        
	(require-extension matchable datatype lalr-driver mathh unitconv with-units fmt fmt-c)
	(require-library data-structures extras srfi-1 srfi-4 srfi-13)
	(import (only srfi-1 zip fold fold-right filter filter-map list-tabulate every delete-duplicates)
                (only srfi-4 list->s32vector)
                (only srfi-13 string-null? string-concatenate string-map string<)
		(only data-structures ->string alist-ref conc intersperse compose sort topological-sort)
                (only extras pp fprintf)
                (only ports with-output-to-port)
		)


(include "mathh-constants.scm")
(include "units.scm")
(include "parser.scm")
(include "env.scm")
(include "codegen.scm")


(define (s+ . rst) (string-concatenate (map ->string rst)))

(define (salt:warning x . rest)
  (let loop ((port (open-output-string)) (objs (cons x rest)))
    (if (null? objs)
	(begin
	  (newline port)
	  (print-error-message (get-output-string port) 
			       (current-error-port) "salt warning"))
	(begin (display (car objs) port)
	       (display " " port)
	       (loop port (cdr objs))))))


(define (salt:error x . rest)
  (let ((port (open-output-string)))
    (if (port? x)
	(begin
	  (display "[" port)
	  (display (port-name x) port)
	  (display "] " port)))
    (let loop ((objs (if (port? x) rest (cons x rest))))
      (if (null? objs)
	  (begin
	    (newline port)
	    (error 'salt (get-output-string port)))
	  (let ((obj (car objs)))
	    (if (procedure? obj) 
		(with-output-to-port port obj)
		(begin
		  (display obj port)
		  (display " " port)))
	    (loop (cdr objs)))))))


(define verbose (make-parameter 0))

(define trace-locations (make-parameter '()))

(define (add-trace loc)
  (if (not (member loc (trace-locations)))
      (trace-locations (cons loc (trace-locations)))))

(define (clear-trace loc)
  (trace-locations '()))


(define (d loc fstr . args)
  (let ([port (current-error-port)])
    (if (positive? (verbose)) 
	(begin 
          (fprintf port "~A: " loc)
          (apply fprintf port fstr args)
          (flush-output port) ) )))

(define (trace loc fstr . args)
  (let ([port (current-error-port)])
    (if (member loc (trace-locations))
	(begin 
          (fprintf port "~A: " loc)
          (apply fprintf port fstr args)
          (flush-output port) ) )))


(define syntactic-keywords
  '(function if cond and or let else))


(define-record-type variable
  (make-variable  name label value history dim)
  variable?
  (name        variable-name)
  (label       variable-label)
  (value       variable-value)
  (history     variable-history)
  (dim        variable-dim)
  )


(define-record-printer (variable x out)
  (fprintf out "#(variable ~S label=~S value=~S history=~S dim=~A)"
	   (variable-name x)
	   (variable-label x)
	   (variable-value x)
	   (variable-history x)
	   (variable-dim x)
           ))


(define (unknown value label dim)
  (make-variable (gensym 'u) label value #t dim))


(define-record-type free-variable
  (make-free-variable name)
  free-variable?
  (name free-variable-name)
  )


(define-record-printer (free-variable x out)
  (fprintf out "#(free ~S)"
	   (free-variable-name x)
           ))


(define-record-type derivative-variable
  (make-derivative-variable parent)
  derivative-variable?
  (parent      derivative-variable-parent)
  )


(define-record-printer (derivative-variable x out)
  (fprintf out "#(deriv ~S)"
	   (derivative-variable-parent x)
           ))



(define-record-type discrete-variable
  (make-discrete-variable name label value dim)
  discrete-variable?
  (name       discrete-variable-name)
  (label      discrete-variable-label)
  (value      discrete-variable-value)
  (dim        discrete-variable-dim)
  )

(define (discrete value label dim)
  (make-discrete-variable (gensym 'd) label value dim))


(define-record-printer (discrete-variable x out)
  (fprintf out "#(discrete ~S (~a) [~A] = ~A)"
	   (discrete-variable-name x)
	   (discrete-variable-label x)
	   (discrete-variable-dim x)
	   (discrete-variable-value x)
           ))


(define-record-type regime-variable
  (make-regime-variable name)
  regime-variable?
  (name       regime-variable-name)
  )

(define-record-printer (regime-variable x out)
  (fprintf out "#(regime-variable ~S)"
	   (regime-variable-name x)
           ))


(define-record-type reduce-variable
  (make-reduce-variable op parent)
  reduce-variable?
  (op          reduce-variable-op)
  (parent      reduce-variable-parent)
  )


(define-record-printer (reduce-variable x out)
  (fprintf out "#(reduce ~S ~S)"
	   (reduce-variable-op x)
	   (reduce-variable-parent x)
           ))


(define-record-type parameter
  (parameter name label value dim)
  parameter?
  (name parameter-name)
  (label parameter-label)
  (value parameter-value)
  (dim  parameter-dim)
  )


(define-record-printer (parameter x out)
  (fprintf out "#(parameter ~S (~A) [~A] = ~A)"
	   (parameter-name x)
	   (parameter-dim x)
	   (parameter-label x)
	   (parameter-value x)
           ))


(define-record-type field
  (field name label value dim)
  field?
  (name field-name)
  (label field-label)
  (value field-value)
  (dim  field-dim)
  )


(define-record-printer (field x out)
  (fprintf out "#(field ~S (~A) [~A] = ~A)"
	   (field-name x)
	   (field-dim x)
	   (field-label x)
	   (field-value x)
           ))


(define-record-type declared-constant
  (declared-constant label content)
  declared-constant?
  (label declared-constant-label)
  (content declared-constant-content)
  )


(define-record-type constant
  (constant type value unit)
  constant?
  (type constant-type)
  (value constant-value)
  (unit constant-unit)
  )



(define-record-printer (constant x out)
  (if (constant-unit x)
      (fprintf out "#(constant ~S = ~A ~A)"
               (constant-type x)
               (constant-value x)
               (constant-unit x)
               )
      (fprintf out "#(constant ~S = ~A)"
               (constant-type x)
               (constant-value x)
               )))


(define-record-type external
  (external name label initial dim)
  external?
  (name external-name)
  (label external-label)
  (initial external-initial)
  (dim external-dim)
  )


(define-record-printer (external x out)
  (fprintf out "#(external ~S (~A) [~A] = ~A)"
	   (external-name x)
	   (external-dim x)
	   (external-label x)
	   (external-initial x)
           ))


(define-record-type externalev
  (externalev name label initial dim)
  externalev?
  (name externalev-name)
  (label externalev-label)
  (initial externalev-initial)
  (dim externalev-dim)
  )


(define-record-printer (externalev x out)
  (fprintf out "#(externalev ~S (~A) [~A] = ~A)"
	   (externalev-name x)
	   (externalev-label x)
	   (externalev-dim x)
	   (externalev-initial x)
           ))


(define-record-type left-var
  (left-var u )
  left-var?
  (u left-var-u)
  )


(define-record-printer (left-var x out)
  (fprintf out "#(left-var ~S)"
	   (left-var-u x)
           ))


(define-record-type ref-var
  (make-ref-var  u idx )
  ref-var?
  (u        ref-var-u)
  (idx      ref-var-idx)
  )


(define-record-printer (ref-var x out)
  (fprintf out "#(ref-var ~S [~A])"
	   (ref-var-u x)
	   (ref-var-idx x)
           ))


(define-record-type pair-arg
  (make-pair-arg  fst snd )
  pair-arg?
  (fst      pair-arg-fst)
  (snd      pair-arg-snd)
  )

(define-record-type null-arg
  (make-null-arg)
  null-arg?
  )


(define-record-printer (pair-arg x out)
  (fprintf out "#(pair-arg ~A ~A)"
	   (pair-arg-fst x)
	   (pair-arg-snd x)
           ))


(define-record-type pair-formal
  (make-pair-formal  fst snd )
  pair-formal?
  (fst      pair-formal-fst)
  (snd      pair-formal-snd)
  )


(define-record-printer (pair-formal x out)
  (fprintf out "#(pair-formal ~A ~A)"
	   (pair-formal-fst x)
	   (pair-formal-snd x)
           ))


(define-record-type null-formal
  (make-null-formal )
  null-formal?
  )


(define-record-type var-def
  (make-var-def sym )
  var-def?
  (sym      var-def-sym)
  )


(define-record-printer (var-def x out)
  (fprintf out "#(var-def ~S)"
	   (var-def-sym x)
           ))


(define-record-type function
  (function name formals body)
  function?
  (name    function-name)
  (formals function-formals)
  (body    function-body))


(define-record-printer (function x out)
  (fprintf out "#(function ~S (~S) = ~S)"
	   (function-name x)
	   (function-formals x)
	   (function-body x)
           ))


(define-record-type equation
  (eq pattern rhs)
  equation?
  (pattern equation-pattern)
  (rhs     equation-rhs))


(define-record-printer (equation x out)
  (fprintf out "#(equation ~S = ~A)"
	   (equation-pattern x)
	   (equation-rhs x)
           ))


(define-record-type initial-equation
  (init variable expr)
  initial-equation?
  (variable equation-variable)
  (expr   equation-expr))


(define-record-type event
  (make-event name condition pos neg)
  event?
  (name event-name)
  (condition event-condition)
  (pos event-pos)
  (neg event-neg))


(define-record-type evcondition
  (make-evcondition name expr)
  evcondition?
  (name evcondition-name)
  (expr evcondition-expr))


(define-record-type evresponse
  (make-evresponse name expr)
  evresponse?
  (name evresponse-name)
  (expr evresponse-expr))


(define-record-type transition 
  (make-transition event target condition response)
  transition?
  (event transition-event)
  (target transition-target)
  (condition transition-condition)
  (response transition-response)
  )


(define-record-type structural-event
  (make-structural-event name label regime transition )
  structural-event?
  (name structural-event-name)
  (label structural-event-label)
  (regime structural-event-regime)
  (transition structural-event-transition)
  )


(define-record-type astdecls
  (make-astdecls decls)
  astdecls?
  (decls astdecls-decls)
  )



(define (expr-value x)
  (cond ((variable? x) (variable-value x))
        ((ref-var? x) (vector-ref (ref-var-u x) (ref-var-idx x)))
        ((list? x) (eval `(,(car x) . ,(map expr-value x))))
        (else x)))


(define (variable-identity x)
  (cond ((variable? x)  
         (variable-name x))
        ((derivative-variable? x)   
         (variable-identity (derivative-variable-parent x)))
        ((discrete-variable? x)   
         (discrete-variable-name x))
        ((regime-variable? x)   
         (regime-variable-name x))
        ((reduce-variable? x)   
         (variable-identity (reduce-variable-parent x)))
        (else (error 'variable-identity "invalid argument" x))
        ))

(define (reinit e x y)
  (cond ((left-var? x) `(signal.reinit ,e ,x ,y))
        ((variable? x)  (reinit e (left-var x) y))
        ((ref-var? x)   (reinit e (left-var x) y))
        ((derivative-variable? x)   
         (reinit e (left-var (derivative-variable-parent x)) y))
        ((discrete-variable? x)   
         (reinit e (left-var x) y))
        ((regime-variable? x)   
         (reinit e (left-var x) y))
        ((reduce-variable? x)   
         (reinit e (left-var (reduce-variable-parent x)) y))
        (else (error 'reinit "invalid argument to reinit" x))
        ))



(define math-constant-env
  (fold (lambda (k v env)
          (let ((binding (gen-binding k v)))
            (extend-env-with-binding env binding)))
          empty-env
        `(UNITZERO 
          E 1/E E^2 E^PI/4 LOG2E LOG10E LN2 LN3 LNPI LN10 1/LN2 1/LN10 PI PI/2
          PI/4 1/PI 2/PI 2/SQRTPI SQRTPI PI^2 DEGREE SQRT2 1/SQRT2 SQRT3 SQRT5
          SQRT10 CUBERT2 CUBERT3 4THRT2 GAMMA1/2 GAMMA1/3 GAMMA2/3 PHI LNPHI
          1/LNPHI EULER E^EULER SIN1 COS1 ZETA3)
        (cons (constant 'number 0.0 'unitbottom)
              (map (lambda (x) (constant 'number x unitless))
                   (list E 1/E E^2 E^PI/4 LOG2E LOG10E LN2 LN3 LNPI LN10 1/LN2 1/LN10 PI PI/2
                         PI/4 1/PI 2/PI 2/SQRTPI SQRTPI PI^2 DEGREE SQRT2 1/SQRT2 SQRT3 SQRT5
                         SQRT10 CUBERT2 CUBERT3 4THRT2 GAMMA1/2 GAMMA1/3 GAMMA2/3 PHI LNPHI
                         1/LNPHI EULER E^EULER SIN1 COS1 ZETA3)))
        ))
     

(define math-binop-env
  (let ((function-names `(+ - * / pow max min == < <= > >= random.unifrange))
        (op-names `(signal.add
                    signal.sub signal.mul signal.div signal.pow signal.max signal.min signal.eqnum 
                    signal.lt signal.lte signal.gt signal.gte
                    random.unifrange)))
    (fold (lambda (k v env)
            (let ((binding (gen-binding k v)))
              (extend-env-with-binding env binding)))
          empty-env
          function-names
          (map (lambda (f fn)
                 (function (make-free-variable f)
                           (make-pair-formal (make-var-def 'a) (make-pair-formal (make-var-def 'b) (make-null-formal)))
                           `(signal.call ,fn ,(make-pair-arg (make-var-def 'a) (make-pair-arg (make-var-def 'b) (make-null-arg))))
                           ))
               function-names 
               op-names))
    ))


(define math-unop-env
  (let ((function-names
         `(neg abs atan asin acos sin cos exp ln
               sqrt tan cosh sinh tanh hypot gamma lgamma log10 log2 log1p ldexp cube
               round ceiling floor 
               heaviside random.exponential))
        (op-names
         `(signal.neg signal.abs signal.atan signal.asin signal.acos signal.sin signal.cos signal.exp signal.ln
                      signal.sqrt signal.tan signal.cosh signal.sinh signal.tanh signal.hypot signal.gamma signal.lgamma signal.log10
                      signal.log2 signal.log1p signal.ldexp signal.cube
                      signal.round signal.ceiling signal.floor
                      signal.heaviside 
                      random.exponential)))
    (fold (lambda (k v env)
            (let ((binding (gen-binding k v)))
              (extend-env-with-binding env binding)))
          empty-env
          function-names
          (map (lambda (f fn)
                 (function (make-free-variable f)
                           (make-pair-formal (make-var-def 'a) (make-null-formal))
                           `(signal.call ,fn ,(make-pair-arg (make-var-def 'a) (make-null-arg))))
                 )
               function-names
               op-names)
          ))
  )
  

(define math-zop-env
  (let ((function-names
         `(random.uniform))
        (op-names
         `(random.uniform)))
    (fold (lambda (k v env)
            (let ((binding (gen-binding k v)))
              (extend-env-with-binding env binding)))
          empty-env
          function-names
          (map (lambda (f fn)
                 (function (make-free-variable f)
                           (make-null-formal)
                           `(signal.call ,fn ,(make-null-arg)))
                 )
               function-names
               op-names)
          ))
  )
  


;; A representation of a flattened model, normally created with
;; `elaborate(model)`. The code generation procedures take an
;; elaborated model as input.
;;
;; Contains the hierarchical equations, flattened equations, flattened
;; initial equations, events, event response functions, and a map of
;; nodes.


(define-record-type equation-set
  (make-equation-set model definitions discrete-definitions parameters fields externals externalevs
                     equations initial conditions pos-responses neg-responses 
                     functions nodemap regimemap dimenv)
  equation-set?
  (model equation-set-model)
  (definitions equation-set-definitions)
  (discrete-definitions equation-set-discrete-definitions)
  (parameters equation-set-parameters)
  (fields equation-set-fields)
  (externals equation-set-externals)
  (externalevs equation-set-externalevs)
  (equations equation-set-equations)
  (initial equation-set-initial)
  (conditions equation-set-conditions)
  (pos-responses equation-set-pos-responses)
  (neg-responses equation-set-neg-responses)
  (functions equation-set-functions)
  (nodemap equation-set-nodemap)
  (regimemap equation-set-regimemap)
  (dimenv equation-set-dimenv)
  )


(define-record-printer (equation-set x out)
  (fprintf out "#")
  (pp
   `(equation-set
    (definitions=,(equation-set-definitions x))    
    (discrete-definitions=,(equation-set-discrete-definitions x))
    (parameters=,(equation-set-parameters x))
    (fields=,(equation-set-fields x))
    (externals=,(equation-set-externals x))
    (externalevs=,(equation-set-externalevs x))
    (equations=,(equation-set-equations x))
    (functions=,(equation-set-functions x))
    (conditions=,(equation-set-conditions x))
    (pos-responses=,(equation-set-pos-responses x))
    (neg-responses=,(equation-set-neg-responses x))
    (nodemap=,(equation-set-nodemap x))
    (regimemap=,(equation-set-regimemap x)))
   out))


;; runtime representation of a simulation object
(define-record-type simruntime
  (make-simruntime eqset cindexmap dindexmap evindexmap rindexmap extindexmap extevindexmap
                   parameters defs discrete-defs external-defs externalev-defs 
                   eqblock condblock posresp negresp)
  simruntime?
  (eqset simruntime-eqset)
  (cindexmap simruntime-cindexmap)
  (dindexmap simruntime-dindexmap)
  (evindexmap simruntime-evindexmap)
  (rindexmap simruntime-rindexmap)
  (extindexmap simruntime-extindexmap)
  (extevindexmap simruntime-extevindexmap)
  (parameters simruntime-parameters)
  (defs simruntime-definitions)
  (discrete-defs simruntime-discrete-definitions)
  (external-defs simruntime-external-definitions)
  (externalev-defs simruntime-externalev-definitions)
  (eqblock simruntime-eqblock)
  (condblock simruntime-condblock)
  (posresp simruntime-posresp)
  (negresp simruntime-negresp)
  )


(define-record-printer (simruntime x out)
    (fprintf out "#")
    (pp `(simruntime 
          (cindexmap=,(simruntime-cindexmap x))
          (dindexmap=,(simruntime-dindexmap x))
          (evindexmap=,(simruntime-evindexmap x))
          (extindexmap=,(simruntime-extindexmap x))
          (extevindexmap=,(simruntime-extevindexmap x))
          (parameters=,(simruntime-parameters x))
          (defs=,(simruntime-definitions x))
          (external-defs=,(simruntime-external-definitions x))
          (externalev-defs=,(simruntime-externalev-definitions x))
          (eqblock=,(simruntime-eqblock x))
          (evblock=,(simruntime-condblock x))
          (posresp=,(simruntime-posresp x))
          (negresp=,(simruntime-negresp x))
          )
        out)
    )
    
(define-record-printer (astdecls x out)
    (fprintf out "#")
    (pp `(ast . ,(astdecls-decls x)) out))


(define model-time (unknown (constant 'number 0.0 millisecond) 't millisecond))


(define (map-equations f eqs)
  (map 
   (match-lambda*
    [($ initial-equation s expr) 
     (init s (f expr))]
    [($ equation s expr) 
     (eq s (f expr))]
    [eqn (error 'map-equation "unknown equation type" eqn)])
   eqs))
      


(define (replace-fixed expr)
  (map
   (lambda (x)
     (cond ((variable? x) (or (variable-value x) x))
           ((list? x) `(,(car x) . ,(map replace-fixed (cdr x))))
           (else x)))
   expr))



;; Equation parsing

(define default-parse-env
  (fold (lambda (ext-env env) (extend-env-with-env env ext-env))
        empty-env
        (list math-constant-env
              math-binop-env
              math-unop-env
              math-zop-env)
        ))


(define (parse decls #!key (env default-parse-env))
    (letrec
        ((parse0
          (lambda (decls)
            (d 'parse "decls = ~A~%" decls)
            (map (lambda (x) 
                   (cond
                    ((astdecls? x) x)
                    (else (parse-declaration env x))
                    ))
                 decls))
          ))
      (make-astdecls (parse0 decls)))
    )


;; Expression resolution

(define (resolve expr env-stack)

  (d 'resolve "expr: ~A env-stack: ~A~%" 
     expr (env-stack-show env-stack))

  (let recur ((e expr))
    (trace 'resolve "e: ~A~%" e)

    (cond
     
     ((free-variable? e)
      (let ((assoc-var-def (env-stack-lookup (free-variable-name e) env-stack)))
        (trace 'resolve "free-var: e = ~A assoc-var-def = ~A~%" (free-variable-name e) assoc-var-def)
        (if assoc-var-def
            (let recur ((resval (binding-value assoc-var-def)))
              (let ((resval1 (resolve resval env-stack)))
                (if (equal? resval resval1) resval
                    (recur resval1))))
            (let ((assoc-unit (assv (free-variable-name e) (model-units))))
              (trace 'resolve "free-var: e = ~A assoc-unit = ~A~%" (free-variable-name e) assoc-unit)
              (if assoc-unit
                  (constant 'number 1.0 (cdr assoc-unit))
                  e)))
        ))

     ((derivative-variable? e)
      (make-derivative-variable (resolve (derivative-variable-parent e) env-stack)))

     ((variable? e)
      (let ((name    (variable-name e))
            (label   (variable-label e))
            (value   (let recur ((resval (variable-value e)))
                       (let ((resval1 (resolve resval env-stack)))
                         (if (equal? resval resval1) resval
                             (recur resval1)))))
            (history (variable-history e))
            (dim     (variable-dim e)))
        (make-variable name label value history dim)))

     ((discrete-variable? e)
      (let ((name    (discrete-variable-name e))
            (label   (discrete-variable-label e))
            (value   (let recur ((resval (discrete-variable-value e)))
                       (let ((resval1 (resolve resval env-stack)))
                         (if (equal? resval resval1) resval
                             (recur resval1)))))
            (dim     (discrete-variable-dim e)))
        (make-discrete-variable name label value dim)))

     ((parameter? e)
      (let ((name    (parameter-name e))
            (label   (parameter-label e))
            (value   (let recur ((resval (parameter-value e)))
                       (let ((resval1 (resolve resval env-stack)))
                         (if (equal? resval resval1) resval
                             (recur resval1)))))
            (dim     (parameter-dim e)))
        (parameter name label value dim)))

     ((field? e)
      (let ((name    (field-name e))
            (label   (field-label e))
            (value   (let recur ((resval (field-value e)))
                       (let ((resval1 (resolve resval env-stack)))
                         (if (equal? resval resval1) resval
                             (recur resval1)))))
            (dim     (field-dim e)))
        (field name label value dim)))

     ((reduce-variable? e)
      (make-reduce-variable
       (resolve (reduce-variable-op e) env-stack)
       (resolve (reduce-variable-parent e) env-stack)))

     ((external? e)
      (let ((name    (external-name e))
            (label   (external-label e))
            (initial (let recur ((resval (external-initial e)))
                       (let ((resval1 (resolve resval env-stack)))
                         (if (equal? resval resval1) resval
                             (recur resval1)))))
            (dim     (external-dim e)))
        (external name label initial dim)))

     ((externalev? e)
      (let ((name    (externalev-name e))
            (label   (externalev-label e))
            (initial (let recur ((resval (externalev-initial e)))
                       (let ((resval1 (resolve resval env-stack)))
                         (if (equal? resval resval1) resval
                             (recur resval1)))))
            (dim     (externalev-dim e)))
        (externalev name label initial dim)))

     ((pair-arg? e)
      (make-pair-arg (recur (pair-arg-fst e))
                     (recur (pair-arg-snd e))))

     ((function? e)
      (let ((name    (function-name e))
            (formals (function-formals e))
            (body    (function-body e)))
        (let* ((env-stack1 (push-env-stack
                            (function-formal-env formals empty-env) 
                            env-stack))
               (body1 (resolve body env-stack1)))
          (d 'resolve "function: name = ~A body = ~A~%" name body)
          (d 'resolve "function: body1 = ~A~%" body1)
          (function name formals body1)
          ))
      )


     ((pair? e)
      (let ((op (car e)) (args (cdr e)))
        (trace 'resolve "op = ~A~%" op)
        (trace 'resolve "args = ~A~%" args)
        (case op
          ((signal.if)   `(,op . ,(map recur args)))
          ((signal.cond) `(,op . ,(map recur args)))
          ((signal.and)  `(,op . ,(map recur args)))
          ((signal.or)   `(,op . ,(map recur args)))
          ((signal.let)  `(,op ,(map (lambda (b) (cons (car b) (recur (cdr b)))) (car args))
                               ,(recur (cadr args))))
          ((signal.call) `(,op . ,(map recur args)))
          (else   (map recur e)))))

     (else e)
     ))
  )

;; Resolution of reinit expressions
(define (resolve-reinit evname expr)

  (d 'resolve-reinit "expr: ~A~%" expr)

  (let recur ((e expr))

    (trace 'resolve-reinit "e: ~A~%" e)

    (cond

     ((pair? e)
      (let ((op (car e)) (args (cdr e)))
        (trace 'resolve "op = ~A~%" op)
        (trace 'resolve "args = ~A~%" args)
        (case op
          ((reinit)   (reinit evname (car args) (cadr args)))
          (else       e))
        ))

     (else e)

     ))
  )

;; Merge the equations of the particular regime into the main list of
;; equations with the requisite conditionals.
(define (merge-regime-eq name regime-eq equations env-stack)
  (d 'merge-regime-eq "regime-eq = ~A~%equations = ~A~%" regime-eq equations)
  (match-let ((($ equation s expr) regime-eq))
             (let (
                   (equations1
                    (let ((s1 (resolve s env-stack))
                          (expr1 (resolve expr env-stack)))
                      (trace 'merge-regime-eq "s1 = ~A~%expr1 = ~A~%" s1 expr1) 
                      (if (null? equations)
                          (list (list s1 `(signal.if ,(make-regime-variable name) 
                                                     ,expr1 ,(constant 'number 0.0 'unitbottom))))
                          (match-let
                           (((equations1 found?)
                             (fold (match-lambda* 
                                    [((and eq (s2 expr2)) (eqs found?))
                                     (begin
                                       (trace 'merge-regime-eq "s1 = ~A~%s2 = ~A~%" s1 s2)
                                       (trace 'merge-regime-eq "expr1 = ~A expr2 = ~A~%" expr1 expr2)
                                       (if (equal? (variable-identity s2) (variable-identity s1))
                                           (list (cons (list s1 `(signal.if ,(make-regime-variable name) 
                                                                            ,expr1 ,expr2)) eqs)
                                                 #t)
                                           (list (cons eq eqs) found?)))]
                                     [(eq (eqs found?))
                                      (list (cons eq eqs) found?)])
                                   '(() #f)
                                   equations)))
                           (if found? 
                               equations1 
                               (cons (list s1 `(signal.if ,(make-regime-variable name) 
                                                          ,expr1 ,(constant 'number 0.0 'unitbottom)))
                                     equations1))
                           ))
                      ))
                   )
               (trace 'merge-regime-eq "regime-eq = ~A equations1 = ~A~%" regime-eq equations1)
               equations1)
  ))
  

;;
;; Equation elaboration (flattening)
;;
;; The main steps in flattening are:
;;
;; Creates a name resolution environment (parameters,fields,externals,constants,variables,functions).
;; Replaces fixed initial values.
;; Flattens models and populates equation, definition, function lists.
;; Pulls out initial equations and populate initial list.
;; Pulls out event definitions and populates event list.
;; Handle structural events.
;;


(define (elaborate model)

  (define (rename-decls decls)

    (let recur ((decls decls) (ax '()))

      (if (null? decls) (reverse ax)

          (let ((decl (car decls)))
      
            (if (pair? decl)
                
                (recur (append decl (cdr decls)) ax)
                
                (match decl  
                       (($ discrete-variable name label value dim)
                        (let ((decl1 (make-discrete-variable (gensym 'dvar) label value dim)))
                          (recur (cdr decls) (cons decl1 ax))
                          ))
                       (($ variable name label value has-history dim)
                        (let ((decl1 (make-variable (gensym 'var) label value has-history dim)))
                          (recur (cdr decls) (cons decl1 ax))))
                       (($ parameter name label value dim)
                        (let ((decl1 (parameter (gensym 'p) label value dim)))
                          (recur (cdr decls) (cons decl1 ax))))
                       (($ field name label value dim)
                        (let ((decl1 (field (gensym 'fld) label value dim)))
                          (recur (cdr decls) (cons decl1 ax))))
                       (($ external name label initial dim)
                        (let ((decl1 (external (gensym 'ext) label initial dim)))
                          (recur (cdr decls) (cons decl1 ax))))
                       (($ externalev name label initial dim)
                        (let ((decl1 (externalev (gensym 'extev) label initial dim)))
                          (recur (cdr decls) (cons decl1 ax))))
                       (($ function name formals expr)
                        (recur (cdr decls) (cons decl ax)))
                       (($ structural-event name label regime transition)
                        (let ((decl1 (make-structural-event (gensym 'sevn) label regime transition)))
                          (recur (cdr decls) (cons decl1 ax))))
                       (($ event name condition pos neg)
                        (let ((decl1 (make-event (gensym 'evn) condition pos neg)))
                          (recur (cdr decls) (cons decl1 ax))))
                       (else (recur (cdr decls) (cons decl ax)))
                       )
                ))
          ))
    )

  (define (model-env decls)

    (let recur ((decls decls) (env empty-env))

      (if (null? decls) env

          (let ((decl (car decls)))
      
            (if (pair? decl)
                
                (recur (append decl (cdr decls)) env)
                
                (match decl  
                       (($ discrete-variable name label value dim)
                        (recur (cdr decls)
                               (extend-env-with-binding env (gen-binding label decl))
                               ))
                       (($ variable name label value has-history dim)
                        (recur (cdr decls)
                               (extend-env-with-binding env (gen-binding label decl))
                               ))
                       (($ parameter name label value dim)
                        (recur (cdr decls)
                               (extend-env-with-binding env (gen-binding label decl))
                               ))
                       (($ declared-constant label content)
                        (recur (cdr decls)
                               (extend-env-with-binding env (gen-binding label content))
                               ))
                       (($ field name label value dim)
                        (recur (cdr decls)
                               (extend-env-with-binding env (gen-binding label decl))
                               ))
                       (($ external name label initial dim)
                        (recur (cdr decls)
                               (extend-env-with-binding env (gen-binding label decl))
                               ))
                       (($ externalev name label initial dim)
                        (recur (cdr decls)
                               (extend-env-with-binding env (gen-binding label decl))
                               ))
                       (($ function name formals expr)
                        (recur (cdr decls)
                               (extend-env-with-binding env (gen-binding name decl))
                               ))
                       (($ structural-event name label regime transition)
                        (recur (cdr decls)
                               (extend-env-with-binding env (gen-binding label (make-regime-variable name)))
                               ))
                       (($ unit name dims factor abbrevs)
                        (let* ((dimsval (eval-dim-expr dims #f (model-quantities)))
                               (factorval (eval-unit-factor factor (model-units)))
                               (factordim (dim-unit-factor factor (model-units)))
                               (unitdecl (make-unit name (make-quantity (gensym 'dim) dimsval) factorval abbrevs)))
                          (if (or (zero? factordim) (= dimsval factordim))
                              (model-units
                               (cons (cons name unitdecl) (model-units)))
                              (error 'elaborate "unit definition dimension mismatch" dims factor))
                          (recur (cdr decls) env)))
                       (else (recur (cdr decls) env))
                       ))
            ))
      ))

  (let ((decls (rename-decls (astdecls-decls model))))

    (let recur (
                (entries        decls)
                (env-stack      (extend-env-stack-with-binding
                                 (push-env-stack (model-env decls) empty-env-stack)
                                 (gen-binding (variable-label model-time) model-time)
                                 ))
                (definitions    '()) 
                (discrete-definitions '()) 
                (parameters     '())
                (fields         '())
                (externals      '())
                (externalevs    '())
                (equations      '())
                (initial        '())
                (conditions     '())
                (pos-responses  '())
                (neg-responses  '())
                (functions      '())
                (nodemap        empty-env)
                (regimemap      empty-env)
                (dimenv         empty-env)
                )
      
    (if (null? entries)
        (begin
          (make-equation-set model
                             (reverse definitions)
                             (reverse discrete-definitions)
                             (reverse parameters)
                             (reverse fields)
                             (reverse externals)
                             (reverse externalevs)
                             (reverse equations)
                             (map-equations replace-fixed initial)
                             (reverse conditions)
                             (reverse pos-responses)
                             (reverse neg-responses)
                             functions
                             nodemap
                             regimemap
                             dimenv
                             )
          )

        (let ((en (car entries)))
          (trace 'elaborate "en = ~A~%" en)

          (if (astdecls? en)

              (let ((decls1 (rename-decls (astdecls-decls en))))

                (recur (append decls1 (cons 'pop-env-stack (cdr entries)))
                       (push-env-stack (model-env decls1) env-stack)
                       definitions discrete-definitions parameters fields externals externalevs
                       equations initial conditions pos-responses neg-responses 
                       functions nodemap regimemap dimenv))

              (match en  

                     ('pop-env-stack
                      (recur (cdr entries) (pop-env-stack env-stack)
                             definitions discrete-definitions parameters fields externals externalevs
                             equations initial conditions pos-responses neg-responses 
                             functions nodemap regimemap dimenv
                             )
                      )

                     (($ variable name label value has-history dim)
                      (let* ((resolved-value (resolve value env-stack))
                             (en1 (make-variable name label resolved-value has-history dim)))
                        (trace 'elaborate "variable: name = ~A label = ~A value = ~A~%" name label value)
                        (recur (cdr entries) env-stack
                               (cons (cons name resolved-value) definitions)
                               discrete-definitions
                               parameters fields externals externalevs equations initial conditions 
                               pos-responses neg-responses functions
                               (extend-env-with-binding nodemap (gen-binding name en1))
                               regimemap
                               (extend-env-with-binding dimenv (gen-binding name dim))
                               )
                        ))

                     (($ discrete-variable name label value dim)
                      (let* ((resolved-value (resolve value env-stack))
                             (en1 (make-discrete-variable name label resolved-value dim)))
                        (recur (cdr entries) env-stack
                               definitions
                               (cons (cons name resolved-value) discrete-definitions)
                               parameters fields externals externalevs equations initial conditions 
                               pos-responses neg-responses functions
                               (extend-env-with-binding nodemap (gen-binding name en1))
                               regimemap
                               (extend-env-with-binding dimenv (gen-binding name dim))
                               )
                        ))

                     (($ parameter name label value dim)
                      (trace 'elaborate "parameter: label = ~A value = ~A~%" label value)
                      (let* ((resolved-value (resolve value env-stack))
                             (en1 (parameter name label resolved-value dim)))
                        (recur (cdr entries) env-stack
                               definitions discrete-definitions
                               (cons (cons name resolved-value) parameters) fields externals externalevs
                               equations initial conditions pos-responses neg-responses 
                               functions
                               (extend-env-with-binding nodemap (gen-binding name en1))
                               regimemap
                               (extend-env-with-binding dimenv (gen-binding name dim))
                               )
                        ))

                     (($ declared-constant label content)
                      (recur (cdr entries) env-stack
                             definitions discrete-definitions
                             parameters fields externals externalevs
                             equations initial conditions pos-responses neg-responses 
                             functions
                             nodemap
                             regimemap
                             dimenv
                             )
                      )

                     (($ field name label value dim)
                      (trace 'elaborate "field: label = ~A value = ~A~%" label value)
                      (let* ((resolved-value (resolve value env-stack))
                             (en1 (field name label resolved-value dim)))
                        (recur (cdr entries) env-stack
                               definitions discrete-definitions
                               parameters (cons (cons name resolved-value) fields) externals externalevs
                               equations initial conditions pos-responses neg-responses 
                               functions
                               (extend-env-with-binding nodemap (gen-binding name en1))
                               regimemap
                               (extend-env-with-binding dimenv (gen-binding name dim))
                               )
                        ))

                     (($ external name label initial-value dim)
                      (trace 'elaborate "external: label = ~A initial-value = ~A~%" label initial-value)
                      (let* ((resolved-initial (resolve initial-value env-stack))
                             (en1 (external name label resolved-initial dim)))
                        (recur (cdr entries) env-stack
                               definitions discrete-definitions
                               parameters fields (cons (cons name resolved-initial) externals) externalevs
                               equations initial conditions pos-responses neg-responses 
                               functions
                               (extend-env-with-binding nodemap (gen-binding name en1))
                               regimemap
                               (extend-env-with-binding dimenv (gen-binding name dim))
                               )
                        ))

                     (($ externalev name label initial-value dim)
                      (trace 'elaborate "externalevs: label = ~A initial-value = ~A~%" label initial-value)
                      (let* ((resolved-initial (resolve initial-value env-stack))
                             (en1 (externalev name label resolved-initial dim)))
                        (recur (cdr entries) env-stack
                               definitions discrete-definitions
                               parameters fields externals (cons (cons name resolved-initial) externalevs)
                               equations initial conditions pos-responses neg-responses 
                               functions
                               (extend-env-with-binding nodemap (gen-binding name en1))
                               regimemap
                               (extend-env-with-binding dimenv (gen-binding name dim))
                               )
                        ))

                     (($ equation s expr)
                      (trace 'elaborate "equation: unresolved rhs = ~A~%" expr)
                      (trace 'elaborate "equation: rhs = ~A~%" (resolve expr env-stack))
                      (recur (cdr entries) env-stack
                             definitions discrete-definitions parameters fields externals externalevs
                             (cons (list (resolve s env-stack) (resolve expr env-stack)) equations)
                             initial conditions pos-responses neg-responses 
                             functions nodemap regimemap dimenv
                             )
                      )
                     (($ initial-equation s expr)
                      (recur (cdr entries) env-stack
                             definitions discrete-definitions parameters fields  externals externalevs
                             equations (cons (cons s (resolve expr env-stack)) initial) 
                             conditions pos-responses neg-responses 
                             functions nodemap regimemap dimenv
                             )
                      )
                     (($ structural-event name label regime ($ transition event target condition pos))
                      (let (
                            (condition-index (length conditions))
                            )
                        (trace 'elaborate "structural-event: name = ~A label = ~A event = ~A ~%" name label event)
                        (trace 'elaborate "structural-event: target = ~A~%" target)
                        (recur (cdr entries) env-stack
                               definitions discrete-definitions parameters fields externals externalevs
                               (fold (lambda (eq ax) (merge-regime-eq name eq ax env-stack)) equations regime) initial
                               (cons (make-evcondition event (resolve condition env-stack)) conditions)
                               (append (cons
                                        (make-evresponse event (resolve-reinit event `(reinit ,(make-regime-variable name) #f)))
                                         (cons
                                          (make-evresponse event (resolve-reinit event `(reinit ,(resolve target env-stack) #t)))
                                          (map
                                           (lambda (x) 
                                             (make-evresponse event (resolve-reinit event (resolve x env-stack))))
                                           pos)))
                                         pos-responses)
                               neg-responses
                               functions 
                               nodemap
                               (extend-env-with-binding regimemap (gen-binding name condition-index))
                               dimenv
                               ))
                      )
                     (($ event name condition pos neg)
                      (recur (cdr entries) env-stack
                             definitions discrete-definitions parameters fields externals externalevs
                             equations initial 
                             (cons (make-evcondition name (resolve condition env-stack)) conditions) 
                             (append (map (lambda (x) 
                                            (make-evresponse name (resolve-reinit name (resolve x env-stack)))) pos) 
                                     pos-responses)
                             (or (and neg (append (map (lambda (x) 
                                                         (make-evresponse name (resolve-reinit name (resolve x env-stack)))) neg)
                                                  neg-responses))
                                 neg-responses)
                             functions nodemap regimemap dimenv
                             ))
                     (($ function name formals expr)
                      (recur (cdr entries) env-stack
                             definitions discrete-definitions parameters fields externals externalevs
                             equations initial conditions pos-responses neg-responses 
                             (cons (list name formals (resolve expr env-stack)) functions)
                             nodemap regimemap dimenv
                             ))
                     ((or (? unit?) (? quantity?))
                      (recur (cdr entries) env-stack
                             definitions discrete-definitions parameters fields externals externalevs
                             equations initial conditions pos-responses neg-responses 
                             functions nodemap regimemap dimenv
                             ))

                     (decl
                      (error 'elaborate "unknown declaration type" decl))
                     ))
          ))
    ))
  )



(define (function-formal-env formals env)
  (d 'function-formal-env "formals = ~A~%" formals)
  (match formals
         (($ pair-formal x rst)
          (let ((b (gen-binding (var-def-sym x) x)))
            (function-formal-env rst (extend-env-with-binding env b))))
         (($ null-formal)
          env)
         ))


(define (function-call-env formals args env)
  (d 'function-call-env "formals = ~A args = ~A~%" formals args)
  (match formals
         (($ pair-formal x frst)
          (match args
                 ((or ($ pair-arg xv arst) (xv . arst))
                  (let ((b (gen-binding (var-def-sym x) xv)))
                    (function-call-env frst arst (extend-env-with-binding env b))))
                 ((or ($ null-arg) ())
                  (error 'function-call-env
                         "function arity mismatch"))
                 ))
         (($ null-formal)
          (match args
                 ((or ($ null-arg) ()) env)
                 (else 
                  (error 'function-call-env
                         "function arity mismatch"))
                 ))
         ))


(define (subst-symbol sym env-or-env-stack)
  (let ((assoc-var-def   (if (env-stack? env-or-env-stack)
                             (env-stack-lookup sym env-or-env-stack)
                             (env-lookup sym env-or-env-stack))))
    (if assoc-var-def
        (binding-value assoc-var-def) 
        sym)))
        

(define (subst-symbol* sym env-or-env-stack)
  (let ((assoc-var-def   (if (env-stack? env-or-env-stack)
                             (env-stack-lookup sym env-or-env-stack)
                             (env-lookup sym env-or-env-stack))))
    (if assoc-var-def
        (binding-value assoc-var-def) 
        (error 'subst-symbol* "symbol not found" sym))))
        

(define (subst-if args env-stack)
  (map (lambda (x) (subst-expr x env-stack)) args))

(define (subst-reinit args env-stack)
  (cons (car args) (map (lambda (x) (subst-expr x env-stack)) (cdr args))))

(define (subst-cond args env-stack)
  (map (lambda (x) (subst-expr x env-stack)) args))

(define (subst-and args env-stack)
  (map (lambda (x) (subst-expr x env-stack)) args))

(define (subst-or args env-stack)
  (map (lambda (x) (subst-expr x env-stack)) args))

(define (subst-let args env-stack)
  (let ((lbnds (car args))
        (body (cadr args)))
    (list (map (lambda (lb) (cons (car lb) (subst-expr (cdr lb) env-stack))) lbnds)
          (subst-expr body env-stack))))

(define (subst-pair-arg arg env-stack)
  (match arg
         (($ pair-arg fst snd)
          (cons (subst-expr fst env-stack)
                (subst-pair-arg snd env-stack)))
         (($ null-arg)
          (list))))


(define (subst-function-call f arg env-stack)
  (d 'subst-function-call "f = ~A arg=~A~%" f arg)
  (if (symbol? f)
      (let ((args (subst-pair-arg arg env-stack)))
        `(signal.primop ,f . ,args))
      (match f (($ function name formals body)
                (let ((args (subst-pair-arg arg env-stack)))
                  (let ((fenv (function-call-env formals args empty-env)))
                    (trace 'subst-function-call "fenv = ~A~%" (map car (env->list fenv)))
                    (let ((expr1 (subst-expr body (push-env-stack fenv env-stack))))
                      (trace 'subst-function-call "expr1 = ~A~%" expr1)
                      expr1))
                  ))
             (($ free-variable name)
              (error 'subst-function-call "undefined function" name))
             (else
              (error 'subst-function-call "unknown callable object" f))
             )
      ))


(define (subst-expr e env-stack)
  (d 'subst-expr "e = ~A~%" e)
  (cond ((symbol? e)
         (subst-symbol e env-stack))
        ((pair? e)
         (let ((op (car e)) (args (cdr e)))
           (trace 'subst-expr "op = ~A args = ~A~%" op args)
           (case op
             ((signal.reinit) (cons 'signal.reinit (subst-reinit args env-stack)))
             ((signal.if)     (cons 'signal.if (subst-if args env-stack)))
             ((signal.cond)   (cons 'signal.cond (subst-cond args env-stack)))
             ((signal.and)    (cons 'signal.and (subst-and args env-stack)))
             ((signal.or)     (cons 'signal.or (subst-or args env-stack)))
             ((signal.let)    (cons 'signal.let (subst-let args env-stack)))
             ((signal.call)  
              (let ((f (car args))
                    (fargs (cadr args)))
                (trace 'subst-expr "f = ~A fargs = ~A~%" f args)
                (subst-function-call f fargs env-stack)
                ))
             ((signal.primop)  
              (let ((op (car args))
                    (opargs (cdr args)))
                `(signal.primop ,op . ,(map (lambda (x) (subst-expr x env-stack)) opargs))))
             (else e)
             )))
        ((var-def? e)
         (subst-symbol* (var-def-sym e) env-stack))
        ((free-variable? e)
         (subst-symbol (free-variable-name e) env-stack))
        ((left-var? e)
         (subst-expr (left-var-u e) env-stack))
        (else e)
        ))

(define (units-unify u1 u2)
  (cond ((and (unit? u1) (unit? u2))
         (unit-equal? u1 u2))
        ((and (equal? u1 'unitbottom) (equal? u2 'unitbottom)) #t)
        ((and (unit? u2) (equal? u1 'unitbottom)) #t)
        ((and (unit? u1) (equal? u2 'unitbottom)) #t)
        (else #f)))

(define (units-reinit args env)
  (let ((x (cadr args))
        (y (caddr args)))
    (let ((xu (expr-units x env))
          (yu (expr-units y env)))
      (if (units-unify xu yu)
          xu
          (error 'units-reinit "units mismatch in reinit" args))
      ))
  )

        
(define (units-if args env)
  (match args
         ((c x y)
          (let ((cu (expr-units c env))
                (xu (expr-units x env))
                (yu (expr-units y env)))
            (trace 'units-if "cu = ~A xu = ~A yu = ~A~%" cu xu yu)
            ;; TODO: this is not necessary if regime equations take
            ;; into account units of quantities
            (cond 
             ((units-unify xu yu) (if (unit? xu) xu yu))
             ((and (not (unitless? xu)) (unitless? yu)) xu)
             ((and (unitless? xu) (not (unitless? yu))) yu)
             (else (error 'units-reinit "units mismatch in if" args)))
          ))
  (else (error 'units-reinit "invalid arguments to if" args))
  ))


(define (units-let args env)
  (let ((lbnds (car args))
        (body (cadr args)))
    (let ((env1
           (fold
            (lambda (lb env) 
              (extend-env-with-binding 
               env (gen-binding (car lb) (expr-units (cdr lb) env))))
            env
            lbnds)))
          (expr-units body env1))))


(define (units-pair-arg arg env)
  (match arg
         (($ pair-arg fst snd)
          (cons (expr-units fst env)
                (expr-units snd env)))
         (($ null-arg)
          (list))))


(define (units-primop f args units-args)
  (d 'units-primop "f = ~A units-args = ~A args = ~A~%" f units-args args)
  (let ((u (case f 
             ((signal.add signal.sub) 
              (if (units-unify (car units-args) (cadr units-args))
                  (car units-args) 
                  (error 'units-primop "units mismatch" f units-args
                         (quantity-int (unit-dims (car units-args)))
                         (quantity-int (unit-dims (cadr units-args))))))
             ((signal.mul) 
              (unit* (car units-args) (cadr units-args) ))
             ((signal.div) 
              (unit/ (car units-args) (cadr units-args) ))
             ((signal.pow) 
              (unit-expt (car units-args) (constant-value (cadr args) )))
             ((signal.neg) 
              (car units-args))
             (else unitless))))
    (trace 'units-primop "f = ~A args = ~A units-args = ~A units-args-dims = ~A u = ~A dims(u) = ~A~%" 
           f args units-args (map (compose quantity-int unit-dims) units-args) u (quantity-int (unit-dims u)))
    u))


(define (units-function-call f arg env)
  (d 'units-function-call "f = ~A arg = ~A~%" f arg)
  (if (symbol? f)
      (let ((units-args (units-pair-arg arg env)))
        (trace 'units-function-call "units-args = ~A~%" units-args)
        (units-primop f arg units-args))
      (match-let
       ((($ function name formals body) f))
       (let ((fenv (function-call-env formals arg empty-env)))
         (trace 'units-function-call "fenv = ~A~%" fenv)
         (let ((u (expr-units 
                   (subst-expr body (push-env-stack fenv empty-env-stack))
                   env)))
           (trace 'units-function-call "u = ~A~%" u)
           u)
         ))
      ))


(define (expr-units e env)
  (d 'expr-units "e = ~A~%" e)
  (cond ((symbol? e)
         (let ((u (assv e (model-units))))
           (trace 'expr-units "(symbol? e) => e = ~A u = ~A dims(u) = ~A~%" e u (and (unit? u) (quantity-int (unit-dims u))))
           (or (and u (cdr u)) (env-lookup e env) unitless)))
        ((pair? e)
         (let ((op (car e)) (args (cdr e)))
           (let ((u (case op
                      ((signal.primop) 
                       (units-primop (car args) (cdr args) 
                                     (map (lambda (e) (expr-units e env)) (cdr args))))
                      ((signal.reinit) 
                       (units-reinit args env))
                      ((signal.if)     
                       (units-if args env))
                      ((signal.let)    
                       (units-let args env))
                      ((signal.call)  
                       (let ((f (car args))
                             (fargs (cadr args)))
                         (units-function-call f fargs env)
                         ))
                      (else e)
                      )))
             (trace 'expr-units "(pair? e) => op = ~A args = ~A u = ~A dims(u) = ~A~%" e args u (and (unit? u) (quantity-int (unit-dims u))))
             u)))
        ((var-def? e)
         (let ((u (expr-units (subst-symbol (var-def-sym e) env) env)))
           (trace 'expr-units "(var-def? e) => e = ~A u = ~A dims(u) = ~A~%" e u (and (unit? u) (quantity-int (unit-dims u))))
           u))
        ((free-variable? e)
         (error 'expr-units "unknown variable" (free-variable-name e)))
        ((variable? e)
         (let ((u (expr-units (variable-value e) env)))
           (trace 'expr-units "(variable? e) => e = ~A u = ~A dims(u) = ~A~%" e u (and (unit? u) (quantity-int (unit-dims u))))
           u))
        ((discrete-variable? e)
         (let ((u (expr-units (discrete-variable-value e) env)))
           (trace 'expr-units "(discrete-variable? e) => e = ~A u = ~A dims(u) = ~A~%" e u (and (unit? u) (quantity-int (unit-dims u))))
           u))
        ((parameter? e)
         (let ((u (expr-units (parameter-value e) env)))
           (trace 'expr-units "(parameter? e) => e = ~A u = ~A dims(u) = ~A~%" e u (and (unit? u) (quantity-int (unit-dims u))))
           u))
        ((field? e)
         (let ((u (expr-units (field-value e) env)))
           (trace 'expr-units "(field? e) => e = ~A u = ~A dims(u) = ~A~%" e u (and (unit? u) (quantity-int (unit-dims u))))
           u))
        ((external? e)
         (let ((u (expr-units (external-initial e) env)))
           (trace 'expr-units "(external? e) => e = ~A u = ~A dims(u) = ~A~%" e u (and (unit? u) (quantity-int (unit-dims u))))
           u))
        ((left-var? e)
         (let ((u (expr-units (left-var-u e) env)))
           (trace 'expr-units "(left-var? e) => e = ~A u = ~A dims(u) = ~A~%" e u (and (unit? u) (quantity-int (unit-dims u))))
           u))
        ((constant? e)
         (let ((u (constant-unit e)))
           (trace 'expr-units "(constant? e) => e = ~A u = ~A dims(u) = ~A~%" e u (and (unit? u) (quantity-int (unit-dims u))))
           u))
        (else e))
  )
         

(define (reduce-expr expr indexmaps )
  (let (
        (pindexmap  (alist-ref 'pindexmap indexmaps))
        (cindexmap  (alist-ref 'cindexmap indexmaps))
        (dindexmap  (alist-ref 'dindexmap indexmaps))
        (rindexmap  (alist-ref 'rindexmap indexmaps))
        (evindexmap (alist-ref 'evindexmap indexmaps))
        (extindexmap (alist-ref 'extindexmap indexmaps))
        (extevindexmap (alist-ref 'extevindexmap indexmaps))
        )
  (d 'reduce-expr "expr = ~A~%" expr)
  (match expr

         (($ pair-arg fst snd)
          (make-pair-arg
           (reduce-expr fst indexmaps)
           (reduce-expr snd indexmaps)))

         (($ derivative-variable y)
          (let ((yindex (env-lookup (variable-name y) cindexmap)))
            (if (not yindex)
                (error 'reduce-expr "variable not in index" y)
                `(getindex dy ,(cdr yindex)))
            ))
         
         (($ left-var y)
          (cond ((variable? y)
                 (let ((yindex (env-lookup (variable-name y) cindexmap)))
                   (if (not yindex)
                       (error 'reduce-expr "variable not in index" y)
                       `(getindex y ,(cdr yindex)))
                   ))
                ((discrete-variable? y)
                 (let ((yindex (env-lookup (discrete-variable-name y) dindexmap)))
                   (if (not yindex)
                       (error 'reduce-expr "variable not in index" y)
                       `(getindex d ,(cdr yindex)))
                   ))
                ((regime-variable? y)
                 (let ((yindex (env-lookup (regime-variable-name y) rindexmap)))
                   (if (not yindex)
                       (error 'reduce-expr "regime variable not in index" y)
                       `(getindex r ,(cdr yindex)))
                   ))
                (else 
                 (error 'reduce-expr "unknown left variable type" expr))
                ))
         
         (($ variable name label value has-history dim)
          (let ((yindex (env-lookup name cindexmap)))
            (if (not yindex)
                (if (equal? name (variable-name model-time))
                    label
                    (error 'reduce-expr "variable not in index" name))
                `(getindex y ,(cdr yindex)))
            ))

         (($ reduce-variable op y)
          (let ((yindex (env-lookup (variable-name y) cindexmap)))
            (if (not yindex)
                (error 'reduce-expr "variable not in index" y)
                `(getindex y ,(cdr yindex)))
            ))
         
         (($ discrete-variable name label value dim)
          (let ((dindex (env-lookup name dindexmap)))
            (if (not dindex)
                (error 'reduce-expr "discrete variable not in index" name)
                `(getindex d ,(cdr dindex)))
            ))

         (($ regime-variable name label value)
          (let ((rindex (env-lookup name rindexmap)))
            (if (not rindex)
                (error 'reduce-expr "regime variable not in index" name)
                `(getindex r ,(cdr rindex)))
            ))

         (($ parameter name label value dim)
          (let ((yindex (env-lookup name pindexmap)))
            (trace 'reduce-expr "pindexmap = ~A~%" pindexmap)
            (if (not yindex)
                (error 'reduce-expr "parameter not in index" name)
                `(getindex p ,(cdr yindex)))
            ))

         (($ field name label value dim)
          (let ((yindex (env-lookup name pindexmap)))
            (trace 'reduce-expr "pindexmap = ~A~%" pindexmap)
            (if (not yindex)
                (error 'reduce-expr "parameter not in index" name)
                `(getindex p ,(cdr yindex)))
            ))

         (($ external name label initial dim)
          (let ((yindex (env-lookup name extindexmap)))
            (trace 'reduce-expr "extindexmap = ~A~%" extindexmap)
            (if (not yindex)
                (error 'reduce-expr "external not in index" name)
                `(ext ,(cdr yindex) ,(variable-name model-time)))
            ))

         (($ externalev name label initial dim)
          (let ((yindex (env-lookup name extevindexmap)))
            (trace 'reduce-expr "extevindexmap = ~A~%" (env->list extevindexmap))
            (if (not yindex)
                (error 'reduce-expr "external event not in index" name)
                `(extev ,(cdr yindex) ,(variable-label model-time)))
            ))

         (('signal.if ifcond ift iff)
          (let ((expr1 (reduce-expr ifcond indexmaps))
                (expr2 (reduce-expr ift indexmaps))
                (expr3 (reduce-expr iff indexmaps)))
            (if (and (constant? expr2) (constant? expr3)
                     (= (constant-value expr2) (constant-value expr3)))
                ift `(signal.if ,expr1 ,expr2 ,expr3))
            ))

         (('signal.let lbnds body)
          (subst-expr
           `(signal.let 
             ,(map (lambda (lb) (cons (car lb) (reduce-expr (cdr lb) indexmaps))) lbnds)
             ,(reduce-expr body indexmaps))
           empty-env-stack))

         (('signal.call f args)
          (reduce-expr
           (subst-expr 
            `(signal.call 
              ,f ,(reduce-expr args indexmaps))
            empty-env-stack)
           indexmaps))

         (('signal.reinit e x y)
          (let ((eindex (env-lookup e evindexmap)))
            (subst-expr 
             `(signal.reinit (getindex c ,(cdr eindex))
                             ,(reduce-expr x indexmaps) 
                             ,(reduce-expr y indexmaps))
             empty-env-stack)))

         ((op . args)
           (subst-expr
            (cons op (map (lambda (x) (reduce-expr x indexmaps)) args))
            empty-env-stack))

         (else (subst-expr expr empty-env-stack))

         ))
)


;; reduce expr for constant expressions
(define (reduce-constant-expr expr indexmaps)
  (let (
        (pindexmap  (alist-ref 'pindexmap indexmaps))
        (cindexmap  (alist-ref 'cindexmap indexmaps))
        (dindexmap  (alist-ref 'dindexmap indexmaps))
        (evindexmap (alist-ref 'evindexmap indexmaps))
        )

  (d 'reduce-constant-expr "expr = ~A~%" expr)
  (match expr

         (($ pair-arg fst snd)
          (make-pair-arg
           (reduce-constant-expr fst indexmaps)
           (reduce-constant-expr snd indexmaps)))

         (($ variable name label value has-history dim)
          (reduce-constant-expr value indexmaps))

         (($ parameter name label value dim)
          (reduce-constant-expr value indexmaps))

         (($ field name label value dim)
          (reduce-constant-expr value indexmaps))

         (('signal.let lbnds body)
          (subst-expr
           `(signal.let 
             ,(map (lambda (lb) (cons (car lb) (reduce-constant-expr (cdr lb) indexmaps))) lbnds)
             ,(reduce-constant-expr body indexmaps))
           empty-env-stack))

         (('signal.call f args)
          (subst-expr 
           `(signal.call ,f ,(reduce-constant-expr args indexmaps))
           empty-env-stack))

         ((op . args)
          (subst-expr
           (cons op (map (lambda (x) (reduce-constant-expr x indexmaps)) args))
           empty-env-stack))

         (else expr)
         ))
)


(define (reduce-eq eq indexmaps unit-env)

  (define (make-call fn a b)
    `(signal.call ,fn ,(make-pair-arg a (make-pair-arg b (make-null-arg)))))

  (let (
        (pindexmap  (alist-ref 'pindexmap indexmaps))
        (cindexmap  (alist-ref 'cindexmap indexmaps))
        (dindexmap  (alist-ref 'dindexmap indexmaps))
        (evindexmap (alist-ref 'evindexmap indexmaps))
        (rindexmap  (alist-ref 'rindexmap indexmaps))
        )
    (d 'reduce-eq "eq = ~A~%" eq)

    (match eq

           ((($ derivative-variable y) rhs)
            (let ((yindex (env-lookup (variable-name y) cindexmap))
                  (dim (variable-dim y)))
              (if (not yindex)
                  (error 'reduce-eq "variable not in index" y)
                  (let ((expr (reduce-expr rhs indexmaps))
                        (ddim (if (equal? (quantity-int dim) (quantity-int Unity))
                                  (quantity-int dim)
                                  (- (quantity-int dim) (quantity-int Time)))))
                    (trace 'reduce-eq "dim = ~A ddim = ~A rhs = ~A ~%" dim ddim rhs)
                    (let ((rhs-units (expr-units rhs unit-env)))
                      (trace 'reduce-eq "rhs-units = ~A dims(rhs-units) = ~A ~%" rhs-units (unit-dims rhs-units))
                      (if (equal? ddim (quantity-int (unit-dims rhs-units)))
                          `(setindex dy_out ,(cdr yindex) ,expr)
                          (error 'reduce-eq "dimension mismatch in rhs" 
                                 (variable-label y) ddim (unit-dims rhs-units)))
                    ))
              ))
           )
           
           ((($ variable name label value has-history dim) rhs)
            (let ((yindex (env-lookup name cindexmap)))
              (if (not yindex)
                  (error 'reduce-eq "variable not in index" name)
                  (let ((expr (reduce-expr rhs indexmaps)))
                    (let ((rhs-units (expr-units rhs unit-env)))
                      (trace 'reduce-eq "rhs = ~A~%" rhs)
                      (trace 'reduce-eq "dim = ~A dims(rhs-units) = ~A ~%" 
                             dim (and (unit? rhs-units) (unit-dims rhs-units)))
                      (if (or (equal? rhs-units 'unitbottom) (equal? dim (unit-dims rhs-units)))
                          `(setindex y_out ,(cdr yindex) ,expr)
                          (error 'reduce-eq "dimension mismatch in rhs" 
                                 name label
                                 dim (unit-dims rhs-units)))
                          ))
                    ))
              )

           ((($ reduce-variable op y) rhs)
            (let ((yindex (env-lookup (variable-name y) cindexmap))
                  (dim (variable-dim y)))
              (if (not yindex)
                  (error 'reduce-eq "variable not in index" y)
                  (let ((expr (reduce-expr (make-call op `(getindex y ,(cdr yindex)) rhs) indexmaps)))
                    (let ((rhs-units (expr-units rhs unit-env)))
                      (trace 'reduce-eq "dim = ~A dims(rhs-units) = ~A ~%" dim (unit-dims rhs-units))
                      (if (equal? dim (unit-dims rhs-units))
                          `(reduceindex y_out ,(cdr yindex) ,expr)
                          (error 'reduce-eq "dimension mismatch in rhs" 
                                 (variable-name y)
                                 (variable-label y)
                                 dim (unit-dims rhs-units)))
                      ))
                  ))
            )
           
           (($ evcondition name rhs)
            (let ((evindex (env-lookup name evindexmap))
                  (expr (reduce-expr rhs indexmaps)))
              (if (not evindex)
                  (error 'reduce-eq "event identifier not found in indexmap" name))
              (trace 'reduce-eq "evcondition name = ~A~%" name)
              (trace 'reduce-eq "expr = ~A~%" expr)
              (if (expr-units rhs unit-env)
                  `(setindex c_out ,(cdr evindex) ,expr)
                  (error 'reduce-eq "dimension mismatch in condition" expr))
              ))
           
           (($ evresponse name rhs)
            (trace 'reduce-eq "evresponse name = ~A rhs = ~A~%" name rhs)
            (let ((y (match rhs
                            (('signal.reinit e ($ left-var u) yindex . rest) u)
                            (error 'reduce-eq "unknown event response equation" eq))))
              (cond
               ((variable? y)
                (let (
                      (yindex (env-lookup (variable-name y) cindexmap))
                      (dim (variable-dim y))
                      )
                  (if (not yindex)
                      (error 'reduce-eq "variable not in index" y)
                      (let ((expr (reduce-expr rhs indexmaps)))
                        (if (equal? dim (unit-dims (expr-units rhs unit-env)))
                            `(setindex y_out ,(cdr yindex) ,expr)
                            (error 'reduce-eq "variable dimension mismatch in event response" 
                                   y dim (unit-dims (expr-units rhs unit-env))))
                        ))
                  ))
               ((discrete-variable? y)
                (let (
                      (yindex (env-lookup (discrete-variable-name y) dindexmap))
                      (dim (discrete-variable-dim y))
                      )
                  (if (not yindex)
                      (error 'reduce-eq "variable not in index" y)
                      (let ((expr (reduce-expr rhs indexmaps)))
                        (if (equal? dim (unit-dims (expr-units rhs unit-env)))
                            `(setindex d_out ,(cdr yindex) ,expr)
                            (error 'reduce-eq "variable dimension mismatch in event response" y))
                        ))
                  ))
               ((regime-variable? y)
                (let (
                      (rindex (env-lookup (regime-variable-name y) rindexmap))
                      )
                  (if (not rindex)
                      (error 'reduce-eq "regime variable not in index" y)
                      (let ((expr (reduce-expr rhs indexmaps)))
                        `(setindex r_out ,(cdr rindex) ,expr)
                        ))
                  ))
               (else (error 'reduce-eq "unknown variable type in reinit equation" eq))
               ))
            )

           (else eq)))
  )          


(define (simcreate eqset)

  (let* ((nodemap    (equation-set-nodemap eqset))
         (regimemap  (equation-set-regimemap eqset))
         (dimenv     (equation-set-dimenv eqset))
         (conditions (equation-set-conditions eqset))

         (nodelst   (reverse (env->list nodemap)))
         (unit-env
          (fold (lambda (p env)
                  (let* ((name (car p)) (rhs (cdr p))
                         (rhs-units (expr-units rhs env)))
                    (if (equal? (quantity-int (binding-value (env-lookup name dimenv)))
                                (quantity-int (unit-dims rhs-units)))
                        (extend-env-with-binding
                         env (gen-binding name rhs-units))
                        (error 'simcreate "dimension mismatch in initial value" 
                               name (binding-value (env-lookup name dimenv))
                               rhs rhs-units))
                    ))
                empty-env 
                (append
                 (equation-set-parameters eqset)
                 (equation-set-fields eqset)
                 (equation-set-externals eqset)
                 (equation-set-externalevs eqset))))

         (cindexmap 
           (let recur ((nodelst nodelst)
                       (indexmap  empty-env)
                       (index     0))
             (if (null? nodelst)
                 indexmap
                 (let ((node (car nodelst)))
                   (if (variable? (cdr node))
                       (recur (cdr nodelst)
                              (extend-env-with-binding indexmap (gen-binding (car node) index))
                              (+ 1 index))
                       (recur (cdr nodelst) indexmap index))))
             ))
          
         (dindexmap 
           (let recur ((nodelst nodelst)
                       (indexmap  empty-env)
                       (index     0))
             (if (null? nodelst)
                 indexmap
                 (let ((node (car nodelst)))
                   (if (discrete-variable? (cdr node))
                       (recur (cdr nodelst)
                              (extend-env-with-binding indexmap (gen-binding (car node) index))
                              (+ 1 index))
                       (recur (cdr nodelst) indexmap index))))
             ))
          

         (pindexmap 
           (let recur ((nodelst nodelst)
                       (indexmap  empty-env)
                       (index     0))
             (if (null? nodelst)
                 indexmap
                 (let ((node (car nodelst)))
                   (cond ((parameter? (cdr node))
                          (recur (cdr nodelst)
                                 (extend-env-with-binding indexmap (gen-binding (car node) index))
                                 (+ 1 index)))

                          ((field? (cdr node))
                           (recur (cdr nodelst)
                                  (extend-env-with-binding indexmap (gen-binding (car node) index))
                                  (+ 1 index)))

                         (else
                          (recur (cdr nodelst) indexmap index)))))
             ))
          
         (evindexmap 
           (let recur ((conditions conditions)
                       (indexmap  empty-env)
                       (index     0))
             (if (null? conditions)
                 indexmap
                 (recur (cdr conditions)
                        (extend-env-with-binding indexmap (gen-binding (evcondition-name (car conditions)) index))
                        (+ 1 index)))
             ))

         (extindexmap 
           (let recur ((nodelst nodelst)
                       (indexmap  empty-env)
                       (index     0))
             (if (null? nodelst)
                 indexmap
                 (let ((node (car nodelst)))
                   (cond ((external? (cdr node))
                          (recur (cdr nodelst)
                                 (extend-env-with-binding indexmap (gen-binding (car node) index))
                                 (+ 1 index)))

                         (else
                          (recur (cdr nodelst) indexmap index)))))
             ))
          
         (extevindexmap 
           (let recur ((nodelst nodelst)
                       (indexmap  empty-env)
                       (index     0))
             (if (null? nodelst)
                 indexmap
                 (let ((node (car nodelst)))
                   (cond ((externalev? (cdr node))
                          (recur (cdr nodelst)
                                 (extend-env-with-binding indexmap (gen-binding (car node) index))
                                 (+ 1 index)))

                         (else
                          (recur (cdr nodelst) indexmap index)))))
             ))
          

         (indexmaps `((cindexmap  . ,cindexmap)
                      (pindexmap  . ,pindexmap)
                      (dindexmap  . ,dindexmap)
                      (rindexmap  . ,regimemap)
                      (evindexmap . ,evindexmap)
                      (extindexmap . ,extindexmap)
                      (extevindexmap . ,extevindexmap)
                      ))
         
         (param-block 
          (map (lambda (x) (reduce-constant-expr (cdr x) indexmaps))
               (append (equation-set-parameters eqset)
                       (equation-set-fields eqset))))

         (init-block 
          (map (lambda (x) (reduce-expr (cdr x) indexmaps))
               (equation-set-definitions eqset)))

         (discrete-init-block
          (map (lambda (x) (reduce-expr (cdr x) indexmaps))
               (equation-set-discrete-definitions eqset)))
         
         (external-init-block
          (map (lambda (x) (reduce-expr (cdr x) indexmaps))
               (equation-set-externals eqset)))
         
         (externalev-init-block
          (map (lambda (x) (reduce-expr (cdr x) indexmaps))
               (equation-set-externalevs eqset)))

         (eq-block
          (map (lambda (x) (reduce-eq x indexmaps unit-env)) 
               (equation-set-equations eqset)))
         
         (cond-block
          (map (lambda (c) (reduce-eq c indexmaps unit-env))
               (equation-set-conditions eqset)))

         (pos-responses 
          (map (lambda (x) (reduce-eq x indexmaps unit-env))
               (equation-set-pos-responses eqset)))

         (neg-responses 
          (map (lambda (x) (reduce-eq x indexmaps unit-env))
               (equation-set-neg-responses eqset)))

         )

    (make-simruntime 
     eqset
     cindexmap dindexmap 
     evindexmap regimemap
     extindexmap extevindexmap
     param-block
     init-block
     discrete-init-block
     external-init-block
     externalev-init-block
     eq-block 
     cond-block
     pos-responses
     neg-responses)

    )
  )



        

)

