 
;;
;; Hybrid dynamical systems modeling.
;;
;; Copyright 2015 Ivan Raikov and the University of California, Irvine.
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

         parse elaborate simcreate codegen-ODE codegen-ODE/ML
         verbose

	 )

	(import scheme chicken)
        
	(require-extension matchable datatype lalr-driver mathh unitconv with-units)
	(require-library data-structures extras srfi-1 srfi-13)
	(import (only srfi-1 zip fold fold-right filter filter-map list-tabulate every)
                (only srfi-13 string-null? string-concatenate string<)
		(only data-structures ->string alist-ref conc intersperse compose sort)
                (only extras pp fprintf)
                (only ports with-output-to-port)
		)


(define-unit-prefix    milli second ms)
(define-unit-prefix    milli volt    mV)
(define-unit-prefix    milli amp     mA)
(define-unit-prefix    pico  amp     pA)
(define-unit-prefix    nano  amp     nA)
(define-unit-prefix    micro amp     uA)
(define-unit-prefix    micro siemens uS)
(define-unit-prefix    milli siemens mS)
(define-unit-prefix    milli mole    mM)
(define-unit-prefix    mega ohm      )
(define-unit-prefix    kilo ohm      kohm)
 
(define-quantity   CurrentDensity        (/ Current Area))
(define-quantity   CapacitanceDensity    (/ Capacitance Area))
(define-quantity   ConductanceDensity    (/ Conductance Area))
(define-quantity   Resistivity           (* Resistance Length))
(define-quantity   ReactionRate1         (** Time -1))
(define-quantity   ReactionRate2         (* (** Substance -1) (** Time -1)))
(define-quantity   InversePotential      (** Potential -1))
(define-quantity   InversePotentialTime  (* (** Potential -1) (** Time -1)))

(define-unit milliamp-per-square-centimeter   CurrentDensity  (/ mA (* cm cm)) mA/cm2)
(define-unit microfarad-per-square-centimeter CapacitanceDensity (/ uF (* cm cm)) uf/cm2)
(define-unit siemens-per-square-centimeter    ConductanceDensity (/ S (* cm cm)) S/cm2)
(define-unit ohm.cm                           Resistivity     (* ohm cm) ohm.cm)

(define-unit degC    Temperature      1.0)
(define-unit /ms     ReactionRate1    1000.0)
(define-unit /mM-ms  ReactionRate2    1000000.0)
(define-unit /mV     InversePotential 1000.0)
(define-unit /mV-ms  InversePotentialTime 1000000.0)

(define model-units
  (make-parameter
   (map cons
    `(ms mV mA/cm2 pA nA uA mA mM uF uf/cm2 um S/cm2 uS mS ohm.cm ohm kohm megaohm degC /ms /mM-ms /mV /mV-ms)
    (list ms mV mA/cm2 pA nA uA mA mM uF uf/cm2 um S/cm2 uS mS ohm.cm ohm kohm megaohm degC /ms /mM-ms /mV /mV-ms))
   ))

(define model-quantities
  (make-parameter
   (map cons
    `(Time Area Volume Temperature
      Potential Current Capacitance Conductance Resistance
      CurrentDensity
      CapacitanceDensity
      ConductanceDensity
      Resistivity
      ReactionRate1
      ReactionRate2
      InversePotential
      InversePotentialTime
      )
    (list Time Area Volume Temperature
          Potential Current Capacitance Conductance Resistance
          CurrentDensity
          CapacitanceDensity
          ConductanceDensity
          Resistivity
          ReactionRate1
          ReactionRate2
          InversePotential
          InversePotentialTime
          ))
   ))

(include "mathh-constants.scm")
(include "parser.scm")
(include "env.scm")
(include "codegen.scm")


(define nl "\n")
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


(define verbose (make-parameter 1))


(define (d loc fstr . args)
  (let ([port (current-error-port)])
    (if (positive? (verbose)) 
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


(define-record-type regime-flag
  (make-regime-flag name)
  regime-flag?
  (name       regime-flag-name)
  )

(define-record-printer (regime-flag x out)
  (fprintf out "#(regime ~S)"
	   (regime-flag-name x)
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
  (make-transition target condition response)
  transition?
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


(define (expr-value x)
  (cond ((variable? x) (variable-value x))
        ((ref-var? x) (vector-ref (ref-var-u x) (ref-var-idx x)))
        ((list? x) (eval `(,(car x) . ,(map expr-value x))))
        (else x)))


(define (reinit e x y)
  (cond ((left-var? x) `(signal.reinit ,e ,x ,y))
        ((variable? x)  (reinit e (left-var x) y))
        ((ref-var? x)   (reinit e (left-var x) y))
        ((derivative-variable? x)   
         (reinit e (left-var (derivative-variable-parent x)) y))
        ((discrete-variable? x)   
         (reinit e (left-var x) y))
        (else (error 'reinit "invalid argument to reinit" x))
        ))


(define math-constant-env
  (fold (lambda (k v env)
          (let ((binding (gen-binding k v)))
            (extend-env-with-binding env binding)))
          empty-env
        `(E 1/E E^2 E^PI/4 LOG2E LOG10E LN2 LN3 LNPI LN10 1/LN2 1/LN10 PI PI/2
            PI/4 1/PI 2/PI 2/SQRTPI SQRTPI PI^2 DEGREE SQRT2 1/SQRT2 SQRT3 SQRT5
            SQRT10 CUBERT2 CUBERT3 4THRT2 GAMMA1/2 GAMMA1/3 GAMMA2/3 PHI LNPHI
            1/LNPHI EULER E^EULER SIN1 COS1 ZETA3)
        (map (lambda (x) (constant 'number x unitless))
             (list E 1/E E^2 E^PI/4 LOG2E LOG10E LN2 LN3 LNPI LN10 1/LN2 1/LN10 PI PI/2
                   PI/4 1/PI 2/PI 2/SQRTPI SQRTPI PI^2 DEGREE SQRT2 1/SQRT2 SQRT3 SQRT5
                   SQRT10 CUBERT2 CUBERT3 4THRT2 GAMMA1/2 GAMMA1/3 GAMMA2/3 PHI LNPHI
                   1/LNPHI EULER E^EULER SIN1 COS1 ZETA3))
        ))
     

(define math-binop-env
  (let ((function-names `(+ - * / pow max min ==))
        (op-names `(signal.add signal.sub signal.mul signal.div signal.pow signal.max signal.min signal.eqnum)))
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
               round ceiling floor))
        (op-names
         `(signal.neg signal.abs signal.atan signal.asin signal.acos signal.sin signal.cos signal.exp signal.ln
                      signal.sqrt signal.tan signal.cosh signal.sinh signal.tanh signal.hypot signal.gamma signal.lgamma signal.log10
                      signal.log2 signal.log1p signal.ldexp signal.cube
                      signal.round signal.ceiling signal.floor)))
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
  


;; A representation of a flattened model, normally created with
;; `elaborate(model)`. The code generation procedures take an
;; elaborated model as input.
;;
;; Contains the hierarchical equations, flattened equations, flattened
;; initial equations, events, event response functions, and a map of
;; nodes.


(define-record-type equation-set
  (make-equation-set model definitions discrete-definitions parameters equations 
                     initial conditions pos-responses neg-responses 
                     functions nodemap regimemap dimenv)
  equation-set?
  (model equation-set-model)
  (definitions equation-set-definitions)
  (discrete-definitions equation-set-discrete-definitions)
  (parameters equation-set-parameters)
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
    (equations=,(equation-set-equations x))
    (functions=,(equation-set-functions x))
    (conditions=,(equation-set-conditions x))
    (pos-responses=,(equation-set-pos-responses x))
    (neg-responses=,(equation-set-neg-responses x))
    (nodemap=,(equation-set-nodemap x))
    (regimemap=,(equation-set-regimemap x))
    )))


;; runtime representation of a simulation object
(define-record-type simruntime
  (make-simruntime eqset cindexmap dindexmap evindexmap parameters defs eqblock condblock posresp negresp)
  simruntime?
  (eqset simruntime-eqset)
  (cindexmap simruntime-cindexmap)
  (dindexmap simruntime-dindexmap)
  (evindexmap simruntime-evindexmap)
  (parameters simruntime-parameters)
  (defs simruntime-definitions)
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
          (parameters=,(simruntime-parameters x))
          (defs=,(simruntime-definitions x))
          (eqblock=,(simruntime-eqblock x))
          (evblock=,(simruntime-condblock x))
          (posresp=,(simruntime-posresp x))
          (negresp=,(simruntime-negresp x))
          )
        out)
    )
    


(define model-time (unknown (constant 'number 0.0 millisecond) 't millisecond))


(define (map-equations f eqs)
  (map 
   (match-lambda*
    [($ initial-equation s expr) 
     (init s (f expr))]
    [($ equation s expr) 
     (eq s (f expr))]
    [eq (error 'map-equation "unknown equation type" eq)])
   eqs))
      


(define (replace-fixed expr)
  (map
   (lambda (x)
     (cond ((variable? x) (or (variable-value x) x))
           ((list? x) `(,(car x) . ,(map replace-fixed (cdr x))))
           (else x)))
   expr))



;; Equation parsing

(define (parse decls) 
  (d 'parse "decls = ~A~%" decls)
  (let ((initial-env
         (fold (lambda (ext-env env)
                 (extend-env-with-env env ext-env))
               empty-env
               (list math-constant-env
                     math-binop-env
                     math-unop-env
                     ))))
    (map (lambda (x) (parse-declaration initial-env x)) decls)
    ))


;; Expression resolution

(define (resolve expr env-stack)

  (d 'resolve "expr: ~A env-stack: ~A~%" 
     expr (map (lambda (x) (map car x)) env-stack))

  (let recur ((e expr))

    (d 'resolve "e: ~A~%" e)

    (cond
     
     ((free-variable? e)
      (let ((assoc-var-def (env-stack-lookup (free-variable-name e) env-stack)))
        (d 'resolve "free-var: e = ~A assoc-var-def = ~A~%" (free-variable-name e) assoc-var-def)
        (if assoc-var-def
            (let recur ((resval (binding-value assoc-var-def)))
              (let ((resval1 (resolve resval env-stack)))
                (if (equal? resval resval1) resval
                    (recur resval1))))
            (let ((assoc-unit (assv (free-variable-name e) (model-units))))
              (d 'resolve "free-var: e = ~A assoc-unit = ~A~%" (free-variable-name e) assoc-unit)
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

     ((pair-arg? e)
      (make-pair-arg (recur (pair-arg-fst e))
                     (recur (pair-arg-snd e))))

     ((pair? e)
      (let ((op (car e)) (args (cdr e)))
        (d 'resolve "op = ~A~%" op)
        (d 'resolve "args = ~A~%" args)
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

    (d 'resolve-reinit "e: ~A~%" e)

    (cond

     ((pair? e)
      (let ((op (car e)) (args (cdr e)))
        (d 'resolve "op = ~A~%" op)
        (d 'resolve "args = ~A~%" args)
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
  (match-let ((($ equation s expr) regime-eq))
             (let ((s1 (resolve s env-stack))
                   (expr1 (resolve expr env-stack)))
               (map (lambda (eq)
                      (match eq
                             (($ equation s2 expr2)
                              (if (equal? s2 s1)
                                  (eq s `(if (= ,(make-regime-flag name) 1)
                                             ,expr1 ,expr2))
                                  eq))
                             (else eq)
                             ))
                    equations))
             ))
  
  

;;
;; Equation elaboration (flattening)
;;
;; The main steps in flattening are:
;;
;; Creates a name resolution environment (parameters,constants,variables,functions).
;; Replaces fixed initial values.
;; Flattens models and populates equation, definition, function lists.
;; Pulls out initial equations and populate initial list.
;; Pulls out event definitions and populates event list.
;; Handle structural events.
;;


(define (elaborate model)

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
                       (($ function name formals expr)
                        (recur (cdr decls)
                               (extend-env-with-binding env (gen-binding name decl))
                               ))
                       (else (recur (cdr decls) env))
                       ))
            ))
      ))


  (let recur (
              (entries        model)
              (env-stack      (extend-env-stack-with-binding
                               (make-env-stack (model-env model))
                               (gen-binding (variable-label model-time) model-time)
                               ))
              (definitions    '()) 
              (discrete-definitions '()) 
              (parameters     '())
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
                             (reverse equations)
                             (map-equations replace-fixed initial)
                             (reverse conditions)
                             (reverse pos-responses)
                             (reverse neg-responses)
                             functions
                             (reverse nodemap)
                             (reverse regimemap)
                             dimenv
                             )
          )

        (let ((en (car entries)))

          (if (pair? en)
              
              (recur (append en (cons 'pop-env-stack (cdr entries)))
                     (push-env-stack (model-env en) env-stack)
                     definitions discrete-definitions parameters equations
                     initial conditions pos-responses neg-responses 
                     functions nodemap regimemap dimenv)

              (match en  

                     ('pop-env-stack
                      (recur (cdr entries) (pop-env-stack env-stack)
                             definitions discrete-definitions parameters equations initial 
                             conditions pos-responses neg-responses functions
                             regimemap nodemap dimenv
                             )
                      )

                     (($ variable name label value has-history dim)
                      (let* ((resolved-value (resolve value env-stack))
                             (en1 (make-variable name label resolved-value has-history dim)))
                        (recur (cdr entries) env-stack
                               (cons (cons name resolved-value) definitions)
                               discrete-definitions
                               parameters equations initial conditions 
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
                               parameters equations initial conditions 
                               pos-responses neg-responses functions
                               (extend-env-with-binding nodemap (gen-binding name en1))
                               regimemap
                               (extend-env-with-binding dimenv (gen-binding name dim))
                               )
                        ))

                     (($ parameter name label value dim)
                      (let* ((resolved-value (resolve value env-stack))
                             (en1 (parameter name label resolved-value dim)))
                        (recur (cdr entries) env-stack
                               definitions discrete-definitions
                               (cons (cons name resolved-value) parameters) 
                               equations initial conditions pos-responses neg-responses 
                               functions
                               (extend-env-with-binding nodemap (gen-binding name en1))
                               regimemap
                               (extend-env-with-binding dimenv (gen-binding name dim))
                               )
                        ))
                     (($ equation s expr)
                      (d 'elaborate "equation: unresolved rhs = ~A~%" expr)
                      (d 'elaborate "equation: rhs = ~A~%" (resolve expr env-stack))
                      (recur (cdr entries) env-stack
                             definitions discrete-definitions parameters
                             (cons (list (resolve s env-stack) (resolve expr env-stack)) equations)
                             initial conditions pos-responses neg-responses 
                             functions regimemap nodemap dimenv
                             )
                      )
                     (($ initial-equation s expr)
                      (recur (cdr entries) env-stack
                             definitions parameters discrete-definitions
                             equations (cons (cons s (resolve expr env-stack)) initial) 
                             conditions pos-responses neg-responses 
                             functions regimemap nodemap dimenv
                             )
                      )
                     (($ structural-event name label regime ($ transition target condition pos))
                      (let ((condition-index (length conditions)))
                        (recur (cdr entries) env-stack
                               definitions discrete-definitions parameters
                               (fold (lambda (eq ax) (merge-regime-eq name eq ax env-stack)) regime equations) initial
                               (cons (make-evcondition name (resolve condition env-stack)) conditions) 
                               (append (map (lambda (x) 
                                              (make-evresponse name (resolve-reinit name (resolve x env-stack)))) pos)
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
                             definitions discrete-definitions parameters
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
                             definitions discrete-definitions parameters
                             equations initial conditions pos-responses neg-responses 
                             (cons (list name formals (resolve expr env-stack)) functions)
                             nodemap regimemap dimenv
                             ))
                     (else
                      (error 'elaborate "unknown equation type" eq))
                     ))
          ))
    ))




(define (function-call-env formals args env)
  (d 'function-call-env "formals = ~A args = ~A~%" formals args)
  (match formals
         (($ pair-formal x frst)
          (match args
                 (($ pair-arg xv arst)
                  (let ((b (gen-binding (var-def-sym x) xv)))
                    (function-call-env frst arst (extend-env-with-binding env b))))
                 (($ null-arg)
                  (error 'function-call-env
                         "function arity mismatch"))
                 ))
         (($ null-formal)
          (match args
                 (($ null-arg) env)
                 (else 
                  (error 'function-call-env
                         "function arity mismatch"))
                 ))
         ))


(define (subst-symbol sym env-stack)
  (let ((assoc-var-def (env-stack-lookup sym env-stack)))
    (if assoc-var-def
        (binding-value assoc-var-def) 
        sym)))

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
      (match-let ((($ function name formals body) f))
                 (let ((fenv (function-call-env formals arg empty-env)))
                   (d 'subst-function-call "fenv = ~A~%" fenv)
                   (subst-expr body (push-env-stack env-stack fenv)))
                 ))
  )


(define (subst-expr e env-stack)
  (d 'subst-expr "e = ~A~%" e)
  (cond ((symbol? e)
         (subst-symbol e env-stack))
        ((pair? e)
         (let ((op (car e)) (args (cdr e)))
           (d 'subst-expr "op = ~A args = ~A~%" op args)
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
                (d 'subst-expr "f = ~A fargs = ~A~%" f args)
                (subst-function-call f fargs env-stack)
                ))
             (else e)
             )))
        ((var-def? e)
         (subst-symbol (var-def-sym e) env-stack))
        ((free-variable? e)
         (subst-symbol (free-variable-name e) env-stack))
        ((left-var? e)
         (subst-expr (left-var-u e) env-stack))
        (else e)))


(define (units-reinit args env)
  (let ((x (cadr args))
        (y (caddr args)))
    (let ((xu (expr-units x env))
          (yu (expr-units y env)))
      (if (unit-equal? xu yu)
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
            (if (unit-equal? xu yu)
                xu (error 'units-reinit "units mismatch in if" args))
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
  (d 'units-primop "f = ~A units-args = ~A~%" f units-args)
  (let ((u (case f 
             ((signal.add signal.sub) 
              (if (unit-equal? (car units-args) (cadr units-args))
                  (car units-args) 
                  (error 'units-primop "units mismatch" f units-args)))
             ((signal.mul) 
              (unit* (car units-args) (cadr units-args) ))
             ((signal.div) 
              (unit/ (car units-args) (cadr units-args) ))
             ((signal.pow) 
              (unit-expt (car units-args) (constant-value (cadr args) )))
             ((signal.neg) 
              (car units-args))
             (else unitless))))
    (d 'units-primop "u = ~A~%" u)
    u))


(define (units-function-call f arg env)
  (d 'units-function-call "f = ~A arg = ~A~%" f arg)
  (if (symbol? f)
      (let ((units-args (units-pair-arg arg env)))
        (d 'units-function-call "units-args = ~A~%" units-args)
        (units-primop f arg units-args))
      (match-let
       ((($ function name formals body) f))
       (let ((fenv (function-call-env formals arg empty-env)))
         (d 'units-function-call "fenv = ~A~%" fenv)
         (let ((u (expr-units 
                   (subst-expr body (make-env-stack fenv))
                   env)))
           (d 'units-function-call "u = ~A~%" u)
           u)
         ))
      ))


(define (expr-units e env)
  (d 'expr-units "e = ~A~%" e)
  (cond ((symbol? e)
         (let ((u (assv e (model-units))))
           (or (and u (cdr u)) (env-lookup e env) unitless)))
        ((pair? e)
         (let ((op (car e)) (args (cdr e)))
           (d 'expr-units "op = ~A args = ~A~%" op args)
           (case op
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
        ((var-def? e)
         (expr-units (subst-symbol (var-def-sym e) env) env))
        ((free-variable? e)
         (expr-units (free-variable-name e) env))
        ((variable? e)
         (expr-units (variable-value e) env))
        ((discrete-variable? e)
         (expr-units (discrete-variable-value e) env))
        ((parameter? e)
         (expr-units (parameter-value e) env))
        ((left-var? e)
         (expr-units (left-var-u e) env))
        ((constant? e)
         (constant-unit e)) 
        (else e)))
         

(define (reduce-expr expr indexmaps )
  (let (
        (pindexmap (alist-ref 'pindexmap indexmaps))
        (cindexmap (alist-ref 'cindexmap indexmaps))
        (dindexmap (alist-ref 'dindexmap indexmaps))
        (evindexmap (alist-ref 'evindexmap indexmaps))
        )
  (d 'reduce-expr "expr = ~A~%" expr)
  (d 'reduce-expr "cindexmap = ~A~%" cindexmap)
  (match expr

         (($ pair-arg fst snd)
          (make-pair-arg
           (reduce-expr fst indexmaps)
           (reduce-expr snd indexmaps)))

         (($ derivative-variable y)
          (let ((yindex (assv (variable-name y) cindexmap)))
            (if (not yindex)
                (error 'reduce-expr "variable not in index" y)
                `(getindex dy ,(cdr yindex)))
            ))
         
         (($ left-var y)
          (cond ((variable? y)
                 (let ((yindex (assv (variable-name y) cindexmap)))
                   (if (not yindex)
                       (error 'reduce-expr "variable not in index" y)
                       `(getindex y ,(cdr yindex)))
                   ))
                ((discrete-variable? y)
                 (let ((yindex (assv (discrete-variable-name y) dindexmap)))
                   (if (not yindex)
                       (error 'reduce-expr "variable not in index" y)
                       `(getindex d ,(cdr yindex)))
                   ))
                (else 
                 (error 'reduce-expr "unknown left variable type" expr))
                ))
         
         (($ variable name label value has-history dim)
          (let ((yindex (assv name cindexmap)))
            (if (not yindex)
                (if (equal? name (variable-name model-time))
                    label
                    (error 'reduce-expr "variable not in index" name))
                `(getindex y ,(cdr yindex)))
            ))

         (($ discrete-variable name label value dim)
          (let ((dindex (assv name dindexmap)))
            (if (not dindex)
                (error 'reduce-expr "discrete variable not in index" name)
                `(getindex d ,(cdr dindex)))
            ))

         (($ parameter name label value dim)
          (let ((yindex (assv name pindexmap)))
            (if (not yindex)
                (error 'reduce-expr "parameter not in index" name)
                `(getindex p ,(cdr yindex)))
            ))

         (('signal.let lbnds body)
          (subst-expr
           `(signal.let 
             ,(map (lambda (lb) (cons (car lb) (reduce-expr (cdr lb) indexmaps))) lbnds)
             ,(reduce-expr body indexmaps))
           empty-env-stack))

         (('signal.call f args)
          (subst-expr 
           `(signal.call ,f ,(reduce-expr args indexmaps))
           empty-env-stack))

         (('signal.reinit e x y)
          (let ((eindex (assv e evindexmap)))
            (subst-expr 
             `(signal.reinit (getindex c ,(cdr eindex))
                             ,(reduce-expr x indexmaps) 
                             ,(reduce-expr y indexmaps))
             empty-env-stack)))

         ((op . args)
          (subst-expr
           (cons op (map (lambda (x) (reduce-expr x indexmaps)) args))
           empty-env-stack))

         (else expr)
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
  (d 'reduce-constant-expr "cindexmap = ~A~%" cindexmap)
  (match expr

         (($ pair-arg fst snd)
          (make-pair-arg
           (reduce-constant-expr fst indexmaps)
           (reduce-constant-expr snd indexmaps)))

         (($ variable name label value has-history dim)
          (reduce-constant-expr value indexmaps))

         (($ parameter name label value dim)
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
  (let (
        (pindexmap  (alist-ref 'pindexmap indexmaps))
        (cindexmap  (alist-ref 'cindexmap indexmaps))
        (dindexmap  (alist-ref 'dindexmap indexmaps))
        (evindexmap (alist-ref 'evindexmap indexmaps))
        )
    (d 'reduce-eq "eq = ~A~%" eq)

    (match eq

           ((($ derivative-variable y) rhs)
            (let ((yindex (assv (variable-name y) cindexmap))
                  (dim (variable-dim y)))
              (if (not yindex)
                  (error 'reduce-eq "variable not in index" y)
                  (let ((expr (reduce-expr rhs indexmaps))
                        (ddim (if (equal? (quantity-int dim) (quantity-int Unity))
                                  (quantity-int dim)
                                  (- (quantity-int dim) (quantity-int Time)))))
                    (let ((rhs-units (expr-units rhs unit-env)))
                      (d 'reduce-eq "ddim = ~A dims(rhs-units) = ~A ~%" ddim (unit-dims rhs-units))
                      (if (equal? ddim (quantity-int (unit-dims rhs-units)))
                          `(setindex dy ,(cdr yindex) ,expr)
                          (error 'reduce-eq "dimension mismatch in rhs" 
                                 (variable-name y) (variable-label y)))
                      ))
                  ))
            )
           
           ((($ variable name label value has-history dim) rhs)
            (let ((yindex (assv name cindexmap)))
              (if (not yindex)
                  (error 'reduce-eq "variable not in index" name)
                  (let ((expr (reduce-expr rhs indexmaps)))
                    (let ((rhs-units (expr-units rhs unit-env)))
                      (d 'reduce-eq "dim = ~A dims(rhs-units) = ~A ~%" dim (unit-dims rhs-units))
                      (if (equal? dim (unit-dims rhs-units))
                          `(setindex y ,(cdr yindex) ,expr)
                          (error 'reduce-eq "dimension mismatch in rhs" name label))
                      ))
                  ))
            )

           (($ evcondition name rhs)
            (let ((evindex (assv name evindexmap))
                  (expr (reduce-expr rhs indexmaps)))
              (if (not evindex)
                  (error 'reduce-eq "event identifier not found in indexmap" name))
              (d 'reduce-eq "evcondition name = ~A~%" name)
              (d 'reduce-eq "expr = ~A~%" expr)
              (if (expr-units rhs unit-env)
                  `(setindex c ,(cdr evindex) ,expr)
                  (error 'reduce-eq "dimension mismatch in condition" expr))
              ))
           
           (($ evresponse name rhs)
            (d 'reduce-eq "evresponse rhs = ~A~%" rhs)
            (let ((y (match rhs
                            (('signal.reinit e ($ left-var u) yindex . rest) u)
                            (error 'reduce-eq "unknown event response equation" eq))))
              (cond
               ((variable? y)
                (let (
                      (yindex (assv (variable-name y) cindexmap))
                      (dim (variable-dim y))
                      )
                  (if (not yindex)
                      (error 'reduce-eq "variable not in index" y)
                      (let ((expr (reduce-expr rhs indexmaps)))
                        (if (equal? dim (unit-dims (expr-units rhs unit-env)))
                            `(setindex y ,(cdr yindex) ,expr)
                            (error 'reduce-eq "variable mismatch in event response" y))
                        ))
                  ))
               ((discrete-variable? y)
                (let (
                      (yindex (assv (discrete-variable-name y) dindexmap))
                      (dim (discrete-variable-dim y))
                      )
                  (if (not yindex)
                      (error 'reduce-eq "variable not in index" y)
                      (let ((expr (reduce-expr rhs indexmaps)))
                        (if (equal? dim (unit-dims (expr-units rhs unit-env)))
                            `(setindex d ,(cdr yindex) ,expr)
                            (error 'reduce-eq "variable mismatch in event response" y))
                        ))
                  ))
               (else (error 'reduce-eq "unknown variable type in reinit equation" eq))
               ))
            )

           (else eq)))
  )          


(define (simcreate eqset)

  (let* ((nodemap    (equation-set-nodemap eqset))
         (dimenv     (equation-set-dimenv eqset))
         (conditions (equation-set-conditions eqset))

         (unit-env
          (fold (lambda (p env)
                  (let* ((name (car p)) (rhs (cdr p))
                         (rhs-units (expr-units rhs env)))
                    (d 'unit-env "rhs-units = ~A (env-lookup dimenv name) ~A~%" 
                       rhs-units (env-lookup name dimenv))
                    (if (equal? (quantity-int (binding-value (env-lookup name dimenv)))
                                (quantity-int (unit-dims rhs-units)))
                        (extend-env-with-binding
                         env (gen-binding name rhs-units))
                        (error 'simcreate "dimension mismatch in initial value" name rhs-units))
                    ))
                empty-env 
                (equation-set-parameters eqset)))

         (cindexmap 
           (let recur ((nodemap nodemap)
                       (indexmap  '())
                       (index     0))
             (if (null? nodemap)
                 indexmap
                 (let ((node (car nodemap)))
                   (if (variable? (cdr node))
                       (recur (cdr nodemap)
                              (cons (cons (car node) index) indexmap)
                              (+ 1 index))
                       (recur (cdr nodemap) indexmap index))))
             ))
          
         (dindexmap 
           (let recur ((nodemap nodemap)
                       (indexmap  '())
                       (index     0))
             (if (null? nodemap)
                 indexmap
                 (let ((node (car nodemap)))
                   (if (discrete-variable? (cdr node))
                       (recur (cdr nodemap)
                              (cons (cons (car node) index) indexmap)
                              (+ 1 index))
                       (recur (cdr nodemap) indexmap index))))
             ))
          

         (pindexmap 
           (let recur ((nodemap nodemap)
                       (indexmap  '())
                       (index     0))
             (if (null? nodemap)
                 indexmap
                 (let ((node (car nodemap)))
                   (if (parameter? (cdr node))
                       (recur (cdr nodemap)
                              (cons (cons (car node) index) indexmap)
                              (+ 1 index))
                       (recur (cdr nodemap) indexmap index))))
             ))
          
         (evindexmap 
           (let recur ((conditions conditions)
                       (indexmap  '())
                       (index     0))
             (if (null? conditions)
                 indexmap
                 (recur (cdr conditions)
                        (cons (cons (evcondition-name (car conditions)) index) indexmap)
                        (+ 1 index)))
             ))

         (indexmaps `((cindexmap . ,cindexmap)
                      (pindexmap . ,pindexmap)
                      (dindexmap . ,dindexmap)
                      (evindexmap . ,evindexmap)))
                      

         (param-block 
          (map (lambda (x) (reduce-constant-expr (cdr x) indexmaps))
               (equation-set-parameters eqset)))

         (init-block 
          (map (lambda (x) (reduce-expr (cdr x) indexmaps))
               (equation-set-definitions eqset)))

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
     evindexmap
     param-block
     init-block
     eq-block 
     cond-block
     pos-responses
     neg-responses)

    )
  )



        

)

