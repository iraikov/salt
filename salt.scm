 
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
        
	(require-extension matchable datatype lalr-driver mathh)
	(require-library data-structures extras srfi-1 srfi-13)
	(import (only srfi-1 first second zip fold fold-right every)
                (only srfi-13 string-null? string-concatenate string<)
		(only data-structures ->string alist-ref conc intersperse compose sort)
                (only extras pp fprintf)
                (only ports with-output-to-port)
		)


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



;(define-record-type SI-quantity  
;{T,m,kg,s,A,K,mol,cd,rad,sr}


(define syntactic-keywords
  '(function if cond and or let else))


(define-record-type variable
  (make-variable  name label value history )
  variable?
  (name        variable-name)
  (label       variable-label)
  (value       variable-value)
  (history     variable-history)
  )


(define-record-printer (variable x out)
  (fprintf out "#(variable ~S label=~S value=~S history=~S)"
	   (variable-name x)
	   (variable-label x)
	   (variable-value x)
	   (variable-history x)
           ))


(define (unknown value label)
  (make-variable (gensym 'u) label value #t))


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
  (make-discrete-variable u initial )
  discrete-variable?
  (u          discrete-variable-u)
  (initial    discrete-variable-initial)
  )

(define-record-printer (discrete-variable x out)
  (fprintf out "#(deriv ~S = ~A)"
	   (discrete-variable-u x)
	   (discrete-variable-initial x)
           ))


(define-record-type parameter
  (parameter name label value)
  parameter?
  (name parameter-name)
  (label parameter-label)
  (value parameter-value)
  )


(define-record-printer (parameter x out)
  (fprintf out "#(parameter ~S [~A] = ~A)"
	   (parameter-name x)
	   (parameter-label x)
	   (parameter-value x)
           ))


(define-record-type constant
  (constant type value)
  constant?
  (type constant-type)
  (value constant-value)
  )


(define-record-printer (constant x out)
  (fprintf out "#(constant ~S = ~A)"
	   (constant-type x)
	   (constant-value x)
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
  (fprintf out "#(ref-var ~S[~A])"
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


(define-record-type structural-event
  (make-structural-event left-condition left right-condition right)
  structural-event?
  (left-condition structural-event-left-condition)
  (left structural-event-left)
  (right-condition structural-event-right-condition)
  (right structural-event-right)
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
        (map (lambda (x) (constant 'number x))
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
  (make-equation-set model definitions parameters equations initial conditions pos-responses neg-responses functions nodemap)
  equation-set?
  (model equation-set-model)
  (definitions equation-set-definitions)
  (parameters equation-set-parameters)
  (equations equation-set-equations)
  (initial equation-set-initial)
  (conditions equation-set-conditions)
  (pos-responses equation-set-pos-responses)
  (neg-responses equation-set-neg-responses)
  (functions equation-set-functions)
  (nodemap equation-set-nodemap)
  )


(define-record-printer (equation-set x out)
  (fprintf out "#")
  (pp
   `(equation-set
    (definitions=,(equation-set-definitions x))    
    (parameters=,(equation-set-parameters x))
    (equations=,(equation-set-equations x))
    (functions=,(equation-set-functions x))
    (conditions=,(equation-set-conditions x))
    (pos-responses=,(equation-set-pos-responses x))
    (neg-responses=,(equation-set-neg-responses x))
    (nodemap=,(equation-set-nodemap x)))))


;; runtime representation of a simulation object
(define-record-type simruntime
  (make-simruntime eqset cindexmap dindexmap parameters defs derivblock asgnblock condblock posresp negresp)
  simruntime?
  (eqset simruntime-eqset)
  (cindexmap simruntime-cindexmap)
  (dindexmap simruntime-dindexmap)
  (parameters simruntime-parameters)
  (defs simruntime-definitions)
  (derivblock simruntime-derivblock)
  (asgnblock simruntime-asgnblock)
  (condblock simruntime-condblock)
  (posresp simruntime-posresp)
  (negresp simruntime-negresp)
  )


(define-record-printer (simruntime x out)
    (fprintf out "#")
    (pp `(simruntime 
          (cindexmap=,(simruntime-cindexmap x))
          (dindexmap=,(simruntime-dindexmap x))
          (parameters=,(simruntime-parameters x))
          (defs=,(simruntime-definitions x))
          (derivblock=,(simruntime-derivblock x))
          (asgnblock=,(simruntime-asgnblock x))
          (evblock=,(simruntime-condblock x))
          (posresp=,(simruntime-posresp x))
          (negresp=,(simruntime-negresp x))
          )
        out)
    )
    


(define MTime (unknown (constant 'number 0.0) 't))


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
            (binding-value assoc-var-def)
            e)))

     ((derivative-variable? e)
      (make-derivative-variable (resolve (derivative-variable-parent e) env-stack)))

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
                       (($ variable name label value has-history)
                        (recur (cdr decls)
                               (extend-env-with-binding env (gen-binding label decl))
                               ))
                       (($ parameter name label value )
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
                               (gen-binding (variable-label MTime) MTime)
                               ))
              (definitions    '()) ;;(list (cons (variable-name MTime) (variable-value MTime))))
              (parameters     '())
              (equations      '())
              (initial        '())
              (conditions     '())
              (pos-responses  '())
              (neg-responses  '())
              (functions      '())
              (nodemap        empty-env)
                              ;(extend-env-with-binding
                              ; empty-env
                              ; (gen-binding (variable-name MTime) MTime)
                               
              )
      
    (if (null? entries)
        (begin
          (make-equation-set model
                             (reverse definitions)
                             (reverse parameters)
                             equations
                             (map-equations replace-fixed initial)
                             conditions
                             pos-responses
                             neg-responses
                             functions
                             (reverse nodemap)
                             )
          )

        (let ((en (car entries)))

          (if (pair? en)
              
              (recur (append en (cons 'pop-node-env-stack (cdr entries)))
                     (push-env-stack (model-env en) env-stack)
                     definitions parameters equations
                     initial conditions pos-responses neg-responses 
                     functions nodemap)

              (match en  

                     ('pop-node-env-stack
                      (recur (cdr entries) (pop-env-stack env-stack)
                             definitions parameters equations initial 
                             conditions pos-responses neg-responses functions
                             nodemap
                             )
                      )

                     (($ variable name label value has-history)
                      (recur (cdr entries) env-stack
                             (cons (cons name (resolve value env-stack)) definitions)
                             parameters equations initial conditions pos-responses 
                             neg-responses functions
                             (extend-env-with-binding nodemap (gen-binding name en))
                             )
                      )

                     (($ parameter name label value )
                      (recur (cdr entries) env-stack
                             definitions 
                             (cons (cons name (resolve value env-stack)) parameters) 
                             equations initial conditions pos-responses neg-responses 
                             functions
                             (extend-env-with-binding nodemap (gen-binding name en))
                             )
                      )
                     (($ equation s expr)
                      (d 'elaborate "equation: unresolved rhs = ~A~%" expr)
                      (d 'elaborate "equation: rhs = ~A~%" (resolve expr env-stack))
                      (recur (cdr entries) env-stack
                             definitions parameters
                             (cons (list (resolve s env-stack) (resolve expr env-stack)) equations)
                             initial conditions pos-responses neg-responses 
                             functions nodemap
                             )
                      )
                     (($ initial-equation s expr)
                      (recur (cdr entries) env-stack
                             definitions parameters
                             equations (cons (cons s (resolve expr env-stack)) initial) 
                             conditions pos-responses neg-responses 
                             functions nodemap
                             )
                      )
                     (($ structural-event left-condition left right-condition right)
                      (recur (cdr entries) env-stack
                             definitions parameters
                             equations initial
                             (cons (resolve left-condition env-stack) (cons (resolve right-condition env-stack) conditions) )
                             pos-responses 
                             neg-responses
                             functions nodemap
                             ;; TODO: generate equations for new regimes
                             ))
                     (($ event name condition pos neg)
                      (recur (cdr entries) env-stack
                             definitions parameters
                             equations initial 
                             (cons (make-evcondition name (resolve condition env-stack)) conditions) 
                             (append (map (lambda (x) (make-evresponse name (resolve-reinit name (resolve x env-stack)))) pos) pos-responses)
                             (or (and neg (append (map (lambda (x) (make-evresponse name (resolve-reinit name (resolve x env-stack)))) neg)
                                                  neg-responses))
                                 neg-responses)
                             functions nodemap
                             ))
                     (($ function name formals expr)
                      (recur (cdr entries) env-stack
                             definitions parameters
                             equations initial conditions pos-responses neg-responses 
                             (cons (list name formals (resolve expr env-stack)) functions)
                             nodemap
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
        (binding-value assoc-var-def) sym)))

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
         

(define (reduce-expr expr pindexmap cindexmap dindexmap)
  (d 'reduce-expr "expr = ~A~%" expr)
  (d 'reduce-expr "cindexmap = ~A~%" cindexmap)
  (match expr

         (($ pair-arg fst snd)
          (make-pair-arg
           (reduce-expr fst pindexmap cindexmap dindexmap)
           (reduce-expr snd pindexmap cindexmap dindexmap)))

         (($ derivative-variable y)
          (let ((yindex (assv (variable-name y) cindexmap)))
            (if (not yindex)
                (error 'reduce-expr "variable not in index" y)
                `(getindex dy ,(cdr yindex)))
            ))
         
         (($ left-var y)
          (let ((yindex (assv (variable-name y) cindexmap)))
            (if (not yindex)
                (error 'reduce-expr "variable not in index" y)
                `(getindex y ,(cdr yindex)))
            ))
         
         (($ variable name label value has-history)
          (let ((yindex (assv name cindexmap)))
            (if (not yindex)
                (if (equal? name (variable-name MTime))
                    label
                    (error 'reduce-expr "variable not in index" name))
                `(getindex y ,(cdr yindex)))
            ))

         (($ parameter name label value)
          (let ((yindex (assv name pindexmap)))
            (if (not yindex)
                (error 'reduce-expr "parameter not in index" name)
                `(getindex p ,(cdr yindex)))
            ))

         (('signal.let lbnds body)
          (subst-expr
           `(signal.let 
             ,(map (lambda (lb) (cons (car lb) (reduce-expr (cdr lb) pindexmap cindexmap dindexmap))) lbnds)
             ,(reduce-expr body pindexmap cindexmap dindexmap))
           empty-env-stack))

         (('signal.call f args)
          (subst-expr 
           `(signal.call ,f ,(reduce-expr args pindexmap cindexmap dindexmap))
           empty-env-stack))

         (('signal.reinit e x y)
          (let ((eindex (assv e dindexmap)))
            (subst-expr 
             `(signal.reinit (getindex c ,(cdr eindex))
                             ,(reduce-expr x pindexmap cindexmap dindexmap) 
                             ,(reduce-expr y pindexmap cindexmap dindexmap))
             empty-env-stack)))

         ((op . args)
          (subst-expr
           (cons op (map (lambda (x) (reduce-expr x pindexmap cindexmap dindexmap)) args))
           empty-env-stack))

         (else expr)
         ))

;; reduce expr for constant expressions
(define (reduce-constant-expr expr pindexmap cindexmap dindexmap)
  (d 'reduce-constant-expr "expr = ~A~%" expr)
  (d 'reduce-constant-expr "cindexmap = ~A~%" cindexmap)
  (match expr

         (($ pair-arg fst snd)
          (make-pair-arg
           (reduce-constant-expr fst pindexmap cindexmap dindexmap)
           (reduce-constant-expr snd pindexmap cindexmap dindexmap)))

         (($ variable name label value has-history)
          (reduce-constant-expr value pindexmap cindexmap dindexmap))

         (($ parameter name label value)
          (reduce-constant-expr value pindexmap cindexmap dindexmap))

         (('signal.let lbnds body)
          (subst-expr
           `(signal.let 
             ,(map (lambda (lb) (cons (car lb) (reduce-constant-expr (cdr lb) pindexmap cindexmap dindexmap))) lbnds)
             ,(reduce-constant-expr body pindexmap cindexmap dindexmap))
           empty-env-stack))

         (('signal.call f args)
          (subst-expr 
           `(signal.call ,f ,(reduce-constant-expr args pindexmap cindexmap dindexmap))
           empty-env-stack))

         ((op . args)
          (subst-expr
           (cons op (map (lambda (x) (reduce-constant-expr x pindexmap cindexmap dindexmap)) args))
           empty-env-stack))

         (else expr)
         ))


(define (reduce-eq eq pindexmap cindexmap dindexmap)

  (match eq
         ((($ derivative-variable y) rhs)
          (let ((yindex (assv (variable-name y) cindexmap)))
            (if (not yindex)
                (error 'reduce-eq "variable not in index" y)
                `(setindex dy ,(cdr yindex) ,(reduce-expr rhs pindexmap cindexmap dindexmap)))
            ))
         
         ((($ variable name label value has-history) rhs)
          (let ((yindex (assv name cindexmap)))
            (if (not yindex)
                (error 'reduce-eq "variable not in index" name)
                `(setindex y ,(cdr yindex) ,(reduce-expr rhs pindexmap cindexmap dindexmap)))
            ))

         (($ evcondition name expr)
          (let ((cindex (assv name dindexmap)))
            `(setindex c ,(cdr cindex) ,(reduce-expr expr pindexmap cindexmap dindexmap))))
         
         (($ evresponse name expr)
          (let* ((y (match expr
                           (('signal.reinit e ($ left-var u) yindex . rest) u)
                           (error 'reduce-eq "unknown event response equation" eq)))
                 (yindex (assv (variable-name y) cindexmap)))
            (if (not yindex)
                (error 'reduce-eq "variable not in index" y)
                `(setindex y ,(cdr yindex) ,(reduce-expr expr pindexmap cindexmap dindexmap)))
            ))
         
         (else eq)))
          
                

(define (simcreate eqset)


  (let* ((nodemap (equation-set-nodemap eqset))
         (conditions (equation-set-conditions eqset))

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
          
         (dindexmap 
           (let recur ((conditions conditions)
                       (indexmap  '())
                       (index     0))
             (if (null? conditions)
                 indexmap
                 (recur (cdr conditions)
                        (cons (cons (evcondition-name (car conditions)) index) indexmap)
                        (+ 1 index)))
             ))

         (param-block 
          (map (lambda (x) (reduce-constant-expr (cdr x) pindexmap cindexmap dindexmap))
               (equation-set-parameters eqset)))

         (init-block 
          (map (lambda (x) (reduce-expr (cdr x) pindexmap cindexmap dindexmap))
               (equation-set-definitions eqset)))

         (deriv-block
          (map (lambda (x) (reduce-eq x pindexmap cindexmap dindexmap)) 
               (filter ode? (equation-set-equations eqset))))
         
         (asgn-block
          (map (lambda (x) (reduce-eq x pindexmap cindexmap dindexmap)) 
               (filter asgn? (equation-set-equations eqset))))
         
         (cond-block
          (map (lambda (c) (reduce-eq c pindexmap cindexmap dindexmap))
               (equation-set-conditions eqset)))

         (pos-responses 
          (map (lambda (x) (reduce-eq x pindexmap cindexmap dindexmap))
               (equation-set-pos-responses eqset)))

         (neg-responses 
          (map (lambda (x) (reduce-eq x pindexmap cindexmap dindexmap))
               (equation-set-neg-responses eqset)))

         )

    (make-simruntime 
     eqset
     cindexmap dindexmap
     param-block
     init-block
     deriv-block
     asgn-block
     cond-block
     pos-responses
     neg-responses)

    )
  )



        

)

