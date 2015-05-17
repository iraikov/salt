 
;;
;; Hybrid dynamical systems modeling.
;;
;; Copyright 2015 Ivan Raikov.
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

         parse elaborate simcreate
         verbose
         ;parameter unknown der ev reinit 

	 )

	(import scheme chicken)
        
	(require-extension matchable lalr-driver mathh)
	(require-library data-structures extras srfi-1 srfi-13)
	(import (only srfi-1 first second zip fold)
                (only srfi-13 string-null? string-concatenate string<)
		(only data-structures ->string alist-ref conc)
                (only extras pp fprintf)
                (only ports with-output-to-port)
		)


(include "mathh-constants.scm")
(include "parser.scm")
(include "env.scm")

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

;; based on SRV:send-reply by Oleg Kiselyov
(define (print-fragments b)
  (let loop ((fragments b) (result #f))
    (cond
      ((null? fragments) result)
      ((not (car fragments)) (loop (cdr fragments) result))
      ((null? (car fragments)) (loop (cdr fragments) result))
      ((eq? #t (car fragments)) (loop (cdr fragments) #t))
      ((pair? (car fragments))
        (loop (cdr fragments) (loop (car fragments) result)))
      ((procedure? (car fragments))
        ((car fragments))
        (loop (cdr fragments) #t))
      (else
       (display (car fragments))
       (loop (cdr fragments) #t)))))



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
  (make-variable (gensym 'u) value label #t))


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
  (parameter name value)
  parameter?
  (name parameter-name)
  (value parameter-value)
  )


(define-record-printer (parameter x out)
  (fprintf out "#(parameter ~S = ~A)"
	   (parameter-name x)
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
  (make-event condition pos neg)
  event?
  (condition event-condition)
  (pos event-pos)
  (neg event-neg))


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


(define (reinit x y)
  (cond ((left-var? x) `(variable.reinit ,x ,y))
        ((variable? x)  (reinit (left-var x) y))
        ((ref-var? x)   (reinit (left-var x) y))
        ((derivative-variable? x)   (reinit (left-var x) y))
        (else (error 'reinit "invalid argument to reinit" x))
        ))


(define (ev condition p . rest)
  (if (null? rest)
      (make-event condition p '())
      (make-event condition p (car rest))))

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
  (let ((function-names `(+ - * / max min))
        (op-names `(signal.fadd signal.fsub signal.fmul signal.fdiv signal.max signal.min)))
    (fold (lambda (k v env)
            (let ((binding (gen-binding k v)))
              (extend-env-with-binding env binding)))
          empty-env
          function-names
          (map (lambda (f fn)
                 (function (make-free-variable f)
                           (make-pair-formal (make-var-def 'a) (make-pair-formal (make-var-def 'b) (make-null-formal)))
                           `(signal.call ,fn (make-pair-arg (make-var-def 'a) (make-pair-arg (make-var-def 'b) (make-null-arg))))
                           ))
               function-names 
               op-names))
    ))


(define math-unop-env
  (let ((function-names
         `(pow neg abs atan asin acos sin cos exp ln
               sqrt tan cosh sinh tanh hypot gamma lgamma log10 log2 log1p ldexp cube
               round ceiling floor))
        (op-names
         `(signal.pow signal.neg signal.abs signal.atan signal.asin signal.acos signal.sin signal.cos signal.exp signal.ln
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
                           `(signal.call ,fn (make-pair-arg (make-var-def 'a) (make-null-arg))))
                 )
               function-names
               op-names)
          ))
  )
  

;;(define (ifelse x y z)  `(signal.ifelse ,x ,y ,z))


;; A representation of a flattened model, normally created with
;; `elaborate(model)`. The code generation procedures take an
;; elaborated model as input.
;;
;; Contains the hierarchical equations, flattened equations, flattened
;; initial equations, events, event response functions, and a map of
;; Unknown nodes.


(define-record-type equation-set
  (make-equation-set model definitions parameters equations initial events pos-reponses neg-responses functions nodemap)
  equation-set?
  (model equation-set-model)
  (definitions equation-set-definitions)
  (parameters equation-set-parameters)
  (equations equation-set-equations)
  (initial equation-set-initial)
  (events equation-set-events)
  (pos-responses equation-set-pos-responses)
  (neg-responses equation-set-neg-responses)
  (functions equation-set-functions)
  (nodemap equation-set-nodemap)
  )

(define-record-printer (equation-set x out)
  (fprintf out "#(equation-set: ~%    definitions = ~A~%    parameters = ~A~%    equations = ~A~%    functions = ~A~%    events = ~A~%)"
           (equation-set-definitions x)
           (equation-set-parameters x)
           (equation-set-equations x)
           (equation-set-functions x)
           (equation-set-events x)
           ))


;; runtime representation of a simulation object
(define-record-type simruntime
  (make-simruntime indexmap eqset)
  simruntime?
  (indexmap simruntime-indexmap)
  (eqset simruntime-eqset)
  )


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
                     math-unop-env))))
    (map (lambda (x) (parse-declaration initial-env x)) decls)
    ))


;; Expression resolution

(define (resolve expr env)

  (d 'resolve "env = ~A~%" (map car env))

  (let recur ((e expr))

    (cond
     
     ((free-variable? e)
      (let ((assoc-var-def (env-lookup (free-variable-name e) env)))
        (d 'resolve "e = ~A assoc = ~A~%" (free-variable-name e) assoc-var-def)
        (if assoc-var-def
            (binding-value assoc-var-def)
            e)))

     ((pair? e)
      (let ((op (car e)) (args (cdr e)))
        (d 'resolve "op = ~A~%" op)
        (case op
          ((signal.if)   `(,op . ,(map recur args)))
          ((signal.cond) `(,op . ,(map recur args)))
          ((signal.and)  `(,op . ,(map recur args)))
          ((signal.or)   `(,op . ,(map recur args)))
          ((signal.let)  `(,op ,(map (lambda (b) (cons (car b) (recur (cdr b)))) (car args))
                               ,(recur (cadr args))))
          ((signal.call)  `(,op . ,(map recur args)))
          (else   (map recur e)))))

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
;; Handle StructuralEvents.
;;


(define (elaborate model)

  (define (model-env decls)

    (let recur ((decls decls) (env empty-env))

      (if (null? decls) env

          (let ((decl (car decls)))
      
            (if (pair? decl)
                
                (recur (append decl (cdr decls)) env)
                
                (match decl  
                       (($ variable name value label has-history)
                        (recur (cdr decls)
                               (extend-env-with-binding env (gen-binding label decl))
                               ))
                       (($ parameter name value )
                        (recur (cdr decls)
                               (extend-env-with-binding env (gen-binding name decl))
                               ))
                       (($ function name formals expr)
                        (recur (cdr decls)
                               (extend-env-with-binding env (gen-binding name decl))
                               ))
                       (else (recur (cdr decls) env))
                       ))
            ))
      ))


  (let ((nodemap (model-env model)))

    (let recur (
                (entries model)
                (definitions '())
                (parameters '())
                (equations '())
                (initial  '())
                (events   '())
                (pos-responses '())
                (neg-responses  '())
                (functions  '())
                )
      
      (if (null? entries)
            
            (make-equation-set model
                               definitions
                               parameters
                               equations
                               (map-equations replace-fixed initial)
                               events
                               pos-responses
                               neg-responses
                               functions
                               nodemap
                               )
          
          (let ((en (car entries)))

            (if (pair? en)

                (recur (append en (cdr entries))
                       definitions parameters equations
                       initial events pos-responses neg-responses 
                       functions)

                (match en  
                       (($ variable name value label has-history)
                        (recur (cdr entries)
                               (cons (cons name (resolve value nodemap)) definitions)
                               parameters equations initial events pos-responses 
                               neg-responses functions
                               )
                        )
                       (($ parameter name value )
                        (recur (cdr entries)
                               definitions (cons (cons name (resolve value nodemap)) parameters) 
                               equations initial events pos-responses neg-responses 
                               functions
                               )
                        )
                       (($ equation s expr)
                        (d 'elaborate "equation: unresolved rhs = ~A~%" expr)
                        (d 'elaborate "equation: rhs = ~A~%" (resolve expr nodemap))
                        (recur (cdr entries)
                               definitions parameters
                               (cons (list s (resolve expr nodemap)) equations)
                               initial events pos-responses neg-responses 
                               functions
                               )
                        )
                       (($ initial-equation s expr)
                        (recur (cdr entries)
                               definitions parameters
                               equations (cons (cons s (resolve expr nodemap)) initial) events pos-responses neg-responses 
                               functions
                               ))
                       (($ structural-event left-condition left right-condition right)
                        (recur (cdr entries)
                               definitions parameters
                               equations initial  ;; todo: generate conditional equations
                               (cons (resolve left-condition nodemap) (cons (resolve right-condition nodemap) events) )
                               pos-responses ;; todo: generate discrete equation
                               neg-responses
                               functions
                               ))
                       (($ event condition pos neg)
                        (recur (cdr entries)
                               definitions parameters
                               equations initial 
                               (cons (resolve condition nodemap) events) 
                               (cons (resolve pos nodemap) pos-responses)
                               (or (and neg (cons (resolve neg nodemap) neg-responses)) neg-responses)
                               functions
                               ))
                       (($ function name formals expr)
                        (recur (cdr entries)
                               definitions parameters
                               equations initial events pos-responses neg-responses 
                               (cons (list name formals (resolve expr nodemap)) functions)
                               ))
                       (else
                        (error 'elaborate "unknown equation type" eq))
                       ))
            ))
      ))
  
)

(define (simcreate eqset)
  
  (let ((indexmap 
         (let recur ((nodemap   (equation-set-nodemap eqset))
                     (indexmap  '())
                     (index     1))
           (if (null? nodemap)
               indexmap
               (recur (cdr nodemap)
                      (cons (cons (car (car nodemap)) index) indexmap)
                      (+ 1 index)))
           ))
        )

    (pp indexmap)

    ))



)

