 
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

         elaborate simcreate
         ;parameter unknown der ev reinit 

	 function? make-function function-formals function-body
	 )

	(import scheme chicken)
        
	(require-extension matchable lalr-driver)
	(require-library data-structures extras srfi-1 srfi-13)
	(import (only srfi-1 first second fold)
                (only srfi-13 string-null? string-concatenate string<)
		(only data-structures ->string alist-ref conc)
                (only extras pp)
                (only with-output-to-port ports)
		)


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
			       (current-error-port) "nemo warning"))
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
	    (error 'nemo (get-output-string port)))
	  (let ((obj (car objs)))
	    (if (procedure? obj) 
		(with-output-to-port port obj)
		(begin
		  (display obj port)
		  (display " " port)))
	    (loop (cdr objs)))))))


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
  (make-variable  name value label history )
  variable?
  (name        variable-name)
  (label       variable-label)
  (value       variable-value)
  (history     variable-history)
  )

(define-record-type definition
  (make-definition  name value label has-history )
  definition?
  (name        definition-name)
  (label       definition-label)
  (value       definition-value)
  (has-history     definition-has-history)
  )

(define (unknown value label)
  (make-definition (gensym 'u) value label #t))


(define-record-type derivative-variable
  (make-derivative-variable parent)
  derivative-variable?
  (parent      derivative-variable-parent)
  )


(define-record-type discrete-variable
  (make-discrete-variable u initial )
  discrete-variable?
  (u          discrete-variable-u)
  (initial    discrete-variable-initial)
  )


(define-record-type parameter
  (parameter name value)
  parameter?
  (name parameter-name)
  (value parameter-value)
  )


(define-record-type constant
  (constant type value)
  constant?
  (type constant-type)
  (value constant-value)
  )



(define-record-type leftvar
  (leftvar u )
  leftvar?
  (u leftvar-variable)
  )


(define-record-type ref-variable
  (make-ref-variable  u idx )
  ref-variable?
  (u        ref-variable-u)
  (idx      ref-variable-idx)
  )


(define-record-type function
  (make-function formals body)
  function?
  (formals function-formals)
  (body    function-body))


(define-record-type equation
  (eq variable expr)
  equation?
  (variable equation-variable)
  (expr   equation-expr))


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
        ((ref-variable? x) (vector-ref (ref-variable-u x) (ref-variable-idx x)))
        ((list? x) (eval `(,(car x) . ,(map expr-value x))))
        (else x)))


(define (reinit x y)
  (cond ((leftvar? x) `(variable.reinit ,x ,y))
        ((variable? x)   (reinit (leftvar x) y))
        ((ref-variable? x)   (reinit (leftvar x) y))
        ((derivative-variable? x)   (reinit (leftvar x) y))
        (else (error 'reinit "invalid argument to reinit" x))
        ))


(define (ev condition p . rest)
  (if (null? rest)
      (make-event condition p '())
      (make-event condition p (car rest))))



;;(define (ifelse x y z)  `(variable.ifelse ,x ,y ,z))


;; A representation of a flattened model, normally created with
;; `elaborate(model)`. The code generation procedures take an
;; elaborated model as input.
;;
;; Contains the hierarchical equations, flattened equations, flattened
;; initial equations, events, event response functions, and a map of
;; Unknown nodes.


(define-record-type equation-set
  (make-equation-set model equations initial events pos-reponses neg-responses functions nodemap)
  equation-set?
  (model equation-set-model)
  (equations equation-set-equations)
  (initial equation-set-initial)
  (events equation-set-events)
  (pos-responses equation-set-pos-responses)
  (neg-responses equation-set-neg-responses)
  (functions equation-set-functions)
  (nodemap equation-set-nodemap)
  )


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





;;
;; Equation elaboration (flattening)
;;
;; The main steps in flattening are:
;;
;; Replace fixed initial values.
;; Flatten models and populate equations.
;; Pull out InitialEquations and populate initial
;; Pull out Events and populate events.
;; Handle StructuralEvents.
;;

(define (elaborate model)

  (let recur ((entries model)
              (definitions '())
              (equations '())
              (initial  '())
              (events   '())
              (pos-responses '())
              (neg-responses  '())
              (functions  '())
              (nodemap '())
              )
               
    (if (null? entries)

        (make-equation-set model
                           definitions
                           equations
                           (map-equations replace-fixed initial)
                           events
                           pos-responses
                           neg-responses
                           functions
                           nodemap)
        
        (let ((en (car entries)))

          (if (pair? en)

              (recur (append en (cdr entries))
                     definitions equations
                     initial events pos-responses neg-responses 
                     functions nodemap)

              (match en  
                     (($ definition s expr)
                      (recur (cdr entries)
                             (cons def definitions)
                             equations initial events pos-responses neg-responses 
                             functions
                             (env-enter s (freevars expr) nodemap)))
                     (($ equation s expr)
                      (recur (cdr entries)
                             definitions
                             (cons eq equations)
                             initial events pos-responses neg-responses 
                             functions
                             (env-enter s (freevars expr) nodemap)))
                     (($ initial-equation s expr)
                      (recur (cdr entries)
                             definitions
                             equations (cons eq initial) events pos-responses neg-responses 
                             functions
                             nodemap))
                     (($ structural-event left-condition left right-condition right)
                      (recur (cdr entries)
                             definitions
                             equations initial  ;; todo: generate conditional equations
                             (cons left-condition (cons right-condition events) )
                             pos-responses ;; todo: generate discrete equation
                             neg-responses
                             functions
                             nodemap))
                     (($ event condition pos neg)
                      (recur (cdr entries)
                             definitions
                             equations initial 
                             (cons en events) 
                             (cons pos pos-responses)
                             (or (and neg (cons neg neg-responses)) neg-responses)
                             functions
                             nodemap))
                     (($ function formals expr)
                      (recur (cdr entries)
                             definitions
                             equations initial events pos-responses neg-responses 
                             (cons fn functions)
                             nodemap))
                     (else
                      (error 'elaborate "unknown equation type" eq))
                     ))
          ))
    ))


(define (simcreate eqset)
  
  (let ((indexmap 
         (let recur ((nodemap (equation-set-nodemap eqset))
                     (indexmap '())
                     (index 1))
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

