
;; Based on the Scheme dynamic type inferencer by Andrew Wright.


;----------------------------------------------------------------------------
; Environment management
;----------------------------------------------------------------------------

;; Environments are lists of pairs, the first component being the key

;; General environment operations:
;;
;; empty-env: Env
;; gen-binding: Key x Value -> Binding
;; binding-key: Binding -> Key
;; binding-value: Binding -> Value
;; binding-show: Binding -> Symbol*
;; extend-env-with-binding: Env x Binding -> Env
;; extend-env-with-env: Env x Env -> Env
;; env-lookup: Key x Env -> (Binding + False)
;; env->list: Env -> Binding*
;; env-show: Env -> Symbol*

;; Environments stacks are lists of environments, the first element in
;; the list being the current environment.  The lookup operation
;; traverses the entire stack until a key is found or the last element
;; in the stack is reached. All other operations apply to the current
;; environment.
;;
;; make-env-stack: Env -> Stack Env
;; extend-env-stack-with-binding: Stack Env x Binding -> Stack Env
;; extend-env-stack-with-env: Stack Env x Env -> Stack Env
;; pop-env-stack: Stack Env -> Stack Env
;; push-env-stack: Stack Env x Env -> Stack Env
;; env-stack-lookup: Key x Env -> (Binding + False)


; bindings

(define gen-binding cons)
; generates a binding, binding a symbol to a value

(define binding-key car)
; returns the key of a binding

(define binding-value cdr)
; returns the value of a binding

(define (key-show key)
  ; default show procedure for keys
  key)

(define (value-show value)
  ; default show procedure for values
  value)

(define (binding-show binding)
  ; returns a printable representation of a binding
  (cons (key-show (binding-key binding))
        (cons ': (value-show (binding-value binding)))))


; environments

(define empty-env '())
; returns the empty environment

(define (extend-env-with-binding env binding)
  ; extends env with a binding, which hides any other binding in env
  ; for the same key
  ; returns the extended environment
  (cons binding env))

(define (extend-env-with-binding env binding)
  ; extends env with a binding, which hides any other binding in env
  ; for the same key (see dynamic-lookup)
  ; returns the extended environment
  (cons binding env))

(define (extend-env-with-env env ext-env)
  ; extends environment env with environment ext-env 
  ; a binding for a key in ext-env hides any binding in env for
  ; the same key (see dynamic-lookup)
  ; returns the extended environment
  (append ext-env env))

(define env-lookup (lambda (x l) (assv x l)))
; returns the first pair in env that matches the key; returns #f
; if no such pair exists

(define (env->list e)
  ; converts an environment to a list of bindings
  e)

(define (env-show env)
  ; returns a printable list representation of an environment
  (map binding-show env))


; stacks of environments

(define empty-env-stack '())

(define (make-env-stack env) (list env))

(define (extend-env-stack-with-binding env-stack binding)
  (cons (extend-env-with-binding (car env-stack) binding) (cdr env-stack)))
  
(define (extend-env-stack-with-env env-stack env)
  (cons (extend-env-with-env (car env-stack) env) (cdr env-stack)))
  
(define (pop-env-stack env-stack) (cdr env-stack))

(define (push-env-stack env-stack env) (cons env env-stack))

;; env-stack-lookup: Key x Env -> (Binding + False)

(define (env-stack-lookup x env-stack) 
  (if (null? env-stack) #f
      (begin
        (or (env-lookup x (car env-stack))
            (env-stack-lookup x (cdr env-stack)))
        )))
      
