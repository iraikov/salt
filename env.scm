
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
;; env-size: Env -> Int

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
;; push-env-stack: Env x Stack Env -> Stack Env
;; env-stack-lookup: Key x Env -> (Binding + False)


; bindings

(define (env-binding? x) 
  (pair? x))

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

(define-datatype env env?
  (Env (first env-binding?) (rest env?))
  (EmptyEnv))

(define empty-env (EmptyEnv))
; returns the empty environment

(define (env-empty? e)
  (cases env e
         (EmptyEnv () #t)
         (Env (first rest) #f)))

(define (extend-env-with-binding e binding)
  ; extends env with a binding, which hides any other binding in env
  ; for the same key
  ; returns the extended environment
  (Env binding e))

(define (extend-env-with-env e ext-env)
  ; extends environment env with environment ext-env 
  ; a binding for a key in ext-env hides any binding in env for
  ; the same key (see dynamic-lookup)
  ; returns the extended environment
  (cases env ext-env
         (EmptyEnv () e)
         (Env (first rest)
              (Env first (extend-env-with-env e rest)))
         ))

(define (env-lookup x e)
; returns the first pair in env that matches the key; returns #f
; if no such pair exists
  (cases env e 
         (Env (first rest)
              (if (eqv? (binding-key first) x)
                  first
                  (env-lookup x rest)))
         (EmptyEnv () #f)
         ))


(define (env->list e)
  ; converts an environment to a list of bindings
  (cases env e
         (Env (first rest)
              (cons first (env->list rest)))
         (EmptyEnv () '())))


(define (env-show e)
  ; returns a printable list representation of an environment
  (cases env e
         (Env (first rest)
              (cons (binding-show first) (env-show rest)))
         (EmptyEnv () '())))


(define (env-size e)
  ; returns the number of items in the environment
  (let recur ((e e) (n 0))
    (cases env e
           (Env (first rest)
                (recur rest (+ n 1)))
           (EmptyEnv () n))
    ))


; stacks of environments

(define-datatype env-stack env-stack?
  (EnvStack (first env?) (rest env-stack?))
  (EmptyEnvStack))

(define empty-env-stack (EmptyEnvStack))

(define (extend-env-stack-with-binding es binding)
  (cases env-stack es
         (EnvStack (first rest)
                   (EnvStack
                    (extend-env-with-binding first binding) 
                    rest))
         (EmptyEnvStack ()
                        (error 'extend-env-stack-with-binding
                               "empty environment stack"))
         ))
  
(define (extend-env-stack-with-env es env)
  (cases env-stack es
         (EnvStack (first rest)
                   (EnvStack
                    (extend-env-with-env first env) 
                    rest))
         (EmptyEnvStack ()
                        (error 'extend-env-stack-with-env
                               "empty environment stack"))
         ))
  

(define (pop-env-stack es) 
  (cases env-stack es
         (EnvStack (first rest) rest)
         (EmptyEnvStack ()
                        (error 'pop-env-stack
                               "empty environment stack"))
         ))
  

(define (push-env-stack e es) 
  (if (env? e) (EnvStack e es)
      (error 'push-env-stack "invalid environment" e)))
      

(define (peek-env-stack es) 
  (cases env-stack es
         (EnvStack (first rest) first)
         (EmptyEnvStack ()
                        (error 'peek-env-stack
                               "empty environment stack"))
         ))
  

;; env-stack-lookup: Key x Env -> (Binding + False)

(define (env-stack-lookup x es) 
  (cases env-stack es
         (EnvStack (first rest) 
                   (or (env-lookup x first)
                       (env-stack-lookup x rest)))
         (EmptyEnvStack () #f)))
                        

(define (env-stack-show es)
  (cases env-stack es
         (EnvStack (first rest) 
                   (cons (env-show first)
                         (env-stack-show rest)))
         (EmptyEnvStack () '())))
                        
