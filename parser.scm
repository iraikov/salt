

; forall?

(define (forall? pred list)
  (if (null? list)
      #t
      (and (pred (car list)) (forall? pred (cdr list)))))

; list-of-1?

(define (list-of-1? l)
  (and (pair? l) (null? (cdr l))))

; list-of-2?

(define (list-of-2? l)
  (and (pair? l) (pair? (cdr l)) (null? (cddr l))))

; list-of-3?

(define (list-of-3? l)
  (and (pair? l) (pair? (cdr l)) (pair? (cddr l)) (null? (cdddr l))))

; list-of-list-of-2s?

(define (list-of-list-of-2s? e)
  (cond
   ((null? e)
    #t)
   ((pair? e)
    (and (list-of-2? (car e)) (list-of-list-of-2s? (cdr e))))
   (else #f)))

;; Infix expression parser for equation expressions

;; Chicken Scheme implementation of the box routines.  Based on
;; dfa2.sc in the benchmarks code supplied with Stalin 0.11

(define-record-type box (make-box contents)
  box? (contents box-contents box-contents-set!))

(define box make-box)
(define unbox box-contents)
(define set-box! box-contents-set!)

;; Stack routines.  Based on dfa2.sc in the benchmarks code supplied
;; with Stalin 0.11

(define (make-stack)
  (box '()))

(define (stack-empty? s)
  (null? (unbox s)))

(define (stack-push! s obj)
  (set-box! s (cons obj (unbox s)))
  s)

(define (stack-pop! s)
  (let ((l (unbox s)))
    (set-box! s (cdr l))
    (car l)))

(define (stack-cut! s start end)
  (cond
   ((negative? start)
    (error 'stack-cut! "start depth must be >= 0"))
   ((negative? end)
    (error 'stack-cut! "end depth must be >= 0"))
   ((< end start)
    (error 'stack-cut! "start depth must be <= to the end depth")))
  (let ((l (unbox s)))
    (let loop ((i 0) (l l) (nl (list)))
      (if (null? l) (set-box! s (reverse nl))
	  (if (and (>= i start) (<= i end))
	      (loop (+ i 1) (cdr l) nl)
	      (loop (+ i 1) (cdr l) (cons (car l) nl))))))
  s)


(define (stack-depth s)
  (let ((l (unbox s)))
    (length l)))


(define (stack-peek s)
  (let ((l (unbox s)))
    (car l)))


(define stack->list unbox)
(define (list->stack lst)
  (and (pair? lst) (box lst)))


(define-syntax tok
  (syntax-rules ()
    ((tok loc t) (make-lexical-token (quasiquote t) loc #f))
    ((tok loc t l) (make-lexical-token (quasiquote t) loc l))))


(define (make-parse-error loc)
  (lambda (msg #!optional arg)
    (let ((loc-str (or (and loc (if (list? loc) (conc " " loc " ") (conc " (" loc ") "))) "")))
      (cond  [(not arg) (error loc-str msg)]
	     [(lexical-token? arg)
	      (salt:error (conc "line " (source-location-line (lexical-token-source arg)) ": " msg) loc-str
		     (conc (lexical-token-category arg) 
			   (if (lexical-token-value arg) (conc " " (lexical-token-value arg)) "")))]
	     [else (salt:error loc-str (conc msg arg))]
	     ))))


(define (make-char-lexer port errorp loc)
  (lambda ()
    (letrec ((skip-spaces
              (lambda ()
                (let loop ((c (peek-char port)))
                  (if (and (not (eof-object? c))
                           (or (char=? c #\space) (char=? c #\tab)))
                      (begin
                        (read-char port)
                        (loop (peek-char port)))))))
             (read-number
              (lambda (l e? minus?)
                (let ((c (peek-char port)))
                  (if (and (char? c) 
			   (or (char-numeric? c) (case c ((#\. #\e) c) (else #f))
			       (and e? (not minus?) (char=? c #\-))))
                      (read-number (cons (read-char port) l)
				   (or e? (char=? c #\e))
				   (or minus? (char=? c #\-)))
		      (let ((s (list->string (reverse l))))
			(let ((n (string->number s)))
			  (if (not n) (errorp "invalid numeric string: " s) n))
			  )))))
             (read-id
              (lambda (l)
                (let ((c (peek-char port)))
                  (if (and (char? c) (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)))
                      (read-id (cons (read-char port) l))
                      (string->symbol (apply string (reverse l))))))))

      ;; -- skip spaces
      (skip-spaces)
      ;; -- read the next token
      (let loop ((c (read-char port)))
        (cond
         ((eof-object? c)      '*eoi*)
         ((char=? c #\>)       '>)
         ((char=? c #\<)       '<)
         ((char=? c #\^)       '^)
         ((char=? c #\+)       '+)
         ((char=? c #\-)       (let ((k (peek-char port)))
				 (if (or (char-numeric? k) (char=? k #\.))
				     (let ((n (read-number (list c) #f #f)))
				       (tok loc NUM n)) '-)))
         ((char=? c #\*)       '*)
         ((char=? c #\/)       '/)
         ((char=? c #\=)       '=)
         ((char=? c #\?)       (tok loc QUESTION))
         ((char=? c #\:)       (tok loc COLON))
         ((char=? c #\,)       (tok loc COMMA))
         ((char=? c #\~)       (tok loc COMMA))
         ((char=? c #\()       (tok loc LPAREN))
         ((char=? c #\))       (tok loc RPAREN))
         ((or (char-numeric? c)  (char=? c #\.) (char=? c #\-))
	  (tok loc NUM (read-number (list c) #f #f)))
         ((char-alphabetic? c)  
	  (let ((id (read-id (list c))))
	    (case id
	      ((let LET Let)       (tok loc LET))
	      ((if IF If)          (tok loc IF))
	      ((then THEN Then)    (tok loc THEN))
	      ((else ELSE Else)    (tok loc ELSE))
	      (else 
	       (tok loc ID id)))
	    ))
	 ((or (char=? c #\space) 
	      (char=? c #\tab)
	      (char=? c #\newline)
	      (char=? c #\return))
	  (loop (read-char port)))
         (else
          (errorp "illegal character: " c)
          (skip-spaces)
          (loop (read-char port))))))))


(include "expr.grm.scm")


(define (port-line port) 
  (let-values (((line _) (port-position port)))
    line))
  
(define (port-column port)
  (let-values (((_ column) (port-position port)))
    column))


(define (parse-string-infix-expr s #!optional loc)
  (or (and (string? s) (string-null? s) '())
      (let ((port
	     (cond ((string? s)  (open-input-string s))
		   ((port? s)    s)
		   (else (salt:error 'parse-string-expr "bad argument type: not a string or a port: " s)))))
	(expr-parser  (let ((ll (make-char-lexer port (make-parse-error loc) (make-source-location loc (port-line port) (port-column port) -1 -1))))
			(lambda ()
			  (let ((t (ll)))
			    t)))
		      (make-parse-error loc)
		      ))))


(define (make-sym-lexer lst errorp loc)
  (if (not (list? lst)) (errorp "illegal list: " lst))
  (let ((is (make-stack)))
    (stack-push! is lst)
    (lambda ()
      (if (stack-empty? is)  '*eoi*
	  (let* ((p     (stack-pop! is))
		 (x     (and (not (null? p)) (car p)))
		 (t     (if x
			    (begin (stack-push! is (cdr p))
				   (match x
					  ((or '== '< '> '>= '<= '^ '+ '- '* '/ '= )      x)
					  ('?           (tok loc QUESTION))
					  (':           (tok loc COLON))
					  ('~           (tok loc COMMA))
					  ((or 'let 'LET)     (tok loc LET))
					  ((or 'if  'IF)      (tok loc IF))
					  ((or 'else 'ELSE)   (tok loc ELSE))
					  ((? number?)  (tok loc NUM x))
					  ((? symbol?)  (tok loc ID x))
					  ((? list?)    (begin 
                                                          (stack-push! is x)
                                                          (tok loc LPAREN)))
					  (else (errorp "invalid input: " x))))
			    (if (not (stack-empty? is)) (tok loc RPAREN) '*eoi*))))
	    t)))))

(define (parse-sym-infix-expr lst #!optional loc)
  (let ((ret (cond ((number? lst)  lst)
		   ((symbol? lst)  lst)
		   ((and (list? lst) (null? lst) '()))
		   (else (expr-parser  (make-sym-lexer lst (make-parse-error loc) (make-source-location loc 0 0 -1 -1))
				       (make-parse-error loc))))))
    ret))
    



;; Main parsing routines

(define (parse-datum e)
  (cond
   ((boolean? e) (constant 'boolean e unitless))
   ((number? e)  (constant 'number e unitless))
   ((symbol? e)  (constant 'symbol e unitless))
   ((vector? e)  (constant 'vector (vector->list e) unitless))
   (else (salt:error 'parse-datum "Unknown datum: " e))
   ))



(define (parse-formal f-env e)
  ; e is an arbitrary object, f-env is a forbidden environment;
  ; returns: a variable definition (a binding for the symbol), plus
  ; the value of the binding as a result
  (if (symbol? e)
      (cond
       ((memq e syntactic-keywords)
        (salt:error 'parse-formal "Illegal identifier (keyword): " e))
       ((env-lookup e f-env)
        (salt:error 'parse-formal "Duplicate variable definition: " e))
       (else (let ((result (make-var-def e)))
               (cons (gen-binding e result) result))))
      (salt:error 'parse-formal "Not an identifier: " e)))



(define (parse-formal* formals)
  ;; parses a list of formals and returns a pair consisting of generated
  ;; environment and list of parsing action results
  (letrec
      ((pf*
        (lambda (f-env results formals)
          ;; f-env: "forbidden" environment (to avoid duplicate defs)
          ;; results: the results of the parsing actions
          ;; formals: the unprocessed formals
          ;; Note: generates the results of formals in reverse order!
          (cond
           ((null? formals)
            (cons f-env results))
           ((pair? formals)
            (let* ((fst-formal (car formals))
                   (binding-result (parse-formal f-env fst-formal))
                   (binding (car binding-result))
                   (var-result (cdr binding-result)))
              (pf*
               (extend-env-with-binding f-env binding)
               (cons var-result results)
               (cdr formals))))
           (else (salt:error 'parse-formal* "Illegal formals: " formals))))))
    (let ((renv-rres (pf* empty-env '() formals)))
      (cons (car renv-rres) (reverse (cdr renv-rres))))))



(define (parse-formals formals)
  ;; returns a pair: env and result
  (letrec ((pfs (lambda (f-env formals)
                  (cond
                   ((null? formals)
                    (cons empty-env (make-null-formal)))
                   ((pair? formals)
                    (let* ((fst-formal (car formals))
                           (rem-formals (cdr formals))
                           (bind-res (parse-formal f-env fst-formal))
                           (bind (car bind-res))
                           (res (cdr bind-res))
                           (nf-env (extend-env-with-binding f-env bind))
                           (renv-res* (pfs nf-env rem-formals))
                           (renv (car renv-res*))
                           (res* (cdr renv-res*)))
                      (cons
                       (extend-env-with-binding renv bind)
                       (make-pair-formal res res*))))
                   (else
                    (let* ((bind-res (parse-formal f-env formals))
                           (bind (car bind-res))
                           (res (cdr bind-res)))
                      (cons
                       (extend-env-with-binding empty-env bind)
                       res)))))))
    (pfs empty-env formals)))


; Expr

(define (parse-expression env e)
  (cond
   ((symbol? e)
    (parse-variable env e))
   ((pair? e)
    (let ((op (car e)) (args (cdr e)))
      (case op
        ((if)   (parse-if env args))
        ((cond) (parse-cond env args))
        ((and)  (parse-and env args))
        ((or)   (parse-or env args))
        ((let)  (parse-let env args))
        (else   (parse-function-call env op args)))))
   (else (parse-datum e))
  ))


(define (parse-expression* env exprs)
  ;; Parses lists of expressions (returns them in the right order!)
  (letrec ((pe*
            (lambda (results es)
              (cond
               ((null? es) results)
               ((pair? es) (pe* (cons (parse-expression env (car es)) results) (cdr es)))
               (else (salt:error 'parse-expression* "Not a list of expressions: " es))))))
    (reverse (pe* '() exprs))))


; parse-expressions

(define (parse-expressions env exprs)
  ;; parses lists of arguments of a procedure call
  (cond
   ((null? exprs) (make-null-arg))
   ((pair? exprs) (let* ((fst-expr (car exprs))
                         (rem-exprs (cdr exprs))
                         (fst-res (parse-expression env fst-expr))
                         (rem-res (parse-expressions env rem-exprs)))
                    (make-pair-arg fst-res rem-res)))
   (else (salt:error 'parse-expressions "Illegal expression list: "
                exprs))))



(define (parse-variable env e)
  (if (symbol? e)
      (if (memq e syntactic-keywords)
          (salt:error 'parse-variable "Illegal identifier (keyword): " e)
          (let ((assoc-var-def (env-lookup e env)))
            (if assoc-var-def
                (binding-value assoc-var-def)
                (make-free-variable e))))
      (match e
             (('der ((and x (? symbol?)))) 
              (let ((assoc-var-def (env-lookup x env)))
                (make-derivative-variable
                 (if assoc-var-def
                     (binding-value assoc-var-def)
                     (make-free-variable x)))))
             (((and x (? symbol?)))
              (let ((assoc-var-def (env-lookup x env)))
                (if assoc-var-def
                    (binding-value assoc-var-def)
                    (make-free-variable x))))
             (else (salt:error 'parse-variable "Not an identifier: " e)))
      ))


(define (parse-function-call env op args)
  `(signal.call
    ,(parse-expression env op)
    ,(parse-expressions env args)))


(define (parse-if env args)
  (cond
   ((list-of-3? args)
    `(signal.ifelse
      ,(parse-expression env (car args))
      ,(parse-expression env (cadr args))
      ,(parse-expression env (caddr args))))
   ((list-of-2? args)
    `(signal.if
      ,(parse-expression env (car args))
      ,(parse-expression env (cadr args))
      ))
   (else (salt:error 'parse-if "Not an if-expression: " args))))


(define (parse-cond env args)
  (if (and (pair? args) (list? args))
      `(signal.cond . ,(map (lambda (e)
                              (parse-cond-clause env e))
                            args))
      (salt:error 'parse-cond "Not a list of cond-clauses: " args)))


(define (parse-cond-clause env e)
  ;; ***Note***: Only (<test> <sequence>) is permitted!
  (if (pair? e)
      (cons
       (if (eqv? (car e) 'else)
           '(empty)
           (parse-expression env (car e)))
       (parse-expression env (cdr e)))
      (salt:error 'parse-cond-clause "Not a cond-clause: " e)))


(define (parse-and env args)
  (if (list? args)
      `(signal.and . ,(parse-expression* env args))
      (salt:error 'parse-and "Not a list of arguments: " args)))


(define (parse-or env args)
  (if (list? args)
      `(signal.or . ,(parse-expression* env args))
      (salt:error 'parse-or "Not a list of arguments: " args)))


(define (parse-let env args)
  (if (pair? args)
      (let* ((bindings (car args))
             (body (cdr args))
             (env-ast (parse-sequential-bindings env bindings))
             (nenv (car env-ast))
             (bresults (cdr env-ast)))
        `(signal.let ,bresults ,(parse-expression (extend-env-with-env env nenv) body)))
      (salt:error 'parse-let* "Illegal bindings/body: " args)))


(define (parse-sequential-bindings env bindings)
  ; returns a pair consisting of an environment
  ; and a list of pairs (variable . asg)
  ;; ***Note***: the list of pairs is returned in reverse unzipped form!
  (letrec
      ((psb
        (lambda (f-env c-env var-defs expr-asgs binds)
          ;; f-env: forbidden environment
          ;; c-env: constructed environment
          ;; var-defs: results of formals
          ;; expr-asgs: results of corresponding expressions
          ;; binds: reminding bindings to process
          (cond
           ((null? binds)
            (cons f-env (cons var-defs expr-asgs)))
           ((pair? binds)
            (let ((fst-bind (car binds)))
              (if (list-of-2? fst-bind)
                  (let* ((fbinding-bres
                          (parse-formal f-env (car fst-bind)))
                         (fbind (car fbinding-bres))
                         (bres (cdr fbinding-bres))
                         (new-expr-asg
                          (parse-expression c-env (cadr fst-bind))))
                    (psb
                     (extend-env-with-binding f-env fbind)
                     (extend-env-with-binding c-env fbind)
                     (cons bres var-defs)
                     (cons new-expr-asg expr-asgs)
                     (cdr binds)))
                  (salt:error 'parse-sequential-bindings
                         "Illegal binding: " fst-bind))))
           (else (salt:error 'parse-sequential-bindings
                        "Illegal bindings: " binds))))))
    (let ((env-vdefs-easgs (psb empty-env env '() '() bindings)))
      (cons (car env-vdefs-easgs)
            (cons (reverse (cadr env-vdefs-easgs))
                  (reverse (cddr env-vdefs-easgs)))))))


(define (parse-function env args)
  (if (pair? args)
      (let ((pattern (car args))
            (exp-or-body (cdr args)))
        (cond
         ((pair? pattern)
          (let* ((function-name (car pattern))
                 (function-arg-names (cdr pattern))
                 (env-ast (parse-formals function-arg-names))
                 (formals-env (car env-ast))
                 (formals-ast (cdr env-ast)))
            (d 'parse-function "formals-ast = ~A formals-env = ~A~%" formals-ast formals-env)
            (match exp-or-body
                   (('= . expr)
                    (function
                     (free-variable-name 
                      (parse-variable env function-name))
                     formals-ast
                     (parse-expression (extend-env-with-env env formals-env) 
                                       (parse-sym-infix-expr expr))))
                   (else (salt:error 'parse-function "Not a valid function body: " exp-or-body))
                   ))
          )
         (else (salt:error 'parse-function "Not a valid pattern: " pattern))))
      (salt:error 'parse-function "Not a valid definition: " args)))


;; The number of base quantities defined
(define Q 9)

(define (parse-dim-vector dims)
  (let ((n (length dims)))
    (if (< n Q)
        (parse-dim-vector (append dims (list-tabulate (- Q n) (lambda (x) 0))))
        (dimint (list->s32vector dims))
        )))
  
(define (parse-define-unit pattern rhs env)
  (match rhs 
         ((('dim-vector . dims) factor-expr)
          (make-unit (free-variable-name (parse-variable env pattern))
                     (parse-dim-vector dims)
                     (parse-sym-infix-expr factor-expr) '()))
         ((dim-expr factor-expr)
          (d 'parse-define-unit "dim-expr = ~A~%" dim-expr)
          (make-unit (free-variable-name (parse-variable env pattern))
                     (parse-sym-infix-expr dim-expr)
                     (parse-sym-infix-expr factor-expr) '()))
         (else
          (salt:error 'parse-define-unit "Not a valid unit definition: " rhs))
         ))
  

(define (parse-definition env args)
  (if (pair? args)
      (let ((pattern (car args))
            (exp-or-body (cdr args)))
        (cond
         ((symbol? pattern)
          (match exp-or-body
                 (('= 'discrete ('dim u) . expr)
                  (discrete
                   (parse-expression env (parse-sym-infix-expr expr))
                   (free-variable-name 
                    (parse-variable env pattern))
                   (cdr (assv u (model-quantities)))
                   ))
                 (('= 'discrete . expr)
                  (discrete
                   (parse-expression env (parse-sym-infix-expr expr))
                   (free-variable-name 
                    (parse-variable env pattern))
                   Unity))
                 (('= 'unknown ('dim u) . expr)
                  (unknown
                   (parse-expression env (parse-sym-infix-expr expr))
                   (free-variable-name 
                    (parse-variable env pattern))
                   (cdr (assv u (model-quantities)))
                   ))
                 (('= 'unknown . expr)
                  (unknown
                   (parse-expression env (parse-sym-infix-expr expr))
                   (free-variable-name 
                    (parse-variable env pattern))
                   Unity))
                 (('= 'parameter ('dim u) . expr)
                  (parameter
                   (gensym 'p)
                   (free-variable-name 
                    (parse-variable env pattern))
                   (parse-expression env (parse-sym-infix-expr expr))
                   (cdr (assv u (model-quantities)))
                   ))
                 (('= 'parameter . expr)
                  (parameter
                   (gensym 'p)
                   (free-variable-name 
                    (parse-variable env pattern))
                   (parse-expression env (parse-sym-infix-expr expr))
                   Unity
                   ))
                 (('= 'constant ('unit u) . expr)
                  (constant
                   (free-variable-name
                    (parse-variable env pattern))
                   (parse-expression env (parse-sym-infix-expr expr))
                   (cdr (assv u (model-units)))
                   ))
                 (('= 'constant . expr)
                  (constant
                   (free-variable-name
                    (parse-variable env pattern))
                   (parse-expression env (parse-sym-infix-expr expr))
                   Unity
                   ))
                 (('= 'unit . args)
                  (parse-define-unit pattern args env))
                 (else (salt:error 'parse-define "Not a valid definition expression: " exp-or-body))
                 ))
         (else (salt:error 'parse-define "Not a valid pattern: " pattern))))
      (salt:error 'parse-define "Not a valid definition: " args)))


(define (parse-equation env args)
  (if (pair? args)
      (let ((pattern (car args))
            (rhs (cdr args)))
        (match rhs 
               (('= . rhs)
                (eq (parse-variable env pattern)
                    (parse-expression env (parse-sym-infix-expr rhs))))
               (else
                (salt:error 'parse-equation "Not a valid rhs: " rhs))
               ))
      (salt:error 'parse-equation "Not a valid equation: " args)))


(define (parse-reinit env asgn)
  (match asgn 
         ((pattern ':= . rhs)
          `(reinit ,(parse-variable env pattern)
                   ,(parse-expression env (parse-sym-infix-expr rhs))))
         (else
          (salt:error 'parse-reinit "Not a valid reinit: " asgn))
         ))

(define (parse-event env args)
  (if (pair? args)
      (let ((condition (parse-expression env (parse-sym-infix-expr (car args))))
            (pos-asgns (cadr args))
            (neg-asgns (if (list-of-2? args) '() (caddr args))))
        (let ((pos-asgns1 (map (lambda (x) (parse-reinit env x)) pos-asgns))
              (neg-asgns1 (map (lambda (x) (parse-reinit env x)) neg-asgns)))
          (make-event (gensym 'e) condition pos-asgns1 neg-asgns1)))
      (salt:error 'parse-event "Not a valid event: " args)))


(define (parse-transition env args)
  (d 'parse-transition "args = ~A~%" args)

  (if (pair? args)
      (let* (
             (target (parse-variable env (car args)))
             (condition (parse-expression env (parse-sym-infix-expr (cadr args))))
             (response (map (lambda (x) (parse-reinit env x)) (caddr args)))
             )
        (make-transition target condition response)
        )
      (error 'parse-transition "Not a valid transition: " args)
      ))


(define (parse-structural-event env args)
  (d 'parse-structural-event "(cadr args) = ~A~%" (cadr args))
  (d 'parse-structural-event "(caddr args) = ~A~%" (caddr args))

  (if (pair? args)
      (let (
            (label (parse-variable env (car args)))
            (equations (map (lambda (x) (parse-equation env x)) (cadr args)))
            (transition (parse-transition env (caddr args)))
            )
        (make-structural-event (gensym 'se) (free-variable-name label) equations transition))
      (salt:error 'parse-event "Not a valid structural event: " args)))


(define (parse-declaration env c)
  (d 'parse-declaration "c = ~A~%" c)
  (if (pair? c)
      (let ((op (car c))
            (args (cdr c)))
        (d 'parse-declaration "op = ~A args = ~A~%" op args)
        (case op
          ((fun function)    
           (parse-function env args))
          ((define)      
           (parse-definition env args))
          ((event)       
           (parse-event env args))
          ((structural-event)       
           (parse-structural-event env args))
          (else          
           (parse-equation env c)))
        )
      (parse-equation env c)))
