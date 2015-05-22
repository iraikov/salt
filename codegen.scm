
(define nl "\n")


(define-datatype value value?
  (V:C       (v (lambda (x) (or (symbol? x) (number? x) ))))
  (V:Var     (name symbol?))
  (V:Sub     (index (lambda (x) (and (integer? x) (or (zero? x) (positive? x))))) (v value?))
  (V:Fn      (args list?) (body expr?))
  (V:Op      (name symbol?)  (args (lambda (x) (every value? x))))
  (V:Ifv     (test value?)  (ift value?) (iff value?))
  )

  
(define-record-printer (value x out)
  (fprintf out "~A"
	   (cases value x
		  (V:C  (v)    (sprintf "V:C ~A" v))
		  (V:Var (n)   (sprintf "V:Var ~A " n))
		  (V:Sub (i v)  (sprintf "V:Sub ~A ~A" i v))
		  (V:Ifv (test tv fv)   "V:Ifv ~A ~A ~A" test tv fv)
		  (V:Fn  (args body) (sprintf "V:Fn ~A = ~A" args body))
		  (V:Op (name args) (sprintf "(V:Op ~A ~A)" name args)))))
		  

(define-datatype binding binding?
  (B:Val     (name symbol?)  (v value?))
  )

  
(define-record-printer (binding x out)
  (fprintf out "~A"
	   (cases binding x
		  (B:Val (name v)       (sprintf "B:Val ~A = ~A" name v))
		  )))


(define-datatype expr expr?
  (E:Ife     (test value?)   (ift expr?) (iff expr?))
  (E:Let     (bnds (lambda (x) (every binding? x)))    (body expr?))
  (E:Set     (s symbol?) (v value?) (index (lambda (x) (and (integer? x) (or (zero? x) (positive? x))))) (v value?))
  (E:Noop)
  )

  
(define-record-printer (expr x out)
  (fprintf out "~A"
	   (cases expr x
		  (E:Ife (test ift iff) (sprintf "E:Ife ~A ~A ~A" test ift iff))
		  (E:Let (bnds body) (sprintf "E:Let ( ~A ) ~A" bnds body))
		  (E:Set (s v i)     (sprintf "E:Set ~A ~A ~A" s v i))
		  (E:Noop ()         (sprintf "E:Noop"))
		  )))




(define (name/scheme s)
  (let ((cs (string->list (->string s))))
    (let loop ((lst (list)) (cs cs))
      (if (null? cs) (string->symbol (list->string (reverse lst)))
	  (let* ((c (car cs))
		 (c1 (cond ((or (char-alphabetic? c) (char-numeric? c) 
				(char=? c #\_) (char=? c #\-)) c)
			   (else #\-))))
	    (loop (cons c1 lst) (cdr cs)))))))

(define (binding->scheme x)
  (cases binding x
	 (B:Val (name v)
                (list "(" (name/scheme name) " " (value->scheme v) ")" nl))))


(define (expr->scheme x . rest)
  (let-optionals rest ((bnd? #t))
    (cases expr x

	 (E:Ife     (test ift iff)
		    (list "(cond (" (value->scheme test) " " nl
			  " " (expr->scheme ift ) ")" nl
			  "(else " (expr->scheme iff) "))" nl))

	 (E:Let     (bnds body)
		    (list "(let* (" nl
			  (map (lambda (x) (binding->scheme x)) bnds) nl
			  ") " nl
			  (expr->scheme body #f) nl
			  ")" nl))
			  
	 (E:Set     (s v i)  
                    (list "(vector-set! " 
                          (name/scheme s) " " i " "
                          (value->scheme v)  
                          ")"))
		    
	 (E:Noop    () (list "(void)"))
	 )))


(define (value->scheme v)
  (let ((result
  (cases value v
	 (V:C       (v) v)
	 (V:Var     (name) (name/scheme name))
	 (V:Sub     (index v) 
		    (list "(vector-ref " (value->scheme v) " " index ")"))
	 (V:Fn      (args body) 
		    (list "(lambda (" (intersperse (map name/scheme args) " ") ") "
			  (expr->scheme body #f) ")"))
	 (V:Op     (name args)
		    (let* ((fp? (case name
				     ((add sub mul div gte gt lt lte neg)  #t)
				     (else #f)))
			   (op (if fp? (conc "fp" name) name)))
		      (cond ((null? args) 
			     (case name 
			       ((NONE)  (list "#f"))
			       (else    (list "(" name ")"))))
			    
			    (fp?
			     (if (pair? (cdr args))
				 (fold-right (lambda (x ax) (list "(" op " " (value->scheme x) " " ax ")"))
					     (list "(" op " " (value->scheme (car args)) " " (value->scheme (cadr args)) ")")
					     (cddr args))
				 (list "(" op " " (value->scheme (car args)) ")")
				 ))

			    (else
			     (list "(" op " " (intersperse (map value->scheme args) " ") ")")))))
	 (V:Ifv     (test ift iff)
		    (list "(if " (value->scheme test) " " 
			  (value->scheme ift) " " 
			  (value->scheme iff) ")"))

	 )))
    result))

  
(define (name/ML s)
  (let ((cs (string->list (->string s))))
    (let loop ((lst (list)) (cs cs))
      (cond ((null? cs) (string->symbol (list->string (reverse lst))))
	    ((null? (cdr cs))
	     (let ((c (car cs)))
	       (if (or (char-alphabetic? c) (char-numeric? c))
		   (loop (cons c lst) (cdr cs))
		   (loop (append (reverse (string->list (->string (gensym 't)))) lst) (cdr cs))
		   )))
	    (else
	     (let* ((c (car cs))
		    (c1 (cond ((or (char-alphabetic? c) (char-numeric? c) 
				   (char=? c #\_) (char=? c #\#)) c)
			      (else #\_))))
	       (loop (cons c1 lst) (cdr cs))))))))
			    

(define (binding->ML x . rest)
  (let-optionals rest ((bnd? #t))
    (cases binding x
	 (B:Val     (name v)
                    (list "val " (name/ML name) " = " (value->ML v) nl))
         )))


(define (expr->ML x . rest)
  (let-optionals rest ((bnd? #t))
    (cases expr x

	 (E:Ife     (test ift iff)
		    (list "if (" (value->ML test) ") " nl
			  "then " (expr->ML ift ) nl
			  "else " (expr->ML iff) nl))

	 (E:Let     (bnds body)
		    (list "let " nl
			  (map (lambda (x) (binding->ML x #t)) bnds) nl
			  "in " nl
			  (expr->ML body #f) nl
			  "end" nl))
			  
	 (E:Set     (s v i)  
                    (list "Unsafe.Array.update (" (name->ML s) ", " index  ", " (value->ML v) ")"))
		    
	 (E:Noop    () (list "()"))
	 )))


(define (value->ML v)
  (cases value v
	 (V:C       (v) (cond ((and (number? v) (negative? v))
			       (list "~" (abs v)))
			      (else v)))
	 (V:Var     (name) (name/ML name))
	 (V:Sub     (index v) 
		    (list "Unsafe.Array.sub (" (value->ML v) ", " index ")"))
	 (V:Fn      (args body) 
		    (list "(fn (" (intersperse (map name/ML args) ",") ") => "
			  (expr->ML body #f) ")"))
	 (V:Op    (name args)
		    (let* ((infix? (case name
				     ((add sub mul div gte gt lt lte)  #t)
				     (else #f)))
			   (op (if infix? (list "(op " name ")") name)))
                      (let ((name1 (name/ML name)))
                        (cond ((null? args) 
                               (case name 
                                 ((NONE)  (list name1))
                                 (else    (list name1 "()"))))
                              ((null? (cdr args))
                               (list "(" op " " (value->ML (car args)) ")"))
                              ((and infix? (pair? (cddr args)))
                               (list "(foldr " op "(" (value->ML (V:Op name (list (car args) (cadr args)))) ")"
                                     "[" (intersperse (map value->ML (cddr args)) ",") "])"))
                              (else
                               (list "(" op "(" (intersperse (map value->ML args) ",") "))"))))))
	 (V:Ifv     (test ift iff)
		    (list "(if (" (value->ML test) ") " 
			  "then " (value->ML ift ) " " 
			  "else " (value->ML iff) ")"))

	 ))
