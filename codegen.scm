
(define nl "\n")

;; based on SRV:send-reply by Oleg Kiselyov
(define (print-fragments b out)
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
       (display (car fragments) out)
       (loop (cdr fragments) #t)))))


(define-datatype value value?
  (V:C       (v (lambda (x) (or (symbol? x) (number? x) (boolean? x) ))))
  (V:Var     (name symbol?))
  (V:Sub     (v value?) (index (lambda (x) (and (integer? x) (or (zero? x) (positive? x))))) )
  (V:Vec     (vals (lambda (x) (every value? x))))
  (V:Fn      (args list?) (body stmt?))
  (V:Op      (name symbol?)  (args (lambda (x) (every value? x))))
  (V:Ifv     (test value?)  (ift value?) (iff value?))
  )

  
(define-record-printer (value x out)
  (fprintf out "~A"
	   (cases value x
		  (V:C  (v)      (sprintf "(V:C ~A)" v))
		  (V:Var (n)     (sprintf "(V:Var ~A)" n))
		  (V:Sub (v i)   (sprintf "(V:Sub ~A ~A)" v i))
                  (V:Vec  (lst)  (sprintf "(V:Vec ~A)" lst))
		  (V:Ifv (test tv fv) (sprintf  "(V:Ifv ~A ~A ~A)" test tv fv))
		  (V:Fn  (args body) (sprintf "(V:Fn ~A = ~A)" args body))
		  (V:Op (name args) (sprintf "(V:Op ~A ~A)" name args))
                  ))
  )
		  

(define-datatype binding binding?
  (B:Val     (name symbol?)  (v value?))
  )

  
(define-record-printer (binding x out)
  (fprintf out "~A"
	   (cases binding x
		  (B:Val (name v)       (sprintf "B:Val ~A = ~A" name v))
		  )))


(define-datatype stmt stmt?
  (E:Ife     (test value?)   (ift stmt?) (iff stmt?))
  (E:Let     (bnds (lambda (x) (every binding? x)))    (body stmt?))
  (E:Begin   (stmts (lambda (x) (every stmt? x))))
  (E:Ret     (v value?))
  (E:Noop)
  )

  
(define-record-printer (stmt x out)
  (fprintf out "~A"
	   (cases stmt x
		  (E:Ife (test ift iff) (sprintf "E:Ife ~A ~A ~A" test ift iff))
		  (E:Let (bnds body) (sprintf "E:Let ( ~A ) ~A" bnds body))
		  (E:Begin (stmts)   (sprintf "E:Begin ~A" stmts))
                  (E:Ret (v)         (sprintf "E:Ret ~A" v))
		  (E:Noop ()         (sprintf "E:Noop"))
		  )))


(define (codegen-ODE sim)

  (define (update-bucket k v alst)
    (let recur ((alst alst)
                (res  '()))
      (cond ((null? alst)
             (cons (list k v) res))
            ((equal? k (car (car alst)))
             (append (cons (cons k (cons v (cdr (car alst)))) res)
                     (cdr alst)))
            (else
             (recur (cdr alst) (cons (car alst) res)))
            ))
    )


  (define (bucket f lst)
    (let recur ((lst lst)
                (res '()))
      (if (null? lst) res
          (let ((k (f (car lst))))
            (recur (cdr lst) (if k (update-bucket k (car lst) res) res)))
          ))
      )


  (define (fold-reinits lst)
    (let recur ((lst (cdr lst)) (expr (car lst)))
      (if (null? lst) expr
          (match (car lst)
                 (('signal.reinit ev ('getindex vect index) rhs)
                  (recur (cdr lst) `(signal.reinit ,ev ,expr ,rhs)))
                 (else (error 'fold-reinits "unknown reinit equation" (car lst)))))
      ))


  (define (fold-reinit-blocks ode-inds blocks)
    (sort
     (filter-map
      (lambda (x) 
        (let ((reinits 
               (filter-map 
                (match-lambda
                 [('setindex 'y index val) (and (vector-ref ode-inds index) val)]
                 [('setindex 'd index val) val]
                 [else #f])
                (cdr x))))
          (and (pair? reinits) (cons (car x) (fold-reinits reinits)))))
      blocks)
     (lambda (x y) (< (car x) (car y))))
    )
    

  (define (fold-regime-reinit-blocks blocks)
    (sort
     (filter-map
      (lambda (x) 
        (let ((reinits 
               (filter-map 
                (match-lambda
                 [('setindex 'r index val) val]
                 [else #f])
                (cdr x))))
          (and (pair? reinits) (cons (car x) (fold-reinits reinits)))))
      blocks)
     (lambda (x y) (< (car x) (car y))))
    )
    

  (define (ode-def? cindexmap ode-inds)
    (lambda (def)
      (let* ((name (car def))
             (index (cdr (assv name cindexmap))))
        (vector-ref ode-inds index))
      ))

  (define (codegen-primop op args)
    (case op
      ((signal.mul)
       (match args
              ((($ value 'V:C 1.0) right) right)
              ((left ($ value 'V:C 1.0)) left)
              (else (V:Op op args))))
      ((signal.div)
       (match args
              ((left ($ value 'V:C 1.0)) left)
              (else (V:Op op args))))
      (else (V:Op op args))
      ))
      

  (define (codegen-expr ode-inds invcindexmap)
    (lambda (expr)
      (let recur ((expr expr))
        (match expr
               ((? symbol?)
                (V:Var expr))
               ((? boolean?)
                (V:C expr))
               (('signal.primop op . args)
                (codegen-primop op (map recur args)))
               (('getindex vect index)
                (case vect
                  ((y)
                   (if (vector-ref ode-inds index)
                       (V:Sub (recur vect) index)
                       (V:Var (cdr (assv index invcindexmap)))))
                  (else (V:Sub (recur vect) index))))
               (($ constant 'number val unit)
                (V:C val))
               (('signal.if test ift iff)
                (let ((test1 (recur test))
                      (ift1 (recur ift))
                      (iff1 (recur iff)))
                  (let ((ret (V:Ifv test1 ift1 iff1)))
                    ret)))
               (('signal.reinit test dflt upd)
                (V:Ifv (V:Op 'signal.gte (list (recur test) (V:C 0.0)))
                       (recur upd)
                       (recur dflt)))
               (else expr))
        )))


  (let (
        (cindexmap (simruntime-cindexmap sim))
        (dindexmap (simruntime-dindexmap sim))
        (rindexmap (simruntime-rindexmap sim))
        (defs      (simruntime-definitions sim))
        (discrete-defs  (simruntime-discrete-definitions sim))
        (params    (simruntime-parameters sim))
        (eqblock   (simruntime-eqblock sim))
        (condblock (let ((sorted-eqs 
                          (sort (simruntime-condblock sim) 
                                (match-lambda*
                                 [(('setindex vect1 index1 val1) 
                                   ('setindex vect2 index2 val2)) 
                                  (< index1 index2)]))))
                   (map (match-lambda [('setindex vect index val) val]
                                      [eq (error 'codegen "unknown condition type" eq)]
                                      ) 
                        sorted-eqs)))

        (dposblocks (let ((bucket-eqs 
                           (bucket 
                            (match-lambda*
                             [((and eq ('setindex 'd index ('signal.reinit ev y rhs))))
                              index]
                             [else #f])
                            (simruntime-posresp sim))))
                      bucket-eqs))


        (posblocks (let ((bucket-eqs 
                          (bucket 
                           (match-lambda*
                            [((and eq ('setindex 'y index ('signal.reinit ev y rhs))))
                             index]
                            [else #f])
                           (simruntime-posresp sim))))
                     bucket-eqs))

        (negblocks (let ((bucket-eqs 
                          (bucket 
                           (match-lambda*
                            [((and eq ('setindex 'y index ('signal.reinit ev y rhs))))
                             index]
                            [else #f])
                           (simruntime-negresp sim))))
                     bucket-eqs))

        (regblocks (let ((bucket-eqs 
                          (bucket 
                           (match-lambda*
                            [((and eq ('setindex 'r index ('signal.reinit ev y rhs))))
                             index]
                            [_ #f])
                           (simruntime-posresp sim))))
                     bucket-eqs))

        )
    (let* (
           (invcindexmap
            (map (lambda (x) (cons (cdr x) (car x))) cindexmap))
           (ode-inds 
            (list->vector 
             (map (match-lambda [('setindex 'dy index val) #t] 
                                [('setindex 'y index val)  #f] 
                                [eq (error 'codegen "unknown equation type" eq)]) 
                  eqblock)))

           (codegen-expr1 (codegen-expr ode-inds invcindexmap))

           (ode-defs (filter-map (lambda (index) 
                                   (and (vector-ref ode-inds index)
                                        (list-ref defs index))) 
                                 (list-tabulate (vector-length ode-inds) 
                                                (lambda (x) x))))
           (asgn-defs (filter-map (lambda (index) 
                                    (and (not (vector-ref ode-inds index))
                                         (cons (cdr (assv index invcindexmap))
                                               (list-ref defs index))))
                                  (list-tabulate (vector-length ode-inds) 
                                                 (lambda (x) x))))
           (asgns 
            (filter-map
             (match-lambda [('setindex 'dy index val) #f] 
                           [('setindex 'y index val)  
                            (cons (cdr (assv index invcindexmap)) val)]
                           [eq (error 'codegen "unknown equation type" eq)])
             eqblock))

           (odeblock 
            (sort 
             (filter-map
              (match-lambda [(and ode ('setindex 'dy index val)) ode] 
                            [('setindex 'y index val)  #f]
                            [eq (error 'codegen "unknown equation type" eq)])
              eqblock)
             (match-lambda*
              [(('setindex vect1 index1 val1) 
                ('setindex vect2 index2 val2)) 
               (< index1 index2)])))

           (paramfun 
            (V:Fn '() (E:Ret (V:Vec (map codegen-expr1 params)))))

           (initfun  
            (V:Fn '(p) (if (null? asgn-defs)
                           (E:Ret (V:Vec (map codegen-expr1 ode-defs)))
                           (E:Let (map (lambda (x) (B:Val (car x) (codegen-expr1 (cdr x))))
                                   asgn-defs)
                              (E:Ret (V:Vec (map codegen-expr1 ode-defs)))))))
           (dinitfun  
            (if (null? discrete-defs)
                (V:C 'NONE)
                (V:Op 'SOME
                      (list 
                       (V:Fn '(p) (if (null? asgn-defs)
                                      (E:Ret (V:Vec (map codegen-expr1 discrete-defs)))
                                      (E:Let (map (lambda (x) (B:Val (car x) (codegen-expr1 (cdr x))))
                                                  asgn-defs)
                                             (E:Ret (V:Vec (map codegen-expr1 discrete-defs))))))
                       ))
                ))
           (odefun    
            (let ((fnval
                   (V:Fn '(t y) 
                         (let ((resval (V:Vec (map (match-lambda [('setindex 'dy index val) (codegen-expr1 val)]) odeblock))))
                           (if (null? asgns)
                               (E:Ret resval)
                               (E:Let (map (lambda (asgn) (B:Val (car asgn) (codegen-expr1 (cdr asgn)))) asgns)
                                      (E:Ret resval)))
                           ))
                   ))
            (V:Fn '(p) 
                  (E:Ret (if (null? regblocks) 
                             (V:Op 'ContSolver (list (V:Op 'make_stepper (list fnval))))
                             (V:Op 'RegimeSolver (V:Fn '(d r) (E:Ret (V:Op 'make_stepper (list fnval)))))
                             )))
            ))

           (initcondfun 
            (if (null? condblock)
                (V:C 'NONE)
                (V:Op 'SOME
                      (list 
                       (V:Fn '() (E:Ret (V:Vec (map (lambda (x) (V:C 'posInf)) condblock))))))
                ))

           (condfun     
            (if (null? condblock)
                (V:C 'NONE)
                (V:Op 'SOME
                      (list 
                       (V:Fn '(p) 
                             (E:Ret (V:Fn '(d) 
                                          (E:Ret (V:Fn '(t y c) 
                                                       (if (null? asgns)
                                                           (E:Ret (V:Vec (map codegen-expr1 condblock)))
                                                           (E:Let (map (lambda (x) (B:Val (car x) (codegen-expr1 (cdr x))))
                                                                       asgns)
                                                                  (E:Ret (V:Vec (map codegen-expr1 condblock))))))))
                                    ))
                       ))
                ))

           (posfun      
            (if (null? posblocks)
                (V:C 'NONE)
                (let ((blocks (fold-reinit-blocks ode-inds posblocks)))
                  (V:Fn '(p) (E:Ret (V:Fn '(t y c d) 
                                          (if (null? asgns)
                                              (E:Ret (V:Vec (map (compose codegen-expr1 cdr) blocks)))
                                              (E:Let (map (lambda (x) (B:Val (car x) (codegen-expr1 (cdr x))))
                                                          asgns)
                                                     (E:Ret (V:Vec (map (compose codegen-expr1 cdr) blocks)))))))))))
           (dposfun      
            (if (null? dposblocks)
                (V:C 'NONE)
                (V:Op 'SOME
                      (list 
                       (let ((blocks (fold-reinit-blocks ode-inds dposblocks)))
                         (V:Fn '(p) (E:Ret (V:Fn '(t y c d) 
                                                 (if (null? asgns)
                                                     (E:Ret (V:Vec (map (compose codegen-expr1 cdr) blocks)))
                                                     (E:Let (map (lambda (x) (B:Val (car x) (codegen-expr1 (cdr x))))
                                                                 asgns)
                                                            (E:Ret (V:Vec (map (compose codegen-expr1 cdr) blocks)))))))))))
                ))

           (negfun
            (if (null? negblocks)
                (V:C 'NONE)
                (V:Op 'SOME
                      (list 
                       (let ((blocks (fold-reinit-blocks ode-inds negblocks)))
                         (V:Fn '(p) (E:Ret (V:Fn '(t y c d) 
                                                 (if (null? asgns)
                                                     (E:Ret (V:Vec (map (compose codegen-expr1 cdr) blocks)))
                                                     (E:Let (map (lambda (x) (B:Val (car x) (codegen-expr1 (cdr x))))
                                                                 asgns)
                                                            (E:Ret (V:Vec (map (compose codegen-expr1 cdr) blocks)))))))))))
                ))

           (initregfun 
            (if (null? regblocks)
                (V:C 'NONE)
                (V:Op 'SOME
                      (list 
                       (V:Fn '() (E:Ret (V:Vec (cons (V:C #t) (map (lambda (x) (V:C #f)) (cdr regblocks))))))))
                ))

           (regfun      
            (if (null? regblocks)
                (V:C 'NONE)
                (V:Op 'SOME
                      (list 
                       (let ((blocks (fold-regime-reinit-blocks regblocks)))
                         (V:Fn '(c r) 
                               (if (null? asgns)
                                   (E:Ret (V:Vec (map (compose codegen-expr1 cdr) blocks)))
                                   (E:Let (map (lambda (x) (B:Val (car x) (codegen-expr1 (cdr x)))) asgns)
                                          (E:Ret  (V:Vec (map (compose codegen-expr1 cdr) blocks)))))))))
                ))
           )

      (list paramfun initfun dinitfun odefun initcondfun condfun posfun negfun dposfun initregfun regfun)

    ))
)



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


(define (stmt->scheme x . rest)
  (let-optionals rest ((bnd? #t))
    (cases stmt x

	 (E:Ife     (test ift iff)
		    (list "(cond (" (value->scheme test) " " nl
			  " " (stmt->scheme ift ) ")" nl
			  "(else " (stmt->scheme iff) "))" nl))

	 (E:Let     (bnds body)
		    (list "(let* (" nl
			  (map (lambda (x) (binding->scheme x)) bnds) nl
			  ") " nl
			  (stmt->scheme body #f) nl
			  ")" nl))
			  
	 (E:Begin   (stmts)  
                    (list "(begin " 
                          (map (lambda (x) (stmt->scheme x)) stmts) nl
                          ")"))

	 (E:Ret     (v)  (value->scheme v))
		    
	 (E:Noop    () (list "(void)"))
	 )))


(define (value->scheme v)
  (let ((result
  (cases value v
	 (V:C       (v) v)
	 (V:Var     (name) (name/scheme name))
	 (V:Sub     (v index) 
		    (list "(vector-ref " (value->scheme v) " " index ")"))
	 (V:Vec     (lst) 
		    (list "(vector " (intersperse (map value->scheme lst) " ") ")"))
	 (V:Fn      (args body) 
		    (list "(lambda (" (intersperse (map name/scheme args) " ") ") "
			  (stmt->scheme body #f) ")"))
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


(define (stmt->ML x . rest)
  (let-optionals rest ((bnd? #t))
    (cases stmt x

	 (E:Ife     (test ift iff)
		    (list "if (" (value->ML test) ") " nl
			  "then " (stmt->ML ift ) nl
			  "else " (stmt->ML iff) nl))

	 (E:Let     (bnds body)
		    (list "let " nl
			  (map (lambda (x) (binding->ML x #t)) bnds) nl
			  "in " nl
			  (stmt->ML body #f) nl
			  "end" nl))
			  
	 (E:Begin     (stmts)  
                      (list "(" (map (lambda (stmt) (list (stmt->ML stmt) "; ")) stmts)  ")"))

	 (E:Ret     (v)  (value->ML v))
		    
	 (E:Noop    () (list "()"))
	 )))


(define (value->ML v)
  (cases value v
	 (V:C       (v) (cond ((and (number? v) (negative? v))
			       (list "~" (abs v)))
                              ((boolean? v)
                               (if v "true" "false"))
			      (else v)))
	 (V:Var     (name) (name/ML name))
	 (V:Sub     (v index) 
		    (list "getindex (" (value->ML v) ", " index ")"))
	 (V:Vec   (lst) 
		    (let ((n (length lst)))
		      (list "(Vector.fromList [" (intersperse (map (lambda (v) (value->ML v)) lst) ", ") "])")))
	 (V:Fn      (args body) 
		    (list "(fn (" (intersperse (map name/ML args) ",") ") => "
			  (stmt->ML body #f) ")"))
	 (V:Op    (name args)
                  (let ((name1 (name/ML name)))
                    (cond ((null? args) 
                           (case name 
                             ((NONE)  (list name1))
                             (else    (list name1 "()"))))
                          ((null? (cdr args))
                           (list "(" name1 " (" (value->ML (car args)) "))"))
                          (else
                           (list "(" name1 "(" (intersperse (map value->ML args) ",") "))")))))
	 (V:Ifv     (test ift iff)
		    (list "(if (" (value->ML test) ") " 
			  "then " (value->ML ift ) " " 
			  "else " (value->ML iff) ")"))

	 ))


(define (codegen-ODE/ML sim #!key (out (current-output-port)) (mod #t) (libs '()) (solver 'rk4b))

  (match-let (((paramfun    
                initfun     
                dinitfun 
                odefun      
                initcondfun 
                condfun     
                posfun      
                negfun
                dposfun
                initregfun
                regfun)
               (codegen-ODE sim)))

    (if (and solver (not (member solver '(rkfe rk3 rk4a rk4b rkoz rkdp))))
        (error 'codegen-ODE/ML "unknown solver" solver))
    
    (if mod (print-fragments (prelude/ML solver: solver libs: libs) out))
    
    (print-fragments
     (list 
           (binding->ML (B:Val 'paramfun paramfun)) nl
           (binding->ML (B:Val 'initfun initfun)) nl
           (binding->ML (B:Val 'dinitfun dinitfun)) nl
           (binding->ML (B:Val 'odefun odefun)) nl
           (binding->ML (B:Val 'initcondfun initcondfun)) nl
           (binding->ML (B:Val 'condfun condfun)) nl
           (binding->ML (B:Val 'posfun posfun)) nl
           (binding->ML (B:Val 'negfun negfun)) nl
           (binding->ML (B:Val 'dposfun dposfun)) nl
           (binding->ML (B:Val 'initregfun initregfun)) nl
           (binding->ML (B:Val 'regfun regfun)) nl
           )
     out)
    
    (if mod (print-fragments (list nl "end" nl) out))

    ))


(define (prelude/ML  #!key (solver 'rk4b) (libs '()))
`(
 #<<EOF
structure Model = 
struct

open Real
open Math
open RungeKutta
open FunctionalHybridDynamics

fun putStrLn str = 
  (TextIO.output (TextIO.stdOut, str);
   TextIO.output (TextIO.stdOut, "\n"))

fun putStr str = (TextIO.output (TextIO.stdOut, str))

fun showReal n = 
let open StringCvt
in
(if n < 0.0 then "-" else "") ^ (fmt (FIX (SOME 12)) (abs n))
end

val getindex = Unsafe.Vector.sub

EOF

,(if (member 'random libs)
#<<EOF

fun RandomInit () = RandomMTZig.fromEntropy()

val RandomState = RandomInit ()
val RandomZT = RandomMTZig.ztnew()

fun random_uniform () = RandomMTZig.randUniform RandomState
fun random_exponential (mu) = mu * RandomMTZig.randExp (RandomState, RandomZT)


EOF
"")

,(if (not solver)
     `(("(* dummy solver; returns only the computed derivatives *)")
       ("fun integral (f,h) = f" ,nl))

     `(("val summer = fn (a,b) => (vmap2 (fn (x,y) => x+y) (a,b))" ,nl)
       ("val scaler = fn(a,lst) => (Vector.map (fn (x) => a*x) lst)" ,nl)
       . ,(case solver  
            ;; adaptive solvers
            ((rkoz rkdp)
             (let ((esolver (sprintf "ce~A" solver)))
             `(
               ("val " ,solver ": (real vector) stepper2 = make_" ,solver "()" ,nl)
               ("fun make_stepper (deriv) = " ,solver " (scaler,summer,deriv)" ,nl)
               ("val " ,esolver ": (real vector) stepper3 = make_" ,esolver "()" ,nl)
               ("fun make_estepper (deriv) = " ,esolver " (scaler,summer,deriv)" ,nl)
               (,nl)
               )
             ))
            (else
             `(
               ("val " ,solver ": (real vector) stepper1 = make_" ,solver "()" ,nl)
               ("fun make_stepper (deriv) = " ,solver " (scaler,summer,deriv)" ,nl)
               (,nl)
               ))
            ))
     )

))



