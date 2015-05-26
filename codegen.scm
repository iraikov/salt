
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
  (V:C       (v (lambda (x) (or (symbol? x) (number? x) ))))
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
		  (V:Ifv (test tv fv)   "(V:Ifv ~A ~A ~A)" test tv fv)
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
            (recur (cdr lst) (update-bucket k (car lst) res)))
          ))
      )


  (define (fold-reinits lst)
    (let recur ((lst (cdr lst)) (expr (car lst)))
      (if (null? lst) expr
          (match (car lst)
                 (('signal.reinit ev ('getindex 'y index) rhs)
                  (recur (cdr lst) `(signal.reinit ,ev ,expr ,rhs)))
                 (else (error 'fold-reinits "unknown reinit equation" (car lst)))))
      ))

  (define (codegen-expr expr)
    (match expr
           ((? symbol?)
            (V:Var expr))
           (('signal.primop op . args)
            (V:Op op (map codegen-expr args)))
           (('getindex vect index)
            (V:Sub (codegen-expr vect) index))
           (($ constant 'number val)
            (V:C val))
           (('signal.if test ift iff)
            (V:Ifv (codegen-expr test)
                   (codegen-expr ift)
                   (codegen-expr iff)))
           (('signal.reinit test dflt upd)
            (V:Ifv (V:Op 'signal.gte (list (codegen-expr test) (V:C 0.0)))
                   (codegen-expr upd)
                   (codegen-expr dflt)))
           (else expr)))


  (let (
        (cindexmap (simruntime-cindexmap sim))
        (dindexmap (simruntime-dindexmap sim))
        (defs (simruntime-definitions sim))
        (params (simruntime-parameters sim))
        (eqblock (let ((sorted-eqs 
                           (sort (simruntime-eqblock sim) 
                                 (match-lambda*
                                        [(('setindex vect1 index1 val1) 
                                              ('setindex vect2 index2 val2)) 
                                             (< index1 index2)]))))
                             (map (match-lambda [('setindex vect index val) val]
                                                [eq (error 'codegen "unknown equation type" eq)]
                                                ) 
                                  sorted-eqs)))
        (condblock (let ((sorted-eqs 
                          (sort (simruntime-condblock sim) 
                                (match-lambda*
                                 [(('setindex vect1 index1 val1) 
                                   ('setindex vect2 index2 val2)) 
                                  (< index1 index2)]))))
                   (map (match-lambda [('setindex vect index val) val]
                                      [eq (error 'codegen "unknown equation type" eq)]
                                      ) 
                        sorted-eqs)))

        (posblocks (let ((bucket-eqs 
                          (bucket 
                           (match-lambda*
                            [((and eq ('setindex 'y index ('signal.reinit ev y rhs))))
                             index])
                           (simruntime-posresp sim))))
                     (sort
                      (map (lambda (x) 
                             (let ((reinits (map (match-lambda [('setindex vect index val) val]) (cdr x))))
                               (cons (car x) (fold-reinits reinits))))
                           bucket-eqs)
                      (lambda (x y) (< (car x) (car y))))
                     ))

        (negblocks (let ((bucket-eqs 
                          (bucket 
                           (match-lambda*
                            [((and eq ('setindex 'y index ('signal.reinit ev y rhs))))
                             index])
                           (simruntime-negresp sim))))
                     (sort
                      (map (lambda (x) 
                            (let ((reinits (map (match-lambda [('setindex vect index val) val]) (cdr x))))
                              (cons (car x) (fold-reinits reinits))))
                           bucket-eqs)
                      (lambda (x y) (< (car x) (car y))))
                      ))
        )
    (let (
          (sysinds (V:Vec (map (lambda (i) (V:C i))
                               (filter-map (match-lambda [('setindex 'dy index val) 1] 
                                                         [('setindex 'y index val) 0] 
                                                         [eq (error 'codegen "unknown equation type" eq)]) 
                                           (simruntime-eqblock sim)))))

          (paramfun (V:Fn '() (E:Ret (V:Vec (map codegen-expr params)))))
          (initfun (V:Fn '(p) (E:Ret (V:Vec (map codegen-expr defs)))))
          (eqfun   (V:Fn '(p) (E:Ret (V:Fn '(t y) (E:Ret (V:Vec (map codegen-expr eqblock)))))))
          (initcondfun (V:Fn '() (E:Ret (V:Vec (map (lambda (x) (V:C 'negInf)) condblock)))))
          (condfun (V:Fn '(p) (E:Ret (V:Fn '(t y c) (E:Ret (V:Vec (map codegen-expr condblock)))))))
          (posfun (if (null? posblocks)
                       (V:Fn '(p) (E:Ret (V:Fn '(t y c) (E:Ret (V:Var 'y)))))
                       (V:Fn '(p) (E:Ret (V:Fn '(t y c) (E:Ret (V:Vec (map (compose codegen-expr cdr) posblocks))))))))
          (negfun (if (null? negblocks)
                       (V:Fn '(p) (E:Ret (V:Fn '(t y c) (E:Ret (V:Var 'y)))))
                       (V:Fn '(p) (E:Ret (V:Fn '(t y c) (E:Ret (V:Vec (map (compose codegen-expr cdr) negblocks))))))))
          )

      (list sysinds paramfun initfun eqfun initcondfun condfun posfun negfun)

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

  (let* (
         (c (codegen-ODE sim))
         (sysinds     (car c))
         (paramfun    (cadr c))
         (initfun     (caddr c))
         (eqfun       (cadddr c)) 
         (initcondfun (car (cddddr c)))
         (condfun     (cadr (cddddr c)))
         (posfun      (caddr (cddddr c)))
         (negfun      (cadddr (cddddr c)))
         )
    
    (if (and solver (not (member solver '(rkfe rk3 rk4a rk4b rkoz rkdp))))
        (error 'codegen-ODE/ML "unknown solver" solver))
    
    (if mod (print-fragments (prelude/ML solver: solver libs: libs) out))
    
    (print-fragments
     (list (binding->ML (B:Val 'sysinds sysinds)) nl
           (binding->ML (B:Val 'paramfun paramfun)) nl
           (binding->ML (B:Val 'initfun initfun)) nl
           (binding->ML (B:Val 'eqfun eqfun)) nl
           (binding->ML (B:Val 'initcondfun initcondfun)) nl
           (binding->ML (B:Val 'condfun condfun)) nl
           (binding->ML (B:Val 'posfun posfun)) nl
           (binding->ML (B:Val 'negfun negfun)) nl)
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
val setindex = Vector.update


fun vmap2 f (v1,v2) = 
    let 
        val n = Vector.length v1
    in
        Vector.tabulate (n, fn (i) => f (getindex (v1,i), getindex (v2,i))
    end

fun vfind2 f (v1,v2) = 
    let 
        val n = Vector.length v1
        fun recur i = 
            if Int.<(i, n)
            then (if f (getindex (v1,i), getindex (v2,i)) then SOME(i) else recur (Int.+(i,1)))
            else NONE
    in
      recur 0
    end 

fun vfoldpi2 f (v1,v2) = 
    let 
        val n = Vector.length v1
        fun recur (i, ax) = 
            if Int.<(i, n)
            then recur (Int.+(i,1), f (i, getindex (v1,i), getindex (v2,i), ax))
            else ax
    in
      recur (0, NONE)
    end 

exception EmptySignal

val equal = fn (x,y) => (x = y) 

val signal_sign = sign
val signal_eqnum = (op ==)
val signal_neg = (op ~)
val signal_add = (op +)
val signal_sub = (op -)
val signal_mul = (op *)
val signal_div = (op /)
val signal_pow = pow
val signal_max = max
val signal_min = min
val signal_gt = (op >)
val signal_gte = (op >=)

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
       ("fun integral (f,h) = f" ,nl)
       )
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
#<<EOF

val tol = Real.Math.pow (10.0, ~7.0)
val lb = 0.5 * tol
val ub = 0.9 * tol

datatype ('a, 'b) either = Left of 'a | Right of 'b


fun predictor tol (h,ys) =
  let open Real
      val e = Vector.foldl (fn (y,ax) => Real.+ ((abs y),ax)) 0.0 ys
  in 
      if e < lb 
      then Right (1.414*h)	(* step too small, accept but grow *)
      else (if e < ub 
            then Right h	(* step just right *)
            else Left (0.5*h))	(* step too large, reject and shrink *)
  end


exception ConvergenceError


fun secant tol f fg0 guess1 guess0 = 
    let open Real
        val fg1 = f guess1
        val newGuess = guess1 - fg1 * (guess1 - guess0) / (fg1 - fg0)
        val err =  abs (newGuess - guess1)
    in 
        if (err < tol)
        then newGuess
        else secant tol f fg1 newGuess guess1 
    end


datatype 'a result = Next of 'a | Root of 'a

(* 1. Ensures that the event closest is time is selected, not just the first one in the event index.
   2. Determines the direction (positive or negative) of the event. *)
fun ethr (i,v1,v2,ax) = 
    let
        fun ethr0 (i, v1, v2) =
            (case (Real.sign(v1),Real.sign(v2)) of
                 (-1, 1) => SOME (1, i, v1-v2)
               | (-1, 0) => SOME (1, i, v1-v2)
               | (1, -1) => SOME (-1, i, v1-v2)
               | (1,  0) => SOME (-1, i, v1-v2)
               | _ => NONE)
    in
        case ax of 
            NONE => ethr0 (i,v1,v2)
          | SOME (dir,index,diff) =>
            case ethr0 (i, v1, v2) of
                NONE => ax
              | SOME (dir',index',diff') =>
                if abs(diff') < abs(diff)
                then SOME (dir',index',diff')
                else ax
    end


fun esolver (stepper,fcond) (x,ys,ev,h) =
    let open Real
        val (ys',e,finterp) = stepper h (x,ys)
        val ev' = fcond (x+h,ys',ev)
    in
        case predictor tol (h,e) of
            Right h' => 
            (case (vfoldpi2 ethr (ev, ev') of
                 SOME (evdir,evind,evdiff) => 
                 (let
                     fun fy (theta) = Vector.sub(fcond (x+theta*h, finterp theta, ev), evind)
                     val y0    = Vector.sub(fcond(x,ys,ev), evind)
                     val theta = secant tol fy y0 1.0 0.0
                     val x'    = x+(theta+tol)*h
                     val ys''  = finterp (theta+tol)
                 in
                     Root (x',ys'',evdir,fcond (x',ys''),h')
                 end)
               | NONE => Next (x+h,ys',ev',h'))
          | Left h'  => 
            esolver (stepper, fcond) (x,ys,ev,h')
    end


fun eintegral (f,fcond,fpos,fneg) =
    let
       val fstepper = make_estepper (f)
       val fesolver = esolver (fstepper,fcond) 
    in
       fn(x,ys,ev,h) => 
       (case fesolver (x,ys,ev,h) of
            Next (xn,ysn,evn,hn) => (xn,ysn,evn,hn)
          | Root (xn,ysn,evn,hn) => 
            let 
               val ysn' = fneg(xn,fpos(xn,ysn,evn),evn)
            in
               (xn,ysn',evn,hn)
            end)
   end

fun solver stepper (x,ys,h) =
    let open Real
        val (ys',e) = stepper h (x,ys)
    in
        case predictor tol (h,e) of
            Right h' => 
            (x+h,ys',h')
          | Left h'  => 
            solver (stepper) (x,ys,h')
    end

fun integral (f) =
    let 
        val fstepper = make_stepper (f)
        val (xn,ysn,hn) = solver fstepper (x,ys,h) 
    in
        fn(x,y,h) => solver fstepper (x,y,h)
    end



EOF

               )))
            (else
             `(
               ("val " ,solver ": (real vector) stepper1 = make_" ,solver "()" ,nl)
               ("fun make_stepper (deriv) = " ,solver " (scaler,summer,deriv)" ,nl)

(#<<EOF

fun integral (f,h) =
    let
        val solver = (make_stepper (f)) h
    in
        fn(x,y) => let val y' = solver (x,y)
                   in
                       (x+h,y')
                   end
    end

fun eintegral (f,fcond,fpos,fneg,h) =
    let 
        val solver = (make_stepper (f)) h
        fun thr (v1,v2) = (Int.-(Real.sign(v1),Real.sign(v2)) = 0)
    in
        fn(x,y,e) => let val x' = x + h
                         val y' = solver (x,y)
                         val e' = fcond (x',y',e)
                         val pos = vfind2 thr (e', e) 
                         val neg = vfind2 thr (e, e')
                         val y'' = case pos of SOME i => fpos(x',y',e') | _ => y'
                         val y'' = case neg of SOME i => fneg(x',y'',e') | _ => y''
                     in
                         (x',y'',e')
                     end

    end


EOF
)

))
))
))


)


