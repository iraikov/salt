
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
  (E:Let     (bnds (lambda (x) (every binding? x))) (body stmt?))
  (E:Begin   (stmts (lambda (x) (every stmt? x))))
  (E:Set     (v value?)  (i integer?) (x value?))
  (E:Ret     (v value?))
  (E:Noop)
  )

  
(define-record-printer (stmt x out)
  (fprintf out "~A"
	   (cases stmt x
		  (E:Ife (test ift iff) (sprintf "E:Ife ~A ~A ~A" test ift iff))
		  (E:Let (bnds body) (sprintf "E:Let ( ~A ) ~A" bnds body))
		  (E:Begin (stmts)   (sprintf "E:Begin ~A" stmts))
                  (E:Set  (v i x)    (sprintf "(E:Set ~A ~A ~A)" v i x))
                  (E:Ret (v)         (sprintf "E:Ret ~A" v))
		  (E:Noop ()         (sprintf "E:Noop"))
		  )))


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
      


(define (codegen-expr ode-inds ode-order invcindexmap solver)
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
                 (if (member index ode-inds)
                     (let ((ode-index-assoc (assv index ode-order)))
                       (V:Sub (recur vect) (cdr ode-index-assoc)))
                     (V:Var (cdr (assv index invcindexmap)))))
                (else (V:Sub (recur vect) index))))
             (('ext index t)
              (recur `(getindex ext ,index)))
             (('extev index t)
              (recur `(signal.primop signal.sub ,t (getindex extev ,index))))
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


(define (codegen-const-expr expr)
  (let recur ((expr expr))
    (match expr
           ((? symbol?)
            (V:Var expr))
           ((? boolean?)
            (V:C expr))
           (('signal.primop op . args)
            (codegen-primop op (map recur args)))
           (('getindex vect index)
            (error 'codegen-const-expr "not a const expr" expr))
           (($ constant 'number val unit)
            (V:C val))
           (('signal.if test ift iff)
            (let ((test1 (recur test))
                  (ift1 (recur ift))
                  (iff1 (recur iff)))
              (let ((ret (V:Ifv test1 ift1 iff1)))
                ret)))
           (('signal.reinit test dflt upd)
            (error 'codegen-const-expr "not a const expr" expr))
           (else expr))
      ))


(define (codegen-ODE sim solver)

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
    (let recur ((lst lst) (res '()))
      (if (null? lst) res
          (let ((k (f (car lst))))
            (recur (cdr lst) (if k (update-bucket k (car lst) res) res)))
          ))
      )


  (define (update-edge e dag)
    (update-bucket (car e) (cdr e) dag))

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
                 [('setindex 'y_out index val) (and (member index ode-inds) val)]
                 [('setindex 'd_out index val) val]
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
                 [('setindex 'r_out index val) val]
                 [else #f])
                (cdr x))))
          (and (pair? reinits) (cons (car x) (fold-reinits reinits)))))
      blocks)
     (lambda (x y) (< (car x) (car y))))
    )
    
  (define (fold-asgns asgn-idxs expr)
    (let recur ((expr expr) (ax '()))
      (match expr
             ((? symbol?) ax)
             ((? boolean?) ax)
             (('signal.primop op . args)
              (fold recur ax args))
             (('getindex 'y index)
              (if (member index asgn-idxs)
                  (cons index ax) ax))
             (($ constant 'number val unit) ax)
             (('signal.if test ift iff)
              (recur iff (recur ift (recur test ax))))
             (else ax))
      ))


  (define (codegen-set-stmts codegen-expr exprs out)
    (let recur ((i 0) (exprs exprs) (stmts '()))
      (if (null? exprs)
          (append stmts (list (E:Ret (V:Var out))))
          (recur (+ i 1) (cdr exprs)
                 (cons (E:Set (V:Var out) i (codegen-expr (car exprs))) stmts))
          ))
    )

  (define (ode-def? cindexmap ode-inds)
    (lambda (def)
      (let* ((name (car def))
             (index (cdr (assv name cindexmap))))
        (member index ode-inds))
      ))


  (let* (
        (cindexmap      (simruntime-cindexmap sim))
        (invcindexmap   (map (lambda (x) (cons (cdr x) (car x))) (env->list cindexmap)))
        (dindexmap      (simruntime-dindexmap sim))
        (rindexmap      (simruntime-rindexmap sim))
        (defs           (simruntime-definitions sim))
        (discrete-defs  (simruntime-discrete-definitions sim))
        (params         (simruntime-parameters sim))
        (externals      (simruntime-external-definitions sim))
        (externalevs    (simruntime-externalev-definitions sim))
        (eqblock        (simruntime-eqblock sim))

        (ode-inds 
         (sort
          (filter-map 
           (match-lambda [('setindex 'dy_out index val) index] 
                         [('setindex 'y_out index val)  #f] 
                         [('reduceindex 'y_out index val)  #f] 
                         [eq (error 'codegen "unknown equation type" eq)])
           eqblock) <))

        (ode-order 
         (cadr (fold (match-lambda* [(index (order lst))
                                     (list (+ 1 order) (cons (cons index order) lst))])
                     (list 0 '()) ode-inds)))
                    
        (ode-n (length ode-order))

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
                             [((and eq ('setindex 'd_out index ('signal.reinit ev y rhs))))
                              index]
                             [else #f])
                            (simruntime-posresp sim))))
                      bucket-eqs))


        (posblocks (let ((bucket-eqs 
                          (bucket 
                           (match-lambda*
                            [((and eq ('setindex 'y_out index ('signal.reinit ev y rhs))))
                             index]
                            [else #f])
                           (simruntime-posresp sim))))
                     bucket-eqs))
        
        (posblocks (list-tabulate
                    (length eqblock)
                    (lambda (i)
                      (let ((posblock-assoc (assoc i posblocks)))
                        (if (not posblock-assoc)
                            (list i `(setindex y_out ,i (getindex y ,i)))
                            posblock-assoc)))))

        (negblocks (let ((bucket-eqs 
                          (bucket 
                           (match-lambda*
                            [((and eq ('setindex 'y_out index ('signal.reinit ev y rhs))))
                             index]
                            [else #f])
                           (simruntime-negresp sim))))
                     bucket-eqs))

        (negblocks (list-tabulate
                    (length eqblock)
                    (lambda (i)
                      (let ((negblock-assoc (assoc i negblocks)))
                        (if (not negblock-assoc)
                            (list i `(setindex y_out ,i (getindex y ,i)))
                            negblock-assoc)))))

        (regblocks (let ((bucket-eqs 
                          (bucket 
                           (match-lambda*
                            [((and eq ('setindex 'r_out index ('signal.reinit ev y rhs))))
                             index]
                            [_ #f])
                           (simruntime-posresp sim))))
                     bucket-eqs))

        )
    (let* (

           (codegen-expr1 (codegen-expr ode-inds ode-order invcindexmap solver))

           (ode-defs (filter-map (lambda (index) 
                                   (and (member index ode-inds)
                                        (list-ref defs index))) 
                                 (list-tabulate (length invcindexmap) 
                                                (lambda (x) x))))
           (asgn-defs (filter-map (lambda (index) 
                                    (and (not (member index ode-inds))
                                         (let ((invassoc (assv index invcindexmap)))
                                           (if (not invassoc)
                                               (error 'codegen "unknown continuous quantity index" 
                                                      index invcindexmap)
                                               (list index
                                                     (cdr invassoc)
                                                     (list-ref defs index))))))
                                  (list-tabulate (length invcindexmap) 
                                                 (lambda (x) x))))
                        
           (asgn-idxs (delete-duplicates
                       (filter-map
                        (match-lambda [('setindex 'dy_out index val) #f]
                                      [(or ('setindex 'y_out index val)
                                           ('reduceindex 'y_out index val)) index]
                                      [eq (error 'codegen "unknown equation type" eq)])
                        eqblock)))

           (asgn-dag (fold (match-lambda* 
                            [(('setindex 'dy_out index val) dag) dag]
                            [(('setindex 'y_out index val) dag)
                             (let ((edges (map (lambda (src) (cons src index)) 
                                               (fold-asgns asgn-idxs val))))
                               (if (null? edges) (cons `(,index) dag)
                                   (fold (lambda (e dag) (update-edge e dag))  
                                         dag edges)))]
                            [(('reduceindex 'y_out index val) dag)
                             (let ((edges (filter-map (lambda (src) (and (not (= src index)) (cons src index))) 
                                                      (fold-asgns asgn-idxs val))))
                               (if (null? edges) (cons `(,index) dag)
                                   (fold (lambda (e dag) (update-edge e dag)) 
                                         dag edges)))]
                            [eq (error 'codegen "unknown equation type" eq)])
                           '() eqblock))

           (asgn-order (topological-sort asgn-dag eq?))
           
           (asgns 
            (append 
             (delete-duplicates
              (filter-map
               (match-lambda [('setindex 'dy_out index val) #f] 
                             [('setindex 'y_out index val) #f] 
                             [('reduceindex 'y_out index val)
                              (let ((asgn-def (assv index asgn-defs)))
                                (match asgn-def
                                       ((_ name rhs) asgn-def)
                                       (else (error 'codegen "unknown asgn index" index)))
                                )]
                             [eq (error 'codegen "unknown equation type" eq)])
               eqblock))
             (fold-right
              (lambda (ordindex ax)
                (append 
                 (filter-map
                  (match-lambda [('setindex 'dy_out index val) #f] 
                                [(or ('setindex 'y_out index val)  
                                     ('reduceindex 'y_out index val))
                                 (and (= index ordindex) 
                                      (list index (cdr (assv index invcindexmap)) val))]
                                [eq (error 'codegen "unknown equation type" eq)])
                  eqblock) ax))
              '() asgn-order)))

           (odeblock 
            (sort 
             (filter-map
              (match-lambda [(and ode ('setindex 'dy_out index val)) ode] 
                            [('setindex 'y_out index val)  #f]
                            [('reduceindex 'y_out index val)  #f]
                            [eq (error 'codegen "unknown equation type" eq)])
              eqblock)
             (match-lambda*
              [(('setindex vect1 index1 val1) 
                ('setindex vect2 index2 val2)) 
               (< index1 index2)])))

           (paramfun (V:Op 'make_real_initial
                           (list (V:C (length params))
                                 (V:Fn '(p_out) (E:Begin (codegen-set-stmts codegen-expr1 params 'p_out))))))

           (initfun  
            (let ((stmts (codegen-set-stmts codegen-expr1 ode-defs 'y_out)))
              (V:Fn '(p) 
                    (E:Ret (V:Op 'make_real_initial
                           (list (V:C ode-n)
                                 (V:Fn '(y_out) 
                                       (if (null? asgn-defs)
                                           (E:Begin stmts)
                                           (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs))))
                                                       asgn-defs)
                                                  (E:Begin stmts))))
                                 ))
                           ))
              ))

           (dinitfun  
            (let ((stmts (codegen-set-stmts codegen-expr1 discrete-defs 'd_out)))
              (if (null? discrete-defs)
                  (V:C 'NONE)
                  (V:Op 'SOME
                        (list 
                         (V:Fn '(p) 
                               (E:Ret (V:Op 'make_real_initial
                                            (list (V:C (length discrete-defs))
                                                  (V:Fn '(d_out)
                                                        (if (null? asgn-defs)
                                                            (E:Begin stmts)
                                                            (E:Let (map (lambda (x) (B:Val (car x) (codegen-expr1 (cdr x))))
                                                                        asgn-defs)
                                                                   (E:Begin stmts))))))
                                      ))
                         ))
                  ))
            )

           (initextfun  
            (let ((stmts (codegen-set-stmts codegen-expr1 externals 'ext_out)))
              (V:Fn '(p) (E:Ret (V:Op 'make_ext (list (V:C (length externals)) (V:Fn '(ext_out) (E:Begin stmts))))))))

           (initextevfun  
            (let ((stmts (codegen-set-stmts codegen-expr1 externalevs 'extev_out)))
              (V:Fn '(p) (E:Ret (V:Op 'make_ext (list (V:C (length externalevs)) (V:Fn '(extev_out) (E:Begin stmts))))))))

           (odefun    
            (let ((fnval
                   (V:Fn '(t y dy_out)
                         (let* ((odes (map (match-lambda [('setindex 'dy_out index val) val]) odeblock))
                                (stmts (codegen-set-stmts codegen-expr1 odes 'dy_out)))
                           (if (null? asgns)
                               (E:Begin stmts)
                               (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs)))) asgns)
                                      (E:Begin stmts)))
                           ))
                   ))
            (V:Fn '(p) 
                  (E:Ret
                   (cond
                    ((pair? regblocks) 
                     (V:Op 'RegimeStepper (list (V:Fn '(d r) (E:Ret (V:Fn '(ext extev) (E:Ret (V:Op 'make_event_stepper (list (V:C ode-n) fnval)))))))))
                    ((pair? condblock)
                     (V:Op 'EventStepper (list (V:Fn '(ext extev) (E:Ret (V:Op 'make_event_stepper (list (V:C ode-n) fnval)))))))
                    (else
                     (V:Op 'ContStepper (list  (V:Fn '(ext extev) (E:Ret (V:Op 'make_stepper (list (V:C ode-n) fnval)))))))
                    ))
                  ))
            )

           (initcondfun 
            (if (null? condblock)
                (V:C 'NONE)
                (V:Op 'SOME
                      (list
                       (V:Op 'make_real_initial
                             (list (V:C (length condblock))
                                   (V:Fn '(c_out) 
                                         (E:Begin 
                                          (append 
                                           (list-tabulate
                                            (length condblock)
                                            (lambda (i) (E:Set (V:Var 'c_out) i (V:C 'negInf))))
                                           (list (E:Ret (V:Var 'c_out)))
                                           )
                                          ))
                                   ))
                       ))
                ))

           (condfun     
            (let ((used-asgns
                   (map
                    (lambda (index) (assv index asgns))
                    (delete-duplicates
                     (fold (lambda (expr ax) (append (fold-asgns asgn-idxs expr) ax))
                           '() condblock)))))
              (if (null? condblock)
                  (V:C 'NONE)
                  (let ((fnval (V:Fn '(t y c ext extev c_out) 
                                     (let ((stmts (codegen-set-stmts codegen-expr1 condblock 'c_out)))
                                       (if (null? used-asgns)
                                           (E:Begin stmts)
                                           (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs))))
                                                       used-asgns)
                                                  (E:Begin stmts)))))))
                    (V:Op 'SOME
                          (list 
                           (V:Fn '(p) 
                                 (E:Ret (if (null? regblocks)
                                            (V:Op 'SCondition (list (V:Op 'make_condition
                                                                          (list (V:C (length condblock)) fnval))))
                                            (V:Op 'RegimeCondition (list (V:Op 'make_regime_condition 
                                                                               (list (V:C (length condblock)) 
                                                                                     (V:Fn '(d) (E:Ret fnval))))))))
                                 ))
                          ))
                  ))
            )

           (posfun      
            (if (null? posblocks)
                (V:C 'NONE)
                (V:Op 'SOME
                      (list 
                       (let* (
                              (blocks (fold-reinit-blocks ode-inds posblocks))
                              (used-asgns
                               (map (lambda (index) (assv index asgns))
                                    (delete-duplicates
                                     (fold (lambda (expr ax) (append (fold-asgns asgn-idxs expr) ax))
                                           '() blocks))))
                              (fnval (V:Fn (if (null? regblocks) '(t y c ext extev y_out) '(t y c d ext extev y_out))
                                           (let ((stmts (codegen-set-stmts (compose codegen-expr1 cdr) blocks 'y_out)))
                                             (if (null? used-asgns)
                                                 (E:Begin stmts)
                                                 (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs))))
                                                             used-asgns)
                                                        (E:Begin stmts))))))
                              )
                         (V:Fn '(p) (E:Ret (if (null? regblocks)
                                               (V:Op 'SResponse (list (V:Op 'make_response (list (V:C ode-n) fnval))))
                                               (V:Op 'RegimeResponse (list (V:Op 'make_regime_response (list (V:C ode-n) fnval)))))))
                         ))
                      ))
            )

           (negfun      
            (if (null? negblocks)
                (V:C 'NONE)
                (V:Op 'SOME
                      (list 
                       (let* (
                              (blocks (fold-reinit-blocks ode-inds negblocks))
                              (used-asgns
                               (map (lambda (index) (assv index asgns))
                                    (delete-duplicates
                                     (fold (lambda (expr ax) (append (fold-asgns asgn-idxs expr) ax))
                                           '() blocks))))
                              (fnval (V:Fn (if (null? regblocks) '(t y c ext extev y_out) '(t y c d ext extev y_out))
                                           (let ((stmts (codegen-set-stmts (compose codegen-expr1 cdr) blocks 'y_out)))
                                             (if (null? used-asgns)
                                                 (E:Begin stmts)
                                                 (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs))))
                                                             used-asgns)
                                                        (E:Begin stmts))))))
                              )
                         (V:Fn '(p) (E:Ret (if (null? regblocks)
                                               (V:Op 'SResponse (list (V:Op 'make_response (list (V:C ode-n) fnval))))
                                               (V:Op 'RegimeResponse (list (V:Op 'make_regime_response (list (V:C ode-n) fnval)))))))

                         ))
                      ))
            )

           (dposfun      
            (if (null? dposblocks)
                (V:C 'NONE)
                (V:Op 'SOME
                      (list 
                       (let* (
                              (blocks (fold-reinit-blocks ode-inds dposblocks))
                              (used-asgns
                               (map (lambda (index) (assv index asgns))
                                    (delete-duplicates
                                     (fold (lambda (expr ax) (append (fold-asgns asgn-idxs expr) ax))
                                           '() blocks))))
                              (fnval (V:Fn '(t y c d d_out)
                                           (let ((stmts (codegen-set-stmts (compose codegen-expr1 cdr) blocks 'd_out)))
                                             (if (null? used-asgns)
                                                 (E:Begin stmts)
                                                 (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs))))
                                                             used-asgns)
                                                        (E:Begin stmts))))))
                              )
                         (V:Fn '(p) (E:Ret (V:Op 'make_dresponse (list (V:C (length dposblocks)) fnval))))
                         ))
                      ))
            )

           (initregfun 
            (if (null? regblocks)
                (V:C 'NONE)
                (V:Op 'SOME
                      (list 
                       (V:Op 'make_bool_initial
                             (list (V:C (length regblocks))
                                   (V:Fn '(r_out) 
                                         (E:Begin (append 
                                                   (cons 
                                                    (E:Set (V:Var 'r_out) 0 (V:C #t))
                                                    (list-tabulate (length (cdr regblocks))
                                                                   (lambda (i) (E:Set (V:Var 'r_out) (+ 1 i) (V:C #f))))
                                                    )
                                                   (list (E:Ret (V:Var 'r_out))))
                                                  ))
                                   ))
                       ))
                ))

           (regfun      
            (if (null? regblocks)
                (V:C 'NONE)
                (V:Op 'SOME
                      (list 
                       (let* ((blocks (fold-regime-reinit-blocks regblocks))
                              (used-asgns
                               (map (lambda (index) (assv index asgns))
                                    (delete-duplicates
                                     (fold (lambda (expr ax) (append (fold-asgns asgn-idxs expr) ax))
                                          '() blocks)))))
                         (V:Op 'make_transition
                               (list (V:C (length blocks))
                                     (V:Fn '(c r r_out) 
                                           (let ((stmts (codegen-set-stmts (compose codegen-expr1 cdr) blocks 'r_out)))
                                             (if (null? used-asgns)
                                                 (E:Begin stmts)
                                                 (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs))))
                                                             used-asgns)
                                                        (E:Begin stmts)))))
                                     ))
                         ))
                      ))
            )
           )

      `(
        (paramfun . ,paramfun)
        (initfun  . ,initfun)
        (dinitfun . ,dinitfun)
        (odefun   . ,odefun)
        (initcondfun . ,initcondfun)
        (initextfun  . ,initextfun)
        (initextevfun  . ,initextevfun)
        (condfun . ,condfun)
        (posfun  . ,posfun)
        (negfun  . ,negfun)
        (dposfun . ,dposfun)
        (initregfun . ,initregfun)
        (regfun . ,regfun)
        )

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

	 (E:Set   (v i x)  
                    (list "(vector-set! " 
                          (value->scheme v) (number->string i) (value->scheme x) 
                          nl
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
                      (list "(" (intersperse (map (lambda (stmt) (list (stmt->ML stmt) )) stmts) "; ")  ")"))

	 (E:Set   (v i x) 
                  (list "(setindex (" (intersperse (list (value->ML v) (number->string i) (value->ML x)) ", ") "))"))

	 (E:Ret     (v)  (value->ML v))
		    
	 (E:Noop    () (list "()"))
	 )))


(define (value->ML v)
  (cases value v
	 (V:C       (v) (cond ((number? v) 
                               (cond ((= v +inf.0) "posInf")
                                     ((= v -inf.0) "negInf")
                                     (else
                                      (let ((str (number->string v)))
                                        (string-map (lambda (c) (case c ((#\-) #\~) (else c))) str)))
                                     ))
                              ((boolean? v)
                               (if v "true" "false"))
                              ((symbol? v)
                               (case v 
                                 ((+inf) "posInf")
                                 ((-inf) "negInf")
                                 (else v)))
			      (else v)))
	 (V:Var     (name) (name/ML name))
	 (V:Sub     (v index) 
		    (list "getindex (" (value->ML v) ", " index ")"))
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

    (if (and solver (not (member solver '(rkfe rk3 rk4a rk4b rkhe rkbs rkck rkoz rkdp rkf45 rkf78 rkv65 cerkoz cerkdp))))
        (error 'codegen-ODE/ML "unknown solver" solver))

    (let ((fundefs (codegen-ODE sim solver)))
      
      (if mod (print-fragments (prelude/ML solver: solver libs: libs) out))
      
      (print-fragments
       (map (match-lambda
             ((name . def)  
              (list (binding->ML (B:Val name def)) nl)))
            fundefs)
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
open Dynamics

fun putStrLn str = 
  (TextIO.output (TextIO.stdOut, str);
   TextIO.output (TextIO.stdOut, "\n"))

fun putStr str = (TextIO.output (TextIO.stdOut, str))

fun showReal n = 
let open StringCvt
in
(if n < 0.0 then "-" else "") ^ (fmt (FIX (SOME 12)) (abs n))
end

val getindex = Unsafe.Array.sub
val setindex = Unsafe.Array.update

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

     `(
       ("fun alloc n = (fn () => Array.array (n, 0.0))" ,nl)
       ("fun bool_alloc n = (fn () => Array.array (n, false))" ,nl)
       ("val summer = fn (a,b) => (vmap2 (fn (x,y) => x+y) (a,b))" ,nl)
       ("fun scaler n = let val b = LastNBuffer.fromList (List.tabulate (24, fn (i) => alloc n ())) in "
        "  fn(a,lst) => (LastNBuffer.rotate_left b; vmap (fn (x) => a*x) lst (LastNBuffer.sub (b, 0))) end" ,nl)
       ("fun make_bool_initial (n, f) = let val a = bool_alloc n () in fn () => f(a) end" ,nl)
       ("fun make_real_initial (n, f) = let val a = alloc n () in fn () => f(a) end" ,nl)
       ("fun make_ext (n, f) = let val a = alloc n () in fn () => f(a) end" ,nl)
       ("fun make_condition (n, f) = let val a = LastNBuffer.fromList (List.tabulate (12, fn (i) => alloc n ())) in " 
        " fn (x,y,e,ext,extev) => (LastNBuffer.rotate_left a; f(x,y,e,ext,extev,LastNBuffer.sub (a, 0))) end" ,nl)
       ("fun make_regime_condition (n, f) = let val a = LastNBuffer.fromList (List.tabulate (12, fn (i) => alloc n ())) in " 
        " fn (d) => fn (x,y,e,ext,extev) => (LastNBuffer.rotate_left a; f d (x,y,e,ext,extev,LastNBuffer.sub (a, 0))) end" ,nl)
       ("fun make_response (n, f) = let val a = alloc n () in fn (x,y,e,ext,extev) => f(x,y,e,ext,extev,a) end" ,nl)
       ("fun make_regime_response (n, f) = let val a = alloc n () in fn (x,y,e,d,ext,extev) => f(x,y,e,d,ext,extev,a) end" ,nl)
       ("fun make_dresponse (n, f) = let val a = alloc n () in fn (x,y,e,d) => f(x,y,e,d,a) end" ,nl)
       ("fun make_transition (n, f) = let val a = bool_alloc n () in fn (e,r) => f(e,r,a) end" ,nl)

       . ,(case solver  
            ;; adaptive solvers with interpolation
            ((cerkoz cerkdp)
             `(
               ("val " ,solver ": (real array) stepper3 = make_" ,solver "()" ,nl)
               ("fun make_stepper (n, deriv) = " ,solver " (alloc n,scaler n,summer,deriv)" ,nl)
               ("fun make_event_stepper (n, deriv) = " ,solver " (alloc n,scaler n,summer,deriv)" ,nl)
               (,nl)
               )
             )
            ;; adaptive solvers with threshold detection on grid points
            ((rkhe rkbs rkf45 rkck rkoz rkdp rkf45 rkf78 rkv65)
             `(
               ("val " ,solver ": (real array) stepper2 = make_" ,solver "()" ,nl)
               ("fun make_stepper (n, deriv) = " ,solver " (alloc n,scaler n,summer,deriv)" ,nl)
               ("fun make_event_stepper (n, deriv) = " ,solver " (alloc n,scaler n,summer,deriv)" ,nl)
               (,nl)
               )
             )
            (else
             `(
               ("val " ,solver ": (real array) stepper1 = make_" ,solver "()" ,nl)
               ("fun make_stepper (n, deriv) = " ,solver " (alloc n,scaler n,summer,deriv)" ,nl)
               ("fun make_event_stepper (n, deriv) = " ,solver " (alloc n,scaler n,summer,deriv)" ,nl)
               (,nl)
               ))
            ))
     ))
)





