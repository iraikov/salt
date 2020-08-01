
(define nll "\n")


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
  (V:Rcon    (args (lambda (x) (every (lambda (p) (and (pair? p) (symbol? (car p)) (value? (cdr p)))) x))))
  (V:Rsel    (v value?) (fld symbol?))
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
		  (V:Rcon (args) (sprintf "(V:Rcon ~A)" args))
		  (V:Rsel (v fld) (sprintf "(V:Rsel ~A ~A)" v fld))
                  ))
  )
		  
(define (symbol-or-pair? v)
  (or (symbol? v) (and (pair? v) (symbol? (car v)))))

(define-datatype binding binding?
  (B:Val     (name symbol-or-pair?)  (v value?))
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

(define (filter-assoc key alst)
  (filter-map (lambda (x) (and (equal? key (car x)) x)) alst))

(define (codegen-primop op args)
  (case op
    ((signal.mul)
     (match args
            ((($ salt#value 'V:C 1.0) right) right)
            ((left ($ salt#value 'V:C 1.0)) left)
            (else (V:Op op args))))
    ((signal.div)
     (match args
            ((left ($ salt#value 'V:C 1.0)) left)
            (else (V:Op op args))))
    (else (V:Op op args))
    ))
      


(define (codegen-expr ode-inds ode-order invcindexmap invextindexmap)
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
                ((yext)
                 (V:Var (cdr (assv index invextindexmap))))
                (else (V:Sub (recur vect) index))))
             ;(('ext index t)
             ; (recur `(getindex ext ,index)))
             ;(('extev index t)
             ; (recur `(signal.primop signal.sub ,t (getindex extev ,index))))
             (($ salt#constant 'number val unit)
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
             (('prioq.findMinDflt q dflt)
              (V:Op 'prioq.findMinDflt (list (recur q) (recur dflt))))
             (('prioq.insert key val q)
              (V:Op 'prioq.insert (list (recur key) (recur val) (recur q))))
             (('prioq.empty)
              (V:Op 'prioq.empty '()))
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
           (($ salt#constant 'number val unit)
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


(define (codegen-ODE sim #!key (libs '()))


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
                 (('signal.reinit ev lhs rhs)
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
                 [('setindex out index val) val]
                 [else #f])
                (cdr x))))
          (and (pair? reinits) (cons (car x) (fold-reinits reinits)))))
      blocks)
     (lambda (x y) (< (car x) (car y))))
    )

  
  (define (fold-reinits/priority lst dflt)
    (let recur ((lst (cdr lst))
                (expr (match (car lst)
                             (('signal.reinitPriority ev rhs)
                              `(prioq.insert ,ev ,rhs prioq.empty))
                             (else (error 'fold-reinits "unknown reinit equation" (car lst))))))
                                             
      (if (null? lst)
          `(prioq.findMinDflt ,expr ,dflt)
          (match (car lst)
                 (('signal.reinitPriority ev rhs)
                  (recur (cdr lst) `(prioq.insert ,ev ,rhs ,expr)))
                 (else (error 'fold-reinits "unknown reinit equation" (car lst)))))
      ))
  
    
  (define (fold-reinit-blocks/priority ode-inds blocks dflt)
    (sort
     (filter-map
      (lambda (x) 
        (let ((reinits 
               (filter-map 
                (match-lambda
                 [('setindex 'y_out index val) (and (member index ode-inds) val)]
                 [('setindex out index val) val]
                 [else #f])
                (cdr x))))
          (and (pair? reinits) (cons (car x) (fold-reinits/priority reinits dflt)))))
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
              (if (member `(y ,index) asgn-idxs)
                  (cons `(y ,index) ax) ax))
             (('getindex 'yext index)
              (cons `(ext ,index) ax))
             (($ salt#constant 'number val unit) ax)
             (('signal.if test ift iff)
              (recur iff (recur ift (recur test ax))))
             (('signal.reinit test dflt upd)
              (recur test (recur dflt (recur upd ax))))
             (else ax))
      ))



  (define (fold-used-asgns blocks asgn-idxs asgn-defs asgns asgn-dag ext-defs)
    
    (define (fold-eqids myeqid myindex eq-node-map expr)
      (let recur ((expr expr) (ax '()))
        (match expr
               ((? symbol?) ax)
               ((? boolean?) ax)
               (('signal.primop op . args)
                (fold recur ax args))
               (('getindex 'y index)
                (if (and (member index asgn-idxs)
                         (not (equal? `(y ,index) myindex)))
                    (append (map 
                             (match-lambda ((index . eqid) eqid))
                             (filter-assoc `(y ,index) eq-node-map)) ax)
                    ax))
               (('getindex 'yext index)
                (if (not (equal? `(ext ,index) myindex))
                    (append (map 
                             (match-lambda ((index . eqid) eqid))
                             (filter-assoc `(ext ,index) eq-node-map)) ax)
                    ax))
               (($ salt#constant 'number val unit) ax)
               (('signal.if test ift iff)
                (recur iff (recur ift (recur test ax))))
               (('signal.reinit test dflt upd)
                (recur test (recur dflt (recur upd ax))))
               (else ax))
        ))

    (let* (
           (order
            (let recur ((order (delete-duplicates
                                (fold-right
                                 (lambda (expr ax) 
                                   (append (reverse (fold-asgns asgn-idxs expr)) ax))
                                 '() blocks))))
              
              (let ((order1 (if (null? order) '()
                                (concatenate
                                 (map
                                  (lambda (e)
                                    (if (member (last e) order)
                                        (filter-map (lambda (v) (and (not (member v order)) v)) (drop-right e 1))
                                        '()))
                                  asgn-dag)))))


                (if (null? order1) 
                    order
                    (recur (append order1 order)))
                ))
            )

          (eq-nodes
           (cadr
            (fold
             (match-lambda* 
              [((and index ('y _)) (n lst))
               (let ((asgns1 (filter-assoc index asgns)))
                 (let ((ns-asgns1 (list-tabulate (length asgns1) (lambda (i) (+ i n)))))
                   (list (+ n (length asgns1)) (append (map cons ns-asgns1 asgns1) lst))))]
              [((and index ('ext _)) (n lst))
               (let ((asgns1 (append (filter-assoc index asgns)
                                     (filter-assoc index ext-defs))))
                 (let ((ns-asgns1 (list-tabulate (length asgns1) (lambda (i) (+ i n)))))
                   (list (+ n (length asgns1)) (append (map cons ns-asgns1 asgns1) lst))))]
              )
             '(0 ())
             order
             )))


          (eq-node-map
            (map (match-lambda
                  ((eqid index name rhs)
                   (cons index eqid)))
                 (sort
                  eq-nodes
                  (lambda (x y) (< (car x) (car y))))
                 ))
          

          (eq-dag 
           (map
            (match-lambda
             ((eqid index name rhs)
              (append (fold-eqids eqid index eq-node-map rhs) (list eqid))))
            eq-nodes))

          (eq-order (topological-sort eq-dag eq?))
          )

      (delete-duplicates
       (append
        (filter-map (lambda (kv) (or (assoc (car kv) asgn-defs) (assoc (car kv) ext-defs))) eq-node-map)
        (map (lambda (eqid) (cdr (assoc eqid eq-nodes))) eq-order)))
      
    ))

  (define (codegen-set-stmts codegen-expr exprs out)
    (let recur ((i 0) (exprs exprs) (stmts '()))
      (if (null? exprs)
          (append stmts (list (E:Ret (V:Var out))))
          (recur (+ i 1) (cdr exprs)
                 (cons (E:Set (V:Var out) i (codegen-expr (car exprs))) stmts))
          ))
    )

  (define (codegen-set-stmts/index codegen-expr exprs is out)
    (let recur ((is is) (exprs exprs) (stmts '()))
      (if (null? exprs)
          (append stmts (list (E:Ret (V:Var out))))
          (recur (cdr is) (cdr exprs)
                 (cons (E:Set (V:Var out) (car is) (codegen-expr (car exprs))) stmts))
          ))
    )

  (define (codegen-set-stmts* codegen-expr exprs out)
    (let recur ((i 0) (exprs exprs) (stmts '()))
      (if (null? exprs)
          stmts
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

  (define (random-decls body libs)
    (if (member 'random libs)
        (E:Let `(,(B:Val 'random_uniform (V:Op 'mk_random_uniform (list (V:Var 'rs))))
                 ,(B:Val 'random_unifrange (V:Op 'mk_random_unifrange (list (V:Var 'rs))))
                 ,(B:Val 'random_normal (V:Op 'mk_random_normal (list (V:Var 'rs) (V:Var 'rszt))))
                 ,(B:Val 'random_exponential (V:Op 'mk_random_exponential (list (V:Var 'rs) (V:Var 'rszt))))
                 ,(B:Val 'random_poisson (V:Op 'mk_random_poisson (list (V:Var 'rs) (V:Var 'rszt)))))
               body)
        body))

  (define (random-args arg-list libs)
    (if (member 'random libs) (append arg-list '(rs rszt)) arg-list))
  
  (define (random-cargs arg-list libs)
    (if (member 'random libs)
        (append arg-list '(((%pointer int) . rs)
                           ((%pointer uint64_t) . ki)
                           ((%pointer uint64_t) . ke)
                           ((%pointer double) . wi)
                           ((%pointer double) . fi)
                           ((%pointer double) . we)
                           ((%pointer double) . fe)
                           )) arg-list))
  
  (define (random-vargs arg-list libs)
    (if (member 'random libs)
        (append arg-list `((rs . ,(V:Var 'rs))
                           (rszt . ,(V:Var 'rszt))))
        arg-list))
  

  (let* (
        (cindexmap      (simruntime-cindexmap sim))
        (invcindexmap   (map (lambda (x) (cons (cdr x) (car x))) (env->list cindexmap)))
        (dindexmap      (simruntime-dindexmap sim))
        (rindexmap      (simruntime-rindexmap sim))
        (extindexmap    (simruntime-extindexmap sim))
        (invextindexmap (map (lambda (x) (cons (cdr x) (car x))) (env->list extindexmap)))
        (defs           (simruntime-definitions sim))
        (discrete-defs  (simruntime-discrete-definitions sim))
        (params         (simruntime-parameters sim))
        (fields         (simruntime-fields sim))
        (externals      (simruntime-external-definitions sim))
        (externalevs    (simruntime-externalev-definitions sim))
        (extevlinks     (simruntime-externalev-links sim))
        (eqblock        (simruntime-eqblock sim))


        (trstmt (trace 'codegen-ODE "eqblock: ~A~%" eqblock))
        (trstmt (trace 'codegen-ODE "externals: ~A~%" externals))

        (ode-inds 
         (sort
          (filter-map 
           (match-lambda [('setindex 'dy_out index val) index] 
                         [('setindex 'y_out index val)  #f] 
                         [('reduceindex 'y_out index val)  #f] 
                         [('reduceindex 'yext_out index val)  #f] 
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
        (trstmt (trace 'codegen-ODE "condblock: ~A~%" condblock))

        (cond-n (length condblock))

        (dposblocks (let ((bucket-eqs 
                           (bucket 
                            (match-lambda*
                             [((and eq ('setindex 'd_out index ('signal.reinit ev y rhs))))
                              index]
                             [else #f])
                            (simruntime-posresp sim))))
                      bucket-eqs))
        (trstmt (trace 'codegen-ODE "dposblocks: ~A~%" dposblocks))

        (dsc-n (length dposblocks))

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

        (trstmt (trace 'codegen-ODE "posblocks: ~A~%" posblocks))

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

        (trstmt (trace 'codegen-ODE "negblocks: ~A~%" negblocks))

        (regblocks (let ((bucket-eqs 
                          (bucket 
                           (match-lambda*
                            [((and eq ('setindex 'r_out index ('signal.reinit ev y rhs))))
                             index]
                            [_ #f])
                           (simruntime-posresp sim))))
                     bucket-eqs))
        (trstmt (trace 'codegen-ODE "regblocks: ~A~%" regblocks))
        
        (extevlink-blocks (let ((bucket-eqs 
                                 (bucket 
                                  (match-lambda*
                                   [((and eq ('setindex 'ext_out index ('signal.reinitPriority ev rhs))))
                                    index]
                                   [else #f])
                                  extevlinks)))
                            bucket-eqs))

        (regime-n (length regblocks))
        (ext-n (length externals))

        (has-conds?     (> cond-n 0))
        (has-regimes?   (> regime-n 0))
        (has-fields?    (> (length fields) 0))
        (has-params?    (> (length params) 0))
        (has-ext?       (> ext-n 0))
        )

    (let* (

           (codegen-expr1 (codegen-expr ode-inds ode-order invcindexmap invextindexmap))

           (ode-defs (filter-map (lambda (index) 
                                   (and (member index ode-inds)
                                        (list-ref defs index))) 
                                 (list-tabulate (length invcindexmap) 
                                                (lambda (x) x))))

           (trstmt (trace 'codegen-ODE "ode-defs: ~A~%" ode-defs))

           (asgn-defs (filter-map (lambda (index) 
                                    (and (not (member index ode-inds))
                                         (let ((invassoc (assv index invcindexmap)))
                                           (if (not invassoc)
                                               (error 'codegen "unknown continuous quantity index" 
                                                      index invcindexmap)
                                               (list `(y ,index)
                                                     (cdr invassoc)
                                                     (list-ref defs index))))))
                                  (list-tabulate (length invcindexmap) 
                                                 (lambda (x) x))))
                        
           (trstmt (trace 'codegen-ODE "asgn-defs: ~A~%" asgn-defs))

           (ext-defs (map (match-lambda [(index . name) 
                                         `((ext ,index) ,name (getindex ext ,index))])
                          invextindexmap))
           
           (trstmt (trace 'codegen-ODE "ext-defs: ~A~%" ext-defs))

           (asgn-idxs (delete-duplicates
                       (append
                        (map car asgn-defs)
                        (filter-map
                         (match-lambda [('setindex 'dy_out index val) #f]
                                       [(or ('setindex 'y_out index val)
                                            ('reduceindex 'y_out index val)) `(y ,index)]
                                       [('reduceindex 'yext_out index val) #f]
                                       [eq (error 'codegen "unknown equation type" eq)])
                         eqblock))
                       ))

           (trstmt (trace 'codegen-ODE "asgn-idxs: ~A~%" asgn-idxs))

           (asgn-dag (fold (match-lambda* 
                            [(('setindex 'dy_out index val) dag) dag]
                            [(('setindex 'y_out index val) dag)
                             (let ((srcs (fold-asgns asgn-idxs val)))
                               (if (null? srcs) (cons `((y ,index)) dag)
                                   (fold (lambda (src dag) (update-edge (cons src `(y ,index)) dag))
                                         dag srcs)))]
                            [(('reduceindex 'y_out index val) dag)
                             (let ((edges (filter-map (match-lambda
                                                       [(and src ('y srcindex))
                                                        (and (not (= srcindex index)) (cons src `(y ,index)))]
                                                       [(and src ('ext srcindex))
                                                        (cons src `(y ,index))]
                                                       [src (error 'codegen "unknown node type" src)])
                                                      (fold-asgns asgn-idxs val))))
                               (trace 'codegen-ODE "asgn-dag: reduceindex y_out ~A: val = ~A asgns: ~A~%"
                                      index val (fold-asgns asgn-idxs val))
                               (if (null? edges) (cons `((y ,index)) dag)
                                   (fold (lambda (e dag) (update-edge e dag))
                                         dag edges)))]
                            [(('reduceindex 'yext_out index val) dag)
                             (let ((edges (filter-map (match-lambda
                                                       [(and src ('ext srcindex))
                                                        (and (not (= srcindex index)) (cons src `(ext ,index)))]
                                                       [(and src ('y srcindex))
                                                        (cons src `(ext ,index))]
                                                       [src (error 'codegen "unknown node type" src)])
                                                      (fold-asgns asgn-idxs val))))
                               (if (null? edges) (cons `((ext ,index)) dag)
                                   (fold (lambda (e dag) (update-edge e dag))
                                         dag edges)))]
                            [eq (error 'codegen "unknown equation type" eq)])
                           '() eqblock))

           (trstmt (trace 'codegen-ODE "asgn-dag: ~A~%" asgn-dag))

           (asgn-order (topological-sort asgn-dag eq?))

           (trstmt (trace 'codegen-ODE "asgn-order: ~A~%" asgn-order))
           
           (asgns 
            (delete-duplicates
             (fold-right
              (lambda (ordtindex ax)
                (append
                 (reverse
                  (filter-map
                   (match ordtindex
                          [('y ordindex)
                           (match-lambda [('setindex 'dy_out index val) #f] 
                                         [(or ('setindex 'y_out index val)  
                                              ('reduceindex 'y_out index val))
                                          (and (= index ordindex) 
                                               (list ordtindex (cdr (assv index invcindexmap)) val))]
                                         [('reduceindex 'yext_out index val) #f]
                                         [eq (error 'codegen "unknown equation type" eq)])]
                          [('ext ordindex)
                           (match-lambda [('setindex 'dy_out index val) #f] 
                                         [(or ('setindex 'y_out index val)  
                                              ('reduceindex 'y_out index val)) #f]
                                         [('reduceindex 'yext_out index val) 
                                          (and (= index ordindex) 
                                               (list ordtindex (cdr (assv index invextindexmap)) val))]
                                         [eq (error 'codegen "unknown equation type" eq)])]
                          [else (error 'codegen "unknown index type" ordtindex)])
                   eqblock)) ax))
              '() asgn-order)
             equal?))

           (trstmt (trace 'codegen-ODE "asgns: ~A~%" asgns))

           (odeblock 
            (sort 
             (filter-map
              (match-lambda [(and ode ('setindex 'dy_out index val)) ode] 
                            [('setindex 'y_out index val)  #f]
                            [('reduceindex 'y_out index val)  #f]
                            [('reduceindex 'yext_out index val)  #f]
                            [eq (error 'codegen "unknown equation type" eq)])
              eqblock)
             (match-lambda*
              [(('setindex vect1 index1 val1) 
                ('setindex vect2 index2 val2)) 
               (< index1 index2)])))

           (paramfun (if has-params?
                         (V:Op 'make_real_initial
                               (list (V:C (length params))
                                     (V:Fn '(p_out) (E:Begin (codegen-set-stmts codegen-expr1 params 'p_out)))))
                         (V:Fn '() (E:Ret (V:C 'empty_real_initial)))))

           (fieldfun (if has-fields?
                         (V:Op 'make_real_initial
                               (list (V:C (length fields))
                                     (V:Fn '(fld_out) (E:Begin (codegen-set-stmts codegen-expr1 fields 'fld_out)))))
                         (V:Fn '() (E:Ret (V:C 'empty_real_initial)))))

           (initfun  
            (let ((stmts (codegen-set-stmts codegen-expr1 ode-defs 'y_out)))
              (V:Fn (let ((args0 '(p fld)))
                      (random-args args0 libs))
                    (random-decls
                     (E:Ret (V:Fn '(y_out) 
                                  (if (null? asgn-defs)
                                      (E:Begin stmts)
                                      (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs))))
                                                  asgn-defs)
                                             (E:Begin stmts)))))
                     libs))
              ))

           (dinitfun  
            (let ((stmts (codegen-set-stmts codegen-expr1 discrete-defs 'd_out)))
              (if (null? discrete-defs)
                  (V:C 'NONE)
                  (V:Op 'SOME
                        (list 
                         (V:Fn (let ((args0 '(p fld)))
                                 (random-args args0 libs))
                               (random-decls
                                (E:Ret (V:Fn '(d_out)
                                             (if (null? asgn-defs)
                                                 (E:Begin stmts)
                                                 (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs))))
                                                             asgn-defs)
                                                        (E:Begin stmts)))))
                                libs))
                         ))
                  ))
            )

           (initextfun  
            (let ((stmts (codegen-set-stmts codegen-expr1 externals 'ext_out)))
              (V:Fn '(p fld)
                    (E:Ret (V:Op 'make_ext (list (V:C (length externals)) 
                                                 (V:Fn '(ext_out) 
                                                       (if (null? asgn-defs)
                                                           (E:Begin stmts)
                                                           (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs))))
                                                                       asgn-defs)
                                                                  (E:Begin stmts))))))
                           ))
              ))

           (initextevfun  
            (let ((stmts (codegen-set-stmts codegen-expr1 externalevs 'extev_out)))
              (V:Fn '(p fld)
                    (E:Ret (V:Op 'make_ext (list (V:C (length externalevs)) 
                                                 (V:Fn '(extev_out) 
                                                       (if (null? asgn-defs)
                                                           (E:Begin stmts)
                                                           (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs))))
                                                                       asgn-defs)
                                                                  (E:Begin stmts))))))
                           ))
              ))


           (linkextevfun  
            (let* ((dflt   (make-constant 'number +inf.0 'unitbottom))
                   (blocks (fold-reinit-blocks/priority ode-inds extevlink-blocks dflt))
                   (stmts  (codegen-set-stmts/index (compose codegen-expr1 cdr) blocks (map car blocks) 'extev_out)))
              (V:Fn '(extev extev_out) 
                    (E:Begin stmts)
                    ))
            )

           (rhsfun
            (let ((rhs-args  '((double . t) ((%pointer double) . y) ((%pointer double) . dy_out) ((%pointer (%pointer void)) . clos))))
              (V:Fn rhs-args
                    (let* ((clos-args
                            (let ((args0 
                                   (if has-regimes?
                                       `(((%pointer double) . p) ((%pointer double) . fld) 
                                         ((%pointer double) . d) ((%pointer int) . r) 
                                         ((%pointer double) . ext) ((%pointer double) . extev))
                                       `(((%pointer double) . p) ((%pointer double) . fld) 
                                         ((%pointer double) . ext) ((%pointer double) . extev)))))
                              (random-cargs args0 libs)))
                           (odes (map (match-lambda [('setindex 'dy_out index val) val]) odeblock))
                           (used-asgns (fold-used-asgns odes asgn-idxs asgn-defs asgns asgn-dag ext-defs))
                           (stmts (codegen-set-stmts* codegen-expr1 odes 'dy_out)))
                      (E:Let (map (lambda (arg index) (B:Val (cons (cdr arg) (car arg)) (V:Sub (V:Var 'clos) index)))
                                  clos-args (list-tabulate (length clos-args) (lambda (i) i)))
                             (if (null? used-asgns)
                                 (E:Begin stmts)
                                 (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs)))) used-asgns)
                                        (E:Begin stmts)))
                             ))
                    ))
            )

           (stepfun 
            (let* ((odes (map (match-lambda [('setindex 'dy_out index val) val]) odeblock))

                   (used-asgns (fold-used-asgns odes asgn-idxs asgn-defs asgns asgn-dag ext-defs))

                   (clos-args (let ((args0 (if has-regimes? '(p fld d r ext extev) '(p fld ext extev))))
                               (random-args args0 libs)))
                   
                   (rhsfun 
                    (V:Fn '(clos)
                          (E:Let
                           (filter-map (lambda (arg)
                                         (let ((dflt (B:Val arg (V:Rsel (V:Var 'clos) arg))))
                                           (case arg
                                             ((p)  (if has-params? dflt #f)) 
                                             ((fld) (if has-fields? dflt #f))
                                             ((ext) (if has-ext? dflt #f))
                                             ((extev) (if has-ext? dflt #f))
                                             (else  dflt))))
                                       clos-args )
                           (random-decls
                            (E:Ret 
                             (V:Fn '(t y dy_out)
                                   (let ((stmts (codegen-set-stmts codegen-expr1 odes 'dy_out)))
                                     (if (null? used-asgns)
                                         (E:Begin stmts)
                                         (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs)))) used-asgns)
                                                (E:Begin stmts))
                                         ))
                                   ))
                            libs))
                          ))
                   )

              (if has-regimes?
                  (V:Op 'make_regime_stepper (list rhsfun))
                  (V:Op 'make_stepper (list rhsfun))))
            )

           (odefun    
            (V:Fn (random-args '(p fld) libs)
                  (E:Ret
                   (cond
                    (has-regimes?
                     (V:Op 'RegimeStepper
                           (list (V:Fn '(d r ext extev h abstol reltol x y yout) 
                                       (E:Ret (V:Op 'stepfun
                                                    (list (V:Rcon
                                                           (random-vargs
                                                            `((p     . ,(V:Var 'p))
                                                              (fld   . ,(V:Var 'fld))
                                                              (d     . ,(V:Var 'd))
                                                              (r     . ,(V:Var 'r))
                                                              (ext   . ,(V:Var 'ext))
                                                              (extev . ,(V:Var 'extev)))
                                                            libs))
                                                          (V:Var 'h)
                                                          (V:Var 'abstol)
                                                          (V:Var 'reltol)
                                                          (V:Var 'x)
                                                          (V:Var 'y)
                                                          (V:Var 'yout)
                                                          ))
                                              ))
                                 ))
                     )
                    
                    (has-conds?
                     (V:Op 'EventStepper
                           (list (V:Fn '(ext extev h abstol reltol x y yout) 
                                       (E:Ret (V:Op 'stepfun
                                                     (list (V:Rcon
                                                            (random-vargs
                                                            `((p     . ,(V:Var 'p))
                                                              (fld   . ,(V:Var 'fld))
                                                              (ext   . ,(V:Var 'ext))
                                                              (extev . ,(V:Var 'extev)))
                                                            libs))
                                                           (V:Var 'h)
                                                           (V:Var 'abstol)
                                                           (V:Var 'reltol)
                                                           (V:Var 'x)
                                                           (V:Var 'y)
                                                           (V:Var 'yout))
                                                     ))
                                       ))
                           ))
                    
                    (else
                     (V:Op 'ContStepper
                           (list  (V:Fn '(ext extev h abstol reltol x y yout) 
                                        (E:Ret (V:Op 'stepfun
                                                     (list (V:Rcon
                                                            (random-vargs
                                                             `((p     . ,(V:Var 'p))
                                                               (fld   . ,(V:Var 'fld))
                                                               (ext   . ,(V:Var 'ext))
                                                               (extev . ,(V:Var 'extev)))
                                                             libs))
                                                           (V:Var 'h)
                                                           (V:Var 'abstol)
                                                           (V:Var 'reltol)
                                                           (V:Var 'x)
                                                           (V:Var 'y)
                                                           (V:Var 'yout)))
                                               ))
                                  ))
                     ))
                   ))
            )
                                    
           (initcondfun 
            (if (null? condblock)
                (V:C 'NONE)
                (V:Op 'SOME
                      (list
                       (V:Fn '(c_out) 
                             (E:Begin 
                              (append 
                               (list-tabulate
                                cond-n
                                (lambda (i) (E:Set (V:Var 'c_out) i (V:C 'negInf))))
                               (list (E:Ret (V:Var 'c_out)))
                               )
                              ))
                       ))
                ))
           
           (condfun     

            (let ((used-asgns (fold-used-asgns condblock asgn-idxs asgn-defs asgns asgn-dag ext-defs)))
              
              (if (null? condblock)
                  (V:C 'NONE)
                  (let ((fnval (V:Fn  (if has-regimes?
                                         '(t y c d r ext extev c_out)
                                         '(t y c ext extev c_out))
                                     (let ((stmts (codegen-set-stmts codegen-expr1 condblock 'c_out)))
                                       (if (null? used-asgns)
                                           (E:Begin stmts)
                                           (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs))))
                                                       used-asgns)
                                                  (E:Begin stmts)))))))
                    (V:Op 'SOME
                          (list 
                           (V:Fn '(p fld) 
                                 (E:Ret (if has-regimes?
                                            (V:Op 'RegimeCondition  (list (V:Op 'make_regime_cond (list (V:Var 'p) (V:Var 'fld) fnval))))
                                            (V:Op 'SCondition (list (V:Op 'make_cond (list (V:Var 'p) (V:Var 'fld) fnval))))
                                            )
                                        ))
                           ))
                    ))
              ))

           (condrhsfun
            (let ((used-asgns (fold-used-asgns condblock asgn-idxs asgn-defs asgns asgn-dag ext-defs))
                  (rhs-args (let ((args0 (if has-regimes?
                                             '((double . t) ((%pointer double) . p) ((%pointer double) . fld) 
                                               ((%pointer double) . y) ((%pointer double) . c) 
                                               ((%pointer double) . d) ((%pointer int) . r) 
                                               ((%pointer double) . ext) ((%pointer double) . extev) 
                                               ((%pointer double) . c_out))
                                             '((double . t) ((%pointer double) . p) ((%pointer double) . fld) 
                                               ((%pointer double) . y) ((%pointer double) . c) 
                                               ((%pointer double) . ext) ((%pointer double) . extev) 
                                               ((%pointer double) . c_out)))))
                              (random-args args0 libs))))
              (and (not (null? condblock))
                   (V:Fn rhs-args
                    (let ((stmts (codegen-set-stmts* codegen-expr1 condblock 'c_out)))
                      (if (null? used-asgns)
                          (E:Begin stmts)
                          (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs))))
                                       used-asgns)
                                 (E:Begin stmts))
                          ))
                    ))
              ))
           

           (posfun      
            (if (null? posblocks)
                (V:C 'NONE)
                (V:Op 'SOME
                      (list 
                       (let* (
                              (blocks (fold-reinit-blocks ode-inds posblocks))
                              (used-asgns (fold-used-asgns (map cdr blocks) asgn-idxs asgn-defs asgns asgn-dag ext-defs))
                              (rhs-args (if has-regimes? '(t y c d r ext extev y_out) '(t y c ext extev y_out)))
                              
                              (fnval (V:Fn rhs-args
                                           (let ((stmts (codegen-set-stmts (compose codegen-expr1 cdr) blocks 'y_out)))
                                             (if (null? used-asgns)
                                                 (E:Begin stmts)
                                                 (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs))))
                                                             used-asgns)
                                                        (E:Begin stmts)))
                                             )))
                              )
                         (V:Fn (random-args '(p fld) libs)
                               (random-decls
                                (E:Ret (if has-regimes?
                                           (V:Op 'RegimeResponse (list fnval))
                                           (V:Op 'SResponse (list fnval))
                                           ))
                                libs))
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
                              (used-asgns (fold-used-asgns (map cdr blocks) asgn-idxs asgn-defs asgns asgn-dag ext-defs))
                              (rhs-args (if has-regimes? '(t y c d r ext extev y_out) '(t y c ext extev y_out)))

                              (fnval (V:Fn rhs-args
                                           (let ((stmts (codegen-set-stmts (compose codegen-expr1 cdr) blocks 'y_out)))
                                             (if (null? used-asgns)
                                                 (E:Begin stmts)
                                                 (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs))))
                                                             used-asgns)
                                                        (E:Begin stmts))))))
                              )
                         (V:Fn (random-args '(p fld) libs)
                               (random-decls
                                (E:Ret (if has-regimes?
                                           (V:Op 'RegimeResponse (list fnval))
                                           (V:Op 'SResponse (list fnval))
                                           ))
                                libs))

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
                              (used-asgns (fold-used-asgns (map cdr blocks) asgn-idxs asgn-defs asgns asgn-dag ext-defs))

                              (fnval (V:Fn '(t y c d d_out)
                                           (let ((stmts (codegen-set-stmts (compose codegen-expr1 cdr) blocks 'd_out)))
                                             (if (null? used-asgns)
                                                 (E:Begin stmts)
                                                 (E:Let (map (match-lambda ((_ name rhs) (B:Val name (codegen-expr1 rhs))))
                                                             used-asgns)
                                                        (E:Begin stmts))))))
                              )
                         (V:Fn (random-args '(p fld) libs)
                               (random-decls
                                (E:Ret (V:Op 'make_dresponse (list (V:C (length dposblocks)) fnval)))
                                libs))
                         ))
                      ))
            )

           (initregfun 
            (if (null? regblocks)
                (V:C 'NONE)
                (V:Op 'SOME
                      (list 
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

           (regfun      
            (if (null? regblocks)
                (V:C 'NONE)
                (V:Op 'SOME
                      (list 
                       (let* (
                              (blocks (fold-regime-reinit-blocks regblocks))
                              (used-asgns (fold-used-asgns (map cdr blocks) asgn-idxs asgn-defs asgns asgn-dag ext-defs))
                              )
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

      (d 'codegen-ODE "cindexmap = ~A~%" (env->list cindexmap))
      (d 'codegen-ODE "asgn-dag = ~A~%" asgn-dag)
      (d 'codegen-ODE "asgn-order = ~A~%" asgn-order)

      `(
        (ode-n  . ,ode-n)
        (cond-n . ,cond-n)
        (dsc-n  . ,dsc-n)
        (ext-n  . ,ext-n)
        (regime-n . ,regime-n)
        (paramfun . ,paramfun)
        (fieldfun . ,fieldfun)
        (initfun  . ,initfun)
        (dinitfun . ,dinitfun)
        (rhsfun   . ,rhsfun)
        (stepfun  . ,stepfun)
        (odefun   . ,odefun)
        (initcondfun . ,initcondfun)
        (initextfun  . ,initextfun)
        (initextevfun  . ,initextevfun)
        (linkextevfun  . ,linkextevfun)
        (condfun . ,condfun)
        (condrhsfun . ,condrhsfun)
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
                (list "(" (name/scheme name) " " (value->scheme v) ")" nll))))


(define (stmt->scheme x . rest)
  (let-optionals rest ((bnd? #t))
    (cases stmt x

	 (E:Ife     (test ift iff)
		    (list "(cond (" (value->scheme test) " " 
			  " " (stmt->scheme ift ) ")" 
			  "(else " (stmt->scheme iff) "))" ))

	 (E:Let     (bnds body)
		    (list "(let* (" 
			  (map (lambda (x) (binding->scheme x)) bnds) 
			  ") " 
			  (stmt->scheme body #f) 
			  ")" nll))
			  
	 (E:Begin   (stmts)  
                    (list "(begin " 
                          (map (lambda (x) (stmt->scheme x)) stmts) nll
                          ")"))

	 (E:Set   (v i x)  
                    (list "(vector-set! " 
                          (value->scheme v) (number->string i) (value->scheme x) 
                          nll
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

(define (args->sexpr xs)
  (map (lambda (x) 
         (if (symbol? x) (name/scheme x)
             (list (car x) (name/scheme (cdr x)))
             ))
       xs))

(define (binding->sexpr x)
  (cases binding x
	 (B:Val (name v)
                (cases value v
                       (V:Fn (args body)
                             `(%fun void ,name ,(args->sexpr args) ,(stmt->sexpr body)))
                       (else
                        (if (pair? name)
                            `(%var ,(cdr name) ,(car name) ,(value->sexpr v))
                            `(%var double ,name ,(value->sexpr v))))
                       ))
         ))


(define (lbinding->sexpr x decls.env)
  (let ((decls (car decls.env))
        (env (cdr decls.env)))
    (cases binding x
           (B:Val (name v)
                  (cases value v
                         (V:Fn (args body)
                               (cons
                                (cons `(%fun void ,name ,(args->sexpr args) ,(stmt->sexpr body)) decls)
                                (if (member name env) env (cons name env))))
                         (else
                          (if (pair? name)
                              (cons
                               (cons (if (member (car name) env)
                                         `(= ,(car name) ,(value->sexpr v))
                                         `(%var ,(cdr name) ,(car name) ,(value->sexpr v))) decls)
                               (if (member name env) env (cons name env)))
                              (cons
                               (cons (if (member name env)
                                         `(= ,name ,(value->sexpr v))
                                         `(%var double ,name ,(value->sexpr v))) decls)
                               (if (member name env) env (cons name env))))
                          ))
                  ))
    ))

(define (stmt->sexpr x)
    (cases stmt x

	 (E:Ife     (test ift iff)
		    `(if ,(value->sexpr test) ,(stmt->sexpr ift) ,(stmt->sexpr iff)))

	 (E:Let     (bnds body)
                    `(%begin . ,(append (reverse (car (fold lbinding->sexpr '(() . ()) bnds))) (list (stmt->sexpr body)))))
			  
	 (E:Begin   (stmts)  
                    (let ((stmts1 (map (lambda (x) (stmt->sexpr x)) stmts)))
                      `(%begin . ,(if (null? stmts1) (list fmt-null) stmts1))))

	 (E:Set   (v i x)  
                    `(vector-set! ,(value->sexpr v) ,i ,(value->sexpr x) ))

	 (E:Ret     (v)  (value->sexpr v))
		    
	 (E:Noop    () `(%begin ,fmt-null))
	 ))

(define (mathop->cfun op)
  (case op
    ((signal.add) '+) ((signal.sub) '-) ((signal.mul) '*) ((signal.div) '/) 
    ((signal.pow) 'pow) 
    ((signal.gte) '>=) ((signal.gt) '<=) ((signal.lt) '<) ((signal.lte) '<=)
    ((signal.neg) '-)
    ((signal.abs)    'fabs)
    ((signal.atan)   'atan)
    ((signal.asin)   'asin)
    ((signal.acos)   'acos)
    ((signal.sin)    'sin)
    ((signal.cos)    'cos)
    ((signal.exp)    'exp)
    ((signal.ln)     'ln)
    ((signal.sqrt)   'sqrt)
    ((signal.tan)    'tan)
    ((signal.cosh)   'cosh)
    ((signal.sinh)   'sinh)
    ((signal.tanh)   'tanh)
    ((signal.hypot)  'hypot)
    ((signal.gamma)  'gamma)
    ((signal.lgamma) 'lgamma)
    ((signal.log10)  'log10)
    ((signal.log2)   'log2)
    ((signal.log1p)  'log1p)
    ((signal.ldexp)  'ldexp)
    ((signal.cube)   'cube)
    ((signal.round)   'round)
    ((signal.ceiling) 'ceiling)
    ((signal.floor)   'floor)
    (else op)))


(define (value->sexpr v)
  (let ((result
         (cases value v
                (V:C       (v) v)
                (V:Var     (name) (name/scheme name))
                (V:Sub     (v index) 
                           `(vector-ref ,(value->sexpr v) ,index))
                (V:Fn      (args body) 
                           `(%fun #f ,(gensym 'f) ,(args->sexpr args) ,(stmt->sexpr body)))
                (V:Op     (name args)
                          (let* ((op (mathop->cfun name)))
                            (cond ((null? args) 
                                   (case name 
                                     ((NONE)  `false)
                                     (else    '())))
                                  (else
                                   `(,op . ,(map value->sexpr args))))
                            ))
                (V:Ifv     (test ift iff)
                           `(if ,(value->sexpr test)
                                ,(value->sexpr ift)
                                ,(value->sexpr iff)))
                
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
                    (list "val " (name/ML name) " = " (value->ML v) nll))
         )))


(define (stmt->ML x . rest)
  (let-optionals rest ((bnd? #t))
    (cases stmt x

	 (E:Ife     (test ift iff)
		    (list "if (" (value->ML test) ") " nll
			  "then " (stmt->ML ift ) nll
			  "else " (stmt->ML iff) nll))

	 (E:Let     (bnds body)
		    (list "let " nll
			  (map (lambda (x) (binding->ML x #t)) bnds) nll
			  "in " nll
			  (stmt->ML body #f) nll
			  "end" nll))
			  
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
	 (V:Rcon      (args) 
                      (list "{ "
                            (intersperse (map (lambda (x) (list (name/ML (car x)) "=" (value->ML (cdr x)))) args) ",")
                            " }"))
	 (V:Rsel      (v fld) 
                      (list "(#" (name/ML fld) "(" (value->ML v) "))"))
	 (V:Ifv     (test ift iff)
		    (list "(if (" (value->ML test) ") " 
			  "then " (value->ML ift ) " " 
			  "else " (value->ML iff) ")"))

	 ))


(define (codegen-ODE/ML name sim #!key (out (current-output-port)) (mod #t) (libs '()) (solver 'rkdp) (csysname #f))

    (let ((sysdefs (codegen-ODE sim libs: libs)))

      (if mod (print-fragments
               (if (member 'random libs)
                   (list "structure Model : MODEL_RANDMTZIG =" nll
                         "struct" nll
                         nll)
                   (list "structure Model : MODEL =" nll
                         "struct" nll
                         nll))
               out))
      
      (print-fragments
       (map (match-lambda
             ((name . def) (list (binding->ML (B:Val name def)) nll)))
            `((n    . ,(V:C (alist-ref 'ode-n sysdefs)))
              (nev  . ,(V:C (alist-ref 'cond-n sysdefs)))
              (nregime . ,(V:C (alist-ref 'regime-n sysdefs)))
              (ndsc    . ,(V:C (alist-ref 'dsc-n sysdefs)))
              (next    . ,(V:C (alist-ref 'ext-n sysdefs)))
              ))
       out)

      (if mod (print-fragments (prelude/ML libs: libs solver: solver csysname: csysname) out))
      
      (print-fragments
       (map (match-lambda
             (('rhsfun . def)   (list))
             (('condrhsfun . def)   (list))
             (('ode-n . def)    (list))
             (('cond-n . def)   (list))
             (('cond-n . def)   (list))
             (('dsc-n  . def)   (list))
             (('ext-n  . def)   (list))
             (('regime-n . def) (list))
             ((name . def) (list (binding->ML (B:Val name def)) nll)))
            sysdefs)
       out)
      
      (if mod (print-fragments (list nll "end" nll) out))
      
      ))


(define (prelude/ML  #!key (csysname #f) (solver 'rkdp) (libs '()))
`(
#<<EOF
structure ModelPrelude = ModelPreludeFn(val n = n
                                        val nev = nev
                                        val nregime = nregime
                                        val ndsc = ndsc)
open ModelPrelude

EOF

,@(if (not csysname)
 (case solver
   ((rkdp rkoz3 rkoz4 rkoz5)
    `(
      (,(sprintf "val make_stepper = make_stepper_~A" solver) ,nll)
      (,(sprintf "val make_regime_stepper = make_stepper_~A" solver) ,nll)
      (,(sprintf "val interpfun = interp_ce~A" solver) ,nll)
      ))
   (else `(("val make_stepper = make_stepper_rkdp" ,nll)
           ("val make_regime_stepper = make_stepper_rkdp" ,nll)
           ("val interpfun = interp_cerkdp" ,nll)
           ))
   )
'())

,@(if csysname
      `(("open CRungeKutta" ,nll)
        ("val size_closure_cont = 4" ,nll)
        ("val size_closure_regime = 6" ,nll)
        ("val size_closure_cont_rs = 11" ,nll)
        ("val size_closure_regime_rs = 13" ,nll)
        ("val c_regime_cond_eval = _import * : "
         "MLton.Pointer.t -> real * real array * real array * real array * real array * real array * bool array * real array * real array * real array  -> unit;" ,nll)
        ("val c_cond_eval = _import * : "
         "MLton.Pointer.t -> real * real array * real array * real array * real array * real array * real array * real array  -> unit;", nll)
        ("val cond_cb = _address " ,(sprintf "\"cond~A\"" csysname) " public: MLton.Pointer.t;" ,nll)
        ("fun make_cond (p,fld,f) = let val fe = c_cond_eval cond_cb in "
         "fn(t,y,c,ext,extev,c_out) => (fe (t,p,fld,y,c,ext,extev,c_out); c_out) end" ,nll)
        ("fun make_regime_cond (p,fld,f) = let val fe = c_regime_cond_eval cond_cb in "
         "fn(t,y,c,d,r,ext,extev,c_out) => (fe (t,p,fld,y,c,d,r,ext,extev,c_out); c_out) end" ,nll)
        ("val ode_cb = _address " ,(sprintf "\"~A\"" csysname) " public: MLton.Pointer.t;" ,nll))
      '())


,(if (member 'random libs) `(,nll "open ModelRandom" ,nll) '())
    
;; adaptive solvers implemented in C
,@(if (not csysname) '()
      (map
       (lambda (solver)
         `(
           ("val interpfun = make_c" ,solver "_hinterp(n)" ,nll)
           ,@(if (member 'random libs)
                 `(
                   ("type regime_clos = {p: real array,fld: real array,d: real array,r: bool array,ext: real array,extev: real array,rs: RandomMTZig.state,rszt: RandomMTZig.zt}" ,nll
                    "type clos = {p: real array,fld: real array,ext: real array,extev: real array,rs: RandomMTZig.state,rszt: RandomMTZig.zt}" ,nll
                    "fun make_regime_stepper (deriv: regime_clos -> real * real array * real array -> real array) = " ,nll
                    "let " ,nll
                    "    val solver = make_c" ,solver "(n,ode_cb)" ,nll
                    "    val clos = alloc_closure(size_closure_regime_rs)" ,nll
                    "    val err  = state()" ,nll
                    "in " ,nll
                    "fn({p,fld,d,r,ext,extev,rs,rszt as (ki,ke,wi,fi,we,fe)},h,abstol,reltol,x,y,yout) => solver (update_closure_regime_rs (p, fld, d, r, ext, extev, rs, ki, ke, wi, fi, we, fe, clos), h, abstol, reltol, x, y, yout, err)" ,nll
                    "end" ,nll)
                   ("fun make_stepper (deriv: clos -> real * real array * real array -> real array) = " ,nll
                    "let " ,nll
                    "    val solver = make_c" ,solver "(n,ode_cb) " ,nll
                    "    val clos = alloc_closure(size_closure_cont_rs)" ,nll
                    "    val err  = state()" ,nll
                    "in " ,nll
                    "fn({p,fld,ext,extev,rs,rszt as (ki,ke,wi,fi,we,fe)},h,abstol,reltol,x,y,yout) => solver (update_closure_cont_rs (p, fld, ext, extev, rs, ki, ke, wi, fi, we, fe, clos), h, abstol, reltol, x, y, yout,err)" ,nll
                    "end" ,nll))
                 `(("type regime_clos = {p: real array,fld: real array,d: real array,r: bool array,ext: real array,extev: real array}" ,nll
                    "type clos = {p: real array,fld: real array,ext: real array,extev: real array}" ,nll
                    "fun make_regime_stepper (deriv: regime_clos -> real * real array * real array -> real array) = " ,nll
                    "let ",nll
                    "    val solver = make_c" ,solver "(n,ode_cb) " ,nll
                    "    val clos = alloc_closure(size_closure_regime)" ,nll
                    "    val err  = state()" ,nll
                    "in " ,nll
                    "fn({p,fld,d,r,ext,extev},h,abstol,reltol,x,y,yout) => solver (update_closure_regime (p, fld, d, r, ext, extev, clos), h, abstol, reltol, x, y, yout, err)" ,nll
                    "end" ,nll)
                   ("fun make_stepper (deriv: clos -> real * real array * real array -> real array) = " ,nll
                    "let " ,nll
                    "    val solver = make_c" ,solver "(n,ode_cb) " ,nll
                    "    val clos = alloc_closure(size_closure_cont)" ,nll
                    "    val err  = state()" ,nll
                    "in " ,nll
                    "fn({p,fld,ext,extev},h,abstol,reltol,x,y,yout) => solver (update_closure_cont (p, fld, ext, extev, clos), h, abstol, reltol, x, y, yout, err)" ,nll
                    "end" ,nll)))
           (,nll)
           ))
       '(rkdp))
      ))
)


(define (prelude/C  #!key (libs '()))
`(
 #<<EOF
typedef unsigned long long uint64_t;
#define signal_heaviside(x) (x<0.0 ? 0.0 : 1.0)


EOF
))


(define (codegen-ODE/C name sim #!key (out (current-output-port)) (libs '()))

    (let ((sysdefs (codegen-ODE sim libs: libs)))
      
      (print-fragments (prelude/C libs: libs) out)
      
      (for-each
       (match-lambda
        (('rhsfun . def)
         (let ((sexpr (binding->sexpr (B:Val name def))))
           (fmt out (c-expr sexpr))))
        (('condrhsfun . def)
         (if def
             (let ((sexpr (binding->sexpr (B:Val (string->symbol (string-append "cond" (symbol->string name))) def))))
               (fmt out (c-expr sexpr)))))
        (else (begin)))
       sysdefs)
      
      ))
