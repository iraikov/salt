(use extras salt)


(define ast 
  (parse 
   `((define k = parameter 1)
     (define a = unknown 0.0)
     (define b = unknown 0.0)
     (function (plus a b) a + b)
     (a = plus(a ~ k))
     ((der(b)) = a)
     )
   ))

(pp ast)

(define elab (elaborate ast))

(pp elab)


