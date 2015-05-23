(use extras salt)


(define vdp 
  (parse 
   `(
     (define x = unknown 0.0)
     (define y = unknown 1.0)
     ((der(x)) = (1 - y ^ 2) * x - y )
     ((der(y)) = x)
     )
   ))

(pp vdp)

(define elab (elaborate vdp))
(define sim (simcreate elab))

(pp elab)
(pp sim)
(pp (codegen sim))
(codegen/ML sim)



