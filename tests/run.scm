(use extras salt)

(define (test-model model)
  (pp model)

  (define elab (elaborate model))
  (define sim (simcreate elab))
  
  (pp elab)
  (pp sim)
  (pp (codegen sim))
  (codegen/ML sim)

)


(define vdp 
  (parse 
   `(
     (define x = unknown 0.0)
     (define y = unknown 1.0)
     ((der(x)) = (1 - y ^ 2) * x - y )
     ((der(y)) = x)
     )
   ))


(define iaf 
  (parse 
   `(
     (define Isyn = parameter 20.0)
     (define gL   = parameter 0.2)
     (define vL   = parameter -70.0)
     (define C    = parameter 1.0)
     (define theta  = parameter 25.0)
     (define vreset = parameter -65.0)

     (define v = unknown vreset)

     ((der(v)) = ( ((- gL) * (v - vL)) + Isyn) / C)

     (event (v - theta)
            ((v := vreset))
            )

     )
   ))


;(test-model vdp)

(test-model iaf)




