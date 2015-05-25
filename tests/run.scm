(use extras salt)

(define (test-model model)
  (pp model)

  (define elab (elaborate model))
  (define sim (simcreate elab))
  
  (pp elab)
  (pp sim)
  (pp (codegen-ODE sim))
  (codegen-ODE/ML sim)

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



(define izhfs 
  (parse 
   `(
     (define Isyn = parameter 0.0)
     (define Iext = parameter 400.0)

     (define k     = parameter 1.0)
     (define Vinit = parameter -65.0)
     (define Vpeak = parameter  25.0)
     (define Vt    = parameter -55.0)
     (define Vr    = parameter -40.0)
     (define Vb    = parameter -55.0)
     (define Cm    = parameter  20.0)

     (define FS_a = parameter  0.2)
     (define FS_b = parameter  0.025)
     (define FS_c = parameter  -45.0)
     (define FS_U = parameter  FS_b * -65.0)

     (define v  = unknown -60.899)
     (define u  = unknown FS_U)
     (define s  = unknown 0.0)

     ((der(v)) = ((k * (v - Vr) * (v - Vt)) + (- u) + Iext) / Cm)
     ((der(u)) = FS_a * (s - u))
     ((s) = FS_b * (v - Vb) ^ 3)


     (event (v - Vpeak)
            ((v := FS_c)
             (u := u)
             (s := s))
            )
     ))
  )


;(test-model vdp)

;(test-model iaf)

(test-model izhfs)




