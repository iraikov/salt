(use extras salt)

(define (test-model name model #!key (solver 'rk4b))
  (pp model)

  (define elab (elaborate model))
  (define sim (simcreate elab))
  
  (pp elab)
  (pp sim)
  (pp (codegen-ODE sim))
  (let ((port (open-output-file (string-append (->string name) ".sml"))))
    (codegen-ODE/ML sim out: port solver: solver)
    (close-output-port port))

)

(verbose 1)

(define vdp 
  (parse 
   `(
     (define x = unknown -0.25)
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


(define iafrefr
  (parse 
   `(
     (define Isyn = parameter 20.0)
     (define gL   = parameter 0.2)
     (define vL   = parameter -70.0)
     (define C    = parameter 1.0)
     (define theta  = parameter 25.0)
     (define vreset = parameter -65.0)
     (define trefractory = parameter (dim Time) 5.0 * ms)

     (define v = unknown vreset)
     (define trefr = discrete (dim Time) 0.0 * ms)

     (structural-event subthreshold 
      (
       ((der(v)) = ( ((- gL) * (v - vL)) + Isyn) / C)
       )
      (refractory (v - theta)
       ( 
        (v := vreset)
        (trefr := t + trefractory)
        )
       ))

      (structural-event refractory
        (
         ((der(v)) = 0.0)
        )
        (subthreshold (t - trefr) ())
        )
     )
   ))



(define izhfs 
  (parse 
   `(
     (define Isyn = parameter (dim Current) 0.0 * nA)
     (define Iext = parameter (dim Current) 400.0 * nA)

     (define k     = parameter 1.0)
     (define Vinit = parameter (dim Potential)  -65.0 * mV)
     (define Vpeak = parameter (dim Potential)   25.0 * mV)
     (define Vt    = parameter (dim Potential)  -55.0 * mV)
     (define Vr    = parameter (dim Potential)  -40.0 * mV)
     (define Vb    = parameter (dim Potential)  -55.0 * mV)
     (define Cm    = parameter (dim Capacitance) 20.0 * uF)

     (define FS_a = parameter  0.2)
     (define FS_b = parameter  (dim Current)   0.025 * nA)
     (define FS_c = parameter  (dim Potential) -45.0 * mV)
     (define FS_U = parameter  (dim Current) FS_b * (Vinit / mV))

     (define v  = unknown (dim Potential) -60.899 * mV)
     (define u  = unknown (dim Current) FS_U)
     (define s  = unknown (dim Current) 0.0 * nA)

     ((der(v)) = (((k * (v - Vr) * (v - Vt)) / mV + ((- u) + Iext) * megaohm) / Cm) / megaohm)
     ((der(u)) = (FS_a * (s - u)) / ms)
     ((s) = FS_b * ((v - Vb) / mV) ^ 3)


     (event (v - Vpeak)
            ((v := FS_c)
             (u := u)
             (s := s))
            )
     ))
  )


(define iaf0
  (parse
   `(
     (define gL   = parameter 0.2)
     (define vL   = parameter -70.0)
     (define C    = parameter 1.0)

     ((der(v)) = ( ((- gL) * (v - vL)) + Isyn) / C)

     (event (v - theta)
            ((v := vreset))
            )

     )
   ))


(define alphasyn 
  (parse
  `(

     (define vsyn  = parameter 80.0)
     (define alpha = parameter 1.0)
     (define beta  = parameter 0.25)
     (define gsmax = parameter 0.1)
     (define taus  = parameter 2.5)
     (define f     = parameter -100.0)
     (define s0    = parameter 0.5)

     (define S  = unknown 0.0)
     (define SS = unknown 0.0)

     (define gsyn  = unknown 0.0)

     ((der (S)) = (alpha * (1 - S) - beta * S))
     ((der (SS)) = ((s0 - SS) / taus))
        
     ((Isyn) = (gsyn * (v - vsyn)))
     ((gsyn) = (gsmax * S * SS))

     (event (v - theta)
            (
             (S := 0)
             (SS := 0))
            )

     (event (grid_input (t))
            (
             (S := S)
             (SS := (SS + f * (1 - SS)))
            )
            )
     )
))


(define iafsyn
  (parse
   `(
     (define theta = parameter 25.0)
     (define Isyn  = unknown 0.0)
     
     (define v     = unknown -35.0)
     
     ,iaf0
     ,alphasyn
     ))
  )



(define ml 
  (parse
  `(
    (define Istim =  parameter 50.0)
    (define c     =  parameter 20.0)
    (define vk    =  parameter -70.0)
    (define vl    =  parameter -50.0)
    (define vca   =  parameter 100.0)
    (define gk    =  parameter 8.0)
    (define gl    =  parameter 2.0)
    (define gca   =  parameter 4.0)
    (define v1    =  parameter -1.0)
    (define v2    =  parameter 15.0)
    (define v3    =  parameter 10.0)
    (define v4    =  parameter 14.5)
    (define phi   =  parameter 0.0667)

    
    (define v   = unknown -60.899)
                     
    (fun (minf v) = 0.5 * (1.0 + tanh ((v - v1) / v2)))
    (fun (winf v) = 0.5 * (1.0 + tanh ((v - v3) / v4)))
    (fun (lamw v) = phi * cosh ((v - v3) / (2.0 * v4)))

    (define w   = unknown 0.0149)
    (define ica = unknown 0.0)
    (define ik  = unknown 0.0)

    ((der (v)) = (Istim + (gl * (vl - v)) + ica + ik) / c )
    ((der (w)) = lamw (v) * (winf(v) - w))
    ((ica)    = gca * (minf (v) * (vca - v)))
    ((ik)     = gk * (w * (vk - v)))
    )
  ))



;(test-model 'vdp vdp)

;(test-model 'iaf iaf)

;(test-model 'izhfs izhfs solver: 'rkdp)

;(test-model 'iafrefr iafrefr solver: 'rkoz)

(test-model 'ml ml solver: 'rkdp)

;(test-model 'iafsyn iafsyn)




