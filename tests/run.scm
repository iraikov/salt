(use setup-api extras salt)

(define (installation-chicken-home)
  (if (not (installation-prefix)) (chicken-home)
    (make-pathname `(,(installation-prefix) "share") "chicken") ) )


(define SHARED-DIR (installation-chicken-home))
(define SALT-DIR (make-pathname SHARED-DIR "salt"))


(define (run:execute* explist)
  (define (smooth lst)
    (let ((slst (map ->string lst)))
      (string-intersperse (cons (car slst) (cdr slst)) " ")))
  (for-each (lambda (cmd)
	      (printf "  ~A~%~!" cmd)
	      (system* "~a" cmd))
	    (map smooth explist)))


(define-syntax run
  (syntax-rules ()
    ((_ exp ...)
     (run:execute* (list `exp ...)))))

(define adaptive-solvers '(rkhe rkbs rkck rkoz rkdp rkf45 rkf78 rkv65 crkdp crkbs))

(define (test-model name model #!key (solver 'rk4b) (compile #f) (dir "tests"))
  (pp model)

  (define elab (elaborate model))
  (print "elaborate is done")
  (pp elab)

  (define sim (simcreate elab))
  (pp sim)
  (pp (codegen-ODE sim))
  (let* (
         (c-path (make-pathname dir (string-append (->string name) ".c")))
         (c-port (open-output-file c-path))
         (sml-path (make-pathname dir (string-append (->string name) ".sml")))
         (mlb-path (if (member solver adaptive-solvers)
                     (make-pathname dir (string-append (->string name) "_run2.mlb"))
                     (make-pathname dir (string-append (->string name) "_run1.mlb"))))
         (sml-port (open-output-file sml-path))
         )
    (codegen-ODE/C name sim out: c-port solver: solver libs: '(interp))
    (codegen-ODE/ML name sim out: sml-port solver: solver libs: '(interp))
    (close-output-port sml-port)
    (close-output-port c-port)
    (if compile
        (run (mlton 
              -profile alloc
              -default-ann "'allowFFI true'"
              -mlb-path-var ,(sprintf "'SALT_LIB ~A/sml-lib'" SALT-DIR)
              -mlb-path-var ,(sprintf "'RK_LIB $(SALT_LIB)/rk'")
              -mlb-path-var ,(sprintf "'DYNAMICS_LIB $(SALT_LIB)/dynamics'")
              ,mlb-path
              ,@(case solver 
                 ((crk3) (list c-path (make-pathname SALT-DIR "/sml-lib/rk/crk3.c")))
                 ((crkbs) (list c-path (make-pathname SALT-DIR "/sml-lib/rk/crkbs.c")))
                 ((crkdp) (list c-path (make-pathname SALT-DIR "/sml-lib/rk/crkdp.c")))
                 (else '()))
              ))))

)

;(verbose 1)

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
     (define millivolt = unit Potential (1e-3 * volt))

     (define Isyn = parameter (dim Current) 0.0 * nA)
     (define Iext = parameter (dim Current) 400.0 * nA)

     (define k     = parameter 1.0)
     (define Vinit = parameter (dim Potential)  -65.0 * millivolt)
     (define Vpeak = parameter (dim Potential)   25.0 * mV)
     (define Vt    = parameter (dim Potential)  -55.0 * mV)
     (define Vr    = parameter (dim Potential)  -40.0 * mV)
     (define Vb    = parameter (dim Potential)  -55.0 * mV)
     (define Cm    = parameter (dim Capacitance) 20.0 * uF)

     (define FS_a = parameter  0.2)
     (define FS_b = parameter  (dim Current)   0.025 * nA)
     (define FS_c = parameter  (dim Potential) -45.0 * mV)
     (define FS_U = parameter  (dim Current) FS_b * (Vinit / mV))

     (define v  = unknown (dim Potential) -65.0 * mV)
     (define u  = unknown (dim Current) FS_U)
     (define s  = unknown (dim Current) 0.0 * nA)

     ((der(v)) = (((k * (v - Vr) * (v - Vt) / millivolt) + (((- u) + Iext) * megaohm)) / Cm) / megaohm)
     ((der(u)) = (FS_a * (s - u)) / ms)
     ((s) = FS_b * ((v - Vb) / mV) ^ 3)


     (event (v - Vpeak)
            ((v := FS_c)
             (u := u)
             (s := s)
             )
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

     (define grid_input = external 0.0)

     ((der (S)) = (alpha * (1 - S) - beta * S))
     ((der (SS)) = ((s0 - SS) / taus))
        
     ((reduce (+ Isyn)) = (gsyn * (v - vsyn)))
     ((gsyn) = (gsmax * S * SS))

     (event (v - theta)
            (
             (S := 0)
             (SS := 0))
            )

     (event (grid_input)
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



(define adex 
  (parse
  `(
    (define Isyn  =  parameter 210.0)

    (define C     = parameter 200.0)
    (define gL    = parameter 10.0)
    (define EL    = parameter -58.0)
    (define VT    = parameter -50.0)
    (define Delta = parameter 2.0)
    (define theta = parameter -45.0)
    (define trefractory = parameter 0.25)
              
    (define a = parameter 2.0)
    (define b = parameter 100.0)
    (define tau_w = parameter 120.0)
              
    (define Vr = parameter -46.0)

    (define V = unknown Vr)
    (define W = unknown Vr)

    ((der (V)) = (( ((- gL) * (V - EL)) + (gL * Delta * (exp ((V - VT) / Delta))) + (- W) + Isyn) / C))
    ((der (W)) = (((a * (V - EL)) - W) / tau_w))

    (event (V - theta)
           ((V := Vr))
           ((W := W + b))
           )
    ))
  )


(define hr 
  (parse
  `(

    (define I = parameter 0.5)
                       
    (define a = parameter 1.0)
    (define b = parameter 3.0)
    (define c = parameter 1.0)
    (define d = parameter 5.0)

    (define r = parameter 1e-3)
    (define s = parameter 4.0)
    (define xr = parameter -8 / 5)

    (fun (phi x) = - a * x ^ 3 + b * x ^ 2)
    (fun (psi x) = c - d * x ^ 2)

    (define x = unknown -1.0)
    (define y = unknown 0.0)
    (define z = unknown 0.0)
    
    ((der (x)) = y + phi(x) - z + I)
    ((der (y)) = psi(x) - y)
    ((der(z))  = r * (s * (x - xr) - z))
    
    )
  ))


;; (test-model 'vdp vdp solver: 'rk4b compile: #t)
;; (test-model 'vdp vdp solver: 'crkdp compile: #t)

;; (test-model 'ml ml solver: 'rk4b compile: #t)
;; (test-model 'ml ml solver: 'crkdp compile: #t)

;; (test-model 'iaf iaf solver: 'rk3 compile: #t)
;; (test-model 'iaf iaf solver: 'crkdp compile: #t)

;; (test-model 'izhfs izhfs solver: 'rk4b compile: #t)
;; (test-model 'izhfs izhfs solver: 'crkdp compile: #t)

;; (test-model 'iafrefr iafrefr solver: 'rk3  compile: #t)
;; (test-model 'iafrefr iafrefr solver: 'crkdp  compile: #t)

;; (test-model 'adex adex solver: 'rk3  compile: #t)
;; (test-model 'adex adex solver: 'crkdp  compile: #t)

(test-model 'hr hr solver: 'rk3  compile: #t)
(test-model 'hr hr solver: 'crkdp  compile: #t)

;(test-model 'iafsyn iafsyn)





