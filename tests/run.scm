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

(define (test-model name model #!key (solver 'rkdp) (compile #f) (dir "tests"))
  (pp model)

  (define elab (elaborate model))
  (print "elaborate is done")
  (pp elab)

  (define sim (simcreate elab))
  (pp sim)
  (pp (codegen-ODE sim))
  (let* (
         (sml-path (make-pathname dir (string-append (->string name) ".sml")))
         (mlb-path (make-pathname dir (string-append (->string name) "_run.mlb")))
         (sml-port (open-output-file sml-path))
         )
    (codegen-ODE/ML name sim out: sml-port libs: '() solver: solver)
    (close-output-port sml-port)
    (if compile
        (run (mlton 
              -profile alloc
              ;-const "'Exn.keepHistory true'"
              -default-ann "'allowFFI true'"
              -mlb-path-var ,(sprintf "'SALT_HOME ~A'" SALT-DIR)
              -mlb-path-var "'SALT_LIB $(SALT_HOME)/sml-lib'"
              -mlb-path-map ,(sprintf "~A/sml-lib/mlb-path-map" SALT-DIR)
              ,mlb-path
              ))))

  )

(define (test-model/c name model #!key (solver 'rkdp) (dir "tests"))
  (pp model)

  (define elab (elaborate model))
  (print "elaborate is done")
  (pp elab)

  (define sim (simcreate elab))
  (pp sim)
  (pp (codegen-ODE sim))
  (let* (
         (c-path   (make-pathname dir (string-append (->string name) ".c")))
         (c-port   (open-output-file c-path))
         (sml-path (make-pathname dir (string-append (->string name) ".sml")))
         (mlb-path (make-pathname dir (string-append (->string name) "_run.mlb")))
         (sml-port (open-output-file sml-path))
         )
    (codegen-ODE/C name sim out: c-port libs: '())
    (codegen-ODE/ML name sim out: sml-port libs: '() solver: solver csysname: (->string name))
    (close-output-port sml-port)
    (close-output-port c-port)
    (run (mlton 
          ;-profile alloc
          -const "'Exn.keepHistory true'"
          -default-ann "'allowFFI true'"
          -mlb-path-var ,(sprintf "'SALT_HOME ~A'" SALT-DIR)
          -mlb-path-var "'SALT_LIB $(SALT_HOME)/sml-lib'"
          -mlb-path-map ,(sprintf "~A/sml-lib/mlb-path-map" SALT-DIR)
          -output ,(sprintf "~A/~A_run_c" dir name)
          ,mlb-path
          ,(sprintf "~A/sml-lib/rk/crklib.c" SALT-DIR)
          ,c-path
          ))
    ))


;(verbose 1)
;(add-trace 'codegen-ODE)

;; Van der Pol oscillator
(define vdp 
  (parse 
   `(
     (define x = unknown -0.25)
     (define y = unknown 1.0)
     ((der(x)) = (1 - y ^ 2) * x - y )
     ((der(y)) = x)
     )
   ))

;; Leaky integrate-and-fire neuron
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


;; Leaky integrate-and-fire neuron with refractory period
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


;; Izhikevich spiking neuron model 
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
     (define vreset = parameter -65.0)

     ((der(v)) = ( ((- gL) * (v - vL))) / C)

     (event (v - theta)
            ((v := vreset))
            )

     )
   ))


(define iaf1
  (parse
   `(
     (define gL   = parameter 0.2)
     (define vL   = parameter -70.0)
     (define C    = parameter 1.0)
     (define vreset = parameter -65.0)

     ((der(v)) = ( ((- gL) * (v - vL)) + Isyn) / C)

     (event (v - theta)
            ((v := vreset))
            )

     )
   ))


;; Alpha function synapse
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
     (define gain  = parameter 1.0)

     (define S  = unknown 0.0)
     (define SS = unknown 0.0)

     (define gsyn  = unknown 0.0)

     (define grid_input = external 0.0)
     (define grid_ev = external-event 0.0)

     ((der (S)) = (alpha * (1 - S) - beta * S))
     ((der (SS)) = ((s0 - SS) / taus))
        
     ((reduce (+ Isyn)) = (gsyn * (v - vsyn)))
     ((gsyn) = (gsmax * S * SS))

     (event (v - theta)
            (
             (S := 0)
             (SS := 0))
            )

     (event (grid_ev)
            (
             (S := S + grid_input)
             (SS := (SS + f * (1 - SS)))
            )
            )
     )
))


;; Delta function synapse
(define deltasyn 
  (parse
  `(

     (define w  = parameter 0.2)

     (define grid_input = external 0.0)
     (define grid_ev = external-event 0.0)

     (event (grid_ev)
            (
             (v := v + grid_input * w)
            )
            )
     )
))


;; Integrate and fire neuron with delta synapse
(define iafdelta
  (parse
   `(
     (define theta = parameter 25.0)
     
     (define v     = unknown -35.0)
     
     ,iaf0
     ,deltasyn
     ))
  )

;; Integrate and fire neuron with alpha synapse
(define iafalpha
  (parse
   `(
     (define theta = parameter 25.0)
     (define Isyn  = unknown 0.0)
     
     (define v     = unknown -35.0)
     
     ,iaf1
     ,alphasyn
     ,alphasyn
     ))
  )


;; Morris-Lecar model
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


;; Adaptive exponential integrate-and-fire neuron model
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

;; Hindmarsh-Rose neuron model
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



;; Wang, X.-J. and Buzsaki G. (1996) Gamma oscillations by synaptic
;; inhibition in a hippocampal interneuronal network.
;; J. Neurosci. 16, 6402-6413.
(define wb
  (parse
  `(

    (fun (am v) = 0.1 * (v + 35.0) / (1.0 - exp(- (v + 35.0) / 10.0)))
    (fun (bm v) = 4.0 * exp( - (v + 60.0) / 18.0))
    (fun (minf v) = am(v) / (am(v) + bm(v)))

    (fun (ah v) = 0.07 * exp(- (v + 58.0) / 20.0))
    (fun (bh v) = 1.0 / (1.0 + exp( - (v + 28.0) / 10.0)))

    (fun (an v) = 0.01 * (v + 34.0) / (1.0 - exp(- (v + 34.0) / 10.0)))
    (fun (bn v) = 0.125 * exp(- (v + 44.0) / 80.0))


    (define I = parameter 1.0)

    (define diam = parameter 5.6419)
    (define L = parameter 5.6419)

    (define C_m = parameter 1)

    (define gbar_Na = parameter 0.035)
    (define gbar_K  = parameter 0.009)
    (define g_L     = parameter 0.0001)

    (define E_Na = parameter 55)
    (define E_K  = parameter -90)
    (define E_L  = parameter -65)
              
    (define v = unknown -20.0) ;; mV

    (define area = parameter PI * L * diam)
    
    (define h   = unknown 0.283)
    (define n   = unknown 0.265)

    (define I_Na = unknown 0.0) ;; Current
    (define I_K  = unknown 0.0) ;; Current
    (define I_L  = unknown 0.0) ;; Current

    (define g_Na = unknown 0.0) ;; Conductance
    (define g_K  = unknown 0.0) ;; Conductance
    
    ((der (v)) = ((I * (100.0 / area)) - 1e3 * (I_K + I_Na + I_L)) / C_m)

    ((der (n)) = 5.0 * (an(v) * (1 - n) - bn(v) * n))
    ((der (h)) = 5.0 * (ah(v) * (1 - h) - bh(v) * h))

    ((g_Na) = minf(v) ^ 3 * h * gbar_Na)
    ((g_K)  = n ^ 4 * gbar_K)
    
    ((I_Na) = g_Na * (v - E_Na))
    ((I_K)  = g_K  * (v - E_K))
    ((I_L)  = g_L  * (v - E_L))
    
    ))
  )

;; Intrinsic and Network Rhythmogenesis in a Reduced Traub Model for CA3 Neurons.
;; J Comput Neurosci. 1994 Jun;1(1-2):39-60.
;; Pinsky PF, Rinzel J.

(define (pr_Soma V I)
  (parse
  `(
    (define INa  = unknown 0.0) ;; Current
    (define IKdr = unknown 0.0) ;; Current
    
    (define h   = unknown 0.25)
    (define n   = unknown 0.2)

    (define gLs   = parameter 0.1)
    (define gNa   = parameter 30)
    (define gKdr  = parameter 15)

    (fun (alphans v) = 0.016 * (-24.9 - v) / (exp((-24.9 - v) / 5.0) - 1.0))
    (fun (betans v) = 0.25 * exp(-1.0 - (0.025 * v)))

    (fun (alphahs v) = 0.128 * exp((-43.0 - v) / 18.0))
    (fun (betahs v) = 4.0 / (1.0 + exp((-20.0 - v) / 5.0)))

    (fun (alphams v) = 0.32 * (-46.9 - v) / (exp((-46.9 - v) / 4.0) - 1.0))
    (fun (betams v) = 0.28 * (v + 19.9) / (exp((v + 19.9) / 5.0) - 1.0))
    (fun (Minfs v) = alphams(v) / (alphams(v) + betams(v)))

    ((der (,V)) = (((- gLs) * (,V - VL)) - INa - IKdr + ,I + (J / As)) / Cm)
    ((der (h)) = alphahs(,V) - (alphahs(,V) + betahs(,V)) * h)
    ((der (n)) = alphans(,V) - (alphans(,V) + betans(,V)) * n)

    (INa = gNa * (Minfs(,V) ^ 2) * h * (,V - VNa))
    (IKdr = gKdr * n * (,V - VK))
    )
  ))

(define (pr_Dendrite V I)
  (parse
   `(
     
    (define ICad  = unknown 0.0)
    (define IKahp = unknown 0.0);; Current
    (define IK    = unknown 0.0);; Current
    
    (define s     = unknown 0.01)
    (define c     = unknown 0.08)
    (define q     = unknown 0.01)

    (define Cad   = unknown 1e-6)
    
    (define chid  = unknown 0.0)
    (define alphaqd = unknown 0.0)

    (define betaqd =  parameter 0.001)
    (define gLd   = parameter 0.1)
    (define gCa   = parameter 10)
    (define gKahp = parameter 0.8)
    (define gKC   = parameter 15)
    (define VCa   = parameter 80)

    (fun (alphasd v) = 1.6 / (1.0 + exp(-0.072 * (v - 5.0))))
    (fun (betasd v) = 0.02 * (v + 8.9) / (exp((v + 8.9) / 5.0) - 1.0))
    (fun (heav x) = if (x > 0.0) then 1.0 else 0.0)

    (fun (alphacd v) = (1.0 - heav(v + 10.0)) * exp(((v + 50.0) / 11) - ((v + 53.5) / 27)) / 18.975 + (heav(v + 10.0) * 2.0 * exp((-53.5 - v) / 27.0)))
    (fun (betacd v) = (1.0 - heav(v + 10.0)) * ((2.0 * exp((-53.5 - v) / 27.0)) - alphacd(v)))

    ((der (,V))  = (((- gLd) * (,V - VL)) - ICad - IKahp - IK + ,I) / Cm)
    ((der (s))  = alphasd(,V) - (alphasd(,V) + betasd(,V)) * s)
    ((der (c))  = alphacd(,V) - (alphacd(,V) + betacd(,V)) * c)
    ((der (q))  = alphaqd - (alphaqd + betaqd) * q)
    ((der (Cad)) = (-0.13 * ICad) - (0.075 * Cad))
    
    (ICad     = gCa * s * s * (,V - VCa))
    (IKahp    = gKahp * q * (,V - VK))
    (IK       = gKC * c * chid * (,V - VK))
    (alphaqd  = min((0.00002 * Cad) ~ 0.01))
    (chid     = min((Cad / 250.0) ~ 1.0))
    ))
  )

(define (Connect I g n1 n2)
  `(,I = (,n2 - ,n1) / ,g))


(define pr
  (parse
   `(
     (define J     = parameter 0.75)
     (define VL    = parameter -60)
     (define VNa   = parameter 60)
     (define VK    = parameter -75)
     (define Vsyn  = parameter 0.0)


     (define gc = parameter 2.1)
     (define As = parameter 0.5)
     (define Ad = parameter 1.0 - As)
     (define Cm = parameter 3)

     (define Is  = unknown 0.0) ;Current
     (define Id  = unknown 0.0) ;Current
     (define Vs  = unknown -60.0) ;Voltage
     (define Vd  = unknown -60.0) ;Voltage

     ,(pr_Soma 'Vs 'Is)
     ,(pr_Dendrite 'Vd 'Id)
     ,(Connect 'Is '(As / gc) 'Vs 'Vd)
     ,(Connect 'Id '(Ad / gc) 'Vd 'Vs)
     ))
  )



(test-model/c 'vdp vdp)

(test-model/c 'ml ml)

(test-model/c 'iaf iaf)

(test-model/c 'iafrefr iafrefr)

(test-model/c 'izhfs izhfs)

(test-model 'vdp vdp compile: #t)

(test-model 'ml ml compile: #t)

(test-model 'izhfs izhfs compile: #t)

(test-model 'iaf iaf compile: #t)

(test-model 'iafrefr iafrefr  compile: #t)

(test-model 'adex adex compile: #t);

(test-model 'hr hr compile: #t)

(test-model 'wb wb compile: #t)

(test-model 'iafalpha iafalpha)

(test-model 'iafdelta iafdelta)

(test-model 'pr pr compile: #t)


