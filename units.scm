
;; Definitions for units of measurement

(define-unit-prefix    milli second ms)
(define-unit-prefix    milli volt    mV)
(define-unit-prefix    milli amp     mA)
(define-unit-prefix    pico  amp     pA)
(define-unit-prefix    nano  amp     nA)
(define-unit-prefix    micro amp     uA)
(define-unit-prefix    micro siemens uS)
(define-unit-prefix    milli siemens mS)
(define-unit-prefix    milli mole    mM)
(define-unit-prefix    mega ohm      )
(define-unit-prefix    kilo ohm      kohm)
 
(define-quantity   CurrentDensity        (/ Current Area))
(define-quantity   CapacitanceDensity    (/ Capacitance Area))
(define-quantity   ConductanceDensity    (/ Conductance Area))
(define-quantity   Resistivity           (* Resistance Length))
(define-quantity   ReactionRate1         (** Time -1))
(define-quantity   ReactionRate2         (* (** Substance -1) (** Time -1)))
(define-quantity   InversePotential      (** Potential -1))
(define-quantity   InversePotentialTime  (* (** Potential -1) (** Time -1)))

(define-unit milliamp-per-square-centimeter   CurrentDensity  (/ mA (* cm cm)) mA/cm2)
(define-unit microfarad-per-square-centimeter CapacitanceDensity (/ uF (* cm cm)) uf/cm2)
(define-unit siemens-per-square-centimeter    ConductanceDensity (/ S (* cm cm)) S/cm2)
(define-unit ohm.cm                           Resistivity     (* ohm cm) ohm.cm)

(define-unit degC    Temperature      1.0)
(define-unit /ms     ReactionRate1    1000.0)
(define-unit /mM-ms  ReactionRate2    1000000.0)
(define-unit /mV     InversePotential 1000.0)
(define-unit /mV-ms  InversePotentialTime 1000000.0)

(define model-units
  (make-parameter
   (map cons
    `(second kilogram volt ampere coulomb farad ohm siemens mole molarity hertz
      ms mV mA/cm2 pA nA uA mA mM uF uf/cm2 um S/cm2 uS mS ohm.cm ohm kohm megaohm degC /ms /mM-ms /mV /mV-ms)
    (list second kilogram volt ampere coulomb farad ohm siemens mole molarity hertz
          ms mV mA/cm2 pA nA uA mA mM uF uf/cm2 um S/cm2 uS mS ohm.cm ohm kohm megaohm degC /ms /mM-ms /mV /mV-ms))
   ))

(define model-quantities
  (make-parameter
   (map cons
    `(Time Area Volume Temperature
      Potential Current Capacitance Conductance Resistance
      CurrentDensity
      CapacitanceDensity
      ConductanceDensity
      Resistivity
      ReactionRate1
      ReactionRate2
      InversePotential
      InversePotentialTime
      )
    (list Time Area Volume Temperature
          Potential Current Capacitance Conductance Resistance
          CurrentDensity
          CapacitanceDensity
          ConductanceDensity
          Resistivity
          ReactionRate1
          ReactionRate2
          InversePotential
          InversePotentialTime
          ))
   ))


(define (eval-dim-expr expr left? env)
  (match expr 
         ((op x y)
          (let ((x1  (eval-dim-expr x #t env))
                (y1  (eval-dim-expr y #f env)))
            (case op 
              ((^) (* x1 y1))
              ((*) (+ x1 y1))
              ((/) (- x1 y1))
              (else (error 'eval-dim-expr "unknown quantity operation " op)))))
         (x
          (cond
                ((symbol? x)
                 (let ((assoc (assv x env)))
                   (if assoc
                       (eval-dim-expr (cdr assoc) left? env)
                       (error 'eval-dim-expr "unknown quantity" x))
                   ))
                ((quantity? x)
                 (quantity-int x))
                ((and (not left?) (integer? x)) x)
                ((and left? (integer? x))
                 (error 'eval-dim-expr
                        "integers are not allowed as the left operand of quantity expression" expr))
                (else (error 'eval-dim-expr "unknown quantity" x))))
         ))



(define (eval-unit-factor expr env)
  (match expr 
         ((op x y)  
          (let ((x1  (eval-unit-factor x env))
                (y1  (eval-unit-factor y env)))
            (case op 
              ((*) (* x1 y1))
              ((/) (/ x1 y1))
              (else (error 'eval-unit-factor "unknown unit factor operation " op)))))
	     
         ((op x . y) 
          (eval-unit-factor `(,op ,(eval-unit-factor `(,op ,x ,(car y)) env) ,(cdr y)) env))

         (x (cond
             ((symbol? x)    
              (let ((assoc (assv x env)))
                (if assoc
                    (eval-unit-factor (cdr assoc) env)
                    (error 'eval-unit-factor "unknown unit" x))
                ))
             ((unit? x)    
              (unit-factor x))
             ((number? x)  x)
             (else  (error 'eval-unit-factor "unknown unit" x))))

         (else (error 'eval-unit-factor "invalid unit factor expression" expr))
         ))
	 
  
(define (dim-unit-factor expr env)
  (match expr 
         ((op x y)  
          (let ((x1  (dim-unit-factor x env))
                (y1  (dim-unit-factor y env)))
            (case op 
              ((*) (+ x1 y1))
              ((/) (- x1 y1))
              (else (error 'dim-unit-factor "unknown unit factor operation " op)))))
         
         ((op x . y) 
          (dim-unit-factor `(,op ,(dim-unit-factor `(,op ,x ,(car y)) env) ,(cdr y)) env))

         
         (x (cond
             ((symbol? x)    
              (let ((assoc (assv x env)))
                (if assoc
                    (dim-unit-factor (cdr assoc) env)
                    (error 'dim-unit-factor "unknown unit" x))
                ))
             ((unit? x)    
              (quantity-int (unit-dims x)))
             ((number? x)  0)
             (else  (error 'dim-unit-factor "unknown unit " x))))

         (else (error 'dim-unit-factor "invalid unit factor expression" expr))

         ))
	 
