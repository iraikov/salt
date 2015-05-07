
(let* (
      (Isyn (parameter 'ISyn 20.0))
      (gL   (parameter 'gL 0.2))
      (vL   (parameter 'vL -70.0))
      (C    (parameter 'C 1.0))
      (theta  (parameter 'theta 25.0))
      (vreset (parameter 'vreset -65.0))

      (v  (unknown (expr-value vreset) 'v))
     )

C * der(v) = ( ((- gL) * (v - vL)) + Isyn)

ev(v-theta
   reinit(v vreset)

