(*
 * Definitions for fixed-step simulations of functional hybrid dynamical systems.
*)

structure SignalMath =
struct

open Real
open Math

val signal_sign = sign
val signal_eqnum = (op ==)
val signal_neg = (op ~)
val signal_add = (op +)
val signal_sub = (op -)
val signal_mul = (op *)
val signal_div = (op /)
val signal_pow = pow

val signal_sin = sin
val signal_cos = cos
val signal_cosh = cosh
val signal_tanh = tanh
                      
val signal_max = max
val signal_min = min
val signal_gt = (op >)
val signal_gte = (op >=)
val signal_lt = (op <)
val signal_lte = (op <=)

end

structure FunctionalHybridDynamics1 =
struct

open Real
open Math
open SignalMath

type regime_state  = bool vector
type dsc_state     = real vector
type event_state   = real vector
type cont_state    = real vector
type error_state   = real vector
type external_state = real vector

datatype model_state = 
         RegimeState of real * cont_state * event_state * dsc_state * regime_state * external_state * bool
         | EventState of real * cont_state * event_state * external_state * bool
         | ContState  of real * cont_state * external_state


datatype model_stepper = 
         RegimeStepper of dsc_state * regime_state -> external_state -> real -> 
                          (real * cont_state) -> cont_state
         | EventStepper of external_state -> real -> (real * cont_state) -> cont_state
         | ContStepper of external_state -> real -> (real * cont_state) -> cont_state

datatype model_condition = 
         RegimeCondition of dsc_state -> (real * cont_state * event_state * external_state) -> event_state
         | SCondition of (real * cont_state * event_state * external_state) -> event_state

datatype model_response = 
         RegimeResponse of (real * cont_state * event_state * dsc_state * external_state) -> cont_state
         | SResponse of (real * cont_state * event_state * external_state) -> cont_state

val vsub = Unsafe.Vector.sub

fun vmap2 f (v1,v2) = 
    let 
        val n = Vector.length v1
    in
        Vector.tabulate (n, fn (i) => f (vsub (v1,i), vsub (v2,i)))
    end

fun vfind2 f (v1,v2) = 
    let 
        val n = Vector.length v1
        fun recur i = 
            if Int.<(i, n)
            then (if f (vsub (v1,i), vsub (v2,i)) then SOME(i) else recur (Int.+(i,1)))
            else NONE
    in
      recur 0
    end 

fun vfoldpi2 f (v1,v2) = 
    let 
        val n = Vector.length v1
        fun recur (i, ax) = 
            if Int.<(i, n)
            then recur (Int.+(i,1), f (i, vsub (v1,i), vsub (v2,i), ax))
            else ax
    in
      recur (0, NONE)
    end 

val equal = fn (x,y) => (x = y) 

(* Fixed time step integrator *)

fun thr (v1,v2) = case (Real.sign(v1),Real.sign(v2)) of
                      (~1,1) => true
                    | (~1,0) => true
                    | _ => false


fun integral (RegimeStepper stepper,SOME (RegimeCondition fcond),                          
              SOME (RegimeResponse fpos),fneg,
              SOME fdiscrete,SOME fregime,h) =
    (fn(RegimeState (x,y,e,d,r,ext,root)) => 
        let val x'  = x + h
            val y'  = (case root of
                           true => (case fneg of 
                                        NONE => fpos(x',y,e,d,ext)
                                      | SOME (RegimeResponse f) => f (x',fpos(x',y,e,d,ext),e,d,ext)
                                      | _ => raise Domain)
                         | false => stepper (d,r) ext h (x,y))
            val e'  = fcond d (x',y',e,ext)
            val pos = vfind2 thr (e, e') 
            val r'  = fregime (e',r)
            val d'  = fdiscrete (x',y',e',d)
            val root' = (case (vfind2 thr (e, e'), vfind2 thr (e', e)) of
                             (SOME _, SOME _) => true
                           | (SOME _, NONE) => true
                           | (NONE, SOME _) => true
                           | (NONE, NONE) => false)
        in
            RegimeState(x',y',e',d',r',ext,root')
        end
    | _ => raise Domain)
               
| integral (EventStepper stepper,SOME (SCondition fcond),SOME (SResponse fpos),fneg,NONE,NONE,h) =
  (fn(EventState(x,y,e,ext,root)) => 
      let val x'  = x + h
          val y'  = (case root of
                         true => (case fneg of
                                      NONE => fpos(x',y,e,ext) 
                                    | SOME (SResponse f) => f(x',fpos(x',y,e,ext),e,ext)
                                    | _ => raise Domain)
                       | false => stepper ext h (x,y))
          val e'    = fcond (x',y',e,ext)
          val root' = (case (vfind2 thr (e, e'), vfind2 thr (e', e)) of
                           (SOME _, SOME _) => true
                         | (SOME _, NONE) => true
                         | (NONE, SOME _) => true
                         | (NONE, NONE) => false)
      in
          EventState(x',y',e',ext,root')
      end
  | _ => raise Domain)

| integral (ContStepper stepper,NONE,NONE,_,NONE,NONE,h) =
  (fn(ContState(x,y,ext)) => 
      let val x'  = x + h
          val y'  = stepper ext h (x,y)
      in
          ContState(x',y',ext)
      end
  | _ => raise Domain)

| integral _ =
  raise Domain

end

(*
 * Definitions for adaptive step simulations of functional hybrid dynamical systems.
*)

structure FunctionalHybridDynamics2 =
struct

open Real
open Math
open SignalMath

fun putStrLn str = 
    (TextIO.output (TextIO.stdOut, str);
     TextIO.output (TextIO.stdOut, "\n"))
    
fun putStr str = 
    (TextIO.output (TextIO.stdOut, str))
    
fun showReal n = 
    let open StringCvt
	open Real
    in
	(if n < 0.0 then "-" else "") ^ (fmt (FIX (SOME 12)) (abs n))
    end

type regime_state  = bool vector
type dsc_state     = real vector
type event_state   = real vector
type cont_state    = real vector
type error_state   = real vector
type external_state = real vector

datatype model_state = 
         RegimeState  of real * cont_state * event_state * dsc_state * regime_state * external_state * real * bool
         | EventState of real * cont_state * event_state * external_state * real * bool
         | ContState  of real * cont_state * external_state * real

datatype model_stepper = 
         RegimeStepper of dsc_state * regime_state -> external_state -> real -> real * cont_state -> 
                          (cont_state * error_state * (real -> cont_state))
         | EventStepper of external_state -> real -> (real * cont_state) -> 
                                   (cont_state * error_state * (real -> cont_state))
         | ContStepper of external_state -> real -> (real * cont_state) -> 
                                  (cont_state * error_state)

datatype model_condition = 
         RegimeCondition of dsc_state -> (real * cont_state * event_state * external_state) -> event_state
         | SCondition of (real * cont_state * event_state * external_state) -> event_state

datatype model_response = 
         RegimeResponse of (real * cont_state * event_state * dsc_state * external_state) -> cont_state
         | SResponse of (real * cont_state * event_state * external_state) -> cont_state


val vsub = Unsafe.Vector.sub

fun vmap2 f (v1,v2) = 
    let 
        val n = Vector.length v1
    in
        Vector.tabulate (n, fn (i) => f (vsub (v1,i), vsub (v2,i)))
    end

fun vfind2 f (v1,v2) = 
    let 
        val n = Vector.length v1
        fun recur i = 
            if Int.<(i, n)
            then (if f (vsub (v1,i), vsub (v2,i)) then SOME(i) else recur (Int.+(i,1)))
            else NONE
    in
      recur 0
    end 

fun vfoldpi2 f (v1,v2) = 
    let 
        val n = Vector.length v1
        fun recur (i, ax) = 
            if Int.<(i, n)
            then recur (Int.+(i,1), f (i, vsub (v1,i), vsub (v2,i), ax))
            else ax
    in
      recur (0, NONE)
    end 

val equal = fn (x,y) => (x = y) 

(* Adaptive time step integrator *)

val tol = Real.Math.pow (10.0, ~7.0)
val lb = 0.5 * tol
val ub = 0.9 * tol

datatype ('a, 'b) either = Left of 'a | Right of 'b

exception ConvergenceError


fun predictor tol (h,ys) =
  let open Real
      val e = Vector.foldl (fn (y,ax) => Real.+ ((abs y),ax)) 0.0 ys
  in 
      if e < lb 
      then Right (1.414*h)	(* step too small, accept but grow *)
      else (if e < ub 
            then Right h	(* step just right *)
            else Left (0.5*h))	(* step too large, reject and shrink *)
  end



fun secant tol f fg0 guess1 guess0 = 
    let open Real
        val fg1 = f guess1
        val newGuess = guess1 - fg1 * (guess1 - guess0) / (fg1 - fg0)
        val err =  abs (newGuess - guess1)
    in 
        if (err < tol)
        then newGuess
        else secant tol f fg1 newGuess guess1 
    end


datatype 'a result = Next of 'a | Root of 'a

(* 1. Ensures that the event closest in time is selected, not just the first one in the event index.
   2. Determines the direction (positive or negative) of the event. *)
fun adthreshold (i,v1,v2,ax) = 
    let
        fun ethr0 (i, v1, v2) =
            (case (Real.sign(v1),Real.sign(v2)) of
                 (~1, 1) => SOME (1, i, v1-v2)
               | (~1, 0) => SOME (1, i, v1-v2)
               | _ => NONE)
    in
        case ax of 
            NONE => ethr0 (i,v1,v2)
          | SOME (dir,index,diff) =>
            case ethr0 (i, v1, v2) of
                NONE => ax
              | SOME (dir',index',diff') =>
                if abs(diff') < abs(diff)
                then SOME (dir',index',diff')
                else ax
    end


fun adaptive_regime_solver (stepper,fcond,fdiscrete,fregime)  =
    let open Real
        fun f (x,ys,ev,d,r,ext,h) =
            (let
                val (ys',e,finterp) = (stepper (d,r)) ext h (x,ys)
                val ev' = fcond d (x+h,ys',ev,ext)
            in
                case predictor tol (h,e) of
                    Right h' => 
                    (case vfoldpi2 adthreshold (ev, ev') of
                         SOME (evdir,evind,evdiff) => 
                         (let
                             fun fy (theta) = Vector.sub(fcond d (x+theta*h, finterp theta, ev, ext), evind)
                             val y0    = Vector.sub(fcond d (x,ys,ev,ext), evind)
                             val theta = secant tol fy y0 1.0 0.0
                             val x'    = x+(theta+tol)*h
                             val ys''  = finterp (theta+tol)
                             val ev''  = fcond d (x',ys'',ev,ext)
                             val d'    = fdiscrete (x',ys'',ev'',d)
                             val r'    = fregime (ev'',r)
                         in
                             Root (x',ys'',evdir,ev'',d',r',ext,h')
                         end)
                       | NONE => 
                         Next (x+h,ys',0,ev',d,r,ext,h'))
                  | Left h'  => 
                    f (x,ys,ev,d,r,ext,h')
            end)
    in
        f
    end


fun adaptive_event_solver (stepper,fcond)  =
    let open Real
        fun f (x,ys,ev,ext,h) =
            (let
                val (ys',e,finterp) = stepper ext h (x,ys)
                val ev' = fcond (x+h,ys',ev,ext)
            in
                case predictor tol (h,e) of
                    Right h' => 
                    (case vfoldpi2 adthreshold (ev, ev') of
                         SOME (evdir,evind,evdiff) => 
                         (let
                             fun fy (theta) = Vector.sub(fcond (x+theta*h, finterp theta, ev, ext), evind)
                             val y0    = Vector.sub(fcond (x,ys,ev,ext), evind)
                             val theta = secant tol fy y0 1.0 0.0
                             val x'    = x+(theta+tol)*h
                             val ys''  = finterp (theta+tol)
                             val ev''  = fcond (x',ys'',ev,ext)
                         in
                             Root (x',ys'',evdir,ev'',ext,h')
                         end)
                       | NONE => 
                         Next (x+h,ys',0,ev',ext,h'))
                  | Left h'  => 
                    f (x,ys,ev,ext,h')
            end)
    in
        f
    end


fun adaptive_solver stepper  =
    let open Real
        fun f (x,ys,ext,h) =
            (let
                val (ys',e) = stepper ext h (x,ys)
            in
                case predictor tol (h,e) of
                    Right h' => 
                    Next (x+h,ys',ext,h')
                  | Left h'  => 
                    f (x,ys,ext,h')
            end)
    in
        f
    end


fun integral (RegimeStepper fstepper,SOME (RegimeCondition fcond),
               SOME (RegimeResponse fpos),fneg,
               SOME fdiscrete,SOME fregime) =
    let
       (*fun fstepper (d,r) = make_stepper (f(d,r))*)
       val fsolver = adaptive_regime_solver (fstepper,fcond,fdiscrete,fregime)
    in
        fn(RegimeState (x,y,e,d,r,ext,h,root)) => 
           (case root of
                true =>
                (let
                    val y' = case fneg of 
                                 SOME (RegimeResponse f) => 
                                 f(x,fpos(x,y,e,d,ext),e,d,ext)
                               | NONE => 
                                 fpos(x,y,e,ext,d)
                               | SOME _ => raise Domain
                in
                    RegimeState (x,y',e,d,r,ext,h,false) 
                end)
             | false =>  
                case fsolver (x,y,e,d,r,ext,h) of
                    Next (xn,ysn,_,evn,dn,rn,_,hn) => 
                    RegimeState (xn,ysn,evn,dn,rn,ext,hn,false)
                  | Root (xn,ysn,_,evn,dn,rn,ext,hn) => 
                    RegimeState (xn,ysn,evn,dn,rn,ext,hn,true))                          
      | _ => raise Domain
    end

  | integral (EventStepper fstepper,SOME (SCondition fcond),
               SOME (SResponse fpos),fneg,
               NONE,NONE) =
    let
       (*fun fstepper (d,r) = make_stepper (f(d,r))*)
       val fsolver = adaptive_event_solver (fstepper,fcond)
    in
        fn(EventState (x,y,e,ext,h,root)) => 
           (case root of
                true => (let
                            val y' = case fneg of 
                                         SOME (SResponse f) => 
                                         f(x,fpos(x,y,e,ext),e,ext)
                                       | NONE => 
                                         fpos(x,y,e,ext)
                                       | SOME _ => raise Domain
                            val  e' = fcond(x,y',e,ext)
                        in
                            EventState (x,y',e',ext,h,false)
                        end)
               | false => 
                 (case fsolver (x,y,e,ext,h) of
                      Next (xn,ysn,_,evn,ext,hn) => 
                      EventState (xn,ysn,evn,ext,hn,false)
                    | Root (xn,ysn,_,evn,ext,hn) => 
                      EventState (xn,ysn,evn,ext,hn,true)))
      | _ => raise Domain
    end

  | integral (ContStepper fstepper,NONE,NONE,NONE,NONE,NONE) =
    let
       (*fun fstepper (d,r) = make_stepper (f(d,r))*)
       val fsolver = adaptive_solver fstepper
    in
        fn(ContState (x,y,ext,h)) => 
           (case fsolver (x,y,ext,h) of
                Next (xn,ysn,_,hn) => 
                ContState (xn,ysn,ext,hn)
              | Root (xn,ysn,_,hn) => 
                ContState (xn,ysn,ext,hn))
      | _ => raise Domain
    end

| integral _ =
  raise Domain

end
