(*
 * Definitions for simulations of functional hybrid dynamical systems.
*)

structure FunctionalHybridDynamics =
struct

open Real
open Math

datatype model_state = 
         RegimeState of real * real vector * real vector * real vector * bool vector
         | EventState of real * real vector * real vector
         | ContState of real * real vector

datatype model_solver = 
         RegimeSolver of (real vector * bool vector) -> (real * real vector) -> real -> real vector
         | ContSolver of (real * real vector) -> real -> real vector

datatype model_condition = 
         RegimeCondition of real vector -> (real * real vector * real vector) -> real vector
         | SCondition of (real * real vector * real vector) -> real vector

datatype model_response = 
         RegimeResponse of (real * real vector * real vector * real vector) -> real vector
         | SResponse of (real * real vector * real vector) -> real vector


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

val signal_sign = sign
val signal_eqnum = (op ==)
val signal_neg = (op ~)
val signal_add = (op +)
val signal_sub = (op -)
val signal_mul = (op *)
val signal_div = (op /)
val signal_pow = pow
val signal_max = max
val signal_min = min
val signal_gt = (op >)
val signal_gte = (op >=)

(* Fixed time step integrator *)

fun thr (v1,v2) = case (Real.sign(v1),Real.sign(v2)) of
                      (~1,1) => true
                    | (~1,0) => true
                    | _ => false


fun integral1 (RegimeSolver solver,SOME (RegimeCondition fcond),
               SOME (RegimeResponse fpos),fneg,
               SOME fdiscrete,SOME fregime,h) =
    (fn(RegimeState (x,y,e,d,r)) => 
        let val x'  = x + h
            val y'  = solver (d,r) (x,y) h
            val e'  = fcond d (x',y',e)
            val pos = vfind2 thr (e, e') 
            val r'  = fregime (e',r)
            val y'' = case pos of SOME i => fpos(x',y',e',d) | _ => y'
            val y'' = case fneg of 
                          SOME (RegimeResponse f) => 
                          (case vfind2 thr (e', e) of 
                               SOME i => f(x',y'',e',d) 
                             | _ => y'')
                        | SOME _ => raise Domain
                        | NONE => y''
            val d'  = fdiscrete (x',y',e',d)
        in
            RegimeState(x',y'',e',d',r')
        end
    | _ => raise Domain)
               
| integral1 (ContSolver solver,SOME (SCondition fcond),SOME (SResponse fpos),fneg,NONE,NONE,h) =
  (fn(EventState(x,y,e)) => 
      let val x'  = x + h
          val y'  = solver (x,y) h
          val e'  = fcond (x',y',e)
          val pos = vfind2 thr (e, e') 
          val y'' = case pos of SOME i => fpos(x',y',e') | _ => y'
          val y'' = case fneg of 
                        SOME (SResponse f) => 
                        (case vfind2 thr (e', e) of 
                             SOME i => f(x',y'',e') | _ => y'')
                      | SOME _ => raise Domain
                      | NONE => y''
      in
          EventState(x',y'',e')
      end
  | _ => raise Domain)

| integral1 (ContSolver solver,NONE,NONE,_,NONE,NONE,h) =
  (fn(ContState(x,y)) => 
      let val x'  = x + h
          val y'  = solver (x,y) h
      in
          ContState(x',y')
      end
  | _ => raise Domain)

| integral1 (_,_,_,_,_,_,_) = 
  raise Domain


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

(* 1. Ensures that the event closest is time is selected, not just the first one in the event index.
   2. Determines the direction (positive or negative) of the event. *)
fun athreshold (i,v1,v2,ax) = 
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


fun asolver (stepper,fcond,fdiscrete,fregime) (x,ys,ev,d,r,h) =
    let open Real
        val (ys',e,finterp) = (stepper (d,r)) h (x,ys)
        val ev' = fcond d (x+h,ys',ev)
    in
        case predictor tol (h,e) of
            Right h' => 
            (case vfoldpi2 athreshold (ev, ev') of
                 SOME (evdir,evind,evdiff) => 
                 (let
                     fun fy (theta) = Vector.sub(fcond d (x+theta*h, finterp theta, ev), evind)
                     val y0    = Vector.sub(fcond d (x,ys,ev), evind)
                     val theta = secant tol fy y0 1.0 0.0
                     val x'    = x+(theta+tol)*h
                     val ys''  = finterp (theta+tol)
                     val ev''  = fcond d (x',ys'',ev)
                     val d'    = fdiscrete (x',ys'',ev'',d)
                     val r'    = fregime (ev'',r)
                 in
                     Root (x',ys'',evdir,ev'',d',r',h')
                 end)
               | NONE => 
                 Next (x+h,ys',0,ev',d,r,h'))
          | Left h'  => 
            asolver (stepper, fcond, fdiscrete, fregime) (x,ys,ev,d,r,h')
    end

(*
fun integral2 (f,fcond,fpos,fneg,fdiscrete,fregime) =
    let
       fun fstepper (d,r) = make_estepper (f(d,r))
       val fasolver = asolver (fstepper,fcond,fdiscrete,fregime)
    in
       fn(x,ys,ev,d,r,h) => 
       (case fasolver (x,ys,ev,d,r,h) of
            Next (xn,ysn,_,evn,dn,rn,hn) => 
            (xn,ysn,evn,dn,rn,hn)
          | Root (xn,ysn,_,evn,dn,rn,hn) => 
            let 
               val ysn' = fneg(xn,fpos(xn,ysn,evn,dn),evn,dn)
            in
               (xn,ysn',evn,dn,rn,hn)
            end)
   end

*)

end
