(*
 * Definitions for numerical simulations of functional hybrid dynamical systems.
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
val signal_ln = ln
val signal_exp = exp
                      
val signal_max = max
val signal_min = min
val signal_gt = (op >)
val signal_gte = (op >=)
val signal_lt = (op <)
val signal_lte = (op <=)

fun signal_heaviside (x) = 
    if x < 0.0 then 0.0 else 1.0

end

structure FunctionalHybridDynamics =
struct

open Real
open Math
open SignalMath

val debug = false

type regime_state  = bool array
type dsc_state     = real array
type event_state   = real array
type cont_state    = real array
type external_state = real array
type externalev_state = real array

val maxiter = 10
val evtol = 1E~15                             
val tol = ref (Real.Math.pow (10.0, ~7.0))

                
datatype ('a, 'b) either = Left of 'a | Right of 'b

fun predictor tol (h,ys) =
  let open Real
      val lb = 0.5
      val ub = 0.9
      val e  = Array.foldl (fn (y,ax) => Real.+ ((abs y),ax)) 0.0 ys
  in 
      if e < lb*tol
      then Right (1.414*h, ys)	(* step too small, accept but grow *)
      else (if e < ub*tol 
            then Right (h, ys)	(* step just right *)
            else Left (0.5*h, ys)) (* step too large, reject and shrink *)
  end

type error_state   = (real * real array, real * real array) either

exception ConvergenceError

datatype model_root = RootBefore | RootFound of real list | RootAfter of real list | RootStep of real list 
                             
datatype model_state = 
         RegimeState of real * real * cont_state * event_state * dsc_state * regime_state * external_state * externalev_state * 
                        cont_state * cont_state * event_state * error_state * model_root
         | EventState of real * real * cont_state * event_state * external_state * externalev_state * 
                         cont_state * cont_state * event_state * error_state * model_root
         | ContState  of real * real * cont_state * external_state * externalev_state * cont_state * cont_state * error_state


datatype model_stepper = 
         RegimeStepper of (dsc_state * regime_state * external_state * externalev_state *
                           real * real * cont_state * cont_state) -> cont_state
         | EventStepper of (external_state * externalev_state * real * real * cont_state * cont_state) -> cont_state
         | ContStepper  of (external_state * externalev_state * real * real * cont_state * cont_state) -> cont_state

datatype model_stepper = 
         RegimeStepper of dsc_state * regime_state * external_state * externalev_state *
                          real * real * cont_state * cont_state -> 
                          (cont_state * error_state * cont_state)
         | EventStepper of (external_state * externalev_state * real * real * cont_state * cont_state) -> 
                           (cont_state * error_state * cont_state)
         | ContStepper of (external_state * externalev_state * real * real * cont_state * cont_state) -> 
                          (cont_state * error_state)

datatype model_condition = 
         RegimeCondition of (real * cont_state * event_state * dsc_state * regime_state * external_state * externalev_state * event_state) -> event_state
         | SCondition of (real * cont_state * event_state * external_state * externalev_state * event_state) -> event_state

datatype model_response = 
         RegimeResponse of (real * cont_state * event_state * dsc_state * external_state * externalev_state * cont_state) -> cont_state
         | SResponse of (real * cont_state * event_state * external_state * externalev_state * cont_state) -> cont_state

val getindex = Unsafe.Array.sub
val update = Unsafe.Array.update

val vfind = Array.find
val vfindi = Array.findi
fun vset v a = Array.modify (fn (x) => v) a
                            
fun vmap f v u = 
    let
        val n = Array.length v
    in
        (Array.appi (fn (i,x) => update (u, i, f x)) v; u)
    end

fun vmap2 f (v1,v2,a) = 
    let 
        val n = Array.length v1
    in
        (Array.appi (fn (i,x) => update (a, i, f (x, getindex (v2,i)))) v1; a)
    end

fun vfind2 f (v1,v2) = 
    let 
        val n = Array.length v1
        fun recur i = 
            (if Int.<(i, n)
             then (if f (getindex (v1,i), getindex (v2,i)) then SOME(i) else recur (Int.+(i,1)))
             else NONE)
    in
        recur 0
    end 

fun vfindi2 f (v1,v2) = 
    let 
        val n = Array.length v1
        fun recur i = 
            (if Int.<(i, n)
             then (if f (getindex (v1,i), getindex (v2,i)) then SOME(i,getindex(v2,i)) else recur (Int.+(i,1)))
             else NONE)
    in
        recur 0
    end 

fun vfoldpi2 f (v1,v2) = 
    let 
        val n = Array.length v1
        fun recur (i, ax) = 
            if Int.<(i, n)
            then recur (Int.+(i,1), f (i, getindex (v1,i), getindex (v2,i), ax))
            else ax
    in
      recur (0, NONE)
    end 

val equal = fn (x,y) => (x = y) 
  
(* Compensated summation *)
fun csum (x, cx, h) =
  let
      val dx  = h + cx
      val x'  = x + dx
      val cx' = (x - x') + dx
  in
      (x', cx')
  end

(* Fixed time step integrator *)

fun thr (v1) = 
    let
        val s1 = Real.sign v1
    in
        case s1 of
            1 => true
          | 0 => true
          | _ => false
    end

fun thr2 (v1,v2) = 
    let
        val s1 = Real.sign v1
        val s2 = Real.sign v2
    in
        case (s1,s2) of
           (~1,1) => true
         | (~1,0) => true
         | (0,1) => true
         | _ => false
    end

fun fixthr (v) =
    (Array.modify (fn(x) => if Real.>(Real.abs(x), evtol) then x else 0.0) v; v)

        
fun posdetect (x, e, x', e') =
  let
      fun minthr (i,v1,v2,optval) =
        (if debug
         then putStrLn ("posdetect: i = " ^ (Int.toString i) ^ " v1 = " ^ (showReal (v1)) ^ " v2 = " ^ (showReal v2))
         else ();
         if thr2 (v1,v2)
         then (case optval of
                   SOME (i',v,v') => (if v1<v then SOME (i,v1,v2) else optval)
                 | NONE => SOME (i,v1,v2))
         else optval)
  in
      if debug
      then List.app
               (fn(i) => putStrLn ("posdetect: x = " ^ (showReal (x)) ^ " x' = " ^ (showReal x') ^
                                   " e[" ^ (Int.toString i) ^ "] = " ^ (showReal (getindex(e,i))) ^
                                   " e'[" ^ (Int.toString i) ^ "] = " ^ (showReal (getindex(e',i)))))
               (List.tabulate(Array.length e, fn(i) => i))
      else ();
      if (x' > x)
      then 
           (case vfoldpi2 minthr (e, e') of
                SOME (i,ev,ev') => (if debug
                                    then putStrLn ("posdetect: x = " ^ (showReal (x)) ^ 
                                                   " e[" ^ (Int.toString i) ^ "] = " ^ (showReal (ev)) ^
                                                   " x' = " ^ (showReal (x')) ^
                                                   " ev = " ^ (showReal ev))
                                    else ();
                                    SOME  (x, ev, x', ev'))
              | NONE => NONE)
      else NONE
  end

         

fun condApply (SOME (RegimeCondition fcond)) =
    (fn(RegimeState (x,cx,y,e,d,r,ext,extev,ynext,yrsp,enext,root)) => 
        let val e' = fixthr (fcond (x,y,e,d,r,ext,extev,enext))
        in
            RegimeState (x,cx,y,e',d,r,ext,extev,ynext,yrsp,e,root)
        end
    | _ => raise Domain)
  | condApply (SOME (SCondition fcond)) =
    (fn(EventState(x,cx,y,e,ext,extev,ynext,yrsp,enext,root)) =>     
        let val e' = fixthr (fcond (x,y,e,ext,extev,enext))
        in
            EventState(x,cx,y,e',ext,extev,ynext,yrsp,e,root)
        end
    | _ => raise Domain)
  | condApply (NONE) = raise Domain

                             
fun evresponse_regime (fpos,fneg,fdiscrete,fregime) =
  fn(x,y,e,d,r,ext,extev,yrsp) =>
     let
         val y'  = case fpos of 
                       SOME (RegimeResponse f) =>
                       f (x,y,e,d,ext,extev,yrsp)
                     | NONE => y
                     | _ => raise Fail "evresponse_regime: RegimeState integral response"
         val y'' = case fneg of 
                       NONE => y'
                     | SOME (RegimeResponse f) => f (x,y',e,d,ext,extev,yrsp)
                     | _ => raise Fail "evresponse_regime: RegimeState integral response"
         val d'  =  (case fdiscrete of 
                         SOME f => f (x,y,e,d)
                       | NONE => d)
         val r'  = fregime (e,r)
         val _ = vset 0.0 ext
         val _ = vset posInf extev
     in
         (y'',d',r')
     end
    

fun evresponse (fpos,fneg) =
    case fpos of 
        SOME (SResponse fpos) =>
        (fn(x,y,e,ext,extev,yrsp) =>
            let 
                val y' = case fneg of 
                             NONE => fpos(x,y,e,ext,extev,yrsp)
                           | SOME (SResponse f) => f (x,fpos(x,y,e,ext,extev,yrsp),e,ext,extev,yrsp)
                           | _ => raise Fail "evresponse: EventState integral response"
                val _ = vset 0.0 ext
                val _ = vset posInf extev
            in
                y'
            end)
      | _ => raise Fail "evresponse: unsupported event response configuration"

                   
fun adaptive_regime_stepper (stepper,fcond,fdiscrete,fregime)  =
    let open Real
        fun f iter (h,d,r,ext,extev,ynext,yrsp,enext,root) =
            if Int.<(iter,maxiter)
            then 
                (let
                    val (ys',err',w) = stepper (h,d,r,ext,extev,h,x,ys,yout,err)
                    val ev' = fixthr(fcond (x+h,ys',ev,d,r,ext,extev,falloc nev))
                in
                    case tol of
                        SOME => 
                        (case predictor tol (h,err') of
                             Right h' => 
                             (x',ys'',evdir,ev'',d',r',ext,extev,h')
                           | Left h'  => 
                             f (Int.+(iter,1)) (x,ys,ev,d,r,ext,extev,h'))
                     |  NONE => 
                end)
            else raise Fail "convergence error in regime stepper"
    in
        f 0
    end
        

fun integral (RegimeStepper stepper,finterp,SOME (RegimeCondition fcond),                          
              fpos,fneg,fdiscrete,SOME fregime,h) =
  let
      val fstepper = adaptive_regime_stepper (stepper,fcond,fdiscrete,fregime)

      fun integral' (RegimeState (x,cx,y,e,d,r,ext,extev,ynext,yrsp,enext,root)) =
        (case root of
             RootBefore =>
             let
                 val e'  = fixthr (fcond (x,y,e,d,r,ext,extev,enext))
             in
                 integral'(RegimeState(x,cx,y,e',d,r,ext,extev,ynext,yrsp,e,RootStep [h]))
             end
           | RootStep (h::hs) =>
             let val _ = if debug then putStrLn ("RootStep: h = " ^ (showReal h) ^ " x = " ^ (showReal x)) else ()
                 val (x',cx')  = csum (x,cx,h)
                 val (y',err',w) = fstepper (d,r,ext,extev,h,x,y,ynext,err)

                 val e'  = fixthr (fcond (x',y',e,d,r,ext,extev,enext))
                 val (rootp, theta) =
                     case posdetect (x,e,x',e') of
                         SOME _ => (true, RootFind.brent (!tol)
                                                         (evtest o finterp) 0.0 1.0)
                      |  NONE => (false, x')
                 val xe = x+theta*h
                 val _ = if debug
                         then putStrLn ("RootStep: rootp = " ^ (Bool.toString rootp) ^
                                        " h = " ^ (showReal h) ^
                                        " x' = " ^ (showReal x') ^ " xe = " ^ (showReal xe) ^
                                        " e' = " ^ (showReal (getindex (e',0))))
                         else ()
             in
                 if rootp
                 then (if x' > xe
                       then
                           let
                               val h1 = xe-x
                               val (x'',cx'')  = csum (x,cx,h1)
                               val (x''',cx''')  = csum (x'',cx'',h-h1)
                               val y'' = stepper (d,r,ext,extev,h1,x,y,ynext)
                               val h2 = x''' - x''
                               val _ = if debug
                                       then putStrLn ("RootStep: x' = " ^ (showReal x') ^ " x'' = " ^ (showReal x'') ^
                                                      " h2 = " ^ (showReal h2) ^ " cx' = " ^ (showReal cx'))
                                       else ()
                           in
                               RegimeState(x'',cx'',y'',e',d,r,ext,extev,y,yrsp,e,RootFound (if h2>0.0 then h2::hs else hs))
                           end
                       else RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,RootFound hs))
                 else (case hs of [] => RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,RootBefore)
                               | _ => RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,RootStep hs))
             end
           | RootFound hs =>
             let 
                 val (y',d',r') = evresponse_regime (fpos,fneg,fdiscrete,fregime) 
                                                    (x,y,e,d,r,ext,extev,yrsp)
             in
                 RegimeState(x,cx,y',e,d',r',ext,extev,y,ynext,enext,RootAfter hs)
             end
           | RootAfter [] =>
             let 
                  val e'  = fixthr (fcond (x,y,e,d,r,ext,extev,enext))
             in
                 RegimeState(x,cx,y,e',d,r,ext,extev,ynext,y,e,RootBefore)
             end
           | RootAfter (h1::hs) =>
             let 
                 val (x',cx')  = csum (x,cx,evtol)
                 val y'  = stepper (d,r,ext,extev,evtol,x,y,ynext)
                 val e'  = fixthr (fcond (x',y',e,d,r,ext,extev,enext))
             in
                 RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,RootStep ((h1-evtol)::hs))
             end
           | _ => raise Fail "integral: invalid arguments to regime stepper")
            
        | integral' _ = raise Fail "integral: invalid RegimeState"
  in
      integral'
  end
               
  | integral (EventStepper stepper,SOME (SCondition fcond),fpos,fneg,NONE,NONE,h) =
    let
        fun integral' (EventState(x,cx,y,e,ext,extev,ynext,yrsp,enext,root)) =
          (case root of
               RootBefore =>
               let
                   val e' = fixthr (fcond (x,y,e,ext,extev,enext))
               in
                   integral'(EventState(x,cx,y,e',ext,extev,ynext,yrsp,e,RootStep [h]))
               end
             | RootStep (h::hs) =>
               let
                   val _ = if debug then putStrLn ("RootStep: h = " ^ (showReal h) ^ " x = " ^ (showReal x)) else ()
                   val (x',cx')  = csum (x,cx,h)
                   val y'  = stepper (ext,extev,h,x,y,ynext)
                   val e'  = fixthr (fcond (x',y',e,ext,extev,enext))
                   val (rootp, xe) = case posdetect2 (x,e,x',e') of
                                         SOME tbl => (true, LinearInterpolation.interpolate1 tbl 0.0)
                                      |  NONE => (false, x')
                   val _ = if debug then putStrLn ("RootStep: rootp = " ^ (Bool.toString rootp) ^ " h = " ^ (showReal h) ^ " x' = " ^ (showReal x') ^ " xe = " ^ (showReal xe)) else ()
               in
                   if rootp
                   then (let
                            val h1 = xe-x
                            val (x'',cx'')  = csum (x,cx,h1)
                            val (x''',cx''')  = csum (x'',cx'',h-h1)
                            val y'' = stepper (ext,extev,h1,x,y,ynext)
                            val h2 = x''' - x''
                            val e'  = fixthr (fcond (x'',y'',e,ext,extev,enext))
                            val _ = if debug then putStrLn ("RootStep: x' = " ^ (showReal x') ^ " x'' = " ^ (showReal x'') ^ " h2 = " ^ (showReal h2) ^ " cx' = " ^ (showReal cx')) else ()
                        in
                            EventState(x'',cx'',y'',e',ext,extev,y,yrsp,e,RootFound (h2::hs))
                        end)
                   else (case hs of [] => EventState(x',cx',y',e',ext,extev,y,yrsp,e,RootBefore)
                                      | _ => EventState(x',cx',y',e',ext,extev,y,yrsp,e,RootStep hs))
               end
             | RootFound hs =>
               let
                   val y' = evresponse (fpos,fneg) (x,y,e,ext,extev,yrsp)
               in
                   EventState(x,cx,y',e,ext,extev,y,ynext,enext,RootAfter hs)
               end
             | RootAfter hs =>
               let
                   val (x',cx')  = csum (x,cx,evtol)
                   val y'  = stepper (ext,extev,evtol,x,y,ynext)
                   val e'  = fixthr (fcond (x',y',e,ext,extev,enext))
                   val _   = if debug
                             then putStrLn ("RootAfter: x' = " ^ (showReal x') ^ " e' = " ^ (showReal (getindex (e',0))) ^
                                            " y' = " ^ (showReal (getindex (y',0))))
                             else ()
               in
                   case hs of
                       [] => EventState(x',cx',y',e',ext,extev,y,yrsp,e,RootBefore)
                    |  h1::hs => EventState(x',cx',y',e',ext,extev,y,yrsp,e,RootStep ((h1-evtol)::hs))
               end
             |  _ => raise Fail "integral: invalid arguments to event stepper")
          | integral' _ =
            raise Fail "integral: invalid event state"
    in
        integral'
    end
        
| integral (ContStepper stepper,NONE,fpos,fneg,NONE,NONE,h) =
  (fn(ContState(x,cx,y,ext,extev,ynext,yrsp)) => 
      let val (x',cx')  = csum(x,cx,h)
          val y'  = stepper (ext,extev,h,x,y,ynext)
      in
          ContState(x',cx',y',ext,extev,ynext,yrsp)
      end
  | _ => raise Fail "integral: invalid continuous state"

| integral _ =
  raise Fail "integral: unsupported stepper configuration"



end
