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

structure FunctionalHybridDynamics1 =
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

fun showRealArray v = 
    (String.concatWith ", " (Array.foldr (fn (x, ax) => (showReal x)::ax) [] v))

type regime_state  = bool array
type dsc_state     = real array
type event_state   = real array
type cont_state    = real array
type error_state   = real array
type external_state = real array
type externalev_state = real array

datatype model_root = RootBegin | RootCheck of real list | RootFound of real list | RootStep of real list
                             
datatype model_state = 
         RegimeState of real * real * cont_state * event_state * dsc_state * regime_state * external_state * externalev_state * 
                        cont_state * cont_state * event_state * model_root
         | EventState of real * real * cont_state * event_state * external_state * externalev_state * 
                         cont_state * cont_state * event_state * model_root
         | ContState  of real * real * cont_state * external_state * externalev_state * cont_state * cont_state


datatype model_stepper = 
         RegimeStepper of (dsc_state * regime_state * external_state * externalev_state *
                           real * real * cont_state * cont_state) -> cont_state
         | EventStepper of (external_state * externalev_state * real * real * cont_state * cont_state) -> cont_state
         | ContStepper  of (external_state * externalev_state * real * real * cont_state * cont_state) -> cont_state

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
    (Array.modify (fn(x) => if Real.>(Real.abs(x), 1e~6) then x else 0.0) v; v)

fun posdetect1 (x, e) =
  (case vfind thr e of (SOME _) => true | NONE => false)
                       

fun posdetect2 (x, e, x', e') =
  (putStrLn("posdetect2: x = " ^ (showReal (x)) ^ " x' = " ^ (showReal (x')));
   List.app (fn(i) =>
                putStrLn (" e[" ^ (Int.toString i) ^ "] = " ^ (showReal (getindex(e,i))) ^
                          " e'[" ^ (Int.toString i) ^ "] = " ^ (showReal (getindex(e',i)))))
            (List.tabulate (Array.length e, fn x => x));
            
   if (x' > x)
   then 
       (case vfindi2 thr2 (e,e') of
            SOME (i,ev) => (putStrLn("posdetect2: x = " ^ (showReal (x)) ^
                                     " e[" ^ (Int.toString i) ^ "] = " ^ (showReal (getindex(e,i))) ^
                                     " x' = " ^ (showReal (x')) ^
                                     " ev = " ^ (showReal ev));
                            SOME (Vector.fromList [(x, getindex (e,i)), (x', ev)]))
          | NONE => NONE)
   else NONE)

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
                       | _ => (putStrLn "FunctionalHybridDynamics1: RegimeState integral response"; 
                               raise Domain)
           val y'' = case fneg of 
                         NONE => y'
                       | SOME (RegimeResponse f) => f (x,y',e,d,ext,extev,yrsp)
                       | _ => (putStrLn "FunctionalHybridDynamics1: RegimeState integral response"; 
                               raise Domain)
           val d'  =  (case fdiscrete of 
                           SOME f => f (x,y,e,d)
                         | NONE => d)
           val r'  = fregime (e,r)
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
                           | _ => (putStrLn "FunctionalHybridDynamics1: EventState integral response"; 
                                   raise Domain)
            in
                y'
            end)
      | _ => (putStrLn "FunctionalHybridDynamics1: unsupported event response configuration"; 
              raise Domain)
        

fun integral (RegimeStepper stepper,SOME (RegimeCondition fcond),                          
              fpos,fneg,fdiscrete,SOME fregime,h) =
  let
      fun integral' (RegimeState (x,cx,y,e,d,r,ext,extev,ynext,yrsp,enext,root)) =
        (case root of
             RootBegin =>
             let
                 val e'  = fixthr (fcond (x,y,e,d,r,ext,extev,enext))
                 val rootp = posdetect1(x,e')
             in
                 if rootp
                 then (let val (y',d',r') = evresponse_regime (fpos,fneg,fdiscrete,fregime) 
                                                              (x,y,e',d,r,ext,extev,yrsp)
                           val e'    = fixthr (fcond (x,y',e,d',r',ext,extev,enext))
                       in
                           RegimeState(x,cx,y',e',d',r',ext,extev,ynext,y,e,RootStep [h])
                       end)
                 else integral'(RegimeState(x,cx,y,e',d,r,ext,extev,ynext,yrsp,e,RootStep [h]))
             end
          | RootCheck hs =>
             let
                 val e'  = fixthr (fcond (x,y,e,d,r,ext,extev,enext))
                 val rootp = posdetect1(x,e')
             in
                 if rootp
                 then (let val (y',d',r') = evresponse_regime (fpos,fneg,fdiscrete,fregime) 
                                                              (x,y,e',d,r,ext,extev,yrsp)
                           val e'    = fixthr (fcond (x,y',e,d',r',ext,extev,enext))
                       in
                           RegimeState(x,cx,y',e',d',r',ext,extev,ynext,y,e,RootCheck hs)
                       end)
                 else integral'(RegimeState(x,cx,y,e',d,r,ext,extev,ynext,yrsp,e,RootStep hs))
             end
           | RootStep (h::hs) =>
             let
                 val (x',cx')  = csum (x,cx,h)
                 val y'  = stepper (d,r,ext,extev,h,x,y,ynext)
                 val e'  = fixthr (fcond (x',y',e,d,r,ext,extev,enext))
                 val (rootp, xe) = case posdetect2 (x,e,x',e') of
                                       SOME tbl => (true, LinearInterpolation.interpolate1 tbl 0.0)
                                    |  NONE => (false, x')
                 val _ = putStrLn ("RootStep: x = " ^ (showReal x) ^ " x' = " ^ (showReal x') ^
                                   " rootp = " ^ (Bool.toString rootp))
             in
                 if rootp
                 then (let
                          val h1 = xe-x
                          val y'' = stepper (d,r,ext,extev,h1,x,y,ynext)
                          val (x'',cx'')  = csum (x,cx,h1)
                          val h2 = x' - x''
                      in
                          RegimeState(x'',cx'',y'',e',d,r,ext,extev,y,yrsp,e,RootFound (h2::hs))
                      end)
                 else (case hs of [] => RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,RootBegin)
                               | _ => RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,RootStep hs))
             end
           | RootFound hs =>
             let
                 val (y',d',r') = evresponse_regime (fpos,fneg,fdiscrete,fregime) 
                                                    (x,y,e,d,r,ext,extev,yrsp)
                 val e'  = fixthr (fcond (x,y',e,d',r',ext,extev,enext))
             in
                 RegimeState(x,cx,y',e',d',r',ext,extev,ynext,y,e,RootStep hs)
             end
           | _ => raise Domain)
            
        | integral' _ = (putStrLn "FunctionalHybridDynamics1: invalid RegimeState"; raise Domain)
  in
      integral'
  end
               
  | integral (EventStepper stepper,SOME (SCondition fcond),fpos,fneg,NONE,NONE,h) =
    let
        fun integral' (EventState(x,cx,y,e,ext,extev,ynext,yrsp,enext,root)) =
          (case root of
               RootBegin =>
               let
                   val e'  = fixthr (fcond (x,y,e,ext,extev,enext))
                   val rootp = posdetect1(x,e')
               in
                   if rootp
                   then (let val y' = evresponse (fpos,fneg) (x,y,e',ext,extev,yrsp)
                             val e' = fixthr (fcond (x,y',e,ext,extev,enext))
                         in
                             EventState(x,cx,y',e',ext,extev,ynext,y,e,RootStep [h])
                         end)
                   else integral'(EventState(x,cx,y,e',ext,extev,ynext,yrsp,e,RootStep [h]))
               end
             | RootCheck hs =>
               let
                   val e'  = fixthr (fcond (x,y,e,ext,extev,enext))
                   val rootp = posdetect1(x,e')
               in
                   if rootp
                   then (let val y' = evresponse (fpos,fneg) (x,y,e',ext,extev,yrsp)
                             val e'  = fixthr (fcond (x,y',e,ext,extev,enext))
                         in
                             EventState(x,cx,y',e',ext,extev,ynext,y,e,RootCheck hs)
                         end)
                   else integral'(EventState(x,cx,y,e',ext,extev,ynext,yrsp,e,RootStep hs))
               end
             | RootStep (h::hs) =>
               let
                   val (x',cx')  = csum (x,cx,h)
                   val y'  = stepper (ext,extev,h,x,y,ynext)
                   val e'  = fixthr (fcond (x',y',e,ext,extev,enext))
                   val (rootp, xe) = case posdetect2 (x,e,x',e') of
                                         SOME tbl => (true, LinearInterpolation.interpolate1 tbl 0.0)
                                      |  NONE => (false, x')
               in
                   if rootp
                   then (let
                            val h1 = xe-x
                            val y'' = stepper (ext,extev,h1,x,y,ynext)
                            val (x'',cx'')  = csum (x,cx,h1)
                            val h2 = x' - x''
                            val e'  = fixthr (fcond (x'',y'',e,ext,extev,enext))
                        in
                            EventState(x'',cx'',y'',e',ext,extev,y,yrsp,e,RootFound (h2::hs))
                        end)
                   else (case hs of [] => EventState(x',cx',y',e',ext,extev,y,yrsp,e,RootBegin)
                                | _ => EventState(x',cx',y',e',ext,extev,y,yrsp,e,RootStep hs))
               end
             | RootFound hs =>
               let
                   val y' = evresponse (fpos,fneg) (x,y,e,ext,extev,yrsp)
                   val e'  = fixthr (fcond (x,y',e,ext,extev,enext))
               in
                   EventState(x,cx,y',e',ext,extev,ynext,y,e,RootStep hs)
               end
             |  _ => raise Domain)
          | integral' _ =
            (putStrLn "FunctionalHybridDynamics1: invalid EventState"; 
             raise Domain)
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
  | _ => (putStrLn "FunctionalHybridDynamics1: invalid ContState"; 
          raise Domain))

| integral _ =
  (putStrLn "FunctionalHybridDynamics1: unsupported stepper configuration"; 
   raise Domain)



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

fun showRealArray v = 
    (String.concatWith ", " (Array.foldr (fn (x, ax) => (showReal x)::ax) [] v))

type regime_state  = bool array
type dsc_state     = real array
type event_state   = real array
type cont_state    = real array
type error_state   = real array
type external_state = real array
type externalev_state = real array

datatype model_state = 
         RegimeState  of real * real * cont_state * real * event_state * dsc_state * regime_state * external_state * externalev_state * real * 
                         cont_state * cont_state * error_state * event_state * bool
         | EventState of real * real * cont_state * real * event_state * external_state * externalev_state * real * 
                         cont_state * cont_state * error_state * event_state * bool
         | ContState  of real * real * cont_state * external_state * externalev_state * real * 
                         cont_state * cont_state * error_state

datatype model_stepper = 
         RegimeStepper of (dsc_state * regime_state * external_state * externalev_state * 
                           real * real * cont_state * cont_state * error_state) ->
                          (cont_state * error_state)
         | EventStepper of (external_state * externalev_state * real * real * cont_state * cont_state * error_state) -> 
                           (cont_state * error_state)
         | ContStepper of (external_state * externalev_state * real * real * cont_state * cont_state * error_state) -> 
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
            if Int.<(i, n)
            then (if f (getindex (v1,i), getindex (v2,i)) then SOME(i) else recur (Int.+(i,1)))
            else NONE
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

(* Adaptive time step integrator *)

val tol = ref (Real.Math.pow (10.0, ~7.0))
val lb = 0.5 * (!tol)
val ub = 0.9 * (!tol)

datatype ('a, 'b) either = Left of 'a | Right of 'b

exception ConvergenceError


fun thr (v1) = 
    let
        val s1 = Real.sign v1
    in
        case s1 of
            1 => true
          | 0 => true
          | _ => false
    end

fun fixthr (v) =
    (Array.modify (fn(x) => if Real.>(Real.abs(x), (!tol)) then x else 0.0) v; v)

fun posdetect (x, e, x', e') =
    (x' > x) andalso (case vfind thr e' of (SOME _) => true | NONE => false)


fun condApply (SOME (RegimeCondition fcond)) =
    (fn(RegimeState (x,cx,y,xe,e,d,r,ext,extev,h,ynext,yrsp,errnext,enext,root)) => 
        let val e' = fixthr (fcond (x,y,e,d,r,ext,extev,enext))
        in
            RegimeState (x,cx,y,xe,e',d,r,ext,extev,h,ynext,yrsp,errnext,e,root)
        end
    | _ => raise Domain)
  | condApply (SOME (SCondition fcond)) =
    (fn(EventState(x,cx,y,xe,e,ext,extev,h,ynext,yrsp,errnext,enext,root)) =>     
        let val e' = fixthr (fcond (x,y,e,ext,extev,enext))
        in
            EventState(x,cx,y,xe,e',ext,extev,h,ynext,yrsp,errnext,e,root)
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
                       | _ => (putStrLn "FunctionalHybridDynamics1: RegimeState integral response"; 
                               raise Domain)
           val y'' = case fneg of 
                         NONE => y'
                       | SOME (RegimeResponse f) => f (x,y',e,d,ext,extev,yrsp)
                       | _ => (putStrLn "FunctionalHybridDynamics1: RegimeState integral response"; 
                               raise Domain)
           val d'  =  (case fdiscrete of 
                           SOME f => f (x,y,e,d)
                         | NONE => d)
           val r'  = fregime (e,r)
       in
           (y'',d',r')
       end


fun evresponse (fpos,fneg) =
    case fpos of 
        SOME (SResponse fpos) =>
        (fn(x,y,e,ext,extev,yrsp) =>
            let 
                val y' =  case fneg of 
                              NONE => fpos(x,y,e,ext,extev,yrsp)
                            | SOME (SResponse f) => f (x,fpos(x,y,e,ext,extev,yrsp),e,ext,extev,yrsp)
                            | _ => (putStrLn "FunctionalHybridDynamics2: EventState integral response"; 
                                    raise Domain)
            in
                y'
            end)
      | _ => (putStrLn "FunctionalHybridDynamics2: unsupported event response configuration"; 
              raise Domain)



fun predictor tol (h,ys) =
  let open Real
      val e = Array.foldl (fn (y,ax) => Real.+ ((abs y),ax)) 0.0 ys
  in 
      if e < lb 
      then Right (1.414*h)	(* step too small, accept but grow *)
      else (if e < ub 
            then Right h	(* step just right *)
            else Left (0.5*h))	(* step too large, reject and shrink *)
  end


datatype 'a result = Next of 'a | Root of 'a

fun adaptive_regime_solver (stepper,fcond,fdiscrete,fregime)  =
    let open Real
        fun f iter (x,cx,ys,xev,ev,d,r,ext,extev,h,yout,err,enext) =
            if Int.<(iter,10)
            then 
                (let
                    val (ys',e) = stepper (d,r,ext,extev,h,x,ys,yout,err)
                in
                    case predictor tol (h,e) of
                        Right h' => 
                        (let
                            val (x',cx')  = csum(x,cx,h)
                            val ev' = fixthr(fcond (x',ys',ev,d,r,ext,extev,enext))
                            val root = posdetect (xev,ev,x',ev')
                        in
                            if root 
                            then 
                                (let
                                    (*
                                    val d'    = (case fdiscrete of 
                                                     SOME f => f (x',ys',ev',d)
                                                   | NONE => d)
                                    val r' = fregime (ev',r)
                                     *)
                                    val d' = d
                                    val r' = r
                                in
                                    Root (x',cx',ys',x',ev',d',r',ext,extev,ev,h')
                                end)
                            else 
                                    Next (x',cx',ys',xev,ev,d,r,ext,extev,ev,h')
                        end)
                      | Left h'  => 
                        f (Int.+(iter,1)) (x,cx,ys,xev,ev,d,r,ext,extev,h',yout,err,enext)
                end)
            else raise ConvergenceError
    in
        f 0
    end


fun adaptive_event_solver (stepper,fcond)  =
    let open Real
        fun f iter (x,cx,ys,xev,ev,ext,extev,h,yout,err,enext) =
            if (Int.<(iter,10))
            then 
                (let
                    val (ys',e) = stepper (ext,extev,h,x,ys,yout,err)
                in
                    case predictor tol (h,e) of
                        Right h' => 
                        (let
                            val (x',cx')  = csum(x,cx,h)
                            val ev' = fixthr (fcond (x',ys',ev,ext,extev,enext))
                            val root = posdetect (xev,ev,x',ev')
                        in
                            if root 
                            then Root (x',cx',ys',x',ev',ext,extev,ev,h')
                            else Next (x',cx',ys',x',ev',ext,extev,ev,h')
                        end)
                      | Left h'  => 
                        f (Int.+(iter,1)) (x,cx,ys,xev,ev,ext,extev,h',yout,enext,err)
                end)
            else raise ConvergenceError
    in
        f 0
    end


fun adaptive_solver (stepper)  =
    let open Real
        fun f iter (x,cx,ys,ext,extev,h,yout,err) =
            if (Int.<(iter,10))
            then 
                (let
                    val (ys',e) = stepper (ext,extev,h,x,ys,yout,err)
                in
                    case predictor tol (h,e) of
                        Right h' =>
                        let val (x',cx') = csum(x,cx,h)
                        in
                            Next (x',cx',ys',ext,extev,h')
                        end
                      | Left h'  => 
                        f (Int.+(iter,1)) (x,cx,ys,ext,extev,h',yout,err)
                end)
            else raise ConvergenceError
    in
        f 0
    end


fun integral (RegimeStepper fstepper,SOME (RegimeCondition fcond),
               fpos,fneg,fdiscrete,SOME fregime) =
    let
       val fsolver = adaptive_regime_solver (fstepper,fcond,fdiscrete,fregime)
    in
        fn(RegimeState (x,cx,y,xev,e,d,r,ext,extev,h,ynext,yrsp,errnext,enext,root)) => 
           (case root of
                true =>
                (let
                    val (y',d',r')  = evresponse_regime (fpos,fneg,fdiscrete,fregime) (x,y,e,d,r,ext,extev,yrsp)
                    (*
                    val e' = fixthr(fcond (x,y,e,d,r,ext,extev,enext))
                    val hasevent = posdetect (xev,e,x,e')
                    *)
                in
                    RegimeState (x,cx,y',x,e,d',r',ext,extev,h,ynext,y,errnext,e,false)
                end)
             | false =>  
               let
                   val e' = fixthr(fcond (x,y,e,d,r,ext,extev,enext))
                   val hasevent = posdetect (xev,e,x,e')
               in
                   if hasevent
                   then RegimeState (x,cx,y,x,e',d,r,ext,extev,h,ynext,yrsp,errnext,e,true)
                   else (case fsolver (x,cx,y,x,e',d,r,ext,extev,h,ynext,errnext,e) of
                             Next (xn,cxn,ysn,xevn,evn,dn,rn,ext,extev,ev,hn) => 
                             RegimeState (xn,cxn,ysn,xevn,evn,dn,rn,ext,extev,hn,y,yrsp,errnext,ev,false)
                           | Root (xn,cxn,ysn,xevn,evn,dn,rn,ext,extev,ev,hn) => 
                             RegimeState (xn,cxn,ysn,xevn,evn,dn,rn,ext,extev,hn,y,yrsp,errnext,ev,true))
               end)
      | _ => (putStrLn "FunctionalHybridDynamics2: invalid RegimeState"; raise Domain)
    end

  | integral (EventStepper fstepper,SOME (SCondition fcond),
               fpos,fneg,NONE,NONE) =
    let
       val fsolver = adaptive_event_solver (fstepper,fcond)
    in
        fn(EventState (x,cx,y,xev,e,ext,extev,h,ynext,yrsp,errnext,enext,root)) => 
           (case root of
                true => (let
                            val y' = evresponse (fpos,fneg) (x,y,e,ext,extev,yrsp)
                            (*
                            val e' = fixthr(fcond (x,y',e,ext,extev,enext))
                            val hasevent = posdetect (xev,e,x,e')
                            *)
                        in
                            EventState (x,cx,y',x,e,ext,extev,h,ynext,y,errnext,e,false)
                        end)
               | false => 
                 let
                     val e' = fixthr(fcond (x,y,e,ext,extev,enext))
                     val hasevent = posdetect (xev,e,x,e')
                 in
                     if hasevent
                     then EventState (x,cx,y,x,e',ext,extev,h,ynext,yrsp,errnext,e,true)
                     else (case fsolver (x,cx,y,x,e',ext,extev,h,ynext,errnext,e) of
                               Next (xn,cxn,ysn,xevn,evn,ext,extev,ev,hn) => 
                               EventState (xn,cxn,ysn,xevn,evn,ext,extev,hn,y,yrsp,errnext,ev,false)
                             | Root (xn,cxn,ysn,xevn,evn,ext,extev,ev,hn) => 
                               EventState (xn,cxn,ysn,xevn,evn,ext,extev,hn,y,yrsp,errnext,ev,true))
                 end)
      | _ => (putStrLn "FunctionalHybridDynamics2: invalid EventState"; raise Domain)
    end

  | integral (ContStepper fstepper,NONE,fpos,fneg,NONE,NONE) =
    let
       (*fun fstepper (d,r) = make_stepper (f(d,r))*)
       val fsolver = adaptive_solver (fstepper)
    in
        fn(ContState (x,cx,y,ext,extev,h,ynext,yrsp,errnext)) => 
           (case fsolver (x,cx,y,ext,extev,h,ynext,errnext) of
                Next (xn,cxn,ysn,_,_,hn) => 
                ContState (xn,cxn,ysn,ext,extev,hn,y,yrsp,errnext)
              | Root (xn,cxn,ysn,_,_,hn) => 
                ContState (xn,cxn,ysn,ext,extev,hn,y,yrsp,errnext))
      | _ => (putStrLn "FunctionalHybridDynamics2: ContState integral response"; 
              raise Domain)
    end

| integral _ =
  (putStrLn "FunctionalHybridDynamics2: unsupported stepper configuration"; 
   raise Domain)

end

(*
 * Adaptive step simulations with interpolated threshold detection of functional hybrid dynamical systems.
 *)

structure FunctionalHybridDynamics3 =
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

fun showRealArray v = 
    (String.concatWith ", " (Array.foldr (fn (x, ax) => (showReal x)::ax) [] v))

type regime_state  = bool array
type dsc_state     = real array
type event_state   = real array
type cont_state    = real array
type error_state   = real array
type external_state = real array
type externalev_state = real array

datatype model_state = 
         RegimeState  of real * cont_state * event_state * dsc_state * regime_state * external_state * externalev_state * real * bool
         | EventState of real * cont_state * event_state * external_state * externalev_state * real * bool
         | ContState  of real * cont_state * external_state * externalev_state * real

datatype model_stepper = 
         RegimeStepper of dsc_state * regime_state * external_state * externalev_state * real * real * cont_state * cont_state -> 
                          (cont_state * error_state * (real -> cont_state))
         | EventStepper of (external_state * externalev_state * real * real * cont_state * cont_state) -> 
                           (cont_state * error_state * (real -> cont_state))
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
            if Int.<(i, n)
            then (if f (getindex (v1,i), getindex (v2,i)) then SOME(i) else recur (Int.+(i,1)))
            else NONE
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

(* Adaptive time step integrator *)

val tol = ref (Real.Math.pow (10.0, ~7.0))
val lb = 0.5 * (!tol)
val ub = 0.9 * (!tol)

datatype ('a, 'b) either = Left of 'a | Right of 'b

exception ConvergenceError


fun fixthr (v) =
    (Array.modify (fn(x) => if Real.>(Real.abs(x), (!tol)) then x else 0.0) v; v)

fun condApply (SOME (RegimeCondition fcond)) =
    (fn(RegimeState (x,y,e,d,r,ext,extev,h,root), eout) => 
        let val e' = fixthr (fcond (x,y,e,d,r,ext,extev,eout))
        in
            RegimeState (x,y,e',d,r,ext,extev,h,root)
        end
    | _ => raise Domain)
  | condApply (SOME (SCondition fcond)) =
    (fn(EventState(x,y,e,ext,extev,h,root), eout) =>     
        let val e' = fixthr (fcond (x,y,e,ext,extev,eout))
        in
            EventState(x,y,e',ext,extev,h,root)
        end
    | _ => raise Domain)
  | condApply (NONE) = raise Domain

fun predictor tol (h,ys) =
  let open Real
      val e = Array.foldl (fn (y,ax) => Real.+ ((abs y),ax)) 0.0 ys
  in 
      if e < lb 
      then Right (1.414*h)	(* step too small, accept but grow *)
      else (if e < ub 
            then Right h	(* step just right *)
            else Left (0.5*h))	(* step too large, reject and shrink *)
  end



fun secant tol f fg0 guess1 guess0 iter = 
    let open Real
        infix ==
        val fg1 = f guess1
        val newGuess = guess1 - fg1 * (guess1 - guess0) / (fg1 - fg0)
        val err =  abs (newGuess - guess1)
    in 
        if fg1 == fg0
        then guess1
        else 
            (if Int.<(iter, 10)
             then 
                 (if (err < (!tol))
                  then newGuess
                  else secant tol f fg1 newGuess guess1 (Int.+(iter,1)))
             else (putStrLn "FunctionalHybridDynamics3: secant convergence error";
                   putStrLn ("err = " ^ (showReal err) ^ " guess1 = " ^ (showReal guess1) ^
                             " guess0 = " ^ (showReal guess0) ^ " guess1 = " ^ (showReal guess1) ^ 
                             " fg0 = " ^ (showReal fg0) ^ " fg1 = " ^ (showReal fg1));
                   raise ConvergenceError))
                         
    end


datatype 'a result = Next of 'a | Root of 'a

(* 1. Ensures that the event closest in time is selected, not just the first one in the event index.
   2. Determines the direction (positive or negative) of the event. *)
fun adthreshold (i,v1,v2,ax) = 
    let
        fun ethr0 (i, v1, v2) =
            case (Real.sign(v1),Real.sign(v2)) of
                (~1, 1) => SOME (1, i, v1-v2)
              | (~1, 0) => SOME (1, i, v1-v2)
              | (1, 0)  => SOME (1, i, v1-v2)
              | _ => NONE
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


fun adaptive_regime_solver (stepper,fcond,fdiscrete,fregime,falloc,n,nev)  =
    let open Real
        val yout = falloc n
        fun f iter (x,ys,ev,d,r,ext,extev,h) =
            if Int.<(iter,10)
            then 
                (let
                    val (ys',e,finterp) = stepper (d,r,ext,extev,h,x,ys,yout)
                    val ev' = fixthr(fcond (x+h,ys',ev,d,r,ext,extev,falloc nev))
                in
                    case predictor tol (h,e) of
                        Right h' => 
                        (case vfoldpi2 adthreshold (ev, ev') of
                             SOME (evdir,evind,evdiff) => 
                             (let
                                 fun fy (theta) = 
                                     let
                                         val ysc = if theta > 0.0 then finterp theta else ys'
                                     in
                                         Array.sub(fixthr(fcond (x+theta*h, ysc, ev, d, r, ext, extev, falloc nev)), evind)
                                     end
                                 val y0    = Array.sub(fixthr (fcond (x,ys,ev,d,r,ext,extev,falloc n)), evind)
                                 val theta = secant tol fy y0 1.0 0.0 0
                                 val x'    = x+(theta)*h
                                 val ys''  = if theta > 0.0 then finterp (theta) else ys'
                                 val ev''  = fixthr (fcond (x',ys'',ev,d,r,ext,extev,falloc nev))
                                 val d'    = (case fdiscrete of 
                                                  SOME f => f (x',ys'',ev'',d)
                                                | NONE => d)
                                 val r'    = fregime (ev'',r)
                             in
                                 Root (x',ys'',evdir,ev'',d',r',ext,extev,h')
                             end)
                           | NONE => 
                             Next (x+h,ys',0,ev',d,r,ext,extev,h'))
                      | Left h'  => 
                        f (Int.+(iter,1)) (x,ys,ev,d,r,ext,extev,h')
                end)
            else raise ConvergenceError
    in
        f 0
    end


fun adaptive_event_solver (stepper,fcond,falloc,n,nev)  =
    let open Real
        val yout = falloc n
        fun f iter (x,ys,ev,ext,extev,h) =
            if (Int.<(iter,10))
            then 
                (let
                    val (ys',e,finterp) = stepper (ext,extev,h,x,ys,yout)
                    val ev' = fixthr (fcond (x+h,ys',ev,ext,extev,falloc n))
                in
                    case predictor tol (h,e) of
                        Right h' => 
                        (case vfoldpi2 adthreshold (ev, ev') of
                             SOME (evdir,evind,evdiff) => 
                             (let
                                 fun fy (theta) = 
                                     let
                                         val ysc = if theta > 0.0 then finterp theta else ys'
                                     in
                                         Array.sub(fcond (x+theta*h, ysc, ev, ext, extev, falloc nev), evind)
                                     end
                                 val y0    = Array.sub(fixthr(fcond (x,ys,ev,ext,extev,falloc nev)), evind)
                                 val theta = secant tol fy y0 1.0 0.0 0
                                 val x'    = x+(theta*h)
                                 val ys''  = if theta > 0.0 then finterp (theta) else ys'
                                 val ev''  = fixthr (fcond (x',ys'',ev,ext,extev,falloc nev))
                             in
                                 Root (x',ys'',evdir,ev'',ext,extev,h')
                             end)
                           | NONE => 
                             Next (x+h,ys',0,ev',ext,extev,h'))
                      | Left h'  => 
                        f (Int.+(iter,1)) (x,ys,ev,ext,extev,h')
                end)
            else raise ConvergenceError
    in
        f 0
    end


fun adaptive_solver (stepper,falloc,n)  =
    let open Real
        val yout = falloc n
        fun f (x,ys,ext,extev,h) =
            (let
                val (ys',e) = stepper (ext,extev,h,x,ys,yout)
            in
                case predictor tol (h,e) of
                    Right h' => 
                    Next (x+h,ys',ext,extev,h')
                  | Left h'  => 
                    f (x,ys,ext,extev,h')
            end)
    in
        f
    end


fun integral (RegimeStepper fstepper,SOME (RegimeCondition fcond),
               SOME (RegimeResponse fpos),fneg,
               fdiscrete,SOME fregime,falloc,n,SOME nev) =
    let
       (*fun fstepper (d,r) = make_stepper (f(d,r))*)
       val fsolver = adaptive_regime_solver (fstepper,fcond,fdiscrete,fregime,falloc,n,nev)
    in
        fn(RegimeState (x,y,e,d,r,ext,extev,h,root)) => 
           (case root of
                true =>
                (let
                    val y' = case fneg of 
                                 SOME (RegimeResponse f) => 
                                 f(x,fpos(x,y,e,d,ext,extev,falloc n),e,d,ext,extev,falloc n)
                               | NONE => 
                                 fpos(x,y,e,ext,extev,d,falloc n)
                               | SOME _ => (putStrLn "FunctionalHybridDynamics3: RegimeState integral response"; 
                                            raise Domain)
                    val  e' = fcond (x,y',e,d,r,ext,extev,falloc nev)
                in
                    RegimeState (x,y',e',d,r,ext,extev,h,false) 
                end)
             | false =>  
                case fsolver (x,y,e,d,r,ext,extev,h) of
                    Next (xn,ysn,_,evn,dn,rn,_,_,hn) => 
                    RegimeState (xn,ysn,evn,dn,rn,ext,extev,hn,false)
                  | Root (xn,ysn,_,evn,dn,rn,ext,extev,hn) => 
                    RegimeState (xn,ysn,evn,dn,rn,ext,extev,hn,true))                          
      | _ => (putStrLn "FunctionalHybridDynamics3: invalid RegimeState"; raise Domain)
    end

  | integral (EventStepper fstepper,SOME (SCondition fcond),
               SOME (SResponse fpos),fneg,
               NONE,NONE,falloc,n,SOME nev) =
    let
       (*fun fstepper (d,r) = make_stepper (f(d,r))*)
       val fsolver = adaptive_event_solver (fstepper,fcond,falloc,n,nev)
    in
        fn(EventState (x,y,e,ext,extev,h,root)) => 
           (case root of
                true => (let
                            val y' = case fneg of 
                                         SOME (SResponse f) => 
                                         f(x,fpos(x,y,e,ext,extev,falloc n),e,ext,extev,falloc n)
                                       | NONE => 
                                         fpos(x,y,e,ext,extev,falloc n)
                                       | SOME _ => 
                                         (putStrLn "FunctionalHybridDynamics3: EventState integral response"; 
                                          raise Domain)
                            val  e' = fcond(x,y',e,ext,extev,falloc nev)
                        in
                            EventState (x,y',e',ext,extev,h,false)
                        end)
               | false => 
                 (case fsolver (x,y,e,ext,extev,h) of
                      Next (xn,ysn,_,evn,ext,extev,hn) => 
                      EventState (xn,ysn,evn,ext,extev,hn,false)
                    | Root (xn,ysn,_,evn,ext,extev,hn) => 
                      EventState (xn,ysn,evn,ext,extev,hn,true)))
      | _ => (putStrLn "FunctionalHybridDynamics3: invalid EventState"; raise Domain)
    end

  | integral (ContStepper fstepper,NONE,fpos,fneg,NONE,NONE,falloc,n,NONE) =
    let
       (*fun fstepper (d,r) = make_stepper (f(d,r))*)
       val fsolver = adaptive_solver (fstepper,falloc,n)
    in
        fn(ContState (x,y,ext,extev,h)) => 
           (case fsolver (x,y,ext,extev,h) of
                Next (xn,ysn,_,_,hn) => 
                ContState (xn,ysn,ext,extev,hn)
              | Root (xn,ysn,_,_,hn) => 
                ContState (xn,ysn,ext,extev,hn))
      | _ => (putStrLn "FunctionalHybridDynamics3: ContState integral response"; 
              raise Domain)
    end

| integral _ =
  (putStrLn "FunctionalHybridDynamics3: unsupported stepper configuration"; 
   raise Domain)

end
