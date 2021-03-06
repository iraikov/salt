(* 

Definitions for numerical simulations of functional hybrid dynamical
systems.  modeling and simulation of systems that can be described by
ordinary differential equations (ODEs) or differential-algebraic
equations (DAEs) with discontinuities that are represented as
conditional equations.

*)

structure FunctionalHybridDynamics =
struct

structure SignalMath =
struct

open Real
open Math
         
val float_eps = 1E~15

fun fpeq (a, b, epsilon) =
  let val a_abs = abs a
      val b_abs = abs b
  in
      abs(a - b) <= (if a_abs > b_abs then b_abs else a_abs) * epsilon
  end

fun fpgt (a, b, epsilon) =
  let val a_abs = abs a
      val b_abs = abs b
  in
      (a - b) > (if a_abs < b_abs then b_abs else a_abs) * epsilon
  end

fun fplt (a, b, epsilon) =
  let val a_abs = abs a
      val b_abs = abs b
  in
      (b - a) > (if a_abs < b_abs then b_abs else a_abs) * epsilon
  end

val signal_sign = sign
val signal_eqnum = fpeq
val signal_neg = op ~
val signal_add = op +
val signal_sub = op -
val signal_mul = op *
val signal_div = op /
val signal_pow = pow

val signal_sin = sin
val signal_cos = cos
val signal_cosh = cosh
val signal_tanh = tanh
val signal_ln = ln
val signal_exp = exp
                      
val signal_max = max
val signal_min = min
fun signal_gt (a, b)  = fpgt(a, b, float_eps)
fun signal_gte (a, b) = fpeq(a, b, float_eps) orelse fpgt(a, b, float_eps)
fun signal_lt (a, b)  = fplt(a, b, float_eps)
fun signal_lte (a, b) = fpeq(a, b, float_eps) orelse fplt(a, b, float_eps)

fun signal_heaviside (x) = 
    if x < 0.0 then 0.0 else 1.0

end

open Real
open Math
open SignalMath

val controller_debug = false
val debug = false

val B = Printf.B       
val I = Printf.I
val R = Printf.R       
val RA = Printf.RSeq Array.foldr       
val ` = Printf.`
val $ = Printf.$

type regime_state  = bool array
type dsc_state     = real array
type event_state   = real array
type cont_state    = real array
type external_state = real array
type externalev_state = real array

val maxiter = 100
val abstol  = ref (SOME (1E~6))
val reltol  = ref (SOME (1E~3))
val maxstep = ref 0.5
    
datatype ('a, 'b) either = Left of 'a | Right of 'b
datatype ('a, 'b, 'c) ternary = Near of 'a | Mid of 'b | Far of 'c

type controller_state' = {h: real, cst: real, r: real, tevent: real}
type controller_state = (controller_state', controller_state') either

fun controller_h (x: controller_state as Left {h,...}) = h
  | controller_h (x: controller_state as Right {h,...}) = h

fun controller_r (x: controller_state as Left {r,...}) = r
  | controller_r (x: controller_state as Right {r,...}) = r

fun controller_cst (x: controller_state as Left {cst,...}) = cst
  | controller_cst (x: controller_state as Right {cst,...}) = cst

fun controller_tevent (x: controller_state as Left {tevent,...}) = tevent
  | controller_tevent (x: controller_state as Right {tevent,...}) = tevent



fun controller_update_h (Left {h,cst,r,tevent},h') =
  (if h' < float_eps
   then raise Fail ("controller_update_h: new h = (" ^ (Real.toString h') ^ ") < float_eps")
   else Left {h=min(h,h'),cst=cst,r=r,tevent=tevent})
  | controller_update_h (Right {h,cst,r,tevent},h') = 
    (if h' < float_eps
     then raise Fail ("controller_update_h: new h (" ^ (Real.toString h') ^ ") < float_eps")
     else Right {h=min(h,h'),cst=cst,r=r,tevent=tevent})



exception ConvergenceError

datatype model_root = RootBefore | RootFound of int * real list | RootAfter of int * real list | RootStep of real list 

fun showRoot (RootFound (i,lst)) = ("RootFound " ^ (Int.toString i) ^ " " ^ (String.concatWith ", " (map Real.toString lst)))
  | showRoot (RootStep lst)  = ("RootStep " ^ (String.concatWith ", " (map Real.toString lst)))
  | showRoot (RootAfter (i,lst))  = ("RootAfter " ^ (Int.toString i) ^ " " ^ (String.concatWith ", " (map Real.toString lst)))
  | showRoot RootBefore    = "RootBefore"
                             
datatype model_state = 
         RegimeState of real * real * cont_state * event_state * dsc_state * regime_state * external_state * externalev_state * 
                        cont_state * cont_state * event_state * controller_state * model_root
         | EventState of real * real * cont_state * event_state * external_state * externalev_state * 
                         cont_state * cont_state * event_state * controller_state * model_root
         | ContState  of real * real * cont_state * external_state * externalev_state * cont_state * controller_state


datatype model_stepper = 
         RegimeStepper of (dsc_state * regime_state * external_state * externalev_state *
                           real * real * real * real * cont_state * cont_state) -> 
                          (cont_state * cont_state * (real array) list)
         | EventStepper of (external_state * externalev_state * real * real * real * real * cont_state * cont_state) -> 
                           (cont_state * cont_state * (real array) list)
         | ContStepper of (external_state * externalev_state * real * real * real * real * cont_state * cont_state) -> 
                          (cont_state * cont_state * (real array) list)

datatype model_condition = 
         RegimeCondition of (real * cont_state * event_state * dsc_state * regime_state * external_state * externalev_state * event_state) -> event_state
         | SCondition of (real * cont_state * event_state * external_state * externalev_state * event_state) -> event_state

datatype model_response = 
         RegimeResponse of (real * cont_state * event_state * dsc_state * regime_state * external_state * externalev_state * cont_state) -> cont_state
         | SResponse of (real * cont_state * event_state * external_state * externalev_state * cont_state) -> cont_state

val getindex = Unsafe.Array.sub
val update = Unsafe.Array.update

val vfind = Array.find
val vfindi = Array.findi

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

fun vfoldi2 f init (v1,v2) = 
    let 
        val n = Array.length v1
        fun recur (i, ax) = 
            if Int.<(i, n)
            then recur (Int.+(i,1), f (i, getindex (v1,i), getindex (v2,i), ax))
            else ax
    in
      recur (0, init)
    end 
        
        
fun controller_update_event (st, ev, ev') =
  let
      val tevent'  = vfoldi2 (fn(i,a,b,ax) => let val d = a-b
                                              in
                                                  if d > 0.0 then min(abs(a-b),ax) else ax
                                              end)
                             posInf (ev, ev')
      val h  = controller_h st
      val h' = max(min(0.5*tevent',h),float_eps)
      val _ = if debug then Printf.printf `"controller_update_event: tevent' = "R `" h' = "R `"\n" $ tevent' h' else ()
  in
      case st of
          Left {h,cst,r,tevent} =>
          Left{h=h',cst=cst,r=r,tevent=tevent'}
        | Right {h,cst,r,tevent} =>
          Right{h=h',cst=cst,r=r,tevent=tevent'}
  end

fun error_norm (xs) =
  let
      val n = Array.length xs
  in
      if Int.>(n, 0)
      then Math.sqrt((Array.foldl (fn (e,ax) => e*e + ax) 0.0 xs)/(Real.fromInt n))
      else 0.0
  end
      
fun controller (abstol,reltol) (h,ys,yps,es,prev) =
  let
      val safety = 0.9
      val p0     = ~0.25
      val p1     = ~0.2
      val fmax   = 2.0
      val fmin   = 0.1
      val cerr   = error_norm es
      val (cst_prev, h_prev, r_prev, tevent_prev) =
          case prev of
              Left {h=h_prev, cst=cst_prev, r=r_prev, tevent=tevent_prev}  => (cst_prev, h_prev, r_prev, tevent_prev)
           |  Right {h=h_prev, cst=cst_prev, r=r_prev, tevent=tevent_prev} => (cst_prev, h_prev, r_prev, tevent_prev)
  in
      if cerr > 1.0
      then
          let (* step rejected *)
              val cst_next  = safety * Math.pow(cerr, p0)
              val ratio     = if h_prev > float_eps
                              then max(cst_next, fmin)
                              else min(cst_next, fmin)
              val h_next    = max(min(ratio * h_prev, (!maxstep)), float_eps)
              val cerr_prev = cerr
                                  
              val _ = if controller_debug
                      then Printf.printf `"controller: step rejected: cerr = "R `" ratio = "R `" h_next = "R  `"\n" $ cerr ratio h_next
                      else ()
          in
              Left {h=h_next, cst=cerr_prev, r=cerr, tevent=tevent_prev}
          end
      else 
          (* step accepted *)
          (let
              val k      = Math.pow (5.0 / safety, 1.0 / p1)
              val ratio  = if cerr > k then safety * Math.pow(cerr, p1) else fmax
              val h_next = max(min(ratio * h_prev, (!maxstep)), float_eps)
              val cerr_prev = cerr

              val _ = if controller_debug
                      then Printf.printf `"controller: step accepted: abstol = "R `" cerr = "R `" ratio = "R `" h_next = "R  `"\n" $ abstol cerr ratio h_next
                      else ()
          in
              Right {h=h_next, cst=cerr_prev, r=cerr, tevent=tevent_prev}
          end)
  end


      
(* Compensated summation *)
fun csum (x, cx, h) =
  let
      val dx  = h + cx
      val x'  = x + dx
      val cx' = (x - x') + dx
  in
      (x', cx')
  end

      

fun thr2 (i,v1,v2) = 
    let
        val s1 = Real.sign v1
        val s2 = Real.sign v2
    in
        case (s1,s2) of
            (~1,1) =>
            if Real.>(Real.abs(v1), float_eps)
            then (SOME (Mid i)) else SOME (Near i)
          | (~1,0) =>
            SOME (Far i)
          | (0,1)  =>
            SOME (Near i)
          | _      => NONE
    end

        
fun fixthr_s v =
  if Real.>(Real.abs(v), float_eps) then v else 0.0
                                                
fun fixthr v =
    (Array.modify (fn(x) => if Real.>(Real.abs(x), float_eps) then x else 0.0) v; v)

fun resthr (v, i) = (Array.update (v, i, 0.0); v)

        
fun posdetect (x, e, x', e', excl) =
  case excl of
      NONE => 
      if fpgt(x', x, float_eps)
      then vfoldi2 (fn(i,v1,v2,lst) =>
                       case thr2 (i,v1,v2) of
                           SOME t => (t,v1,v2)::lst
                         | NONE   => lst) []
                   (e, e')
      else []
    | SOME iexcl =>
      vfoldi2 (fn(i,v1,v2,lst) =>
                  if not (i=iexcl)
                  then (case thr2 (i,v1,v2) of
                            SOME t => (t,v1,v2)::lst
                          | NONE   => lst)
                  else lst) 
              [] (e, e')
                

         
      
fun condApply (SOME (RegimeCondition fcond)) =
  (fn(RegimeState (x,cx,y,e,d,r,ext,extev,ynext,yrsp,enext,err,root)) => 
        let val e' = fixthr (fcond (x,y,e,d,r,ext,extev,enext))
        in
            RegimeState (x,cx,y,e',d,r,ext,extev,ynext,yrsp,e,err,root)
        end
    | _ => raise Fail "condApply")
  | condApply (SOME (SCondition fcond)) =
    (fn(EventState(x,cx,y,e,ext,extev,ynext,yrsp,enext,err,root)) =>     
        let val e' = fixthr (fcond (x,y,e,ext,extev,enext))
        in
            EventState(x,cx,y,e',ext,extev,ynext,yrsp,e,err,root)
        end
    | _ => raise Fail "condApply")
  | condApply (NONE) = raise Fail "condApply"
                                   
fun evresponse_regime (fpos,fneg,fdiscrete,fregime) =
  fn(i,x,y,e,d,r,ext,extev,yrsp) =>
     let
         val y'  = case fpos of 
                       SOME (RegimeResponse f) =>
                       f (x,y,e,d,r,ext,extev,yrsp)
                     | NONE => (Array.copy {src=y, dst=yrsp, di=0}; yrsp)
                     | _ => raise Fail "evresponse_regime: RegimeState integral response"
         val y'' = case fneg of 
                       NONE => y'
                     | SOME (RegimeResponse f) => f (x,y',e,d,r,ext,extev,yrsp)
                     | _ => raise Fail "evresponse_regime: RegimeState integral response"
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
        (fn(i,x,y,e,ext,extev,yrsp) =>
            let 
                val y' = case fneg of 
                             NONE => fpos(x,y,e,ext,extev,yrsp)
                           | SOME (SResponse f) => f (x,fpos(x,y,e,ext,extev,yrsp),e,ext,extev,yrsp)
                           | _ => raise Fail "evresponse: EventState integral response"
            in
                y'
            end)
      | _ => raise Fail "evresponse: unsupported event response configuration"
                   

fun adaptive_regime_stepper stepper =
  let
      val tiny = 1.0E~30
      fun f iter (d,r,ext,extev,h,x,ys,yout,cst) =
        if Int.<(iter,maxiter)
        then
            (let
                val (ys',err,w) = stepper (d,r,ext,extev,h,getOpt(!reltol,1.0),getOpt(!abstol,1.0),x,ys,yout)
            in
                case !abstol of
                    SOME tolv =>
                    let
                        val cst' = controller (tolv, getOpt(!reltol,1.0)) (h,ys',w,err,cst)
                    in
                        case cst' of
                            Right {h=h',...} => 
                            (ys',h',cst',w)
                          | Left {h=h',...}  => 
                            f (Int.+(iter,1)) (d,r,ext,extev,h',x,ys,yout,cst')
                    end
                  | NONE => (ys',h,cst,w)
                end)
        else raise Fail "convergence error in adaptive regime stepper"
    in
        f 0
    end

        
fun adaptive_stepper stepper =
    let
        fun f iter (ext,extev,h,x,ys,yout,cst) =
            if Int.<(iter,maxiter)
            then 
                (let
                    val (ys',err,w) = stepper (ext,extev,h,getOpt(!abstol,1.0),getOpt(!reltol,1.0),x,ys,yout)
                in
                    case !abstol of
                        SOME tolv =>
                        let
                            val cst' = controller (tolv, getOpt(!reltol,1.0)) (h,ys',w,err,cst)
                        in
                            case cst' of
                                Right {h=h',...} => 
                                (ys',h',cst',w)
                              | Left {h=h',...}  => 
                                f (Int.+(iter,1)) (ext,extev,h',x,ys,yout,cst')
                        end
                     |  NONE =>
                        (ys',h,cst,w)
                end)
            else raise Fail "convergence error in adaptive stepper"
    in
        f 0
    end
        
fun regime_rootval (finterp,fcond) =
  fn (h,w,x,cx,y,e,x',cx',y',e',ext,extev,d,r,enext,excl) =>
     case posdetect (x,e,x',e',excl) of
        [] => NONE
      | lst =>
        foldl (fn((t,_,_),ax) =>
                  (case t of
                       Near i => (* threshold crossing is at first time point *)
                       SOME (t, x, cx, 0.0, y)
                     | Far i =>  (* threshold crossing is at second time point *)
                       (case ax of
                            NONE   => SOME (t, x', cx', 1.0, y')
                          | SOME _ => ax)
                     | Mid i => 
                       (let
                           val finterp' = finterp (h,w,x,y)
                           fun evtest (theta) =
                             let
                                 val (e_x,_) = csum(x,cx,theta*h)
                                 val e_y = finterp' theta
                                 val res = getindex(fcond(e_x,e_y,e,d,r,ext,extev,enext), i)
                                 val _ = if debug
                                         then Printf.printf `"RootStep.evtest: theta = "R
                                                            `" x = "R `" e_x = "R `" e_y = "RA `" res = "R
                                                            `"\n" $ theta x e_x e_y res
                                         else ()
                             in
                                 res
                             end
                           val theta = FindRoot.brent float_eps evtest 0.0 1.0
                           val (xinterp, cxinterp) = csum(x,cx,theta*h)
                       in
                           case ax of
                               NONE => SOME (t, xinterp, cxinterp, theta, finterp' theta)
                            |  SOME (_,xinterp',_,_,_) =>
                               if xinterp < xinterp'
                               then SOME (t, xinterp, cxinterp, theta, finterp' theta)
                               else ax
                       end)))
              NONE lst

        
fun event_rootval (finterp,fcond) =
  fn (h,w,x,cx,y,e,x',cx',y',e',ext,extev,enext,excl) =>
     case posdetect (x,e,x',e',excl) of
        [] => NONE
      | lst =>
        foldl (fn((t,_,_),ax) =>
                  (case t of
                       Near i => (* threshold crossing is at first time point *)
                       SOME (t, x, cx, 0.0, y)
                     | Far i =>  (* threshold crossing is at second time point *)
                       (case ax of
                            NONE   => SOME (t, x', cx', 1.0, y')
                          | SOME _ => ax)
                     | Mid i => 
                       (let
                           val finterp' = finterp (h,w,x,y)
                           fun evtest (theta) =
                             let
                                 val (e_x,_) = csum(x,cx,theta*h)
                                 val e_y = finterp' theta
                                 val res = getindex(fcond(e_x,e_y,e,ext,extev,enext), i)
                                 val _ = if debug
                                         then Printf.printf `"RootStep.evtest: theta = "R
                                                            `" x = "R `" e_x = "R `" e_y = "RA `" res = "R
                                                            `"\n" $ theta x e_x e_y res
                                         else ()
                             in
                                 res
                             end
                           val theta = FindRoot.brent float_eps evtest 0.0 1.0
                           val (xinterp, cxinterp) = csum(x,cx,theta*h)
                       in
                           case ax of
                               NONE => SOME (t, xinterp, cxinterp, theta, finterp' theta)
                            |  SOME (_,xinterp',_,_,_) =>
                               if xinterp < xinterp'
                               then SOME (t, xinterp, cxinterp, theta, finterp' theta)
                               else ax
                       end)))
              NONE lst


                   
fun subtract_h (h, h1::hs) =
  if h1>h then (max((~ h)+h1,float_eps)::hs) else subtract_h ((~ h1)+h, hs)
  | subtract_h (h, hs) = hs
  
fun integral (RegimeStepper stepper,finterp,SOME (RegimeCondition fcond),                          
              fpos,fneg,fdiscrete,SOME fregime) =
  let
      val fstepper = adaptive_regime_stepper stepper
      val frootval = regime_rootval (finterp,fcond)
                                    
      fun integral' (RegimeState (x,cx,y,e,d,r,ext,extev,ynext,yrsp,enext,cst,root)) =
        (if debug
         then (if y = ynext then raise Fail ("Dynamics.integral: RegimeState: y and ynext are the same") else ();
               if y = yrsp then raise Fail ("Dynamics.integral: RegimeState: y and yrsp are the same") else ())
         else ();
         if ((controller_h cst) < float_eps)
         then raise Fail ("Dynamics.integral: RegimeState: zero time step (root=" ^ (showRoot root) ^ ")")
         else ();
         case root of
             RootBefore =>
             let
                 val e'  = fixthr (fcond (x,y,e,d,r,ext,extev,enext))
                 val _ = if debug
                         then Printf.printf `"RootBefore: x = "R `" e'[0] = "R`"\n" $ x (getindex(e',0))
                         else ()
                 val h = controller_h cst
             in
                 RegimeState(x,cx,y,e',d,r,ext,extev,ynext,yrsp,e,cst,RootStep [h])
             end
           | RootStep (h::hs) =>
             let
                 val _ = if debug
                         then Printf.printf `"RootStep: h = "R `" x = "R `" y = "R `" ynext = "R `"\n" $ h x (getindex(y,0)) (getindex(ynext,0))
                         else ()
                 val (x',cx')  = csum (x,cx,h) 
                 val (y',h',cst',w) = fstepper (d,r,ext,extev,h,x,y,ynext,cst)
                 val e'  = fixthr (fcond (x',finterp (h,w,x,y) 1.0,e,d,r,ext,extev,enext))
                 (*val e'  = fixthr (fcond (x',y',e,d,r,ext,extev,enext))*)
                 val _ = if debug
                         then Printf.printf `"RootStep: h' = "R `" x = "R `" y[0] = "R `" x' = "R `" y'[0] = "R `" e[0] = "R `" e'[0] = "R `"\n" $ h' x (getindex(y,0)) x' (getindex(y',0)) (getindex(e,0)) (getindex(e',0))
                         else ()
                 val rootval = frootval (h,w,x,cx,y,e,x',cx',y',e',ext,extev,d,r,enext,NONE)
                     
             in
                 case rootval of
                     SOME (Near i,e_x,e_cx,e_theta,e_y) =>
                     (if debug
                      then Printf.printf `"RootStep: Near: " `" x = "R 
                                         `" y' = "R
                                         `"\n" $ x (getindex(y',0))
                      else ();
                      RegimeState(x,cx,y,e',d,r,ext,extev,ynext,yrsp,e,cst,RootFound (i,h::hs)))
                   | SOME (Far i,e_x,e_cx,e_theta,e_y) =>
                     (if debug
                      then Printf.printf `"RootStep Far: x = "R 
                                         `" y' = "R
                                         `"\n" $ x (getindex(y',0))
                      else ();
                      RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,cst',RootFound (i,hs)))
                   | SOME (Mid i,e_x,e_cx,e_theta,e_y) =>
                     (if x' > e_x
                       then
                           let
                               val x''  = e_x
                               val cx'' = e_cx
                               val y''  = y'
                               val _    = Array.copy {src=e_y, dst=y'', di=0}
                               val h''  = e_theta*h
                               val e''  = resthr (fcond (x'',y'',e,d,r,ext,extev,enext),i)
                               val _ = if debug
                                       then Printf.printf `"RootStep: Mid: x' = "R `" x'' = "R 
                                                          `" h' = "R `" h'' = "R `" y'' = "R `" y = "R `" e'' = "R 
                                                          `"\n" $ x' x'' h' h'' (getindex(y'',0)) (getindex(y,0)) (getindex(e'',0))
                                       else ()
                           in
                               RegimeState(x'',cx'',y'',e'',d,r,ext,extev,y,yrsp,e,cst',
                                           RootFound (i,subtract_h (h'', h::hs)))
                           end
                       else RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,cst',RootFound (i,hs)))
                   | NONE => (case hs of
                                  [] => RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,cst',RootBefore)
                                | _ => RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,cst',RootStep hs))
             end
           | RootFound (i,hs) =>
             let
                 val (y',d',r') = evresponse_regime (fpos,fneg,fdiscrete,fregime) 
                                                    (i,x,y,e,d,r,ext,extev,yrsp)
                 val _ = if debug
                         then Printf.printf `"RootFound: x = "R `" e = "R `" y = "R` " y' = "R `" d = "R` " d' = "R `" r = "B` " r' = "B `"\n" $ x (getindex(e,0)) (getindex(y,0)) (getindex(y',0)) (getindex(d,0)) (getindex(d',0)) (getindex(r,0)) (getindex(r',0))
                         else ()
                 val cst'  = controller_update_event (cst, enext, e)
             in
                 RegimeState(x,cx,y',e,d',r',ext,extev,y,ynext,enext,cst',RootAfter (i,hs))
             end
           | RootAfter (ei,hs) =>
             let
                 val hev        = Real.*(0.5,float_eps)
                 val (x',cx')   = csum (x,cx,hev)
                 val (y',_,_,w) = fstepper (d,r,ext,extev,hev,x,y,ynext,cst)
                 val e'  = fixthr (fcond (x',y',e,d,r,ext,extev,enext))
                 val _ = if debug
                         then Printf.printf `"RootAfter: x = "R `" y = "R `" e = "R `" x' = "R `" y' = "R  `" e' = "R `"\n" $ x (getindex(y,0)) (getindex(e,0)) x' (getindex(y',0)) (getindex(e',0))
                         else ()
                 val rootval = frootval (hev,w,x,cx,y,e,x',cx',y',e',ext,extev,d,r,enext,SOME ei)
             in
                 case rootval of
                     NONE =>
                     (if debug
                      then Printf.printf `"RootAfter: rootval is none: x = "R `"\n" $ x
                      else ();
                      case hs of
                          h1::hs => RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,cst,RootStep (subtract_h (hev, h1::hs)))
                        | [] => RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,cst,RootBefore))
                   | SOME (Near i,e_x,e_cx,e_theta,e_y) =>
                     (if debug
                      then Printf.printf `"RootAfter: Near: " `" x = "R 
                                         `" y' = "R
                                         `"\n" $ x (getindex(y',0))
                      else ();
                      RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,cst,RootFound (i,subtract_h (hev, hs))))
                   | SOME (Far i,e_x,e_cx,e_theta,e_y) =>
                     (if debug
                      then Printf.printf `"RootAfter Far: x = "R 
                                         `" y' = "R
                                         `"\n" $ x (getindex(y',0))
                      else ();
                      (case hs of
                           h1::hs => RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,cst,
                                                 RootFound (i,subtract_h (hev, h1::hs)))
                         | [] => RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,cst,RootBefore)))
                   | SOME (Mid i,e_x,e_cx,e_theta,e_y) =>
                     (if x' > e_x
                      then
                          let
                              val x''  = e_x
                              val cx'' = e_cx
                              val y''  = y'
                              val _    = Array.copy {src=e_y, dst=y'', di=0}
                              val h''  = e_theta*hev
                              val e''  = resthr (fcond (x'',y'',e,d,r,ext,extev,enext),i)                              
                              val _ = if debug
                                      then Printf.printf `"RootAfter: Mid: x' = "R `" x'' = "R 
                                                         `" h'' = "R `" y'' = "R `" y = "R `" e'' = "R 
                                                         `"\n" $ x' x'' h'' (getindex(y'',0)) (getindex(y,0)) (getindex(e'',0))
                                      else ()
                          in
                              case hs of
                                  h1::hs => 
                                  RegimeState(x'',cx'',y'',e'',d,r,ext,extev,y,yrsp,e,cst,
                                              RootFound (i,subtract_h (h'', h1::hs)))
                                | [] =>
                                  RegimeState(x'',cx'',y'',e'',d,r,ext,extev,y,yrsp,e,cst,
                                              RootFound (i,[]))
                          end
                      else (case hs of
                                h1::hs => RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,cst,
                                                      RootFound (i,subtract_h (hev, h1::hs)))
                              | [] => RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,cst,
                                                  RootFound(i,[]))))
                                                     
             end
           | _ => raise Fail "integral: invalid arguments to regime stepper")
            
        | integral' _ = raise Fail "integral: invalid RegimeState"
  in
      integral'
  end
               
  | integral (EventStepper stepper,finterp,SOME (SCondition fcond),fpos,fneg,NONE,NONE) =
    let
        val fstepper = adaptive_stepper stepper
        val frootval = event_rootval (finterp,fcond)

        fun integral' (EventState(x,cx,y,e,ext,extev,ynext,yrsp,enext,cst,root)) =
          (if ((controller_h cst) < float_eps)
           then raise Fail ("Dynamics.integral: EventState: time step too small (root=" ^ (showRoot root) ^ ")")
           else ();
           case root of
               RootBefore =>
               let
                   val e' = fixthr (fcond (x,y,e,ext,extev,enext))
                   val h  = controller_h cst 
               in
                   EventState(x,cx,y,e',ext,extev,ynext,yrsp,e,cst,RootStep [h])
               end
             | RootStep (h::hs) =>
               let
                   val _ = if debug
                           then Printf.printf `"RootStep: h = "R `" x = "R `" y = "R `" ynext = "R `"\n" $ h x (getindex(y,0)) (getindex(ynext,0))
                           else ()
                   val (x',cx')  = csum (x,cx,h)
                   val (y',h',cst',w) = fstepper (ext,extev,h,x,y,ynext,cst)
                   val e'  = fixthr (fcond (x',finterp (h,w,x,y) 1.0,e,ext,extev,enext))
                   (*val e'  = fixthr (fcond (x',y',e,ext,extev,enext))*)
                   val rootval = frootval (h,w,x,cx,y,e,x',cx',y',e',ext,extev,enext,NONE)

               in
                 case rootval of
                     SOME (Near i,e_x,e_cx,e_theta,e_y) =>
                     EventState(x,cx,y,e',ext,extev,y,yrsp,e,cst,RootFound (i,h::hs))
                   | SOME (Far i,e_x,e_cx,e_theta,e_y) =>
                     EventState(x',cx',y',e',ext,extev,y,yrsp,e,cst,RootFound (i,hs))
                   | SOME (Mid i,e_x,e_cx,e_theta,e_y) =>
                     (let
                         val x''  = e_x
                         val cx'' = e_cx
                         val y''  = y'
                         val _    = Array.copy {src=e_y, dst=y'', di=0}
                         val h''  = e_theta*h
                         val e''  = resthr (fcond (x'',y'',e,ext,extev,enext),i)
                         val _ = if debug
                                 then Printf.printf `"RootStep: rootval: x' = "R `" x'' = "R 
                                                    `" h'' = "R `" y'' = "R `" e'' = "R 
                                                       `"\n" $ x' x'' h'' (getindex(y'',0)) (getindex(e'',0))
                                 else ()
                     in
                         EventState(x'',cx'',y'',e'',ext,extev,y,yrsp,e,cst',
                                    RootFound (i,subtract_h (h'', h::hs)))
                     end)
                   | NONE =>
                     (case hs of
                          [] => EventState(x',cx',y',e',ext,extev,y,yrsp,e,cst',RootBefore)
                        | _  => EventState(x',cx',y',e',ext,extev,y,yrsp,e,cst',RootStep hs))
               end
             | RootFound (i,hs) =>
               let
                   val y' = evresponse (fpos,fneg) (i,x,y,e,ext,extev,yrsp)
                   val _   = if debug
                             then Printf.printf `"RootFound: x = "R `" e = "R `" y' = "R `"\n" $ x (getindex(e,0)) (getindex(y',0))
                             else ()
                   val cst'  = controller_update_event (cst, enext, e)
               in
                   EventState(x,cx,y',e,ext,extev,y,ynext,enext,cst',RootAfter (i,hs))
               end
             | RootAfter (ei,hs) =>
               let
                   val hev       = Real.*(0.5,float_eps)
                   val (x',cx')  = csum (x,cx,hev)
                   val (y',_,_,w)  = fstepper (ext,extev,hev,x,y,ynext,cst)
                   val e'  = fixthr (fcond (x',y',e,ext,extev,enext))
                   val _   = if debug
                             then Printf.printf `"RootAfter: x' = "R `" e' = "R
                                                `" y' = "R `"\n" $ x' (getindex(e',0)) (getindex(y',0))
                             else ()
                   val rootval = frootval (hev,w,x,cx,y,e,x',cx',y',e',ext,extev,enext,SOME ei)
               in
                   case rootval of
                     SOME (Near i,e_x,e_cx,e_theta,e_y) =>
                     EventState(x',cx',y',e',ext,extev,ynext,yrsp,e,cst,RootFound (i,subtract_h (hev, hs)))
                    | SOME (Far i,e_x,e_cx,e_theta,e_y) =>
                      (case hs of
                           h1::_ =>
                           EventState(x',cx',y',e',ext,extev,y,yrsp,e,cst,RootFound (i,subtract_h (hev, h1::hs)))
                         | [] =>
                           EventState(x',cx',y',e',ext,extev,y,yrsp,e,cst,RootFound (i,[])))
                   | SOME (Mid i,e_x,e_cx,e_theta,e_y) =>
                     (let
                         val x''  = e_x
                         val cx'' = e_cx
                         val y''  = y'
                         val _    = Array.copy {src=e_y, dst=y'', di=0}
                         val h''  = e_theta*hev
                         val e''  = resthr (fcond (x'',y'',e,ext,extev,enext),i)
                         val _ = if debug
                                 then Printf.printf `"RootStep: rootval: x' = "R `" x'' = "R 
                                                    `" h'' = "R `" y'' = "R
                                                       `"\n" $ x' x'' h'' (getindex(y'',0))
                                 else ()
                     in
                         case hs of
                             h1::hs =>
                             EventState(x'',cx'',y'',e'',ext,extev,y,yrsp,e,cst,
                                        RootFound (i,subtract_h (h'', h1::hs)))
                          |  [] =>
                             EventState(x'',cx'',y'',e'',ext,extev,y,yrsp,e,cst,
                                        RootFound (i,[]))
                     end)
                   | NONE =>
                     case hs of
                         h1::_ => EventState(x',cx',y',e',ext,extev,y,yrsp,e,cst,
                                             RootStep (subtract_h (hev, h1::hs)))
                      |  [] => EventState(x',cx',y',e',ext,extev,y,yrsp,e,cst,
                                          RootBefore)
                                            
               end
             |  _ => raise Fail "integral: invalid arguments to event stepper")
          | integral' _ =
            raise Fail "integral: invalid event state"
    in
        integral'
    end
        
  | integral (ContStepper stepper,finterp,NONE,fpos,fneg,NONE,NONE) =
    let
          val fstepper = adaptive_stepper stepper
    in
        (fn(ContState(x,cx,y,ext,extev,ynext,cst)) => 
            let val h = controller_h cst
                val (x',cx')  = csum(x,cx,h)
                val (y',h',cst',w) = fstepper (ext,extev,h,x,y,ynext,cst)
            in
                ContState(x',cx',y',ext,extev,y,cst')
            end
        | _ => raise Fail "integral: invalid continuous state"
        )
    end
| integral _ =
  raise Fail "integral: unsupported stepper configuration"
         

end
