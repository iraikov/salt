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

val controller_debug = false
val debug = false
val trace = false

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

val maxiter = 10
val float_eps = 1E~12
val tol     = ref (SOME (1E~10))
val maxstep = ref 2.0
    
datatype ('a, 'b) either = Left of 'a | Right of 'b
datatype ('a, 'b, 'c) ternary = Near of 'a | Mid of 'b | Far of 'c

type controller_state = ((real * real * real), (real * real * real)) either

fun controller_h (Left (h,_,_)) = h
  | controller_h (Right (h,_,_)) = h

fun controller_r (Left (_,_,r)) = r
  | controller_r (Right (_,_,r)) = r

fun controller_cst (Left (_,cst,_)) = cst
  | controller_cst (Right (_,cst,_)) = cst

fun controller_update_h (Left (h,cst,r),h') =
  (if h' < float_eps
   then raise Fail ("controller_update_h: new h = (" ^ (Real.toString h') ^ ") < float_eps")
   else Left(h',cst,r))
  | controller_update_h (Right (h,cst,r),h') = 
    (if h' < float_eps
     then raise Fail ("controller_update_h: new h (" ^ (Real.toString h') ^ ") < float_eps")
     else Right(h',cst,r))

                                                                    
(* Stepsize controller from Gustafsson 1991. *)

fun controller tol (h,ys,prev) =
  let open Real
      val k   = 0.87
      val ki  = 0.08
      val kp  = 0.1
      val f   = 1.2
      val r   = (Array.foldl (fn (y,ax) => (abs y) + ax) 0.0 ys) / h
      val est = r / tol
      val _ = if controller_debug
              then Printf.printf `"controller: tol = "R `" est = "R
                                 `" r = "R `" h = "R `"\n" $ tol est r h
              else ()
      val _ = if h <= 0.0 then raise Fail "controller: zero time step" else ()
  in 
      if est <= 1.0
      then (* current step accepted *)
          (if h < (controller_h prev) 
           then prev
           else 
               (let val cst_next =
                        case prev of
                            Left (h_prev, cst_prev, r_prev) => (* if previous step rejected, restart *)
                            (if h > 0.0 andalso h <= (controller_h prev)
                             then h * (h / cst_prev)
                             else h_prev * (h_prev / cst_prev))
                         |  Right (h_prev, cst_prev, r_prev) =>
                            if (r > float_eps) andalso (r_prev > float_eps)
                            then (Math.pow (tol / r, ki)) * (Math.pow (r_prev / r, kp)) * cst_prev
                            else (if cst_prev > 0.0 then f*cst_prev else controller_h prev)
                    val _ = if cst_next <= 0.0 orelse (not (Real.isFinite(cst_next)))
                            then raise Fail ("controller: next state is zero or not finite; cst_next = " ^ (Real.toString cst_next) ^
                                             " h = " ^ (Real.toString h) ^
                                             " prev h = " ^ (Real.toString (controller_h prev)) ^
                                             " r = " ^ (Real.toString r) ^
                                             " r_prev = " ^ (Real.toString (controller_r prev)) ^
                                             " cst_prev = " ^ (Real.toString (controller_cst prev)))
                            else ()
                    val h_next = min(cst_next, !maxstep)
                    val _ = if controller_debug
                            then Printf.printf `"controller: cst_next = "R `" h_next = "R `" h_prev = "R `"\n" $ cst_next h_next (controller_h prev)
                            else ()
                    val r_prev = r
                in
                    Right (h_next, cst_next, r_prev)
                end))
      else (* step rejected *)
          (let
              val (cst_prev, h_prev) =
                  case prev of
                      Left (h_prev, cst_prev, r_prev)  => (cst_prev, h_prev)
                   |  Right (h_prev, cst_prev, r_prev) => (cst_prev, h_prev)
                                                                               
              val h_next = (Math.pow (tol / r, 1.0 / k)) * h_prev
          in
              Left (h_next, cst_prev, r)
          end)
  end


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
         RegimeStepper of dsc_state * regime_state * external_state * externalev_state *
                          real * real * cont_state * cont_state -> 
                          (cont_state * cont_state * (real array) FunQueue.t)
         | EventStepper of (external_state * externalev_state * real * real * cont_state * cont_state) -> 
                           (cont_state * cont_state * (real array) FunQueue.t)
         | ContStepper of (external_state * externalev_state * real * real * cont_state * cont_state) -> 
                          (cont_state * cont_state * (real array) FunQueue.t)

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
            (SOME (Mid i))
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

        
fun posdetect (x, e, x', e') =
  if x'-x >= float_eps
  then vfoldi2 (fn(i,v1,v2,lst) =>
                   case thr2 (i,v1,v2) of
                       SOME t => (t,v1,v2)::lst
                     | NONE   => lst) []
               (e, e')
  else []

         
      
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
                       f (x,y,e,d,ext,extev,yrsp)
                     | NONE => (Array.copy {src=y, dst=yrsp, di=0}; yrsp)
                     | _ => raise Fail "evresponse_regime: RegimeState integral response"
         val y'' = case fneg of 
                       NONE => y'
                     | SOME (RegimeResponse f) => f (x,y',e,d,ext,extev,yrsp)
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
                   

fun adaptive_regime_stepper stepper  =
  let open Real
        fun f iter (d,r,ext,extev,h,x,ys,yout,cst) =
            if Int.<(iter,maxiter)
            then
                (let
                    val (ys',err,w) = stepper (d,r,ext,extev,h,x,ys,yout)
                in
                    case !tol of
                        SOME tolv =>
                        let
                            val cst' = controller tolv (h,err,cst)
                        in
                            case cst' of
                                Right (h',_,_) => 
                                (ys',h',cst',w)
                              | Left (h',_,_)  => 
                                f (Int.+(iter,1)) (d,r,ext,extev,h',x,ys,yout,cst')
                        end
                     | NONE => (ys',h,cst,w)
                end)
            else raise Fail "convergence error in adaptive regime stepper"
    in
        f 0
    end

        
fun adaptive_stepper stepper =
    let open Real
        fun f iter (ext,extev,h,x,ys,yout,cst) =
            if Int.<(iter,maxiter)
            then 
                (let
                    val (ys',err,w) = stepper (ext,extev,h,x,ys,yout)
                in
                    case !tol of
                        SOME tolv =>
                        let
                            val cst' = controller tolv (h,err,cst)
                        in
                            case cst' of
                                Right (h',_,_) => 
                                (ys',h',cst',w)
                              | Left (h',_,_)  => 
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
  fn (h,w,x,cx,y,e,x',cx',y',e',ext,extev,d,r,enext) =>
     case posdetect (x,e,x',e') of
        [] => NONE
      | lst =>
        foldl (fn((t,_,_),ax) =>
                  (case t of
                       Near i => (* threshold crossing is at first time point *)
                       SOME (t, x, cx, 0.0, y)
                     | Far i =>  (* threshold crossing is at second time point *)
                       SOME (t, x', cx', 1.0, y')
                     | Mid i => 
                       (let
                           val finterp' = finterp (h,w,x,y)
                           fun evtest (theta) =
                             let
                                 val (e_x,_) = csum(x,cx,theta*h)
                                 val e_y = finterp' theta
                                 val res = fixthr_s (getindex(fcond(e_x,e_y,e,d,r,ext,extev,enext), i))
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
  fn (h,w,x,cx,y,e,x',cx',y',e',ext,extev,enext) =>
     case posdetect (x,e,x',e') of
        [] => NONE
      | lst =>
        foldl (fn((t,_,_),ax) =>
                  (case t of
                       Near i => (* threshold crossing is at first time point *)
                       SOME (t, x, cx, 0.0, y)
                     | Far i =>  (* threshold crossing is at second time point *)
                       SOME (t, x', cx', 1.0, y')
                     | Mid i => 
                       (let
                           val finterp' = finterp (h,w,x,y)
                           fun evtest (theta) =
                             let
                                 val (e_x,_) = csum(x,cx,theta*h)
                                 val e_y = finterp' theta
                                 val res = fixthr_s (getindex(fcond(e_x,e_y,e,ext,extev,enext), i))
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
         if not ((controller_h cst) > 0.0)
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
                 val rootval = frootval (h,w,x,cx,y,e,x',cx',y',e',ext,extev,d,r,enext)
                     
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
                               val h''  = (1.0-e_theta)*h
                               val _ = if debug
                                       then Printf.printf `"RootStep: Mid: x' = "R `" x'' = "R 
                                                          `" h' = "R `" h'' = "R `" y'' = "R `" y = "R
                                                          `"\n" $ x' x'' h' h'' (getindex(y'',0)) (getindex(y,0))
                                       else ()
                               val e''  = fixthr (fcond (x'',y'',e,d,r,ext,extev,enext))
                               val cst'' = controller_update_h (cst',e_theta*h) 
                           in
                               RegimeState(x'',cx'',y'',e'',d,r,ext,extev,y,yrsp,e,cst'',
                                           RootFound (i,if h''>float_eps then h''::hs else hs))
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
                         then Printf.printf `"RootFound: x = "R `" e = "R `" y' = "R `"\n" $ x (getindex(e,0)) (getindex(y',0))
                         else ()
             in
                 RegimeState(x,cx,y',e,d',r',ext,extev,y,ynext,enext,cst,RootAfter (i,hs))
             end
           | RootAfter (i,[]) =>
             let 
                 val _ = if debug
                         then Printf.printf `"RootAfter: x = "R `" y = "R `" h = "R `"\n" $ x (getindex(y,0)) (controller_h cst)
                         else ()
                 val e'  = fixthr (fcond (x,y,e,d,r,ext,extev,enext))
                 val cst' = case cst of Left _ => cst 
                                     |  Right v => Left v
             in
                 RegimeState(x,cx,y,e',d,r,ext,extev,ynext,yrsp,e,cst',RootBefore)
             end
           | RootAfter (i,h1::hs) =>
             let
                 val _ = if h1 <= 0.0 then raise Fail "RootAfter: zero time step" else ()

                 val hev       = Real.*(0.5,float_eps)
                 val (x',cx')  = csum (x,cx,hev)
                 val (y',_,_,w) = fstepper (d,r,ext,extev,hev,x,y,ynext,cst)
                 val e'  = fixthr (fcond (x',y',e,d,r,ext,extev,enext))
                 val _ = if debug
                         then Printf.printf `"RootAfter: x = "R `" y = "R `" x' = "R `" y' = "R  `" h1 = "R `" h1-hev = "R `"\n" $ x (getindex(y,0)) x' (getindex(y',0)) h1 (h1-hev)
                         else ()
                 val cst' = case cst of Left _ => cst 
                                     |  Right v => Left v
             in
                 RegimeState(x',cx',y',e',d,r,ext,extev,y,yrsp,e,cst',RootStep (((~ hev)+h1)::hs))
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
          (if not ((controller_h cst) > 0.0)
           then raise Fail ("Dynamics.integral: EventState: zero time step (root=" ^ (showRoot root) ^ ")")
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
                   val rootval = frootval (h,w,x,cx,y,e,x',cx',y',e',ext,extev,enext)

               in
                 case rootval of
                     SOME (Near i,e_x,e_cx,e_theta,e_y) =>
                     EventState(x,cx,y,e',ext,extev,ynext,yrsp,e,cst,RootFound (i,h::hs))
                   | SOME (Far i,e_x,e_cx,e_theta,e_y) =>
                     EventState(x',cx',y',e',ext,extev,y,yrsp,e,cst,RootFound (i,hs))
                   | SOME (Mid i,e_x,e_cx,e_theta,e_y) =>
                     (let
                         val x''  = e_x
                         val cx'' = e_cx
                         val y''  = y'
                         val _    = Array.copy {src=e_y, dst=y'', di=0}
                         val h''  = (1.0-e_theta)*h
                         val _ = if debug
                                 then Printf.printf `"RootStep: rootval: x' = "R `" x'' = "R 
                                                    `" h'' = "R `" y'' = "R
                                                       `"\n" $ x' x'' h'' (getindex(y'',0))
                                 else ()
                         val e''  = fixthr (fcond (x'',y'',e,ext,extev,enext))
                         val cst'' = controller_update_h (cst',e_theta*h)
                     in
                         EventState(x'',cx'',y'',e'',ext,extev,y,yrsp,e,cst'',
                                    RootFound (i,if h''>float_eps then h''::hs else hs))
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
                   val cst' = case cst of Left _ => cst 
                                       |  Right v => Left v

               in
                   EventState(x,cx,y',e,ext,extev,y,ynext,enext,cst',RootAfter (i,hs))
               end
             | RootAfter (i,hs) =>
               let
                   val hev       = Real.*(0.5,float_eps)
                   val (x',cx')  = csum (x,cx,hev)
                   val (y',_,_,w)  = fstepper (ext,extev,hev,x,y,ynext,cst)
                   val e'  = fixthr (fcond (x',y',e,ext,extev,enext))
                   val _   = if debug
                             then Printf.printf `"RootAfter: x' = "R `" e' = "R
                                                `" y' = "R `"\n" $ x' (getindex(e',0)) (getindex(y',0))
                             else ()
                   val cst' = case cst of Left _ => cst 
                                       |  Right v => Left v

               in
                   case hs of
                       [] => EventState(x',cx',y',e',ext,extev,y,yrsp,e,cst',RootBefore)
                    |  h1::hs => EventState(x',cx',y',e',ext,extev,y,yrsp,e,cst',RootStep (((~ hev)+h1)::hs))
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
