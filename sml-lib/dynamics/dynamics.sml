(*
 * Definitions for simulations of functional hybrid dynamical systems.
*)

structure FunctionalHybridDynamics =
struct


datatype model_state = 
         RegimeState of real * real vector * real vector * real vector * bool vector
         | EventState of real * real vector * real vector
         | ContState of real * real vector

datatype model_function = 
         RegimeFunction of (real * real vector * real vector * real vector * bool vector) -> 
                           (real * real vector * real vector * real vector * bool vector)
         | EventFunction of (real * real vector * real vector) ->
                            (real * real vector * real vector) ->
         | ContFunction of (real * real vector) ->
                           (real * real vector) 

fun vmap2 f (v1,v2) = 
    let 
        val n = Vector.length v1
    in
        Vector.tabulate (n, fn (i) => f (getindex (v1,i), getindex (v2,i)))
    end

fun vfind2 f (v1,v2) = 
    let 
        val n = Vector.length v1
        fun recur i = 
            if Int.<(i, n)
            then (if f (getindex (v1,i), getindex (v2,i)) then SOME(i) else recur (Int.+(i,1)))
            else NONE
    in
      recur 0
    end 

fun vfoldpi2 f (v1,v2) = 
    let 
        val n = Vector.length v1
        fun recur (i, ax) = 
            if Int.<(i, n)
            then recur (Int.+(i,1), f (i, getindex (v1,i), getindex (v2,i), ax))
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

fun integral1 (RegimeSolver solver,SOME fcond,SOME fpos,fneg,SOME fdiscrete,SOME fregime,h) =
    let 
        fun thr (v1,v2) = case (Real.sign(v1),Real.sign(v2)) of
                              (~1,1) => true
                            | (~1,0) => true
                            | _ => false

    in
        fn(RegimeState (x,y,e,d,r)) => 
           let val x'  = x + h
               val y'  = solver (d,r) (x,y) h
               val e'  = fcond d (x',y',e)
               val pos = vfind2 thr (e, e') 
               val r'  = fregime (e',r)
               val y'' = case pos of SOME i => fpos(x',y',e',d) | _ => y'
               val y'' = case fneg of 
                             SOME f => (case vfind2 thr (e', e) of 
                                            SOME i => fneg(x',y'',e',d) | _ => y'')
                           | NONE => y''
               val d'  = fdiscrete (x',y',e',d)
           in
               (x',y'',e',d',r')
           end
      | _ => raise InvalidArgument

    end
| integral1 (EventSolver solver,SOME fcond,SOME fpos,fneg,NONE,NONE,h) =
    let 
        fun thr (v1,v2) = case (Real.sign(v1),Real.sign(v2)) of
                              (~1,1) => true
                            | (~1,0) => true
                            | _ => false

    in
        fn(EventState(x,y,e)) => 
           let val x'  = x + h
               val y'  = solver (x,y) h
               val e'  = fcond d (x',y',e)
               val pos = vfind2 thr (e, e') 
               val y'' = case pos of SOME i => fpos(x',y',e',d) | _ => y'
               val y'' = case fneg of 
                             SOME f => (case vfind2 thr (e', e) of 
                                            SOME i => fneg(x',y'',e',d) | _ => y'')
                           | NONE => y''
           in
               (x',y'',e')
           end
      | _ => raise InvalidArgument

    end
| integral1 (ContSolver solver,NONE,NONE,_,NONE,NONE,h) =
  (fn(ContState(x,y)) => 
      let val x'  = x + h
          val y'  = solver (x,y) h
      in
          (x',y')
      end
  | _ => raise InvalidArgument)

| integral1 (_,_,_,_,_,_,_) = raise InvalidArgument


end
