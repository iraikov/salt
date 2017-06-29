(*
 * Runge-Kutta integration of ODEs
 * Based on Haskell code by Uwe Hollerbach <uh@alumni.caltech.edu>
 *
 * dy
 * -- = f(t, y)
 * dt
 *
 * y_{n+1} = y_n + h sum_{i=1}^s b_i k_i
 * k_i = f(t_n + c_i h, y_n + h sum_{j=1}^s a_{ij} k_j)
 * * "Butcher Tableau" is
 *
 * c_1  a_11 a_12 ... a_1s
 * c_2  a_21 a_22 ... a_2s
 * ...  ...
 * c_s  a_s1 a_s2 ... a_ss
 *      b_1  b_2  ... b_s
 *
 * This module implements a method that can do a generic tableau, then
 * specializes with different tableaux to let the user pick a specific
 * method. Adaptive step-size methods, see below, add a row of d_j
 * coefficients and use that to report the error:
 *
 * e_{n+1} = h sum_{i=1}^s d_i k_i
 *
 * adaptive solvers with interpolation (CERK):
 *	cerkoz3, cerkoz4, cerkoz5, cerkdp
 *
 * auxiliary non-adaptive solvers (error estimators from the adaptive ones):
 *	rkoz3_aux, rkoz4_aux, rkoz5_aux, rkdp_aux
*)

signature RKSTATE =
sig
  type state

  val state : unit -> state
  val copy  : state * state -> state 
  val scale : real * state * state -> state 
  val sum   : state * state * state -> state
end

functor RungeKuttaFn (structure S: RKSTATE) =
struct

open S


fun foldl1 f (a::b::lst) = List.foldl f (f(a,b)) lst
  | foldl1 f (a::[]) = a
  | foldl1 f _ = raise Fail "RungeKutta.foldl1: insufficient arguments"

fun list_show (toString,sep,lb,rb) xs =
    let 
	fun loop (x::xs,str) = loop (xs, str ^ sep ^ (toString x))
	  | loop ([],str) = str

	val ls = case xs of x::xs => loop (xs, toString x) | [] => ""
    in
	lb ^ ls ^ rb
    end

fun def_list_show toString xs =
    list_show (toString,",","[","]") xs

(* Rational number implementation.
 * Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)


(* Silly implementation of rational numbers
 *
 * HINT: Someone should really implement a nice lib for rational numbers!
 *)
datatype rational =
   RAT of LargeInt.int
 | //  of LargeInt.int * LargeInt.int

infix 7 */
infix 6 +/
infix //

fun rat_show (RAT i) = LargeInt.toString i
  | rat_show (n // d) = String.concat [(LargeInt.toString n), "//", (LargeInt.toString d)]

val fromRational = 
    fn (RAT n) => Real.fromLargeInt n
     | (n // d) => Real./ (Real.fromLargeInt n, Real.fromLargeInt d)
    
val numerator =
    fn (RAT n) => n
     | (n // d) => n

val denominator =
    fn (RAT n) => 1
     | (n // d) => d

fun gcd (a, b) : LargeInt.int = if 0 = b then a else gcd (b, a mod b)

val normalize =
 fn 0 // _     => RAT 0
  | r as RAT _ => r
  | n // d     => let
       val c = gcd (n, d)
    in
       if c = d
       then RAT (n div c)
       else n div c // d div c
    end

val op +/ = let
   fun sym i n d = n + i * d // d
in
   fn (RAT l,   RAT r) => RAT (l + r)
    | (RAT i,  n // d) => sym i n d
    | (n // d,  RAT i) => sym i n d
    | (n // d, m // e) =>
      normalize (if d = e then n + m // d else n*e + m*d // d*e)
end

val op */ = let
   fun sym i n d = normalize (i*n // d)
in
   fn (RAT l,   RAT r) => RAT (l * r)
    | (RAT i,  n // d) => sym i n d
    | (n // d,  RAT i) => sym i n d
    | (n // d, m // e) => normalize (n*m // d*e)
end


(* Store a list of rational numbers as a common denominator, then a list
   of numerators, all stored as doubles: this lets us write the values in
   the source as exact rational numbers, yet we can take advantage of not
   having to do too many conversions and also of not having to multiply
   by 0 or 1 in some cases.
*)

type RCL = real * real list

(* ratToRCL :: [rational] -> RCL *)
fun ratToRCL [] = (1.0, [])
  | ratToRCL rs =
    let 
	val ds = map denominator rs
	val dp = foldl1 ( op * ) ds
	val ns = map (numerator o (fn (x) => (RAT dp) */ x)) rs 
	val g  = foldl gcd dp ns
    in 
	(Real.fromLargeInt (LargeInt.quot (dp, g)), 
	 map (Real.fromLargeInt o (fn x => LargeInt.quot (x,g))) ns)
    end

(* ratToRCLs :: [[rational]] -> [RCL] *)
val ratToRCLs = fn x => map ratToRCL x

(* ratToReals :: [rational] -> [real] *)
val ratToReals = fn x => map fromRational x

fun m_scale scale (s,v,y) =
    if Real.== (s, 0.0)
    then NONE
    else (if Real.== (s, 1.0)
          then SOME v
	  else SOME (scale (s,v,y)))


(* Helper function to sum a list of K_i, skipping
   un-necessary multiplications and additions *)

fun k_sum (h, (d,ns), ks, (t1, t2, t3)) =
    let
        fun recur f g (n::ns, ks, ax) =
            let
                val (k, ks) = valOf (FunQueue.deque ks)
            in
                case f (n,k,t1) of
                    NONE => recur f g (ns, ks, ax)
                  | SOME v => 
                    let val ax' = g (v, ax, t3)
                    in
                        recur f g (ns, ks, ax')
                    end
            end
          | recur f g ([], _, ax) = ax

    in
        scale (Real./ (h,d), recur (m_scale scale) sum (ns, ks, t2), t1)
    end


(* Helper function to generate a list of K_i *)

fun gen_ks (der_fn: real * state * state -> state,
	    h,(tn,yn),ks,[],[],[],[],_) = ks

  | gen_ks (der_fn,h,old as (tn,yn),ks,(c::cs),(a::ar),(t1::ts1),(t2::ts2),ts3) =
    let
	val yn1 = if (FunQueue.empty ks) then yn else sum (yn, k_sum (h,a,ks,ts3), t1)
        val k1  = der_fn (tn + c*h, yn1, t2)
    in
        gen_ks (der_fn, h, old, FunQueue.enque(ks,k1), cs, ar, ts1, ts2, ts3)
    end

  | gen_ks (der_fn,h,(tn,yn),ks,_,_,_,_,_) =
    raise Fail "RungeKutta.k_sum: insufficient arguments"
    

(* Helper function to sum a list of b_i (theta) K_i *)
fun putStrLn str =
    (TextIO.output (TextIO.stdOut, str);
     TextIO.output (TextIO.stdOut, "\n"))

fun bk_sum (bs: RCL list) =
  fn (ks, h: real) =>
     fn (theta: real, ts: state list, yout: state) =>
        let
            val thetas = List.tabulate(List.length bs, fn(i) => Real.Math.pow(theta, Real.fromInt (i+1)))
            fun recur ((d,ns)::bs, ks, fs, ts) =
              let
                  val (bsum,_) = foldl (fn (n,(sum,thetas)) =>
                                           (((n*(List.hd thetas))+sum,List.tl thetas)))
                                       (0.0,thetas) ns
                  val (k, ks) = valOf (FunQueue.deque ks)
                  val t = hd ts
              in
                  case m_scale scale (bsum, k, t) of 
                      SOME bk => recur (bs, ks, (scale (h/d, bk, t))::fs, tl ts)
                    | NONE => recur (bs, ks, fs, ts)
              end
              | recur ([], ks, [], ts) =
                raise Fail "RungeKutta.bk_sum: empty list of function evaluations"
              | recur ([], ks, fs, ts) = foldl1 (fn(x,y) => sum (x,y,yout)) fs
        in
            recur (bs, ks, [], ts)
        end

        
(* Hermite interpolation routine for continuous explicit RK (CERK) methods *)

type hinterp = (real * state FunQueue.t * real * state) ->
               real -> state
                   
fun hinterp (ws: RCL list) =
  let
      val ts1  = List.tabulate (List.length ws, fn (i) => state())
      val t1   = state()
      val yout = state()
  in
      (fn (h: real, ks, tn, yn: state) =>
          fn (theta) =>
             if theta > 0.0 
             then sum (yn, bk_sum ws (ks,h) (theta,ts1,t1), yout)
             else copy (yn, yout))
  end

(*

   This is the first core routine: it does not get exported,
   only partial applications of it; see below.

   Its arguments are:

     c table (specified internally)
     a table (specified internally)
     b table (specified internally)

   user-specified arguments:

     scale function to scale a Y state vector ::
	(real * a -> a)

     sum function to add two Y state vectors ::
	(a * a -> a)

     derivative function F ::
	(real * a -> a)

     step size H ::
	real

     current state (T,Y) ::
	(real, a)

     and the return value is the new state Y_new
*)

type stepper1 =  (real * state * state -> state) ->
		 real -> (real * state * state) -> state
                        
fun core1 (cl: real list, al: RCL list, bl: RCL) =
  let
      val ts1  = List.tabulate (List.length cl, fn (i) => state())
      val ts2  = List.tabulate (List.length cl, fn (i) => state())
      val ts3  = (state(),state(),state())
      val tys  = (state(),state(),state())
  in
      fn (der_fn: real * state * state -> state) =>
	 fn (h: real) =>
	    fn (old as (tn,yn: state,yout: state)) =>
                  let
                      val ks  = gen_ks (der_fn, h, (tn,yn), FunQueue.new(), cl, al, ts1, ts2, ts3)
                  in
                      sum (yn, k_sum (h, bl, ks, tys), yout)
                  end
  end

(*
   Core routine for methods that return error estimate.
   It returns a tuple instead of a single value: (ynew,enew),
   where enew is the error state vector
   e_{n+1} = h sum_{i=1}^s (b_i - b'_i) k_i
*)

type stepper2 =  (real * state * state -> state) ->
		 real -> (real * state * state) -> (state * state)

fun core2 (cl: real list, al: RCL list, bl: RCL, dl: RCL) =
  let
      val ts1  = List.tabulate (List.length cl, fn (i) => state())
      val ts2  = List.tabulate (List.length cl, fn (i) => state())
      val ts3  = (state(),state(),state())
      val tys  = (state(),state(),state())
      val te1  = state()
      val te2  = state()
      val err  = state()
  in
      fn (der_fn: real * state * state -> state) =>
         fn (h: real) =>
            fn (old as (tn,yn: state, yout: state)) =>
               let
                   val ks = gen_ks (der_fn, h, (tn,yn), FunQueue.new(), cl, al, ts1, ts2, ts3)
               in
                   (sum (yn, k_sum (h, bl, ks, tys), yout),
                    k_sum (h, dl, ks, (err,te1,te2)))
               end
  end

(* Core routine for constructing continuous (CERK) methods.  It
   returns a triple (ynew,enew,w), where w are the interpolation
   coefficients for this timestep. *)

type stepper3 = (real * state * state -> state) -> 
		real -> (real * state * state) ->
                (state * state * state FunQueue.t)

fun core3 (cl: real list, al: RCL list, bl: RCL, dl: RCL, wl: RCL list) =
  let
      val ts1  = List.tabulate (List.length cl, fn (i) => state())
      val ts2  = List.tabulate (List.length cl, fn (i) => state())
      val ts3  = (state(),state(),state())
      val tys  = (state(),state(),state())
      val te1  = state()
      val te2  = state()
      val ti   = state()
      val err  = state()
  in
      fn (der_fn: real * state * state -> state) =>
	 fn (h: real) =>
	    fn (old as (tn,yn: state,yout: state)) =>
               let
                   val ks   = gen_ks (der_fn, h, (tn,yn), 
                                      FunQueue.new(), cl, al, ts1, ts2, ts3)
               in
                   (sum (yn, k_sum (h, bl, ks, tys), yout),
                    k_sum (h, dl, ks, (err,te1,te2)),
                    ks)
               end
  end

(* Helper routines to show the internal tables *)

fun rcl_show (d,ns) =
    "<" ^ (Real.toString d) ^ ", " ^ (def_list_show Real.toString ns) ^ ">"

fun rk_show1 (title,cs,ar: RCL list,bs) =
    title ^ ":\ncs:\t" ^ ((def_list_show Real.toString) cs) ^
    "\nas:\t" ^ (list_show (rcl_show,"\n\t","","") ar) ^
    "\nbs:\t" ^ (rcl_show bs) 

fun rk_show2 (title,cs,ar: RCL list,bs,ds) =
    title ^ ":\nds:\t" ^ (rcl_show ds) ^ 
    "\ncs:\t" ^ ((def_list_show Real.toString) cs) ^
    "\nbs:\t" ^ (rcl_show bs) ^ 
    "\nas:\t" ^ (list_show (rcl_show,"\n\t","","") ar) 

fun rk_show3 (title,cs,ar: RCL list,bs,ds,ws) =
  title ^ ":" ^
  "\nws:\t" ^ (list_show (rcl_show,"\n\t","","") ws) ^
  "\nds:\t" ^ (rcl_show ds) ^ 
  "\ncs:\t" ^ ((def_list_show Real.toString) cs) ^
  "\nbs:\t" ^ (rcl_show bs) ^ 
  "\nas:\t" ^ (list_show (rcl_show,"\n\t","","") ar) 

fun negate x = (RAT ~1) */ x

fun diffs ([], []) = []
  | diffs (xs, []) = xs
  | diffs ([], xs) = map negate xs
  | diffs (x::xs, y::ys) = (x +/ (negate y)) :: (diffs (xs,ys))

(* Owren-Zennaro, order 3/2 CERK method *)
val cs_oz = ratToReals [RAT 0, 12//23, 4//5, RAT 1]
val as_oz = ratToRCLs [[], 
                       [12//23], 
                       [~68//375, 368//375], 
                       [31//144, 529//1152, 125//384]]
val r1_oz = [31//144, 529//1152, 125//384]	(* third-order coeffs *)
val r2_oz = [1//24, 23//24] (* second-order coeffs *)
val bs_oz = ratToRCL r1_oz
val ds_oz = ratToRCL (diffs (r1_oz, r2_oz))
val rkoz3: stepper2 = core2 (cs_oz, as_oz, bs_oz, ds_oz)
val show_rkoz3 = rk_show2 ("Owren-Zennaro 3(2)", cs_oz, as_oz, bs_oz, ds_oz)

val bs_oz_aux = ratToRCL r2_oz
val rkoz3_aux: stepper1 = core1 (cs_oz, as_oz, bs_oz_aux)
val show_rkoz3_aux = rk_show1 ("Owren-Zennaro (2)", cs_oz, as_oz, bs_oz_aux)

(* interpolation coeffs for continuous method *)
val ws_oz = ratToRCLs [[RAT 1, ~65//48, 41//72],
                       [RAT 0, 529//384, ~529//576],
                       [RAT 0, 125//128, ~125//192],
                       [RAT 0, RAT ~1, RAT 1]]

val cerkoz3: stepper3  = core3 (cs_oz, as_oz, bs_oz, ds_oz, ws_oz)
val show_cerkoz3 = rk_show3 ("Continuous Owren-Zennaro 3(2)", cs_oz, as_oz, bs_oz, ds_oz, ws_oz)
val interp_cerkoz3: hinterp = hinterp ws_oz

(* Owren-Zennaro, order 4/3 CERK method *)
val cs_oz4 = ratToReals [RAT 0, 1//6, 11//37, 11//17, 13//15, RAT 1]
val as_oz4 = ratToRCLs [[], 
                       [1//6], 
                       [44//1369, 363//1369], 
                       [3388//4913, ~8349//4913, 8140//4913],
                       [~36764//408375, 767//1125, ~32708//136125, 210392//408375],
                       [1697//18876, RAT 0, 50653//116160, 299693//1626240, 3375//11648]]
val r1_oz4 = [1697//18876, RAT 0, 50653//116160, 299693//1626240, 3375//11648]	
val r2_oz4 = [101//363, RAT 0, ~1369//14520, 11849//14520]
val bs_oz4 = ratToRCL r1_oz4
val ds_oz4 = ratToRCL (diffs (r1_oz4, r2_oz4))
val rkoz4: stepper2 = core2 (cs_oz4, as_oz4, bs_oz4, ds_oz4)
val show_rkoz4 = rk_show2 ("Owren-Zennaro 4(3)", cs_oz4, as_oz4, bs_oz4, ds_oz4)

val bs_oz4_aux = ratToRCL r2_oz4
val rkoz4_aux: stepper1 = core1 (cs_oz4, as_oz4, bs_oz4_aux)
val show_rkoz4_aux = rk_show1 ("Owren-Zennaro (3)", cs_oz4, as_oz4, bs_oz4_aux)

(* interpolation coeffs for continuous method *)
val ws_oz4 = ratToRCLs [[RAT 1, ~104217//37466, 1806901//618189, ~866577//824252],
                        [],
                        [RAT 0, 861101//230560, ~2178079//380424, 12308679//5072320],
                        [RAT 0, ~63869//293440, 6244423//5325936, ~7816583//10144640],
                        [RAT 0, ~1522125//762944, 982125//190736, ~624375//217984],
                        [RAT 0, 165//131, ~461//131, 296//131]]

val cerkoz4: stepper3  = core3 (cs_oz4, as_oz4, bs_oz4, ds_oz4, ws_oz4)
val show_cerkoz4 = rk_show3 ("Continuous Owren-Zennaro 4(3)", cs_oz4, as_oz4, bs_oz4, ds_oz4, ws_oz4)
val interp_cerkoz4: hinterp = hinterp ws_oz4

(* Owren-Zennaro, order 5/4 CERK method *)
val cs_oz5 = ratToReals [RAT 0, 1//6, 1//4, 1//2, 1//2, 9//14, 7//8, RAT 1]
val as_oz5 = ratToRCLs [[], 
                       [1//6], 
                       [1//16, 3//16], 
                       [1//4, ~3//4, RAT 1],
                       [~3//4, 15//4, RAT ~3, 1//2],
                       [369//1372, ~243//343, 297//343, 1485//9604, 297//4802],
                       [~133//4512, 1113//6016, 7945//16544, ~12845//24064, ~315//24064, 156065//198528],
                       [83//945, RAT 0, 248//825, 41//180, 1//36, 2401//38610, 6016//20475]]
                       
val r1_oz5 = [83//945, RAT 0, 248//825, 41//180, 1//36, 2401//38610, 6016//20475]
val r2_oz5 = [~1//9, RAT 0, 40//33, ~7//4, ~1//12, 343//198]
val bs_oz5 = ratToRCL r1_oz5
val ds_oz5 = ratToRCL (diffs (r1_oz5, r2_oz5))
val rkoz5: stepper2 = core2 (cs_oz5, as_oz5, bs_oz5, ds_oz5)
val show_rkoz5 = rk_show2 ("Owren-Zennaro 5(4)", cs_oz5, as_oz5, bs_oz5, ds_oz5)

val bs_oz5_aux = ratToRCL r2_oz5
val rkoz5_aux: stepper1 = core1 (cs_oz5, as_oz5, bs_oz5_aux)
val show_rkoz5_aux = rk_show1 ("Owren-Zennaro (4)", cs_oz5, as_oz5, bs_oz5_aux)

(* interpolation coeffs for continuous method *)
val ws_oz5 = ratToRCLs [[RAT 1, ~3292//819, 17893//2457, ~4969//819, 596//315],
                        [],
                        [RAT 0, 5112//715, ~43568//2145, 1344//65, ~1984//275],
                        [RAT 0, ~123//52, 3161//234, ~1465//78, 118//15],
                        [RAT 0, ~63//52, 1061//234, ~413//78, RAT 2],
                        [RAT 0, ~40817//33462, 60025//50193, 2401//1521, ~9604//6435],
                        [RAT 0, 18048//5915, ~637696//53235, 96256//5915, ~48128//6825],
                        [RAT 0, ~18//13, 75//13, ~109//13, RAT 4]]

val cerkoz5: stepper3  = core3 (cs_oz5, as_oz5, bs_oz5, ds_oz5, ws_oz5)
val show_cerkoz5 = rk_show3 ("Continuous Owren-Zennaro 5(4)", cs_oz5, as_oz5, bs_oz5, ds_oz5, ws_oz5)
val interp_cerkoz5: hinterp = hinterp ws_oz5

                                    
(* Dormand-Prince, order 5/4 (use 5th-order sol'n, coeffs chosen to
   minimize error of 5th-order sol'n) This is DOPRI5 from Hairer,
   Norsett, Wanner *)

val cs_dp = ratToReals [RAT 0, 1//5, 3//10, 4//5, 8//9, RAT 1, RAT 1]
val as_dp = ratToRCLs [[],
                       [1//5],
                       [3//40, 9//40],
                       [44//45, ~56//15, 32//9],
                       [19372//6561, ~25360//2187, 64448//6561, ~212//729],
                       [9017//3168, ~355//33, 46732//5247, 49//176, ~5103//18656],
                       [35//384, RAT 0, 500//1113, 125//192, ~2187//6784, 11//84]]
(* fifth-order coeffs *)
val r1_dp = [35//384, RAT 0, 500//1113, 125//192, ~2187//6784, 11//84]
(* fourth-order coeffs *)
val r2_dp = [5179//57600, RAT 0, 7571//16695, 393//640, ~92097//339200, 187//2100, 1//40]
val bs_dp = ratToRCL r1_dp
val ds_dp = ratToRCL (diffs (r1_dp, r2_dp))
(* interpolation coeffs for continuous method *)
val ws_dp = ratToRCLs [[RAT 1, ~1337//480, 1039//360, ~1163//1152],
                       [],
                       [RAT 0, 4216//1113, ~18728//3339, 7580//3339],
                       [RAT 0, ~27//16, 9//2, ~415//192],
                       [RAT 0, ~2187//8480, 2673//2120, ~8991//6784],
                       [RAT 0, 33//35, ~319//105, 187//84]]
                                                         

val cerkdp: stepper3  = core3 (cs_dp, as_dp, bs_dp, ds_dp, ws_dp)
val show_cerkdp = rk_show3 ("Continuous Dormand-Prince 5(4)", cs_dp, as_dp, bs_dp, ds_dp, ws_dp)
val interp_cerkdp: hinterp = hinterp ws_dp

val rkdp: stepper2  = core2 (cs_dp, as_dp, bs_dp, ds_dp)
val show_rkdp = rk_show2 ("Dormand-Prince 5(4) \"DOPRI5\"", cs_dp, as_dp, bs_dp, ds_dp)

val bs_dp_aux = ratToRCL r2_dp
val rkdp_aux: stepper1 = core1 (cs_dp, as_dp, bs_dp_aux)
val show_rkdp_aux = rk_show1 ("Dormand-Prince (4)", cs_dp, as_dp, bs_dp_aux)


end
