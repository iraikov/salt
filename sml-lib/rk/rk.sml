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
 * non-adaptive solvers:
 *	rkfe, rk3, rk4a, rk4b
 *
 * adaptive solvers:
 *	rkhe, rkbs, rkoz, rkf45, rkck, rkdp, rkdpb, rkf78, rkv65
 *
 * adaptive solvers with interpolation (CERK):
 *	cerkoz, cerkdp
 *
 * auxiliary non-adaptive solvers (error estimators from the adaptive ones):
 *	rkhe_aux, rkbs_aux, rkoz_aux, rkf45_aux, rkck_aux, rkdp_aux, rkdpb_aux, 
 *      rkf78_aux, rkv65_aux
 *
 * use rk4[ab] if you don't need an adaptive solver, rkdp or rkv65 if
 * you do; or use what you need if you're an expert.
 *
 * DO NOT USE rkfe EXCEPT FOR DEMONSTRATIONS OF INSTABILITY!
 * (Or if you're an expert.)
 *
 * Reference: E. Hairer, S. P. Norsett, G. Wanner,
 * Solving Ordinary Differential Equations I: Nonstiff Problems
 * (second revised edition, 1993).
*)

structure RungeKutta =
struct


exception InsufficientArguments
exception KsInvalidCoefficients
exception BkInvalidCoefficients

fun putStr str =
    (TextIO.output (TextIO.stdOut, str))

fun putStrLn str =
    (TextIO.output (TextIO.stdOut, str);
     TextIO.output (TextIO.stdOut, "\n"))


fun foldl1 f (a::b::lst) = List.foldl f (f(a,b)) lst
  | foldl1 f (a::[]) = a
  | foldl1 f _ = raise InsufficientArguments

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

fun m_scale sc_fn (s,v) =
    if Real.== (s, 0.0)
    then NONE
    else (if Real.== (s, 1.0)
          then SOME v
	  else SOME (sc_fn (s,v)))


(* Helper function to sum a list of K_i, skipping
   un-necessary multiplications and additions *)

fun k_sum (sc_fn: real * 'a -> 'a, 
	   sum_fn: 'a * 'a -> 'a, 
	   h: real) 
	  ((d,ns), ks) =
    let 
	val ns_ks = ListPair.zip (ns,ks)
    in
	sc_fn (Real./ (h,d), foldl1 sum_fn (List.mapPartial (m_scale sc_fn) ns_ks  ))
    end


(* Helper function to generate a list of K_i *)

fun gen_ks (ksum_fn,sum_fn: 'a * 'a -> 'a,der_fn: real * 'a -> 'a,
	    h,(tn,yn),ks,[],[]) = ks

  | gen_ks (ksum_fn,sum_fn,der_fn,h,old as (tn,yn),ks,(c::cs),(a::ar)) =
    let
	val yn1 = if (List.null ks) then yn else sum_fn (yn, ksum_fn (a,ks))
    in
	gen_ks (ksum_fn, sum_fn, der_fn, h, old, (ks @ [der_fn (tn + c*h, yn1)]), cs, ar)
    end

  | gen_ks (ksum_fn,sum_fn,der_fn,h,(tn,yn),ks,_,_) =
    raise KsInvalidCoefficients
    

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

type 'a stepper1 =  ((real * 'a -> 'a) * 
		     ('a * 'a -> 'a)   *
		     (real * 'a -> 'a)) ->
		     (real -> (real * 'a) -> 'a)

fun core1
	(cl: real list, al: RCL list, bl: RCL)
	(sc_fn:  real * 'a -> 'a, 
	 sum_fn: 'a * 'a -> 'a,
	 der_fn: real * 'a -> 'a)
	(h: real) 
	(old as (tn,yn: 'a)) =
    (let
	 val ksum = k_sum (sc_fn,sum_fn,h)
	 val ks   = gen_ks (ksum, sum_fn, der_fn, h, old, [], cl, al)
     in
	 sum_fn (yn, ksum (bl, ks))
     end)


(*
   This is the second core routine, analogous to the previous one.
   The difference is that this gets an additional internal table arg,
   and it returns a tuple instead of a single value: (ynew,enew),
   where enew is the error state vector
   e_{n+1} = h sum_{i=1}^s (b_i - b'_i) k_i
*)

type 'a stepper2 =  ((real * 'a -> 'a) * 
		     ('a * 'a -> 'a)   *
		     (real * 'a -> 'a)) ->
		    (real -> (real * 'a) -> ('a * 'a))

fun core2 (cl: real list, al: RCL list, bl: RCL, dl: RCL) 
	  (sc_fn: real * 'a -> 'a, 
	   sum_fn: 'a * 'a -> 'a,
	   der_fn: real * 'a -> 'a)
	   (h: real)
	   (old as (tn,yn: 'a)) =
  let
      val ksum = k_sum (sc_fn,sum_fn,h)
      val ks   = gen_ks (ksum, sum_fn, der_fn, h, old, [], cl, al)
  in
      (sum_fn (yn, ksum (bl, ks)), ksum (dl, ks))
  end



(* Helper function to sum a list of b_i (theta) K_i *)

fun bk_sum (bs: RCL list)
           (sc_fn: real * 'a -> 'a, sum_fn: 'a * 'a -> 'a) 
           (ks: 'a list, h: real)
           (theta: real) = 
    let 

        fun recur ((d,ns)::bs, k::ks, fs) =
            let
                val (bsum,_) = foldl (fn (n,(sum,theta)) => 
                                         ((n*theta)+sum,theta*theta)) (0.0,theta) ns
            in
                case m_scale sc_fn (bsum, k) of 
                    SOME bk => recur (bs, ks, (sc_fn (h/d, bk))::fs)
                  | NONE => recur (bs, ks, fs)
            end
          | recur ([], [], fs) = foldl1 sum_fn fs
          | recur (bs, ks, fs) = 
            (putStrLn ("BkInvalidCoefficients: length bs = " ^ (Int.toString (length bs)));
             putStrLn ("BkInvalidCoefficients: length ks = " ^ (Int.toString (length ks)));
             putStrLn ("BkInvalidCoefficients: length fs = " ^ (Int.toString (length fs)));
             raise BkInvalidCoefficients)
    in
        recur (bs, ks, [])
    end
    

(* Interpolation routine for continuous explicit RK (CERK) methods *)
fun interp (ws: RCL list)
           (sc_fn: real * 'a -> 'a, 
	    sum_fn: 'a * 'a -> 'a)
           (ks: 'a list, h: real)
	   (tn,yn: 'a) (theta: real) = 
    sum_fn (yn, bk_sum ws (sc_fn,sum_fn) (ks,h) theta)


(* Core routine for constructing continuous methods.  It returns a
   triple (ynew,enew,interp), where interp is the interpolation
   function for this timestep.
*)

type 'a stepper3 =  ((real * 'a -> 'a) * 
		     ('a * 'a -> 'a)   *
		     (real * 'a -> 'a)) ->
		    (real -> (real * 'a) -> ('a * 'a * (real -> 'a)))

fun core3 (cl: real list, al: RCL list, bl: RCL, dl: RCL, wl: RCL list) 
	  (sc_fn: real * 'a -> 'a, 
	   sum_fn: 'a * 'a -> 'a,
	   der_fn: real * 'a -> 'a)
	   (h: real)
	   (old as (tn,yn: 'a)) =
  let
      val interp'   = interp wl (sc_fn,sum_fn)
      val ksum      = k_sum (sc_fn,sum_fn,h)
      val ks        = gen_ks (ksum, sum_fn, der_fn, h, old, [], cl, al)
  in
      (sum_fn (yn, ksum (bl, ks)),
       ksum (dl, ks),
       interp' (ks, h) (tn,yn))
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
    title ^ ":ws:\t" ^ (list_show (rcl_show,"\n\t","","") ws) ^
    "\nds:\t" ^ (rcl_show ds) ^ 
    "\ncs:\t" ^ ((def_list_show Real.toString) cs) ^
    "\nbs:\t" ^ (rcl_show bs) ^ 
    "\nas:\t" ^ (list_show (rcl_show,"\n\t","","") ar) 


(*
   Some specific explicit methods, taken from
   "List of Runge-Kutta methods" at Wikipedia
*)

(* forward Euler: unconditionally unstable: don't use this! *)

val cs_fe = ratToReals [RAT 0]
val as_fe = ratToRCLs [[]]
val bs_fe = ratToRCL  [RAT 1]
fun make_rkfe (): 'a stepper1 = core1 (cs_fe, as_fe, bs_fe)
val show_rkfe = rk_show1 ("Forward Euler", cs_fe, as_fe, bs_fe)

(* Kutta's third-order method: *)

val cs_rk3 = ratToReals [RAT 0, 1//2, RAT 1]
val as_rk3 = ratToRCLs [[], [1//2], [RAT ~1, RAT 2]]
val bs_rk3 = ratToRCL  [1//6, 2//3, 1//6]
fun make_rk3 (): 'a stepper1 = core1 (cs_rk3, as_rk3, bs_rk3)
val show_rk3 = rk_show1 ("Kutta's third-order method", cs_rk3, as_rk3, bs_rk3)

(* Classic fourth-order method *)

val cs_rk4a = ratToReals [RAT 0, 1//2, 1//2, RAT 1]
val as_rk4a = ratToRCLs [[], [1//2], [RAT 0, 1//2], [RAT 0, RAT 0, RAT 1]]
val bs_rk4a = ratToRCL  [1//6, 1//3, 1//3, 1//6]
fun make_rk4a (): 'a stepper1 = core1 (cs_rk4a, as_rk4a, bs_rk4a)
val show_rk4a = rk_show1 ("Classic fourth-order method", cs_rk4a, as_rk4a, bs_rk4a)

(* Kutta's other fourth-order method... "The first [above] is more popular,
   the second is more precise." (Hairer, Norsett, Wanner) *)

val cs_rk4b = ratToReals [RAT 0, 1//3, 2//3, RAT 1]
val as_rk4b = ratToRCLs [[], [1//3], [~1//3, RAT 1], [RAT 1, RAT ~1, RAT 1]]
val bs_rk4b = ratToRCL  [1//8, 3//8, 3//8, 1//8]
fun make_rk4b (): 'a stepper1 = core1 (cs_rk4b, as_rk4b, bs_rk4b)
val show_rk4b = rk_show1 ("Kutta's other classic fourth-order method", cs_rk4b, as_rk4b, bs_rk4b)

(*
   Some adaptive-stepsize methods, also from Wikipedia; more from HNW.
   These don't auto-adapt, but they do allow the user to make a somewhat
   intelligent decision about what the step size ought to be at each step.
*)

(*
   A helper function to take the difference of two lists of rationals:
   we don't want to use zipWith (-) because that gives only the head
   where both lists have entries; we want implicit zeros at the end,
   as far as is necessary.
*)

fun negate x = (RAT ~1) */ x

fun diffs ([], []) = []
  | diffs (xs, []) = xs
  | diffs ([], xs) = map negate xs
  | diffs (x::xs, y::ys) = (x +/ (negate y)) :: (diffs (xs,ys))

(* Heun-Euler, order 2/1 *)

val cs_he = ratToReals [RAT 0, RAT 1]
val as_he = ratToRCLs [[], [RAT 1]]
val r1_he = [1//2, 1//2]   (* second-order coeffs *)
val r2_he = [RAT 1] 	   (* first-order coeffs *)
val bs_he = ratToRCL r1_he
val ds_he = ratToRCL (diffs (r1_he, r2_he))
fun make_rkhe (): 'a stepper2  = core2 (cs_he, as_he, bs_he, ds_he)
val show_rkhe = rk_show2 ("Heun-Euler 2(1)", cs_he, as_he, bs_he, ds_he)

val bs_he_aux = ratToRCL r2_he
fun make_rkhe_aux (): 'a stepper1 = core1 (cs_he, as_he, bs_he_aux)
val show_rkhe_aux = rk_show1 ("Heun-Euler (1)", cs_he, as_he, bs_he_aux)

(* Bogacki-Shampine, order 3/2 *)

val cs_bs = ratToReals [RAT 0, 1//2, 3//4, RAT 1]
val as_bs = ratToRCLs [[], [1//2], [RAT 0, 3//4], [2//9, 1//3, 4//9]]
val r1_bs = [2//9, 1//3, 4//9]		(* third-order coeffs *)
val r2_bs = [7//24, 1//4, 1//3, 1//8]	(* second-order coeffs *)
val bs_bs = ratToRCL r1_bs
val ds_bs = ratToRCL (diffs (r1_bs, r2_bs))
fun make_rkbs (): 'a stepper2 = core2 (cs_bs, as_bs, bs_bs, ds_bs)
val show_rkbs = rk_show2 ("Bogacki-Shampine 3(2)", cs_bs, as_bs, bs_bs, ds_bs)

val bs_bs_aux = ratToRCL r2_bs
fun make_rkbs_aux (): 'a stepper1 = core1 (cs_bs, as_bs, bs_bs_aux)
val show_rkbs_aux = rk_show1 ("Bogacki-Shampine (2)", cs_bs, as_bs, bs_bs_aux)

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
fun make_rkoz (): 'a stepper2 = core2 (cs_oz, as_oz, bs_oz, ds_oz)
val show_rkoz = rk_show2 ("Owren-Zennaro 3(2)", cs_oz, as_oz, bs_oz, ds_oz)

val bs_oz_aux = ratToRCL r2_oz
fun make_rkoz_aux (): 'a stepper1 = core1 (cs_oz, as_oz, bs_oz_aux)
val show_rkoz_aux = rk_show1 ("Owren-Zennaro (2)", cs_oz, as_oz, bs_oz_aux)

(* interpolation coeffs for continuous method *)
val ws_oz = ratToRCLs [[RAT 1, ~65//48, 41//72],
                       [RAT 0, 529//384, ~529//576],
                       [RAT 0, 125//128, ~125//192],
                       [RAT 0, RAT ~1, RAT 1]]

fun make_cerkoz (): 'a stepper3  = core3 (cs_oz, as_oz, bs_oz, ds_oz, ws_oz)
val show_cerkoz = rk_show3 ("Continuous Owren-Zennaro 3(2)", cs_oz, as_oz, bs_oz, ds_oz, ws_oz)

(* Runge-Kutta-Norsett, order 3/4 *)

val cs_rkn34 = ratToReals [RAT 0, 3//8, 9//16, 25//32, RAT 1]
val as_rkn34 = ratToRCLs [[], 
                          [3//8], 
                          [RAT 0, 9//16], 
                          [~125//672, 325//336],
                          [371//891, ~200//297, 1120//891]]
val r1_rkn34 = [37//225, 44//117, RAT 0, 448//975] (* third-order coeffs *)
val r2_rkn34 = [25//162, 32//135, 256//567, RAT 0, 11//70] (* second-order coeffs *)
val bs_rkn34 = ratToRCL r1_rkn34
val ds_rkn34 = ratToRCL (diffs (r1_rkn34, r2_rkn34))
fun make_rkn34 (): 'a stepper2 = core2 (cs_rkn34, as_rkn34, bs_rkn34, ds_rkn34)
val show_rkn34 = rk_show2 ("Runge-Kutta-Norsett 3(4)", cs_rkn34, as_rkn34, bs_rkn34, ds_rkn34)

val bs_rkn34_aux = ratToRCL r2_rkn34
fun make_rkn34_aux (): 'a stepper1 = core1 (cs_rkn34, as_rkn34, bs_rkn34_aux)
val show_rkn34_aux = rk_show1 ("Runge-Kutta-Norsett (4)", cs_rkn34, as_rkn34, bs_rkn34_aux)

(* Runge-Kutta-Fehlberg, order 4/5 *)

val cs_rkf = ratToReals [RAT 0, 1//4, 3//8, 12//13, RAT 1, 1//2]
val as_rkf = ratToRCLs [[],
			[1//4],
			[3//32, 9//32],
			[1932//2197, ~7200//2197, 7296//2197],
			[439//216, RAT ~8, 3680//513, ~845//4104],
			[~8//27, RAT 2, ~3544//2565, 1859//4104, ~11//40]]
(* fourth-order coeffs *)
val r1_rkf = [25//216, RAT 0, 1408//2565, 2197//4104, ~1//5]
(* fifth-order coeffs *)
val r2_rkf = [16//135, RAT 0, 6656//12825, 28561//56430, ~9//50, 2//55]
val bs_rkf = ratToRCL r1_rkf
val ds_rkf = ratToRCL (diffs (r1_rkf, r2_rkf))
fun make_rkf45 (): 'a stepper2  = core2 (cs_rkf, as_rkf, bs_rkf, ds_rkf)
val show_rkf45 = rk_show2 ("Runge-Kutta-Fehlberg 4(5)", cs_rkf, as_rkf, bs_rkf, ds_rkf)

val bs_rkf_aux = ratToRCL r2_rkf
fun make_rkf45_aux (): 'a stepper1 = core1 (cs_rkf, as_rkf, bs_rkf_aux)
val show_rkf45_aux = rk_show1 ("Runge-Kutta-Fehlberg (5)", cs_rkf, as_rkf, bs_rkf_aux)

(* Cash-Karp, order 4/5 (use 4th-order sol'n,
   coeffs chosen to minimize error of 4th-order sol'n) *)

val cs_ck = ratToReals [RAT 0, 1//5, 3//10, 3//5, RAT 1, 7//8]
val as_ck = ratToRCLs [[],
                       [1//5],
                       [3//40, 9//40],
                       [3//10, ~9//10, 6//5],
                       [~11//54, 5//2, ~70//27, 35//27],
                       [1631//55296, 175//512, 575//13824, 44275//110592, 253//4096]]
(* fourth-order coeffs *)
val r1_ck = [2825//27648, RAT 0, 18575//48384, 13525//55296, 277//14336, 1//4]
(* fifth-order coeffs *)
val r2_ck = [37//378, RAT 0, 250//621, 125//594, RAT 0, 512//1771]
val bs_ck = ratToRCL r1_ck
val ds_ck = ratToRCL (diffs (r1_ck, r2_ck))
fun make_rkck (): 'a stepper2  = core2 (cs_ck, as_ck, bs_ck, ds_ck)
val show_rkck = rk_show2 ("Cash-Karp 4(5)", cs_ck, as_ck, bs_ck, ds_ck)

val bs_ck_aux = ratToRCL r2_ck
fun make_rkck_aux (): 'a stepper1 = core1 (cs_ck, as_ck, bs_ck_aux)
val show_rkck_aux = rk_show1 ("Cash-Karp (5)", cs_ck, as_ck, bs_ck_aux)

(* Dormand-Prince, order 5/4 (use 5th-order sol'n, coeffs chosen to
   minimize error of 5th-order sol'n) This is DOPRI5 from Hairer,
   Norsett, Wanner
*)

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
                       [RAT 0, 33//35, ~319//105, 187//84],
                       []]
                                                         

fun make_cerkdp (): 'a stepper3  = core3 (cs_dp, as_dp, bs_dp, ds_dp, ws_dp)
val show_cerkdp = rk_show3 ("Continuous Dormand-Prince 5(4)", cs_dp, as_dp, bs_dp, ds_dp, ws_dp)

fun make_rkdp (): 'a stepper2  = core2 (cs_dp, as_dp, bs_dp, ds_dp)
val show_rkdp = rk_show2 ("Dormand-Prince 5(4) \"DOPRI5\"", cs_dp, as_dp, bs_dp, ds_dp)

val bs_dp_aux = ratToRCL r2_dp
fun make_rkdp_aux (): 'a stepper1 = core1 (cs_dp, as_dp, bs_dp_aux)
val show_rkdp_aux = rk_show1 ("Dormand-Prince (4)", cs_dp, as_dp, bs_dp_aux)


(* Alternative coefficients for Dormand-Prince, from Butcher p. 195.
   "Although this has larger error constants overall ... it has the
   advantage of a longer stability interval [than DOPRI5]."
*)

val cs_dpb = ratToReals [RAT 0, 2//9, 1//3, 5//9, 2//3, RAT 1, RAT 1]
val as_dpb = ratToRCLs [[],
                        [2//9],
                        [1//12, 1//4],
                        [55//324, ~25//108, 50//81],
                        [83//330, ~13//22, 61//66, 9//110],
                        [~19//28, 9//4, 1//7, ~27//7, 22//7],
                        [19//200, RAT 0, 3//5, ~243//400, 33//40, 7//80]]
(* fifth-order coeffs *)
val r1_dpb = [19//200, RAT 0, 3//5, ~243//400, 33//40, 7//80]
(* fourth-order coeffs *)
val r2_dpb = [431//5000, RAT 0, 333//500, ~7857//10000, 957//1000, 193//2000, ~1//50]
val bs_dpb = ratToRCL r1_dpb
val ds_dpb = ratToRCL (diffs (r1_dpb, r2_dpb))
fun make_rkdpb (): 'a stepper2  = core2 (cs_dpb, as_dpb, bs_dpb, ds_dpb)
val show_rkdpb = rk_show2 ("Dormand-Prince 5(4) B", cs_dpb, as_dpb, bs_dpb, ds_dpb)

val bs_dpb_aux = ratToRCL r2_dpb
fun make_rkdpb_aux (): 'a stepper1 = core1 (cs_dpb, as_dpb, bs_dpb_aux)
val show_rkdpb_aux = rk_show1 ("Dormand-Prince (4) B", cs_dpb, as_dpb, bs_dpb_aux)

(*
   Fehlberg, order 7/8: "... of frequent use in all high precision
   computations, e.g., in astronomy."  --Hairer, Norsett, Wanner.
   But caveat: suffers from the drawback that error estimate is
   identically 0 for quadrature problems y' = f(x)

   NOTE BUG in Hairer Norsett Wanner: the third-last A coefficient in the
   last row of the tableau is listed as "19/41" in the book. This is WRONG:
   that row does not then sum to 1, and the convergence of the auxiliary
   solver is then order 1 or 2, not 8
*)

val cs_f78 = ratToReals [RAT 0, 2//27, 1//9, 1//6, 5//12, 1//2, 5//6, 1//6, 2//3, 1//3, RAT 1, RAT 0, RAT 1]
(* TODO re-check this *)
val as_f78 = ratToRCLs
		 [[],
		  [2//27],
		  [1//36, 1//12],
		  [1//24, RAT 0, 1//8],
		  [5//12, RAT 0, ~25//16, 25//16],
		  [1//20, RAT 0, RAT 0, 1//4, 1//5],
		  [~25//108, RAT 0, RAT 0, 125//108, ~65//27, 125//54],
		  [31//300, RAT 0, RAT 0, RAT 0, 61//225, ~2//9, 13//900],
		  [RAT 2, RAT 0, RAT 0, ~53//6, 704//45, ~107//9, 67//90, RAT 3],
		  [~91//108, RAT 0, RAT 0, 23//108, ~976//135, 311//54, ~19//60, 17//6, ~1//12],
		  [2383//4100, RAT 0, RAT 0, ~341//164, 4496//1025, ~301//82, 2133//4100, 45//82, 45//164, 18//41],
		  [3//205, RAT 0, RAT 0, RAT 0, RAT 0, ~6//41, ~3//205, ~3//41, 3//41, 6//41, RAT 0],
		  [~1777//4100, RAT 0, RAT 0, ~341//164, 4496//1025, ~289//82, 2193//4100, 51//82, 33//164, 12//41, RAT 0, RAT 1]]
(* seventh-order coeffs *)
val r1_f78 = [41//840, RAT 0, RAT 0, RAT 0, RAT 0, 34//105, 9//35, 9//35, 9//280, 9//280, 41//840]
(* eighth-order coeffs *)
val r2_f78 = [RAT 0, RAT 0, RAT 0, RAT 0, RAT 0, 34//105, 9//35, 9//35, 9//280, 9//280, RAT 0, 41//840, 41//840]
val bs_f78 = ratToRCL r1_f78
val ds_f78 = ratToRCL (diffs (r1_f78, r2_f78))
fun make_rkf78 (): 'a stepper2 = core2 (cs_f78, as_f78, bs_f78, ds_f78)
val show_rkf78 = rk_show2 ("Fehlberg 7(8)", cs_f78, as_f78, bs_f78, ds_f78)

val bs_f78_aux = ratToRCL r2_f78
fun make_rkf78_aux (): 'a stepper1 = core1 (cs_f78, as_f78, bs_f78_aux)
val show_rkf78_aux = rk_show1 ("Fehlberg (8)", cs_f78, as_f78, bs_f78_aux)


(* Verner, order 6/5 (DVERK) *)

val cs_v65 = ratToReals [RAT 0, 1//6, 4//15, 2//3, 5//6, RAT 1, RAT 115, RAT 1]
val as_v65 = ratToRCLs
                 [[],
                  [1//6],
                  [4//75, 16//75],
                  [5//6, ~8//3, 5//2],
                  [~165//64, 55//6, ~425//64, 85//96],
                  [12//5, RAT ~8, 4015//612, ~11//36, 88//255],
                  [~8263//15000, 124//75, ~643//680, ~81//250, 2484//10625],
                  [3501//1720, ~300//43, 297275//52632, ~319//2322, 24068//84065, RAT 0, 3850//26703]]
(* sixth-order coeffs *)
val r1_v65 = [3//40, RAT 0, 875//2244, 23//72, 264//1955, RAT 0, 125//11592, 43//616]
(* fifth-order coeffs *)
val r2_v65 = [13//160, RAT 0, 2375//5984, 5//16, 12//85, 3//44]
val bs_v65 = ratToRCL r1_v65
val ds_v65 = ratToRCL (diffs (r1_v65, r2_v65))

fun make_rkv65 (): 'a stepper2 = core2 (cs_v65, as_v65, bs_v65, ds_v65)
val show_rkv65 = rk_show2 ("Verner 6(5) \"DVERK\"", cs_v65, as_v65, bs_v65, ds_v65)

val bs_v65_aux = ratToRCL r2_v65
fun make_rkv65_aux (): 'a stepper1 = core1 (cs_v65, as_v65, bs_v65_aux)
val show_rkv65_aux = rk_show1 ("Verner (5)", cs_v65, as_v65, bs_v65_aux)

end
