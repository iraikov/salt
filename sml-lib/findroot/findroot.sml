
structure FindRoot =
struct

(* Numeric root finding
   Based on Ocaml code by Edgar Friendly <thelema314@gmail.com>
   Use under LGPL2.1 license + OCaml linking exception

   Does not assume differentiability of the function whose roots are
   being found.

   Implements the following algorithms: 
   * Bisection (c = (a+b)/2)
   * Secant method (linear interpolation from a and b)
   * Brent's method (bisection + secant + quadratic interpolation)

*)

val debug = false
val trace = false

val B = Printf.B       
val I = Printf.I
val R = Printf.R       
val ` = Printf.`
val $ = Printf.$

                
(* Internal function for brent's method *)
fun brent_int xdelta ydelta f a fa b fb c fc mflag d i =
  (if debug 
   then Printf.printf `"a:"R `" fa:"R `" b:"R `" fb:"R `" c:"R `" fc:"R `"\n" $ a fa b fb c fc
   else ();
   let open Real
       val s = (if Real.!= (fb, fc) andalso Real.!= (fa, fc) then 
                    (* inverse quadratic interpolation *)
                    (if debug then Printf.printf `"IQI" $ else ();
                     a * fb * fc / (fa - fb) / (fa - fc) +
                 	b * fa * fc / (fb - fa) / (fb - fc) +
                	c * fa * fb / (fc - fa) / (fc - fb))
                else (* secant rule *)
                    (if debug then Printf.printf `" S " $ else ();
                     b - fb * (b - a) / (fb - fa)))
   in
       if debug then Printf.printf `"s0:"R`"\n" $ s else ();
       (* condition 1-5 to reject above and use bisection instead *)
       let val c1 = if a < b 
                    then s < (3.0 * a + b) * 0.25 andalso s > b 
                    else s > (3.0 * a + b) * 0.25 andalso s < b 
           val c2 = (mflag andalso Real.abs (s - b) >= Real.abs (b - c) / 2.0) 
           val c3 = (not mflag andalso Real.abs (s - b) >= Real.abs (c - d) / 2.0) 
           val c4 = (mflag andalso Real.abs (b - c) < xdelta) 
           val c5 = (not mflag andalso Real.abs (c - d) < xdelta)
       in
           if debug then Printf.printf `"c1: "B `"c2: "B `"c3: "B `"c4: "B `"c5: "B  `"\n" $ c1 c2 c3 c4 c5 else ();
           let val (s, mflag) = (* TODO: don't compute all conditions *)
                   if c1 orelse c2 orelse c3 orelse c4 orelse c5 then ((a + b) / 2.0, true)
                   else (s, false)
           in
               let val fs = f s 
                   val _ = if trace then Printf.printf `"p_"I `":"R `" f(p_"I `")="R `"\n" $ i s i fs else ();
                   in
                       if Real.abs fs < ydelta
                       then s
                       else
                           if fa * fs < 0.0 then 
                               brent_int_swap xdelta ydelta f a fa s fs b fb mflag c (Int.+(i,1))
                           else
                               brent_int_swap xdelta ydelta f s fs b fb b fb mflag c (Int.+(i,1))
               end
           end
       end
   end)

(* helper for a-b swapping and xdelts checks *)
and brent_int_swap xdelta ydelta f a fa b fb c fc mflag d i =
  (* finish rootfinding if our range is smaller than xdelta *)
  if Real.abs (b-a) < xdelta then b
  else
  (* ensure that fb is the best estimate so far by swapping b with a *)
    if Real.abs fa < Real.abs fb then 
      brent_int xdelta ydelta f b fb a fa c fc mflag d i
    else
      brent_int xdelta ydelta f a fa b fb c fc mflag d i

fun error_bracket (loc, a, fa, b, fb) =
  raise Fail (loc ^ ": root must be bracketed:" ^
              " f(" ^ (Real.toString a) ^ ") = " ^ (Real.toString fa) ^
              " sign(f(" ^ (Real.toString a) ^ ")) = " ^ (Int.toString (Real.sign fa)) ^	
              " f(" ^ (Real.toString b) ^ ") = " ^ (Real.toString fb) ^
              " sign(f(" ^ (Real.toString b) ^ ")) = " ^ (Int.toString (Real.sign fb)) ^	
              " fa * fb = " ^ (Real.toString (fa * fb)))
        
fun brent delta f a b =
  let val fa = f a 
      val fb = f b
  in
      if fa * fb >= 0.0
      then (if Real.sign(a) = 0 andalso Real.sign(b) = 1
      	   then 0.0 else error_bracket ("RootFind.brent", a, fa, b, fb))
           (* xdelta = ydelta = delta *)
      else brent_int_swap delta delta f a fa b fb a fa true 0.0 1
  end

fun bisect_int delta f a fa b fb =
  let val m = (a + b) * 0.5 
      val fm = f m
  in
      if Real.abs fm < delta orelse (b-a) * 0.5 < delta then m
      else
          (if fa * fm < 0.0 then 
               bisect_int delta f a fa m fm
           else
               bisect_int delta f m fm b fb)
  end

fun bisection delta f a b =
  let val fa = f a 
      val fb = f b
  in
      if fa * fb >= 0.0
      then error_bracket ("RootFind.bisection", a, fa, b, fb)
      else bisect_int delta f a fa b fb
  end

fun secant_int delta f a fa b fb =
  let val m = b - fb * (b - a) / (fb - fa) 
      val fm = f m
  in
      if Real.abs fm < delta orelse (b-a) * 0.5 < delta
      then m
      else (if fa * fm < 0.0 then 
                bisect_int delta f a fa m fm
            else
                bisect_int delta f m fm b fb)
  end

fun secant delta f a b =
  let val fa = f a 
      val fb = f b
  in
      if fa * fb >= 0.0
      then error_bracket ("RootFind.secant", a, fa, fa, fb)
      else secant_int delta f a fa b fb
  end

fun f x = 4.0 * x * x * x - 16.0 * x * x + 17.0 * x - 4.0
(* Actual roots:
0.3285384586114149
1.2646582900644197
2.4068032513241651
 *)

fun f1 x = (x + 3.0) * (x - 1.0) * (x - 1.0) 
(* roots: -3, 1 (double root) *)

fun f2 x = (Math.tan x) - 2.0 *  x
(* root: 1.16556118520721 *)

fun test get_root =
  
  let
      val root0 = get_root f 0.0 1.0 
      val root1 = get_root f 1.0 2.0 
      val root2 = get_root f 2.0 3.0
  in
      Printf.printf `"Roots of 4x^3-16x^2+17x-4 are:" `"\n"R `"\n"R `"\n"R `"\n" $ root0 root1 root2;
      let val root0 = get_root f1 (~4.0) (4.0 / 3.0)
      in
          Printf.printf `"One root of (x+3)(x-1)^2 is:" `"\n"R `"\n" $ root0;
          let val root0 = get_root f2 0.5 1.5 in
              Printf.printf `"One solution of tan x = 2x is:" `"\n"R `"\n" $ root0
          end
      end
  end

val delta = 1E~15

fun test_all () = 
  (test (brent delta);
   test (secant delta);
   test (bisection delta))

end
