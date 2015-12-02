(* Module for testing Runge-Kutta integration of ODEs
   Based on code by  Uwe Hollerbach <uh@alumni.caltech.edu>
*)

structure CRungeKuttaTest =
struct

open CRungeKutta

infix 7 */
infix 6 +/
infix //



fun putStr str =
    (TextIO.output (TextIO.stdOut, str))

fun putStrLn str =
    (TextIO.output (TextIO.stdOut, str);
     TextIO.output (TextIO.stdOut, "\n"))

val t0 = 0.0
val y0 = Array.array (1, 1.75)
val con = ~0.4
fun exact t = 1.75*Real.Math.exp(con*(t - t0))

fun showReal n = Real.toString n

fun showst (t, y) = String.concat [showReal (Array.sub(y,0)), "\t", 
                                   showReal (Array.sub(y,0) - (exact t))]

fun gen_soln (integrator,h,t,st) =
  let 
      val stn = Array.array (1, 0.0)
      val err = Array.array (1, 0.0)
      val _   = integrator h (t,st,stn,err)
      val tn  = Real.+(t,h)
  in 
      if t >= 5.0
      then putStrLn (showst (tn,stn))
      else gen_soln (integrator,h,tn,stn)
  end

fun do_case integrator n =
  let 
      val h = if n < 0 then Real.Math.pow (2.0,Real.fromInt (~n)) 
	      else Real.Math.pow (0.5,Real.fromInt n)
      val sep = if n <= 4 then "\t\t" else "\t"
  in
      putStr (String.concat [(showReal h), sep]);
      gen_soln (integrator,h,t0,y0)
  end

val cb = _address "rhsfun" public: MLton.Pointer.t;


fun run (integrator) =
  (putStrLn "# step yf err";
   List.app (do_case integrator)
	    (List.tabulate (15, fn x => x - 2));
   putStrLn "# All done!\n")


val _ = run (make_c_rkdp(1,cb) (Array.fromList [con]))


end
