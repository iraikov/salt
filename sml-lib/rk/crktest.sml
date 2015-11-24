(* Module for testing Runge-Kutta integration of ODEs
   Based on code by  Uwe Hollerbach <uh@alumni.caltech.edu>
*)

structure CRungeKuttaTest =
struct

open CRungeKutta

infix 7 */
infix 6 +/
infix //



(* Solve the test problem dy/dt = -t^3 y^3 

   which has the exact solution y = 1/sqrt(C + t^4/2)
   where C = 1/y_0^2 - t_0^4/2
*)

fun putStr str =
    (TextIO.output (TextIO.stdOut, str))

fun putStrLn str =
    (TextIO.output (TextIO.stdOut, str);
     TextIO.output (TextIO.stdOut, "\n"))

val con = ~0.4
fun deriv (t,y,yout) = (putStrLn ("deriv: t = " ^ (Real.toString t) ^ " y[0] = " ^ (Real.toString (Array.sub(y,0))));
                        Array.update(yout,0,con*Array.sub(y,0)); 0)
val t0 = 0.0
val y0 = Array.array (1, 1.75)
fun exact t = 1.75*Real.Math.exp(con*(t - t0))

fun showReal n = Real.toString n

fun showst (t, y) = String.concat [showReal (Array.sub(y,0)), "\t", 
                                   showReal (Array.sub(y,0) - (exact t))]

fun gen_soln (integrator,h,t,st) =
  let 
      val (stn,en) = integrator h (t,st)
      val tn       = Real.+(t,h)
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

val e  = _export "deriv": (real * real array * real array -> int) -> unit;
val cb = _address "deriv" public: MLton.Pointer.t;


fun run (integrator) =
  (putStrLn "# step yf err";
   e deriv;
   List.app (do_case integrator)
	    (List.tabulate (15, fn x => x - 2));
   putStrLn "# All done!\n")


val _ = run (make_c_rkdp(1,cb))


end
