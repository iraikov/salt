(* Module for testing Runge-Kutta integration of ODEs
   Based on code by  Uwe Hollerbach <uh@alumni.caltech.edu>
*)

structure RungeKuttaTest =
struct
    
structure State: RKSTATE =
struct

type state = real
fun sum (a,b,y) = Real.+(a,b)
fun scale  (a,b,y) = Real.* (a,b)
fun copy (a,b)  = a
fun state ()   = 0.0

end
    
structure RungeKutta = RungeKuttaFn (structure S = State)

open RungeKutta

(* Solve the test problem dy/dt = -t^3 y^3 

   which has the exact solution y = 1/sqrt(C + t^4/2)
   where C = 1/y_0^2 - t_0^4/2
*)

val con = ~0.4
fun deriv (t,y,_) = con*y
val t0 = 0.0
val y0 = 1.75
fun exact t = y0*Real.Math.exp(con*(t - t0))

fun putStr str =
    (TextIO.output (TextIO.stdOut, str))

fun putStrLn str =
    (TextIO.output (TextIO.stdOut, str);
     TextIO.output (TextIO.stdOut, "\n"))

fun showReal n = Real.toString n

fun showst (t, y) = String.concat [(showReal y), "\t", (showReal (y - (exact t)))]

fun gen_soln1 (integrator,h,t,st) =
  let 
      val stn = integrator (t,st,st)
      val tn  = Real.+(t,h)
  in 
      if t >= 5.0
      then putStrLn (showst (tn,stn))
      else gen_soln1 (integrator,h,tn,stn)
  end

fun gen_soln2 (integrator,h,t,st) =
  let 
      val (stn,en) = integrator (t,st,st)
      val tn       = Real.+(t,h)
  in 
      if t >= 5.0
      then putStrLn (showst (tn,stn))
      else gen_soln2 (integrator,h,tn,stn)
  end

fun do_case1 integrator n =
  let 
      val h = if n < 0 then Real.Math.pow (2.0,Real.fromInt (~n)) 
	      else Real.Math.pow (0.5,Real.fromInt n)
      val sep = if n <= 4 then "\t\t" else "\t"
  in
      putStr (String.concat [(showReal h), sep]);
      gen_soln1 (integrator h,h,t0,y0)
  end

fun solver1 (integrator,stats) =
  (putStrLn stats;
   putStrLn "# step yf err";
   List.app (do_case1 (integrator deriv))
	    (List.tabulate (15, fn x => x - 2));
   putStrLn "# All done!\n")

fun do_case2 integrator n =
  let 
      val h = if n < 0 then Real.Math.pow (2.0,Real.fromInt (~n)) 
	      else Real.Math.pow (0.5,Real.fromInt n)
      val sep = if n <= 4 then "\t\t" else "\t"
  in
      putStr (String.concat [(showReal h), sep]);
      gen_soln2 (integrator h,h,t0,y0)
  end

fun solver2 (integrator,stats) =
  (putStrLn stats;
   putStrLn "# step yf err";
   List.app (do_case2 (integrator deriv))
	    (List.tabulate (15, fn x => x - 2));
   putStrLn "# All done!\n")

fun gen_soln3 (integrator,interp,h,t,st) =
  let 
      val (stn,en,inptbl) = integrator (t,st,st)
      val tn       = Real.+(t,h)
  in 
      if t >= 5.0
      then (putStr (showst (tn,stn));
            putStrLn ("\t" ^ (showReal (interp (h, inptbl, t, st) 1.0))))
      else gen_soln3 (integrator,interp,h,tn,stn)
  end

fun do_case3 (integrator, interp) n =
  let 
      val h = if n < 0 then Real.Math.pow (2.0,Real.fromInt (~n)) 
	      else Real.Math.pow (0.5,Real.fromInt n)
      val sep = if n <= 4 then "\t\t" else "\t"
  in
      putStr (String.concat [(showReal h), sep]);
      gen_soln3 (integrator h,interp,h,t0,y0)
  end

fun solver3 (integrator,stats,interp) =
  (putStrLn stats;
   putStrLn "# step yf err uf";
   List.app (do_case3 (integrator deriv, interp))
	    (List.tabulate (15, fn x => x - 2));
   putStrLn "# All done!\n")



fun run() =
 (
  putStrLn "#### Continuous Solvers";
  List.app solver3 [(cerkoz3, show_cerkoz3, interp_cerkoz3)];
  List.app solver3 [(cerkoz4, show_cerkoz4, interp_cerkoz4)];
  List.app solver3 [(cerkoz5, show_cerkoz5, interp_cerkoz5)];
  List.app solver3 [(cerkdp, show_cerkdp,   interp_cerkdp)];
  putStrLn "#### Auxiliary Solvers: Error Estimators from Adaptives";
  List.app solver1 [(rkoz3_aux, show_rkoz3_aux),
                    (rkoz4_aux, show_rkoz4_aux),
                    (rkoz5_aux, show_rkoz5_aux),
		    (rkdp_aux, show_rkdp_aux)]
  )

val _ = run()
end
