(* Module for testing Runge-Kutta integration of ODEs
   Based on code by  Uwe Hollerbach <uh@alumni.caltech.edu>
*)

structure RungeKuttaTest =
struct

open RungeKutta

val summer = Real.+
val scaler = Real.*

infix 7 */
infix 6 +/
infix //



(* Solve the test problem dy/dt = -t^3 y^3 

   which has the exact solution y = 1/sqrt(C + t^4/2)
   where C = 1/y_0^2 - t_0^4/2
*)

val con = ~0.4
fun deriv (t,y) = con*y
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
      val stn = integrator (t,st)
      val tn  = Real.+(t,h)
  in 
      if t >= 5.0
      then putStrLn (showst (tn,stn))
      else gen_soln1 (integrator,h,tn,stn)
  end

fun gen_soln2 (integrator,h,t,st) =
  let 
      val (stn,en) = integrator (t,st)
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
   List.app (do_case1 (integrator (scaler,summer,deriv)))
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
   List.app (do_case2 (integrator (scaler,summer,deriv)))
	    (List.tabulate (15, fn x => x - 2));
   putStrLn "# All done!\n")

fun gen_soln3 (integrator,h,t,st) =
  let 
      val (stn,en,inp) = integrator (t,st)
      val tn       = Real.+(t,h)
  in 
      if t >= 5.0
      then (putStr (showst (tn,stn));
            putStrLn ("\t" ^ (showReal (inp 1.0))))
      else gen_soln3 (integrator,h,tn,stn)
  end

fun do_case3 integrator n =
  let 
      val h = if n < 0 then Real.Math.pow (2.0,Real.fromInt (~n)) 
	      else Real.Math.pow (0.5,Real.fromInt n)
      val sep = if n <= 4 then "\t\t" else "\t"
  in
      putStr (String.concat [(showReal h), sep]);
      gen_soln3 (integrator h,h,t0,y0)
  end

fun solver3 (integrator,stats) =
  (putStrLn stats;
   putStrLn "# step yf err uf";
   List.app (do_case3 (integrator (scaler,summer,deriv)))
	    (List.tabulate (15, fn x => x - 2));
   putStrLn "# All done!\n")


val rkfe: real stepper1 = make_rkfe()
val rk3:  real stepper1 = make_rk3()
val rk4a: real stepper1 = make_rk4a()
val rk4b: real stepper1 = make_rk4b()

val rkhe:  real stepper2 = make_rkhe()
val rkbs:  real stepper2 = make_rkbs()
val rkoz:  real stepper2 = make_rkoz()
val rkn34: real stepper2 = make_rkn34()
val rkf45: real stepper2 = make_rkf45()
val rkck:  real stepper2 = make_rkck()
val rkdp:  real stepper2 = make_rkdp()
val rkdpb: real stepper2 = make_rkdpb()
val rkf78: real stepper2 = make_rkf78()
val rkv65: real stepper2 = make_rkv65()

val rkhe_aux:  real stepper1  = make_rkhe_aux()
val rkbs_aux:  real stepper1  = make_rkbs_aux()
val rkoz_aux: real stepper1  = make_rkoz_aux()
val rkn34_aux: real stepper1  = make_rkn34_aux()
val rkf45_aux: real stepper1  = make_rkf45_aux()
val rkck_aux:  real stepper1  = make_rkck_aux()
val rkdp_aux:  real stepper1  = make_rkdp_aux()
val rkdpb_aux:  real stepper1 = make_rkdpb_aux()
val rkf78_aux: real stepper1  = make_rkf78_aux()
val rkv65_aux: real stepper1  = make_rkv65_aux()

val cerkdp:  real stepper3 = make_cerkdp()
val cerkoz:  real stepper3 = make_cerkoz()


fun run() =
 (putStrLn "#### Non-Adaptive Solvers";
  List.app solver1 [(rkfe, show_rkfe),
                    (rk3,  show_rk3),
                    (rk4a, show_rk4a),
                    (rk4b, show_rk4b)];
  putStrLn "#### Adaptive Solvers";
  List.app solver2 [(rkhe, show_rkhe),
                    (rkbs, show_rkbs),
                    (rkoz, show_rkoz),
                    (rkn34, show_rkn34),
                    (rkf45, show_rkf45),
                    (rkck, show_rkck),
                    (rkdp, show_rkdp),
                    (rkdpb, show_rkdpb),
                    (rkf78, show_rkf78),
                    (rkv65, show_rkv65)];
  putStrLn "#### Continuous Solvers";
  List.app solver3 [(cerkoz, show_cerkoz)];
  List.app solver3 [(cerkdp, show_cerkdp)];
  putStrLn "#### Auxiliary Solvers: Error Estimators from Adaptives";
  List.app solver1 [(rkhe_aux, show_rkhe_aux),
		    (rkbs_aux, show_rkbs_aux),
		    (rkoz_aux, show_rkoz_aux),
		    (rkn34_aux, show_rkn34_aux),
		    (rkf45_aux, show_rkf45_aux),
		    (rkck_aux, show_rkck_aux),
		    (rkdp_aux, show_rkdp_aux),
		    (rkdpb_aux, show_rkdpb_aux),
		    (rkf78_aux, show_rkf78_aux),
                    (rkv65_aux, show_rkv65_aux)]
  )

val _ = run()
end
