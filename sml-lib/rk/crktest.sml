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

val cb = _address "rhsfun" public: MLton.Pointer.t;

fun showst1 (t, y) = String.concat [showReal (Array.sub(y,0)), "\t", 
                                    showReal (Array.sub(y,0) - (exact t))]

fun gen_soln1 (integrator,h,t,st) =
  let 
      val stn = Array.array (1, 0.0)
      val _   = integrator (h,t,st,stn)
      val tn  = Real.+(t,h)
  in 
      if t >= 5.0
      then putStrLn (showst1 (tn,stn))
      else gen_soln1 (integrator,h,tn,stn)
  end

fun do_case1 (integrator) n =
  let 
      val h = if n < 0 then Real.Math.pow (2.0,Real.fromInt (~n)) 
	      else Real.Math.pow (0.5,Real.fromInt n)
      val sep = if n <= 4 then "\t\t" else "\t"
  in
      putStr (String.concat [(showReal h), sep]);
      gen_soln1 (integrator,h,t0,y0)
  end


fun showst2 (t, y, e) = String.concat [showReal (Array.sub(y,0)), "\t", 
                                       showReal (Array.sub(y,0) - (exact t)), "\t", 
                                       showReal (Array.sub(e,0))]

fun gen_soln2 (integrator,h,t,st) =
  let 
      val stn = Array.array (1, 0.0)
      val err = Array.array (1, 0.0)
      val _   = integrator (h,t,st,stn,err)
      val tn  = Real.+(t,h)
  in 
      if t >= 5.0
      then putStrLn (showst2 (tn,stn,err))
      else gen_soln2 (integrator,h,tn,stn)
  end

fun do_case2 (integrator) n =
  let 
      val h = if n < 0 then Real.Math.pow (2.0,Real.fromInt (~n)) 
	      else Real.Math.pow (0.5,Real.fromInt n)
      val sep = if n <= 4 then "\t\t" else "\t"
  in
      putStr (String.concat [(showReal h), sep]);
      gen_soln2 (integrator,h,t0,y0)
  end



fun run1 (integrator) =
  (putStrLn "# step yf err";
   List.app (do_case1 integrator)
	    (List.tabulate (15, fn x => x - 2));
   putStrLn "# All done!\n")


fun run2 (integrator) =
  (putStrLn "# step yf err est";
   List.app (do_case2 integrator)
	    (List.tabulate (15, fn x => x - 2));
   putStrLn "# All done!\n")


val _ = let val stepper = make_crk3(1,cb)  
            val p = Array.fromList [con]
            val ext = Array.fromList []
            val extev = Array.fromList []
        in 
            run1  (fn(h,t,y,yout) => stepper (p,ext,extev,h,t,y,yout))
        end

val _ = let val stepper = make_crkdp(1,cb)  
            val p = Array.fromList [con]
            val ext = Array.fromList []
            val extev = Array.fromList []
        in 
            run2  (fn(h,t,y,yout,err) => stepper (p,ext,extev,h,t,y,yout,err))
        end

val _ = let val stepper = make_crkbs(1,cb)  
            val p = Array.fromList [con]
            val ext = Array.fromList []
            val extev = Array.fromList []
        in 
            run2  (fn(h,t,y,yout,err) => stepper (p,ext,extev,h,t,y,yout,err))
        end


end
