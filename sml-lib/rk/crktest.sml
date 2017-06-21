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

val numit = 18
                
fun showReal n = Real.toString n

val cb = _address "rhsfun" public: MLton.Pointer.t;
val size_closure_cont = 4

fun showst (t, y, e) = String.concat [showReal (Array.sub(y,0)), "\t", 
                                      showReal (Array.sub(y,0) - (exact t)), "\t",
                                     showReal (Array.sub(e,0))]

fun gen_soln (integrator,interp,h,t,st,err) =
  let 
      val yout = Array.array (1, 0.0)
      val (stn,en,inptbl) = integrator (h,t,st,yout,err)
      val tn       = Real.+(t,h)
  in 
      if t >= 5.0
      then (putStr (showst (tn,stn,en));
            putStrLn ("\t" ^ (showReal (Array.sub (interp (h, inptbl, t, st) 1.0, 0)))))
      else gen_soln (integrator,interp,h,tn,stn,err)
  end

fun do_case (integrator, interp) n =
  let
      val err = Array.array (1, 0.0)
      val h   = if n < 0 then Real.Math.pow (2.0,Real.fromInt (~n)) 
	        else Real.Math.pow (0.5,Real.fromInt n)
      val sep = if n <= 4 then "\t\t" else "\t"
  in
      putStr (String.concat [(showReal h), sep]);
      gen_soln (integrator,interp,h,t0,y0,err)
  end

fun solver (integrator,stats,interp) =
  (putStrLn stats;
   putStrLn "# step yf err uf";
   List.app (do_case (integrator, interp))
	    (List.tabulate (numit, fn x => x - 2));
   putStrLn "# All done!\n")


fun run_h (integrator, interp) h =
  let
      val err = Array.array (1, 0.0)
      val sep = "\t\t"
  in
      putStr (String.concat [(showReal h), sep]);
      gen_soln (integrator,interp,h,t0,y0,err)
  end


fun run (integrator,interp) =
  (putStrLn "# step yf delta err est";
   List.app (do_case (integrator,interp))
	    (List.tabulate (numit, fn x => x - 2));
   putStrLn "# All done!\n")


val _ = let val stepper = make_crkdp(1,cb)  
            val p     = Array.fromList [con]
            val fld   = Array.fromList []
            val ext   = Array.fromList []
            val extev = Array.fromList []
            val clos  = alloc_closure size_closure_cont
        in 
            run (fn(h,t,y,yout,err) =>
                    let
                        val _ = update_closure_cont (p,fld,ext,extev,clos)
                    in
                        stepper (clos,h,t,y,yout,err)
                    end,
                 make_crkdp_hinterp 1)
        end


end
