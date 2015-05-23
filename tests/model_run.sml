
fun putStrLn str = 
    (TextIO.output (TextIO.stdOut, str);
     TextIO.output (TextIO.stdOut, "\n"))
    
fun putStr str = 
    (TextIO.output (TextIO.stdOut, str))
    
fun showReal n = 
    let open StringCvt
	open Real
    in
	(if n < 0.0 then "-" else "") ^ (fmt (FIX (SOME 12)) (abs n))
    end

fun printstate (t,input) = 
    ( (showReal (t)) ^ " " ^ (showReal (Vector.sub(input,0)))  ^ " " ^ (showReal (Vector.sub(input,1))) )

fun start (initial,tmax,f) =
    let
	fun run (t, input) =
	    let val (t',nstate) = f (t, input)
	    in putStrLn (printstate (t,nstate));
	       if (t'  > tmax)
	       then (putStrLn "# All done!"; nstate)
	       else (run (t',nstate))
	    end
    in
	printstate (0.0, initial);
	run (0.0, initial)
    end

val h = 0.01
val _ = start (Model.initfun(),50.0,Model.integral(Model.eqfun,h))

