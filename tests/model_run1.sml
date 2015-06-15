
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
    let
        val n = Vector.length input
        fun fshow i =
            if i < n
            then (putStr(showReal (Vector.sub(input,i))  ^ " ");
                  fshow(i+1))
            else putStrLn("")
    in
        ( putStr ((showReal (t)) ^ " "); fshow 0 )
    end
      

fun start (f,initial,evinitial,dinitial,rinitial,tmax) =
    let
	fun run (t, input, ev, d, r) =
	    let val (t',nstate,ev',d',r') = f (t,input,ev,d,r)
	    in printstate (t',nstate); 
	       if (t'  > tmax)
	       then (putStrLn "# All done!"; nstate)
	       else (run (t',nstate,ev',d',r'))
	    end
    in
	printstate (0.0, initial);
	run (0.0, initial, evinitial, dinitial, rinitial)
    end

val h = 0.01
val p = Model.paramfun()
val initial = Model.initfun(p)
val evinitial = Model.initcondfun()
val dinitial = Model.dinitfun()
val rinitial = Model.initregfun()
val f = Model.eintegral(Model.odefun(p),Model.condfun(p),Model.posfun(p),Model.negfun(p),Model.dposfun(p),Model.regfun,h)
val _ = start (f,initial,evinitial,dinitial,rinitial,100.0)

