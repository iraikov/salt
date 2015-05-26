
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
      

fun start (f,initial,evinitial,tmax) =
    let
	fun run (t, input, ev) =
	    let val (t',nstate,ev') = f (t,input,ev)
	    in printstate (t',nstate); 
	       if (t'  > tmax)
	       then (putStrLn "# All done!"; nstate)
	       else (run (t',nstate,ev'))
	    end
    in
	printstate (0.0, initial);
	run (0.0, initial, evinitial)
    end

val h = 0.01
val p = Model.paramfun()
val initial = Model.initfun(p)
val evinitial = Model.initcondfun()
val f = Model.eintegral(Model.sysinds,Model.eqfun(p),Model.condfun(p),Model.posfun(p),Model.negfun(p),h)
val _ = start (f,initial,evinitial,100.0)

