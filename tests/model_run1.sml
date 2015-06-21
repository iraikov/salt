
structure FHM = FunctionalHybridDynamics

fun optApply fopt args =
    case fopt of SOME f => SOME (f args) | NONE => NONE

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
      

fun start (f,initial,SOME evinitial,SOME dinitial,SOME rinitial,tmax) =
    let
	fun run (rs as FHM.RegimeState (t, input, ev, d, regime)) =
            (case f rs of
	         rs' as FHM.RegimeState (t',nstate,ev',d',regime') =>
                 (printstate (t',nstate); 
	          if (t'  > tmax)
	          then (putStrLn "# All done!"; nstate)
	          else (run rs'))
               | _ => raise Domain)
            | run _ = raise Domain
    in
	printstate (0.0, initial);
	run (FHM.RegimeState (0.0, initial, evinitial, dinitial, rinitial))
    end
|  start (f,initial,SOME evinitial,NONE,NONE,tmax) =
    let
	fun run (es as FHM.EventState (t, input, ev)) =
            (case f es of
	         es' as FHM.EventState (t',nstate,ev') =>
                 (printstate (t',nstate); 
	          if (t'  > tmax)
	          then (putStrLn "# All done!"; nstate)
	          else (run es'))
               | _ => raise Domain)
            | run _ = raise Domain
    in
	printstate (0.0, initial);
	run (FHM.EventState (0.0, initial, evinitial))
    end
|  start (f,initial,NONE,NONE,NONE,tmax) =
    let
	fun run (cs as FHM.ContState (t, input)) =
            (case f cs of
	         cs' as FHM.ContState (t',nstate) =>
                 (printstate (t',nstate); 
	          if (t'  > tmax)
	          then (putStrLn "# All done!"; nstate)
	          else (run cs'))
               | _ => raise Domain)
            | run _ = raise Domain
    in
	printstate (0.0, initial);
	run (FHM.ContState (0.0, initial))
    end
|  start (_,_,_,_,_,_) = raise Domain


val h = 0.01
val tstop = 150.0
val p         = Model.paramfun()
val initial   = Model.initfun(p)
val evinitial = optApply Model.initcondfun ()
val dinitial  = optApply Model.dinitfun ()
val rinitial  = optApply Model.initregfun ()
val f = FHM.integral1(Model.odefun(p),optApply Model.condfun p,optApply Model.posfun p,optApply Model.negfun p,
                      optApply Model.dposfun p,optApply Model.regfun (),h)
val _ = start (f,initial,evinitial,dinitial,rinitial,tstop)

