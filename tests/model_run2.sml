structure D = Dynamics

exception Exit of OS.Process.status 

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

fun exitError (prog, msg) = 
    let 
	val _ = TextIO.output(TextIO.stdErr, prog ^ ": " ^ msg ^ "\n")
    in 
	raise Exit OS.Process.failure 
    end
	    
fun exitHelp prog = 
    let 
	val _ = TextIO.output(TextIO.stdOut, (Options.usage prog) ^ "\n")
    in 
	raise Exit OS.Process.success 
    end

fun printState (t,input) = 
    let
        val n = Array.length input
        fun fshow i =
            if i < n
            then (putStr(showReal (Array.sub(input,i))  ^ " ");
                  fshow(i+1))
            else putStrLn("")
    in
        ( putStr ((showReal (t)) ^ " "); fshow 0 )
    end
      

fun start (f,initial,SOME evinitial,SOME dinitial,SOME rinitial,extinitial,extevinitial,tmax,h0) =
    let
	fun run (rs as D.RegimeState (t, input, ev, d, regime, ext, extev, h, _)) =
            (case f rs of
	         rs' as D.RegimeState (t',nstate,ev',d',regime',_,_,h',_) =>
                 (printState (t',nstate); 
	          if (t'  > tmax)
	          then (putStrLn "# All done!"; nstate)
	          else (run rs'))

               | _ => raise Domain)
            | run _ = raise Domain
    in
	printState (0.0, initial);
	run (D.RegimeState (0.0, initial, evinitial, dinitial, rinitial, extinitial, extevinitial, h0, false))
    end
|  start (f,initial,SOME evinitial,NONE,NONE,extinitial,extevinitial,tmax,h0) =
    let
	fun run (es as D.EventState (t, input, ev, ext, extev, h, _)) =
            (case f es of
	         es' as D.EventState (t',nstate,ev',_,_,h',_) =>
                 (printState (t',nstate); 
	          if (t'  > tmax)
	          then (putStrLn "# All done!"; nstate)
	          else (run es'))
               | _ => raise Domain)
            | run _ = raise Domain
    in
	printState (0.0, initial);
	run (D.EventState (0.0, initial, evinitial, extinitial, extevinitial, h0, false))
    end
|  start (f,initial,NONE,NONE,NONE,ext,extev,tmax,h0) =
    let
	fun run (cs as D.ContState (t, input, ext, extev, h)) =
            (case f cs of
	         cs' as D.ContState (t',nstate,_,_,h') =>
                 (printState (t',nstate); 
	          if (t'  > tmax)
	          then (putStrLn "# All done!"; nstate)
	          else (run cs'))
               | _ => raise Domain)
            | run _ = raise Domain
    in
	printState (0.0, initial);
	run (D.ContState (0.0, initial, ext, extev, h0))
    end
|  start _ = raise Domain


val p           = Model.paramfun()
val initial     = Model.initfun(p) ()
val evinitial   = optApply Model.initcondfun ()
val dinitial    = optApply Model.dinitfun (p)
val rinitial    = optApply Model.initregfun ()
val extinitial  = Model.initextfun (p)
val extevinitial  = Model.initextevfun (p)
val f = D.integral(Model.odefun(p),optApply Model.condfun p,
                   optApply Model.posfun p,optApply Model.negfun p,
                   optApply Model.dposfun p,Model.regfun,Model.alloc,
                   Model.n,case evinitial of SOME ev => SOME (Model.nev) | _ => NONE)

val optStatus = ref NONE
val (opts, _) = (Options.getopt optStatus) (CommandLine.arguments())

val _ = (case !optStatus of 
	     SOME msg => exitError (CommandLine.name(), msg)
	   | NONE => ())
		    
val {is_help, is_time, is_timestep, is_tol} = Options.getstate (opts)
						                 
val _ = if is_help then exitHelp (CommandLine.name()) else ()

val h0     = case is_timestep of SOME dt => dt | NONE => 0.01
val tstop = case is_time of SOME t => t | NONE => 150.0

val _ = start (f,initial,evinitial,optApply dinitial (),rinitial,extinitial(),extevinitial(),tstop,h0)

