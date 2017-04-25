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
      

fun start (f,initial,SOME evinitial,SOME dinitial,SOME rinitial,extinitial,extevinitial,tmax,h0,ynext,rsp,err,SOME evnext) =
    let
	fun run (rs as D.RegimeState (x, cx, input, tev, ev, d, regime, ext, extev, h, _, _, _, _, _)) =
            (case f rs of
	         rs' as D.RegimeState (x',cx',nstate,tev',ev',d',regime',_,_,h',_,_,_,_,_) =>
                 (printState (x',nstate); 
	          if (x'  > tmax)
	          then (putStrLn "# All done!"; nstate)
	          else (run rs'))

               | _ => raise Domain)
            | run _ = raise Domain
    in
	printState (0.0, initial);
	run (D.RegimeState (0.0, 0.0, initial, 0.0, evinitial, dinitial, rinitial, extinitial, extevinitial, h0, ynext, rsp, err, evnext, false))
    end
|  start (f,initial,SOME evinitial,NONE,NONE,extinitial,extevinitial,tmax,h0,ynext,rsp,err,SOME evnext) =
    let
	fun run (es as D.EventState (x, cx, input, tev, ev, ext, extev, h, _, _, _, _, _)) =
            (case f es of
	         es' as D.EventState (x',cx',nstate,tev',ev',_,_,h',_,_,_,_,_) =>
                 (printState (x',nstate); 
	          if (x'  > tmax)
	          then (putStrLn "# All done!"; nstate)
	          else (run es'))
               | _ => raise Domain)
            | run _ = raise Domain
    in
	printState (0.0, initial);
	run (D.EventState (0.0, 0.0, initial, 0.0, evinitial, extinitial, extevinitial, h0, ynext, rsp, err, evnext, false))
    end
|  start (f,initial,NONE,NONE,NONE,ext,extev,tmax,h0,ynext,rsp,err,NONE) =
    let
	fun run (cs as D.ContState (x, cx, input, ext, extev, h, _, _, _)) =
            (case f cs of
	         cs' as D.ContState (x',cx',nstate,_,_,h',_,_,_) =>
                 (printState (x',nstate); 
	          if (x'  > tmax)
	          then (putStrLn "# All done!"; nstate)
	          else (run cs'))
               | _ => raise Domain)
            | run _ = raise Domain
    in
	printState (0.0, initial);
	run (D.ContState (0.0, 0.0, initial, ext, extev, h0, ynext, rsp, err))
    end
|  start _ = raise Domain


val p: real array   = Model.paramfun ()
val fld: real array = Model.fieldfun ()
val initial     = Model.initfun (p, fld) (Model.alloc Model.n)
val evinitial   = optApply Model.initcondfun (Model.alloc Model.nev)
val dinitial    = optApply (optApply Model.dinitfun (p, fld)) (Model.alloc Model.ndsc)
val rinitial    = optApply Model.initregfun (Model.bool_alloc Model.nregime)
val extinitial  = Model.initextfun (p, fld)
val extevinitial  = Model.initextevfun (p, fld)
val next        = Model.initfun (p, fld) (Model.alloc Model.n)
val ynext       = Model.initfun(p, fld) (Model.alloc Model.n)
val rsp         = Model.initfun(p, fld) (Model.alloc Model.n)
val err         = Model.alloc Model.n
val evnext      = optApply Model.initcondfun (Model.alloc Model.nev)

val optStatus = ref NONE
val (opts, _) = (Options.getopt optStatus) (CommandLine.arguments())

val _ = (case !optStatus of 
	     SOME msg => exitError (CommandLine.name(), msg)
	   | NONE => ())
		    
val {is_help, is_time, is_timestep, is_tol, is_solver} = Options.getstate (opts)
						                 
val _ = if is_help then exitHelp (CommandLine.name()) else ()

val h0     = case is_timestep of SOME dt => dt | NONE => 0.01
val tstop = case is_time of SOME t => t | NONE => 150.0
val solver = case is_solver of
                 "rkoz3" => Model.cerkoz3
              |  "rkoz4" => Model.cerkoz4
              |  "rkdp"  => Model.cerkdp
              |  _ => raise Fail ("unknown solver " ^ is_solver)
                                                      

val f = D.integral(Model.odefun solver (p, fld),optApply Model.condfun (p, fld),
                   optApply Model.posfun (p, fld),optApply Model.negfun (p, fld),
                   optApply Model.dposfun (p, fld),Model.regfun)

val _ = start (f,initial,evinitial,dinitial,rinitial,extinitial(),extevinitial(),tstop,h0,ynext,rsp,err,evnext)
