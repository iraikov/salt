structure D = Dynamics

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
        val n = Array.length input
        fun fshow i =
            if i < n
            then (putStr(showReal (Array.sub(input,i))  ^ " ");
                  fshow(i+1))
            else putStrLn("")
    in
        ( putStr ((showReal (t)) ^ " "); fshow 0 )
    end
      

fun start (f,initial,SOME evinitial,SOME dinitial,SOME rinitial,extinitial,tmax,h0) =
    let
	fun run (rs as D.RegimeState (t, input, ev, d, regime, ext, h, _)) =
            (case f rs of
	         rs' as D.RegimeState (t',nstate,ev',d',regime',_,h',_) =>
                 (printstate (t',nstate); 
	          if (t'  > tmax)
	          then (putStrLn "# All done!"; nstate)
	          else (run rs'))

               | _ => raise Domain)
            | run _ = raise Domain
    in
	printstate (0.0, initial);
	run (D.RegimeState (0.0, initial, evinitial, dinitial, rinitial, extinitial, h0, false))
    end
|  start (f,initial,SOME evinitial,NONE,NONE,extinitial,tmax,h0) =
    let
	fun run (es as D.EventState (t, input, ev, ext, h, _)) =
            (case f es of
	         es' as D.EventState (t',nstate,ev',_,h',_) =>
                 (printstate (t',nstate); 
	          if (t'  > tmax)
	          then (putStrLn "# All done!"; nstate)
	          else (run es'))
               | _ => raise Domain)
            | run _ = raise Domain
    in
	printstate (0.0, initial);
	run (D.EventState (0.0, initial, evinitial, extinitial, h0, false))
    end
|  start (f,initial,NONE,NONE,NONE,ext,tmax,h0) =
    let
	fun run (cs as D.ContState (t, input, ext, h)) =
            (case f cs of
	         cs' as D.ContState (t',nstate,_,h') =>
                 (printstate (t',nstate); 
	          if (t'  > tmax)
	          then (putStrLn "# All done!"; nstate)
	          else (run cs'))
               | _ => raise Domain)
            | run _ = raise Domain
    in
	printstate (0.0, initial);
	run (D.ContState (0.0, initial, ext, h0))
    end
|  start _ = raise Domain


val h0          = 0.01
val tstop       = 150.0
val p           = Model.paramfun()
val initial     = Model.initfun(p)
val evinitial   = optApply Model.initcondfun ()
val dinitial    = optApply Model.dinitfun ()
val rinitial    = optApply Model.initregfun ()
val extinitial  = Model.initextfun ()
val f = D.integral(Model.odefun(p),optApply Model.condfun p,optApply Model.posfun p,optApply Model.negfun p,
                   optApply Model.dposfun p,Model.regfun)
val _ = start (f,initial,evinitial,dinitial,rinitial,extinitial,tstop,h0)

