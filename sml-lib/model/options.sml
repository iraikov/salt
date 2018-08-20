
structure Options = 
struct

structure G = GetOpt

exception Error
	      

datatype flag =  Help | Time of real | Timestep of real | AbsTol of real | RelTol of real


fun showflag (Help)       = "Help"
  | showflag (Time x)     = ("Time " ^ (Real.toString x))
  | showflag (Timestep x) = ("Timestep " ^ (Real.toString x))
  | showflag (AbsTol x)   = ("AbsTol " ^ (Real.toString x))
  | showflag (RelTol x)   = ("RelTol " ^ (Real.toString x))
		   

val options = 
    [
     {short="h",
      long=["help"],
      desc=G.NoArg (fn() => Help),
      help="show help"},

     {short="d",
      long=["duration"],
      desc=G.ReqArg (fn(x) => Time (valOf(Real.fromString x)), "N"),
      help="simulation duration"},

     {short="",
      long=["abstol"],
      desc=G.ReqArg (fn(x) => AbsTol (valOf(Real.fromString x)),"N"),
      help="absolute error tolerance"},

     {short="",
      long=["reltol"],
      desc=G.ReqArg (fn(x) => RelTol (valOf(Real.fromString x)),"N"),
      help="relative error tolerance"},

     {short="",
      long=["timestep"],
      desc=G.ReqArg (fn(x) => Timestep (valOf(Real.fromString x)),"N"),
      help="simulation timestep"}

    ]

fun optError (status) (msg) = (status := SOME msg)

fun getopt status = (G.getOpt {argOrder=G.Permute, errFn=optError status,
			       options=options}) 

fun header (progname) = concat ["Usage: ", progname, " [OPTION...] files..."]

fun usage (progname) = G.usageInfo {header=(header progname), options=options}

fun getstate (opts) =

    let
	val O_HELP       = ref false
	val O_ABSTOL     = ref NONE
	val O_RELTOL     = ref NONE
	val O_TIME       = ref NONE
	val O_TIMESTEP   = ref NONE

	fun getstate' (opt) = 
	    (case opt of 
		 Help        => O_HELP := true	
	       | AbsTol x    => O_ABSTOL := SOME x
	       | RelTol x    => O_RELTOL := SOME x
	       | Time x      => O_TIME := SOME x
	       | Timestep x  => O_TIMESTEP := SOME x
            )

	val _ = app getstate' opts

    in {
        is_help=(!O_HELP), 
        is_abstol=(!O_ABSTOL), 
        is_reltol=(!O_RELTOL), 
        is_time=(!O_TIME),
	is_timestep=(!O_TIMESTEP)
       }
    end

end
