signature MODEL =
sig

    type model_stepper
    type model_condition
    type model_response
    type hinterp
    
    val n: int
                
    val nev: int
                  
    val nregime: int
                      
    val ndsc: int

    val make_real_state : int -> real array
    val make_bool_state : int -> bool array

    val paramfun: unit -> real array
                               
    val fieldfun: unit -> real array
                               
    val initfun: real array * real array -> real array -> real array
                               
    val initextfun: real array * real array -> unit -> real array
                               
    val initextevfun: real array * real array -> unit -> real array
                               
    val dinitfun: (real array * real array -> real array -> real array) option
                               
    val initcondfun: (real array -> real array) option
                               
    val initregfun: (bool array -> bool array) option
                               
    val stepfun: {ext: real array,
                  extev: real array,
                  fld: real array,
                  p: real array} *
                 real *
                 real *
                 real array *
                 real array ->
                 real array * real array * real array FunQueue.t

    val odefun: real array * real array -> model_stepper

    val condfun: (real array * real array -> model_condition) option

    val posfun: (real array * real array -> model_response) option

    val negfun: (real array * real array -> model_response) option

    val dposfun: (real array * real array -> (real * real array * real array * real array -> real array)) option

    val regfun: (real array * bool array -> bool array) option

    val interpfun: hinterp
                       
                                               
end
