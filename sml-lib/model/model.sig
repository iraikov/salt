signature MODEL =
sig

    type regime_state  = bool array
    type dsc_state     = real array
    type event_state   = real array
    type cont_state    = real array
    type external_state = real array
    type externalev_state = real array

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

    val paramfun: unit -> cont_state
                               
    val fieldfun: unit -> cont_state
                               
    val initfun: cont_state * cont_state -> cont_state -> cont_state
                               
    val initextfun: cont_state * cont_state -> unit -> external_state
                               
    val initextevfun: cont_state * cont_state -> unit -> externalev_state
                               
    val linkextevfun: externalev_state * externalev_state -> externalev_state
                               
    val dinitfun: (cont_state * cont_state -> dsc_state -> dsc_state) option
                               
    val initcondfun: (event_state -> event_state) option
                               
    val initregfun: (regime_state -> regime_state) option
                                                   
    val odefun: cont_state * cont_state -> model_stepper

    val condfun: (cont_state * cont_state -> model_condition) option

    val posfun: (cont_state * cont_state -> model_response) option

    val negfun: (cont_state * cont_state -> model_response) option

    val dposfun: (cont_state * cont_state -> (real * cont_state * event_state * dsc_state -> dsc_state)) option

    val regfun: (event_state * regime_state -> regime_state) option

    val interpfun: hinterp
                       
                                               
end
