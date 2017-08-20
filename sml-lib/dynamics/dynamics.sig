signature FUNCTIONAL_HYBRID_DYNAMICS =
  sig
    exception ConvergenceError

    type regime_state     = bool array
    type dsc_state        = real array
    type event_state      = real array
    type cont_state       = real array
    type external_state   = real array
    type externalev_state = real array
                                 
    datatype ('a,'b) either = Left of 'a | Right of 'b
    datatype ('a,'b,'c) ternary = Far of 'c | Mid of 'b | Near of 'a
                                                                      
    type controller_state = (real * real * real, real * real * real) either
                                                                    
    val controller_h : ('a * 'b * 'c,'a * 'd * 'e) either -> 'a
    val controller_r : ('a * 'b * 'c,'d * 'e * 'c) either -> 'c
    val controller_cst : ('a * 'b * 'c,'d * 'b * 'e) either -> 'b
    val controller_update_h : ('a * 'b * 'c,'d * 'e * 'f) either * real
                              -> (real * 'b * 'c,real * 'e * 'f) either
    val controller_scale_h : (real * 'a * 'b,real * 'c * 'd) either * real
                             -> (real * 'a * 'b,real * 'c * 'd) either
                  
    datatype model_root
      = RootAfter of int * real list
      | RootBefore
      | RootFound of int * real list
      | RootStep of real list

    val showRoot : model_root -> string
                         
                                     
    datatype model_state
      = ContState of real * real * cont_state * external_state * 
                     externalev_state * cont_state * controller_state
      | EventState of real * real * cont_state * event_state * external_state
                      * externalev_state * cont_state * cont_state * 
                      event_state * controller_state * model_root
      | RegimeState of real * real * cont_state * event_state * dsc_state * 
                       regime_state * external_state * externalev_state * 
                       cont_state * cont_state * event_state * 
                       controller_state * model_root
                                              
    datatype model_stepper
      = ContStepper of external_state * externalev_state * real * real * 
                       cont_state * cont_state
                       -> cont_state * cont_state * real array FunQueue.t
      | EventStepper of external_state * externalev_state * real * real * 
                        cont_state * cont_state
                        -> cont_state * cont_state * real array FunQueue.t
      | RegimeStepper of dsc_state * regime_state * external_state * 
                         externalev_state * real * real * cont_state * 
                         cont_state
                         -> cont_state * cont_state * real array FunQueue.t
                                                           
    datatype model_condition
      = RegimeCondition of real * cont_state * event_state * dsc_state * 
                           regime_state * external_state * externalev_state * 
                           event_state
                           -> event_state
      | SCondition of real * cont_state * event_state * external_state * 
                      externalev_state * event_state
                      -> event_state
                             
    datatype model_response
      = RegimeResponse of real * cont_state * event_state * dsc_state * 
                          external_state * externalev_state * cont_state
                          -> cont_state
      | SResponse of real * cont_state * event_state * external_state * 
                     externalev_state * cont_state
                     -> cont_state

                            
    val maxiter : int
    val tol : real option ref
    val maxstep : real ref

    val getindex : 'a array * int -> 'a
    val update : 'a array * int * 'a -> unit
    val vfind : ('a -> bool) -> 'a array -> 'a option
    val vfindi : (int * 'a -> bool) -> 'a array -> (int * 'a) option
    val vmap : ('a -> 'b) -> 'a array -> 'b array -> 'b array
    val vmap2 : ('a * 'b -> 'c) -> 'a array * 'b array * 'c array -> 'c array
    val vfind2 : ('a * 'b -> bool) -> 'a array * 'b array -> int option
    val vfindi2 : ('a * 'b -> bool)
                  -> 'a array * 'b array -> (int * 'b) option
    val vfoldi2 : (int * 'a * 'b * 'c -> 'c)
                  -> 'c -> 'a array * 'b array -> 'c
    val error_estimate : 'a * 'b * real array -> real
    val controller : real
                     -> 'a * 'b * real array * 
                        (real * 'c * 'd,real * 'c * 'd) either
                        -> (real * real * real,real * real * real) either
    val csum : real * real * real -> real * real
    val thr2 : 'a * real * real -> ('a,'a,'a) ternary option
    val fixthr_s : real -> real
    val fixthr : real array -> real array
    val posdetect : real * real array * real * real array
                    -> ((int,int,int) ternary * real * real) list
    val condApply : model_condition option -> model_state -> model_state
    val evresponse_regime : model_response option * model_response option * 
                            (real * cont_state * event_state * dsc_state
                             -> dsc_state) option * (event_state * 'a -> 'b)
                            -> 'c * real * cont_state * event_state * 
                               dsc_state * 'a * external_state * 
                               externalev_state * cont_state
                               -> cont_state * dsc_state * 'b
    val evresponse : model_response option * model_response option
                     -> 'a * real * cont_state * event_state * external_state
                        * externalev_state * cont_state
                        -> cont_state
    val adaptive_regime_stepper : ('a * 'b * 'c * 'd * real * 'e * 'f * 'g
                                   -> 'h * real array * 'i)
                                  -> 'a * 'b * 'c * 'd * real * 'e * 'f * 'g
                                     * 
                                     (real * real * real,real * real * real) 
                                       either
                                     -> 'h * real * 
                                        (real * real * real,real * real * real) 
                                          either * 'i
    val adaptive_stepper : ('a * 'b * real * 'c * 'd * 'e
                            -> 'f * real array * 'g)
                           -> 'a * 'b * real * 'c * 'd * 'e * 
                              (real * real * real,real * real * real) either
                              -> 'f * real * 
                                 (real * real * real,real * real * real) 
                                   either * 'g
    val regime_rootval : (real * 'a * real * 'b -> real -> 'b) * 
                         (real * 'b * real array * 'c * 'd * 'e * 'f * 'g
                          -> real array)
                         -> real * 'a * real * real * 'b * real array * real
                            * real * 'b * real array * 'e * 'f * 'c * 'd * 'g
                            -> ((int,int,int) ternary * real * real * real * 
                                'b) option
    val event_rootval : (real * 'a * real * 'b -> real -> 'b) * 
                        (real * 'b * real array * 'c * 'd * 'e -> real array)
                        -> real * real * real * real * real array * real array * real * 
                           real * real array * real array * real array * real array * real array
                           -> ((int,int,int) ternary * real * real * real * 'b)
                                option
    val subtract_h : real * real list -> real list
    val integral : model_stepper * 
                   (real * real array FunQueue.t * real * cont_state
                    -> real -> cont_state) * model_condition option * 
                   model_response option * model_response option * 
                   (real * cont_state * event_state * dsc_state -> dsc_state) 
                     option * 
                   (event_state * regime_state -> regime_state) option
                   -> model_state -> model_state
  end

