
structure CRungeKutta =
struct

fun putStr str =
    (TextIO.output (TextIO.stdOut, str))

fun putStrLn str =
    (TextIO.output (TextIO.stdOut, str);
     TextIO.output (TextIO.stdOut, "\n"))

val alloc_closure =  _import "alloc_rhs_closure" public: int -> MLton.Pointer.t;
val free_closure =  _import "free_rhs_closure" public: MLton.Pointer.t -> unit;

val update_closure_regime_rs  =  _import "update_closure_regime_rs" public:
                                 real array * real array * real array * bool array *
                                 real array * real array * Word32Array.array * Word64Array.array * Word64Array.array *
                                 Real64Array.array * Real64Array.array * Real64Array.array * Real64Array.array *
                                 MLton.Pointer.t -> MLton.Pointer.t;

val update_closure_cont_rs  =  _import "update_closure_cont_rs" public:
                               real array * real array * real array * real array *
                               Word32Array.array * Word64Array.array * Word64Array.array *
                               Real64Array.array * Real64Array.array * Real64Array.array * Real64Array.array *
                               MLton.Pointer.t -> MLton.Pointer.t;

val update_closure_regime  =  _import "update_closure_regime" public:
                              real array * real array * real array * bool array *
                              real array * real array * MLton.Pointer.t -> MLton.Pointer.t;

val update_closure_cont  =  _import "update_closure_cont" public:
                            real array * real array * real array * real array * MLton.Pointer.t -> MLton.Pointer.t;
       


val c_rkdp = _import "Dormand_Prince_5_4" public: 
             int * MLton.Pointer.t * MLton.Pointer.t * real * real * 
             real array * real * real * real array * real array * real array * real array * 
             real array * real array * real array * real array * real array * real array *
             real array * real array * real array * real array * real array * real array *
             real array * real array * real array * real array * real array * real array
             -> int;

fun make_crkdp (n, fp) =
    let
        val k1 = Array.array (n, 0.0) 
        val k2 = Array.array (n, 0.0) 
        val k3 = Array.array (n, 0.0) 
        val k4 = Array.array (n, 0.0) 
        val k5 = Array.array (n, 0.0) 
        val k6 = Array.array (n, 0.0) 
        val k7 = Array.array (n, 0.0) 
        val t1 = Array.array (n, 0.0) 
        val t2 = Array.array (n, 0.0) 
        val t3 = Array.array (n, 0.0) 
        val t4 = Array.array (n, 0.0) 
        val t5 = Array.array (n, 0.0) 
        val t6 = Array.array (n, 0.0) 
        val t7 = Array.array (n, 0.0) 
        val t8 = Array.array (n, 0.0) 
        val t9 = Array.array (n, 0.0) 
        val t10 = Array.array (n, 0.0) 
        val t11 = Array.array (n, 0.0) 
        val t12 = Array.array (n, 0.0)
        val yscal = Array.array (n, 0.0)
        val q   = [k1,k2,k3,k4,k5,k6,k7]
    in
        fn (clos, h, abstol, reltol, tn, yn, yout, err) => 
           (c_rkdp (n, fp, clos, abstol, reltol, yn, tn, h, yout, err, yscal, k1, k2, k3, k4, k5, k6, k7,
                    t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12);
            (yout,err,q))
    end

        
val c_rkdp_hinterp = _import "Dormand_Prince_5_4_hinterp" public: 
             int * real * real array * real * real * real array * 
             real array * real array * real array * real array * real array * real array * real array *
             real array * real array * real array * real array * real array * real array 
             -> int;


fun make_crkdp_hinterp (n) =
    let
        val t1 = Array.array (n, 0.0) 
        val t3 = Array.array (n, 0.0) 
        val t4 = Array.array (n, 0.0) 
        val t5 = Array.array (n, 0.0) 
        val t6 = Array.array (n, 0.0) 
        val t7 = Array.array (n, 0.0) 
        val yout = Array.array (n, 0.0) 
    in
        fn (h, ks, tn, yn) =>
           let
               
               val (k1,k2,k3,k4,k5,k6,k7) =
                   case ks of k1::k2::k3::k4::k5::k6::k7::_ => (k1,k2,k3,k4,k5,k6,k7)
                            | _ => raise Fail "crkdp_hinterp: invalid list of coefficients" 
           in
               fn (theta) => 
                  (
                    c_rkdp_hinterp (n, theta, yn, tn, h, yout, k1, k2, k3, k4, k5, k6, k7,
                                   t1, t3, t4, t5, t6, t7);
                   yout)
           end
    end


end
