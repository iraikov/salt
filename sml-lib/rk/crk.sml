
structure CRungeKutta =
struct

fun putStr str =
    (TextIO.output (TextIO.stdOut, str))

fun putStrLn str =
    (TextIO.output (TextIO.stdOut, str);
     TextIO.output (TextIO.stdOut, "\n"))

val c_rkdp = _import "Dormand_Prince_5_4" public: 
             int * MLton.Pointer.t * real array * real array * real * real * real array * real array * real array * 
             real array * real array * real array * real array * real array * real array *
             real array * real array * real array * real array * real array * real array *
             real array * real array * real array * real array * real array * real array
             -> int;

fun make_crkdp (n, fp) =
    let
        val t1 = Array.array (n, 0.0) 
        val t2 = Array.array (n, 0.0) 
        val t3 = Array.array (n, 0.0) 
        val t4 = Array.array (n, 0.0) 
        val t5 = Array.array (n, 0.0) 
        val t6 = Array.array (n, 0.0) 
        val t7 = Array.array (n, 0.0) 
        val k1 = Array.array (n, 0.0) 
        val k2 = Array.array (n, 0.0) 
        val k3 = Array.array (n, 0.0) 
        val k4 = Array.array (n, 0.0) 
        val k5 = Array.array (n, 0.0) 
        val k6 = Array.array (n, 0.0) 
        val k7 = Array.array (n, 0.0) 
        val k8 = Array.array (n, 0.0) 
        val k9 = Array.array (n, 0.0) 
        val k10 = Array.array (n, 0.0) 
        val k11 = Array.array (n, 0.0) 
        val k12 = Array.array (n, 0.0) 
    in
        fn (p) => 
           (fn (h) => 
               (fn (tn,yn,yout,err) => 
                   (c_rkdp (n, fp, p, yn, tn, h, yout, err, t1, t2, t3, t4, t5, t6, t7,
                            k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12);
                    (yout,err))
                ))
    end

end
