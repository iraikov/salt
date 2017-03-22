structure LinearInterpolation =
struct

fun putStr str =
    (TextIO.output (TextIO.stdOut, str))

fun putStrLn str =
    (TextIO.output (TextIO.stdOut, str);
     TextIO.output (TextIO.stdOut, "\n"))

type table = (real * real) vector

fun interpolate1 a x =
    let
        val sub = Unsafe.Vector.sub
        val m = Vector.length a
        val l = sub(a,m-1)
    in
        if Real.>=(x, #2(l))
        then
            #1(l)
        else
            (let
                fun dist i = Real.-(x, (#2(sub(a,i))))
                fun nearer (i, j) = 
                  let val di = dist i
                      val dj = dist j
                  in
                      if Real.sign(di)=Real.sign(dj) andalso Real.<(Real.abs dj,Real.abs di)
                      then j else i
                  end
                fun bounds (i, j) = 
                  if i < j
                  then
                      (let val i' = nearer (i, i+1)
                           val j' = nearer (j, j-1)
                       in
                           if i=i' andalso j=j' then (i,j) else bounds (i',j')
                       end)
                  else (i,j)
                val ((y1,x1),(y2,x2)) = 
                    (let val (i,j) = bounds (0, m-1)
                     in
                 (sub(a,i),sub(a,j))
                     end)
                        
                val A =  Real./ (Real.- (y2, y1), Real.- (x2, x1))
            in
                Real.+ (y1, Real.* (A, Real.-(x, x1)))
            end)
    end
                 

 
fun table f a b n: table =
   let
       open Real
       val m = fromInt (Int.-(n,1))
       fun g i =
           let
               val x = a + (b - a) * (fromInt i) / m 
           in
               (f x, x) 
           end
   in
       Vector.tabulate (n, g)
   end

fun pulse up lo w v a b: table =
    let 
        val n = Real.round (Real./ (Real.-(b,a), w))
    in
        table (fn (x) => if Real.<=(Real.abs (Real.-(x,v)), w)
                         then up else lo) a b n
    end
end


(*
val t = LinearInterpolation.table Real.Math.sin 0.0 180.0 20
val fi = LinearInterpolation.interpolate1 t
*)

