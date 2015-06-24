structure LinearInterpolation =
struct

fun putStr str =
    (TextIO.output (TextIO.stdOut, str))

fun putStrLn str =
    (TextIO.output (TextIO.stdOut, str);
     TextIO.output (TextIO.stdOut, "\n"))


fun interpolate1 a x =
   let
       val m = Vector.length a 
       fun dist i = Real.-(x, (#2(Vector.sub(a,i))))
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
                (Vector.sub(a,i),Vector.sub(a,j))
            end)

       val A =  Real./ (Real.- (y2, y1), Real.- (x2, x1))
       val B =  Real.~ (Real./ (Real.- (Real.* (x1, y2), (Real.* (y1, x2))), Real.- (x2, x1)))
   in
       Real.+ (Real.* (A, x), B)
   end

 
fun table f a b n =
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


end


