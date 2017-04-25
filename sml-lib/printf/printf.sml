structure Printf =
struct

      fun $ (a, f) = f a
      fun id x = x

      structure Fold =
      struct
      fun fold (a, f) g = g (a, f)
      fun step0 h (a, f) = fold (h a, f)
      fun step1 h (a, f) b = fold (h (b, a), f)
      end

      fun fprintf out =
         Fold.fold ((out, id), fn (_, f) => f (fn p => p ()) ignore)

      val printf = fn z => fprintf TextIO.stdOut z

      fun one ((out, f), make) =
         (out, fn r =>
          f (fn p =>
             make (fn s =>
                   r (fn () => (p (); TextIO.output (out, s))))))

      val ` =
         fn z => Fold.step1 (fn (s, x) => one (x, fn f => f s)) z

      fun spec to = Fold.step0 (fn x => one (x, fn f => f o to))

      val B = fn z => spec Bool.toString z
      val I = fn z => spec Int.toString z
      val R = fn z => spec Real.toString z
      fun ISeq foldr z = spec (fn v => "[" ^ (String.concatWith ", " (foldr (fn (x, ax) => (Int.toString x)::ax)  [] v)) ^ "]") z
      fun RSeq foldr z = spec (fn v => "[" ^ (String.concatWith ", " (foldr (fn (x, ax) => (Real.toString x)::ax) [] v)) ^ "]")  z
                           
      fun test () =
        let
            val IA = ISeq Array.foldr
            val RA = RSeq Array.foldr       
        in
            printf `"Int="I`"  Bool="B`"  Real="R `" Real array = "RA `"\n" $ 1 false 2.0 (Array.fromList [1.0,2.0,3.0])
        end
end
    
                   
