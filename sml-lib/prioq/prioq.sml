
structure SignalQueue =
struct

structure SignalPriority = 
struct

type priority     = real
                        
fun compare (x,y) = Real.compare (x,y)
type item         = real * real
fun priority (x : item) = (#1(x))
                              
end


structure PQ = SkewBinomialHeap (SignalPriority)
                                
type priority = SignalPriority.priority
                                   
val empty   = PQ.empty
                  
fun insert (k, v, q)  = PQ.insert ((k,v), q)
                                  
val findMin = PQ.findMin
                  
fun findMinDflt (q, dflt) =
  case PQ.findMin q of
      SOME (k, v) => v
    | NONE => dflt
                  
fun insert (k, v, pq) =
  PQ.insert ((k,v), pq)

end
