
structure ModelRandom =
struct

fun RandomState () = RandomMTZig.fromEntropy()
fun RandomStateFromInt i = RandomMTZig.fromInt(i)

val RandomZT = RandomMTZig.ztnew

fun mk_random_uniform RandomState () = RandomMTZig.randUniform RandomState
fun mk_random_unifrange RandomState (a, b) = let val (a',b') = if a<b then (a,b) else (b,a)
                                             in a' + (RandomMTZig.randUniform RandomState) * (b'-a') end
fun mk_random_normal (RandomState, RandomZT) () = RandomMTZig.randNormal (RandomState, RandomZT)
fun mk_random_exponential (RandomState, RandomZT) (mu) = mu * RandomMTZig.randExp (RandomState, RandomZT)
fun mk_random_poisson (RandomState, RandomZT) (lam) = RandomMTZig.randPoisson (lam, RandomState, RandomZT)

end
