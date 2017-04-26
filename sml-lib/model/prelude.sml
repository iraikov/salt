
functor ModelPreludeFn (val n: int) =
struct

open Real
open Math
open Dynamics
         
structure RKArrayState =
struct

type state = RealArrayState.state
fun state () = RealArrayState.state n
fun sum (a,b,y) = RealArrayState.binop Real.+ a b y 
fun scale (a,b,y) = RealArrayState.unop (fn(x) => Real.* (a, x)) b y
fun copy (x,y) = RealArrayState.unop (fn(x) => x) x y

end

fun make_real_state n = RealArrayState.state n
fun make_bool_state n = BoolArrayState.state n
                                             
fun make_real_initial (n, f: RealArrayState.state -> RealArrayState.state) =
  fn () => f(RealArrayState.state n)
val empty_real_initial: RealArrayState.state = RealArrayState.state 0
fun make_ext (n, f) = let val a = RealArrayState.state n in fn () => f(a) end
fun make_dresponse (n, f) = fn (x,y,e,d) => f(x,y,e,d,RealArrayState.state n)
fun make_transition (n, f) = fn (e,r) => f(e,r,BoolArrayState.state n)
fun make_regime_cond (p, fld, f) = f
fun make_cond (p, fld, f) = f

structure RungeKutta = RungeKuttaFn(structure S = RKArrayState)
open RungeKutta
                                
fun make_regime_stepper deriv =
  fn (p, fld, d, r, ext, extev, h, x, y, yout, err) => (cerkdp (deriv (p,fld,d,r,ext,extev))) h (x,y,yout,err) 
      
fun make_stepper deriv = 
  fn (p, fld, ext, extev, h, x, y, yout, err) => (cerkdp (deriv (p,fld,ext,extev))) h (x,y,yout,err)

val interpfun = interp_cerkdp 

val getindex = Unsafe.Array.sub
val setindex = Unsafe.Array.update
                   
end
