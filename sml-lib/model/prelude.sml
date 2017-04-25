
functor ModelPreludeFn (val n: int) =
struct

open Real
open Math
open Dynamics

structure RKArrayState =
struct

fun state () = RealArrayState.state n
fun sum (a,b,y) = RealArrayState.binop Real.+ a b y 
fun scale (a,b,y) = RealArrayState.unop (fn(x) => Real.* (a, x)) b y
fun copy (x,y) = RealArrayState.unop (fn(x) => x) x y

end
         
structure RungeKutta = RungeKuttaFn(structure S = RKArrayState)
open RungeKutta


fun make_real_initial (n, f: RealArrayState.state -> RealArrayState.state) =
  fn () => f(RealArrayState.state n)
val empty_real_initial: RealArrayState.state = RealArrayState.state 0
fun make_ext (n, f) = let val a = RealArrayState.state n in fn () => f(a) end
fun make_dresponse (n, f) = fn (x,y,e,d) => f(x,y,e,d,RealArrayState.state n)
fun make_transition (n, f) = fn (e,r) => f(e,r,BoolArrayState.state n)
fun make_regime_cond (p, fld, f) = f
fun make_cond (p, fld, f) = f
                                
fun make_regime_stepper deriv solver =
  fn (p, fld, d, r, ext, extev, h, x, y, yout, err) => (regime_solver (deriv (p,fld,d,r,ext,extev))) h (x,y,yout,err) 
      
fun make_stepper deriv solver = 
  fn (p, fld, ext, extev, h, x, y, yout, err) => (solver (deriv (p,fld,ext,extev))) h (x,y,yout,err)

fun make_regime_cond (p, fld, f) = f
fun make_cond (p, fld, f) = f

end
