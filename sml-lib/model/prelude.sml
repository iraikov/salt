
functor ModelPreludeFn (val n: int) =
struct

open Real
open Math
open Dynamics
         
structure RKArrayState =
struct

type state = RealArrayState.state
fun copy (x,y) = RealArrayState.unop (fn(x) => x) x y
fun state () = RealArrayState.state n
fun scale (a,b,y) = RealArrayState.unop (fn(x) => Real.* (a, x)) b y
fun sum (a,b,y) = RealArrayState.binop Real.+ a b y 
fun mul (a,b,y) = RealArrayState.binop Real.* a b y 
fun apply (f,x,y) = RealArrayState.unop f x y
val show = RealArrayState.show
end

fun make_real_state n = RealArrayState.state n
fun make_bool_state n = BoolArrayState.state n
                                             
fun make_real_initial (n, f: RealArrayState.state -> RealArrayState.state) =
  fn () => f(RealArrayState.state n)
val empty_real_initial: RealArrayState.state = RealArrayState.state 0
fun make_ext (n, f) = let val a = RealArrayState.state n in fn () => f(a) end
fun make_dresponse (n, f) = fn (x,y,e,d) => f(x,y,e,d,RealArrayState.state n)
fun make_transition (n, f) = fn (e,r) => f(e,r,BoolArrayState.state n)
fun make_cond (p, fld, f) = f
fun make_regime_cond (p, fld, f) = f

structure RungeKutta = RungeKuttaFn(structure S = RKArrayState)
open RungeKutta

val getindex = Unsafe.Array.sub
val setindex = Unsafe.Array.update

fun make_stepper_rkdp deriv =
  fn (clos: 'a, h, abstol, reltol, x, y, yout) => (cerkdp (deriv clos)) (abstol,reltol,h) (x,y,yout) 

fun make_stepper_rkoz5 deriv =
  fn (clos: 'a, h, abstol, reltol, x, y, yout) => (cerkoz5 (deriv clos)) (abstol,reltol,h) (x,y,yout) 

fun make_stepper_rkoz4 deriv =
  fn (clos: 'a, h, abstol, reltol, x, y, yout) => (cerkoz4 (deriv clos)) (abstol,reltol,h) (x,y,yout) 

fun make_stepper_rkoz3 deriv =
  fn (clos: 'a, h, abstol, reltol, x, y, yout) => (cerkoz3 (deriv clos)) (abstol,reltol,h) (x,y,yout) 

val prioq_insert      = SignalQueue.insert                                                         
val prioq_empty       = SignalQueue.empty                                                         
val prioq_findMinDflt = SignalQueue.findMinDflt
                   
end
