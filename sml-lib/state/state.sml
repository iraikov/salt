(*
 * Definitions for numerical state and state vectors.
*)


signature ELEM =
sig

type elt

val show : elt -> string
val bot : elt

end


signature STATE =
sig

structure Elem: ELEM

type state

val show : state -> string
val state : int -> state
val length : state -> int

val getindex : state * int -> Elem.elt
val setindex : state * int * Elem.elt -> unit

val find :  (Elem.elt -> bool) -> state -> Elem.elt option
val foldl :  (Elem.elt * 'a  -> 'a) -> 'a -> state -> 'a

val unop :  (Elem.elt -> Elem.elt) -> state -> state -> state
val binop :  (Elem.elt * Elem.elt -> Elem.elt) -> state -> state -> state -> state

                                                          
end



functor ArrayState (structure E : ELEM): STATE =
struct

structure Elem = E

type state = Elem.elt Array.array

fun state n  = Array.array (n, Elem.bot)
                           
fun show s   = String.concatWith ", " (Array.foldr (fn (x, ax) => (Elem.show x)::ax) [] s)
val length   = Array.length
val getindex = Unsafe.Array.sub
val setindex = Unsafe.Array.update
val find     = Array.find
val foldl    = Array.foldl

fun unop f input output =
  (Array.modifyi (fn (i,v) => f (getindex (input,i))) output; output)

fun binop f input1 input2 output =
  (Array.modifyi (fn (i,v) => f (getindex (input1,i), getindex (input2,i))) output; output)

end

structure BoolElem : ELEM =
struct

type elt = bool
val bot = false
val show = Bool.toString

end

structure RealElem : ELEM =
struct

type elt = real
val bot = 0.0
val show = Real.toString

end

structure BoolArrayState = ArrayState (structure E = BoolElem)
structure RealArrayState = ArrayState (structure E = RealElem)
