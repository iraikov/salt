(* Source code from
 *   Purely Functional Data Structures
 *   Chris Okasaki
 *   Cambridge University Press, 1998
 *
 * Copyright (c) 1998 Cambridge University Press
 *)

signature HEAP =
sig
  structure P : PRIORITY

  type heap

  val empty     : heap
  val isEmpty   : heap -> bool

  val insert    : P.item * heap -> heap
  val merge     : heap * heap -> heap

  val findMin   : heap -> P.item option  (* returns NONE if heap is empty *)
  val deleteMin : heap -> heap option    (* returns NONE if heap is empty *)

  val app       : (P.item -> unit) -> heap -> unit
  val foldl     : (P.item * 'a -> 'a) -> 'a -> heap -> 'a
end

functor SkewBinomialHeap (P : PRIORITY) : HEAP =
struct
  structure P = P

  datatype Tree = Node of int * P.item * P.item list * Tree list
  type heap = Tree list

  val empty = []
  fun isEmpty ts = null ts

  fun rank (Node (r, x, xs, c)) = r

  fun root (Node (r, x, xs, c)) = x

  fun link (t1 as Node (r, x1, xs1, c1), t2 as Node (_, x2, xs2, c2)) =
      case P.compare (P.priority x1, P.priority x2) of
          GREATER => Node (r+1, x2, xs2, t1 :: c2)
        | _ => Node (r+1, x1, xs1, t2 :: c1)

  fun skewLink (x, t1, t2) =
        let 
            val Node (r, y, ys, c) = link (t1, t2)
	in
            case P.compare (P.priority x, P.priority y) of
                GREATER => Node (r, y, x :: ys, c)
              | _ => Node (r, x, y :: ys, c)
        end

  fun insTree (t, []) = [t]
    | insTree (t1, t2 :: ts) =
        if rank t1 < rank t2 then t1 :: t2 :: ts 
        else insTree (link (t1, t2), ts)

  fun mergeTrees (ts1, []) = ts1
    | mergeTrees ([], ts2) = ts2
    | mergeTrees (ts1 as t1 :: ts1', ts2 as t2 :: ts2') =
        if rank t1 < rank t2 then t1 :: mergeTrees (ts1', ts2)
        else if rank t2 < rank t1 then t2 :: mergeTrees (ts1, ts2')
        else insTree (link (t1, t2), mergeTrees (ts1', ts2'))

  fun normalize [] = []
    | normalize (t :: ts) = insTree (t, ts)

  fun insert (x, ts as t1 :: t2 :: rest) =
        if rank t1 = rank t2 then skewLink (x, t1, t2) :: rest
        else Node (0, x, [], []) :: ts
    | insert (x, ts) = Node (0, x, [], []) :: ts

  fun merge (ts1, ts2) = mergeTrees (normalize ts1, normalize ts2)

  fun removeMinTree [] = NONE
    | removeMinTree (t :: ts) =
      case removeMinTree ts of
          SOME (t', ts') =>
          (case P.compare (P.priority (root t), P.priority (root t'))  of
               GREATER => SOME (t', t :: ts') 
             | _ => SOME (t, ts))
        | NONE => SOME (t, [])


  fun findMin ts = 
      case removeMinTree ts of
          NONE => NONE
        | SOME (t, _) => SOME (root t)


  fun deleteMin ts =
      case removeMinTree ts of
          SOME (Node (_, x, xs, ts1), ts2) =>
          let
	      fun insertAll ([], ts) = ts
	        | insertAll (x :: xs, ts) = insertAll (xs, insert (x, ts))
          in
	      SOME (insertAll (xs, merge (rev ts1, ts2)))
          end
          | NONE => NONE

  fun appNode f (Node (r, x, xs, rst)) =
      (List.app f (x::xs); app f rst)

  and app f ([]) = () | app f (x::rst) = (appNode f x; app f rst)

  fun foldlNode f init (Node (r, x, xs, rst)) =
      let
          val init' = List.foldl f init (x::xs)
      in
          foldl f init' rst
      end

  and foldl f init ([]) = init
    | foldl f init (x :: rst) =
      foldl f (foldlNode f init x) rst

end
