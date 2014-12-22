(*  Title:      Pure/General/heap.ML
    Author:     Lawrence C Paulson and Markus Wenzel

Heaps over linearly ordered types.  See also Chris Okasaki: "Purely
Functional Data Structures" (Chapter 3), Cambridge University Press,
1998.
*)

signature HEAP =
sig
  type elem
  type T
  val empty: T
  val is_empty: T -> bool
  val merge: T * T -> T
  val insert: elem -> T -> T
  val min: T -> elem            (*exception Empty*)
  val delete_min: T -> T        (*exception Empty*)
  val min_elem: T -> elem * T   (*exception Empty*)
  val upto: elem -> T -> elem list * T
end;

functor Heap(type elem val ord: elem * elem -> order): HEAP =
struct


(* datatype heap *)

type elem = elem;
datatype T = Empty | Heap of int * elem * T * T;


(* empty heaps *)

val empty = Empty;

fun is_empty Empty = true
  | is_empty (Heap _) = false;


(* build heaps *)

local

fun rank Empty = 0
  | rank (Heap (r, _, _, _)) = r;

fun heap x a b =
  if rank a >= rank b then Heap (rank b + 1, x, a, b)
  else Heap (rank a + 1, x, b, a);

in

fun merge (h, Empty) = h
  | merge (Empty, h) = h
  | merge (h1 as Heap (_, x1, a1, b1), h2 as Heap (_, x2, a2, b2)) =
      (case ord (x1, x2) of
        GREATER => heap x2 a2 (merge (h1, b2))
      | _ => heap x1 a1 (merge (b1, h2)));

fun insert x h = merge (Heap (1, x, Empty, Empty), h);

end;


(* minimum element *)

fun min Empty = raise List.Empty
  | min (Heap (_, x, _, _)) = x;

fun delete_min Empty = raise List.Empty
  | delete_min (Heap (_, _, a, b)) = merge (a, b);

fun min_elem h = (min h, delete_min h);


(* initial interval *)

nonfix upto;

fun upto _ Empty = ([], Empty)
  | upto limit (h as Heap (_, x, _, _)) =
      (case ord (x, limit) of
        GREATER => ([], h)
      | _ => upto limit (delete_min h) |>> cons x);

end;
