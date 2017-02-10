(*  Title:      Pure/General/ord_list.ML
    Author:     Makarius

Ordered lists without duplicates -- a light-weight representation of
finite sets, all operations take linear time and economize heap usage.
*)

signature ORD_LIST =
sig
  type 'a T = 'a list
  val make: ('a * 'a -> order) -> 'a list -> 'a T
  val member: ('b * 'a -> order) -> 'a T -> 'b -> bool
  val insert: ('a * 'a -> order) -> 'a -> 'a T -> 'a T
  val remove: ('b * 'a -> order) -> 'b -> 'a T -> 'a T
  val subset: ('b * 'a -> order) -> 'b T * 'a T -> bool
  val union: ('a * 'a -> order) -> 'a T -> 'a T -> 'a T
  val unions: ('a * 'a -> order) -> 'a T list -> 'a T
  val merge: ('a * 'a -> order) -> 'a T * 'a T -> 'a T
  val inter: ('b * 'a -> order) -> 'b T -> 'a T -> 'a T
  val subtract: ('b * 'a -> order) -> 'b T -> 'a T -> 'a T
end;

structure Ord_List: ORD_LIST =
struct

type 'a T = 'a list;
fun make ord = sort_distinct ord;


(* single elements *)

fun find_index ord list x =
  let
    fun find i [] = ~ i
      | find i (y :: ys) =
          (case ord (x, y) of
            LESS => ~ i
          | EQUAL => i
          | GREATER => find (i + 1) ys);
  in find 1 list end;

fun member ord list x = find_index ord list x > 0;

fun insert ord x list =
  let
    fun insrt 1 ys = x :: ys
      | insrt i (y :: ys) = y :: insrt (i - 1) ys;
    val idx = find_index ord list x;
  in if idx > 0 then list else insrt (~ idx) list end;

fun remove ord x list =
  let
    fun rmove 1 (_ :: ys) = ys
      | rmove i (y :: ys) = y :: rmove (i - 1) ys;
    val idx = find_index ord list x;
  in if idx > 0 then rmove idx list else list end;


(* lists as sets *)

fun subset ord (list1, list2) =
  let
    fun sub [] _ = true
      | sub _ [] = false
      | sub (lst1 as x :: xs) (y :: ys) =
          (case ord (x, y) of
            LESS => false
          | EQUAL => sub xs ys
          | GREATER => sub lst1 ys);
  in sub list1 list2 end;


(* algebraic operations *)

exception SAME;
fun handle_same f x = f x handle SAME => x;

(*union: insert elements of first list into second list*)
fun union ord list1 list2 =
  let
    fun unio [] _ = raise SAME
      | unio xs [] = xs
      | unio (lst1 as x :: xs) (lst2 as y :: ys) =
          (case ord (x, y) of
            LESS => x :: handle_same (unio xs) lst2
          | EQUAL => y :: unio xs ys
          | GREATER => y :: unio lst1 ys);
  in if pointer_eq (list1, list2) then list1 else handle_same (unio list1) list2 end;

fun unions ord lists =
  let
    fun unios (xs :: ys :: rest) acc = unios rest (union ord xs ys :: acc)
      | unios [xs] (ys :: acc) = unios (union ord xs ys :: acc) []
      | unios [xs] [] = xs
      | unios [] [] = []
      | unios [] acc = unios acc [];
  in unios lists [] end;

fun merge ord (list1, list2) = union ord list2 list1;

(*intersection: filter second list for elements present in first list*)
fun inter ord list1 list2 =
  let
    fun intr _ [] = raise SAME
      | intr [] _ = []
      | intr (lst1 as x :: xs) (lst2 as y :: ys) =
          (case ord (x, y) of
            LESS => intr xs lst2
          | EQUAL => y :: intr xs ys
          | GREATER => handle_same (intr lst1) ys);
  in handle_same (intr list1) list2 end;

(*subtraction: filter second list for elements NOT present in first list*)
fun subtract ord list1 list2 =
  let
    fun subtr [] _ = raise SAME
      | subtr _ [] = raise SAME
      | subtr (lst1 as x :: xs) (lst2 as y :: ys) =
          (case ord (x, y) of
            LESS => subtr xs lst2
          | EQUAL => handle_same (subtr xs) ys
          | GREATER => y :: subtr lst1 ys);
  in handle_same (subtr list1) list2 end;

end;
