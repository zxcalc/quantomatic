(*  Title:      Pure/item_net.ML
    Author:     Markus Wenzel, TU Muenchen

Efficient storage of items indexed by terms; preserves order and
prefers later entries.
*)

signature ITEM_NET =
sig
  type 'a T
  val init: ('a * 'a -> bool) -> ('a -> term list) -> 'a T
  val content: 'a T -> 'a list
  val length: 'a T -> int
  val retrieve: 'a T -> term -> 'a list
  val retrieve_matching: 'a T -> term -> 'a list
  val member: 'a T -> 'a -> bool
  val merge: 'a T * 'a T -> 'a T
  val remove: 'a -> 'a T -> 'a T
  val update: 'a -> 'a T -> 'a T
end;

structure Item_Net: ITEM_NET =
struct

(* datatype *)

datatype 'a T =
  Items of {
    eq: 'a * 'a -> bool,
    index: 'a -> term list,
    content: 'a list,
    next: int,
    net: (int * 'a) Net.net};

fun mk_items eq index content next net =
  Items {eq = eq, index = index, content = content, next = next, net = net};

fun init eq index = mk_items eq index [] ~1 Net.empty;

fun content (Items {content, ...}) = content;
fun length items = List.length (content items);
fun retrieve (Items {net, ...}) = order_list o Net.unify_term net;
fun retrieve_matching (Items {net, ...}) = order_list o Net.match_term net;


(* standard operations *)

fun member (Items {eq, index, content, net, ...}) x =
  (case index x of
    [] => Library.member eq content x
  | t :: _ => exists (fn (_, y) => eq (x, y)) (Net.unify_term net t));

fun cons x (Items {eq, index, content, next, net}) =
  mk_items eq index (x :: content) (next - 1)
    (fold (fn t => Net.insert_term (K false) (t, (next, x))) (index x) net);

fun merge
    (items1 as Items {net = net1, ...},
     items2 as Items {net = net2, content = content2, ...}) =
  if pointer_eq (net1, net2) then items1
  else if Net.is_empty net1 then items2
  else fold_rev (fn y => if member items1 y then I else cons y) content2 items1;

fun remove x (items as Items {eq, index, content, next, net}) =
  if member items x then
    mk_items eq index (Library.remove eq x content) next
      (fold (fn t => Net.delete_term_safe (eq o apply2 #2) (t, (0, x))) (index x) net)
  else items;

fun update x items = cons x (remove x items);

end;
