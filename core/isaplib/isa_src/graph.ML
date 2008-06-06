(*  Title:      Pure/General/graph.ML
    ID:         $Id: graph.ML,v 1.46 2007/12/05 13:16:09 haftmann Exp $
    Author:     Markus Wenzel and Stefan Berghofer, TU Muenchen

Directed graphs.
*)

signature GRAPH =
sig
  type key
  type 'a T
  exception DUP of key
  exception SAME
  exception UNDEF of key
  val empty: 'a T
  val keys: 'a T -> key list
  val dest: 'a T -> (key * key list) list
  val fold: (key * ('a * (key list * key list)) -> 'b -> 'b) -> 'a T -> 'b -> 'b
  val minimals: 'a T -> key list
  val maximals: 'a T -> key list
  val subgraph: (key -> bool) -> 'a T -> 'a T
  val map_nodes: ('a -> 'b) -> 'a T -> 'b T
  val fold_map_nodes: (key * 'a -> 'b -> 'c * 'b) -> 'a T -> 'b -> 'c T * 'b
  val get_node: 'a T -> key -> 'a                                     (*exception UNDEF*)
  val map_node: key -> ('a -> 'a) -> 'a T -> 'a T
  val map_node_yield: key -> ('a -> 'b * 'a) -> 'a T -> 'b * 'a T
  val imm_preds: 'a T -> key -> key list
  val imm_succs: 'a T -> key -> key list
  val all_preds: 'a T -> key list -> key list
  val all_succs: 'a T -> key list -> key list
  val strong_conn: 'a T -> key list list
  val new_node: key * 'a -> 'a T -> 'a T                              (*exception DUP*)
  val default_node: key * 'a -> 'a T -> 'a T
  val del_nodes: key list -> 'a T -> 'a T                             (*exception UNDEF*)
  val is_edge: 'a T -> key * key -> bool
  val add_edge: key * key -> 'a T -> 'a T
  val del_edge: key * key -> 'a T -> 'a T
  val merge: ('a * 'a -> bool) -> 'a T * 'a T -> 'a T                 (*exception DUP*)
  val join: (key -> 'a * 'a -> 'a) (*exception DUP/SAME*) ->
    'a T * 'a T -> 'a T                                               (*exception DUP*)
  val irreducible_paths: 'a T -> key * key -> key list list
  val all_paths: 'a T -> key * key -> key list list
  exception CYCLES of key list list
  val add_edge_acyclic: key * key -> 'a T -> 'a T                     (*exception CYCLES*)
  val add_deps_acyclic: key * key list -> 'a T -> 'a T                (*exception CYCLES*)
  val merge_acyclic: ('a * 'a -> bool) -> 'a T * 'a T -> 'a T         (*exception CYCLES*)
  val topological_order: 'a T -> key list
  val add_edge_trans_acyclic: key * key -> 'a T -> 'a T               (*exception CYCLES*)
  val merge_trans_acyclic: ('a * 'a -> bool) -> 'a T * 'a T -> 'a T   (*exception CYCLES*)
end;

functor GraphFun(Key: KEY): GRAPH =
struct

(* keys *)

type key = Key.key;

val eq_key = is_equal o Key.ord;

val member_key = member eq_key;
val remove_key = remove eq_key;


(* tables and sets of keys *)

structure Table = TableFun(Key);
type keys = unit Table.table;

val empty_keys = Table.empty: keys;

fun member_keys tab = Table.defined (tab: keys);
fun insert_keys x tab = Table.insert (K true) (x, ()) (tab: keys);


(* graphs *)

datatype 'a T = Graph of ('a * (key list * key list)) Table.table;

exception DUP = Table.DUP;
exception UNDEF = Table.UNDEF;
exception SAME = Table.SAME;

val empty = Graph Table.empty;
fun keys (Graph tab) = Table.keys tab;
fun dest (Graph tab) = map (fn (x, (_, (_, succs))) => (x, succs)) (Table.dest tab);

fun fold_graph f (Graph tab) = Table.fold f tab;

fun minimals G = fold_graph (fn (m, (_, ([], _))) => cons m | _ => I) G [];
fun maximals G = fold_graph (fn (m, (_, (_, []))) => cons m | _ => I) G [];

fun subgraph P G =
  let
    fun subg (k, (i, (preds, succs))) =
      if P k then Table.update (k, (i, (filter P preds, filter P succs))) else I;
  in Graph (fold_graph subg G Table.empty) end;

fun get_entry (Graph tab) x =
  (case Table.lookup tab x of
    SOME entry => entry
  | NONE => raise UNDEF x);

fun map_entry x f (G as Graph tab) = Graph (Table.update (x, f (get_entry G x)) tab);

fun map_entry_yield x f (G as Graph tab) =
  let val (a, node') = f (get_entry G x)
  in (a, Graph (Table.update (x, node') tab)) end;


(* nodes *)

fun map_nodes f (Graph tab) = Graph (Table.map (fn (i, ps) => (f i, ps)) tab);

fun fold_map_nodes f (Graph tab) =
  apfst Graph o Table.fold_map (fn (k, (i, ps)) => f (k, i) #> apfst (rpair ps)) tab;

fun get_node G = #1 o get_entry G;

fun map_node x f = map_entry x (fn (i, ps) => (f i, ps));

fun map_node_yield x f = map_entry_yield x (fn (i, ps) =>
  let val (a, i') = f i in (a, (i', ps)) end);


(* reachability *)

(*nodes reachable from xs -- topologically sorted for acyclic graphs*)
fun reachable next xs =
  let
    fun reach x (rs, R) =
      if member_keys R x then (rs, R)
      else apfst (cons x) (fold reach (next x) (rs, insert_keys x R))
  in fold_map (fn x => fn X => reach x ([], X)) xs empty_keys end;

(*immediate*)
fun imm_preds G = #1 o #2 o get_entry G;
fun imm_succs G = #2 o #2 o get_entry G;

(*transitive*)
fun all_preds G = flat o fst o reachable (imm_preds G);
fun all_succs G = flat o fst o reachable (imm_succs G);

(*strongly connected components; see: David King and John Launchbury,
  "Structuring Depth First Search Algorithms in Haskell"*)
fun strong_conn G = filter_out null (fst (reachable (imm_preds G)
  (flat (rev (fst (reachable (imm_succs G) (keys G)))))));


(* nodes *)

fun new_node (x, info) (Graph tab) =
  Graph (Table.update_new (x, (info, ([], []))) tab);

fun default_node (x, info) (Graph tab) =
  Graph (Table.default (x, (info, ([], []))) tab);

fun del_nodes xs (Graph tab) =
  Graph (tab
    |> fold Table.delete xs
    |> Table.map (fn (i, (preds, succs)) =>
      (i, (fold remove_key xs preds, fold remove_key xs succs))));


(* edges *)

fun is_edge G (x, y) = member_key (imm_succs G x) y handle UNDEF _ => false;

fun add_edge (x, y) G =
  if is_edge G (x, y) then G
  else
    G |> map_entry y (fn (i, (preds, succs)) => (i, (x :: preds, succs)))
      |> map_entry x (fn (i, (preds, succs)) => (i, (preds, y :: succs)));

fun del_edge (x, y) G =
  if is_edge G (x, y) then
    G |> map_entry y (fn (i, (preds, succs)) => (i, (remove_key x preds, succs)))
      |> map_entry x (fn (i, (preds, succs)) => (i, (preds, remove_key y succs)))
  else G;

fun diff_edges G1 G2 =
  flat (dest G1 |> map (fn (x, ys) => ys |> map_filter (fn y =>
    if is_edge G2 (x, y) then NONE else SOME (x, y))));

fun edges G = diff_edges G empty;


(* join and merge *)

fun no_edges (i, _) = (i, ([], []));

fun join f (Graph tab1, G2 as Graph tab2) =
  let fun join_node key ((i1, edges1), (i2, _)) = (f key (i1, i2), edges1)
  in fold add_edge (edges G2) (Graph (Table.join join_node (tab1, Table.map no_edges tab2))) end;

fun gen_merge add eq (Graph tab1, G2 as Graph tab2) =
  let fun eq_node ((i1, _), (i2, _)) = eq (i1, i2)
  in fold add (edges G2) (Graph (Table.merge eq_node (tab1, Table.map no_edges tab2))) end;

fun merge eq GG = gen_merge add_edge eq GG;


(* irreducible paths -- Hasse diagram *)

fun irreducible_preds G X path z =
  let
    fun red x x' = is_edge G (x, x') andalso not (eq_key (x', z));
    fun irreds [] xs' = xs'
      | irreds (x :: xs) xs' =
          if not (member_keys X x) orelse eq_key (x, z) orelse member_key path x orelse
            exists (red x) xs orelse exists (red x) xs'
          then irreds xs xs'
          else irreds xs (x :: xs');
  in irreds (imm_preds G z) [] end;

fun irreducible_paths G (x, y) =
  let
    val (_, X) = reachable (imm_succs G) [x];
    fun paths path z =
      if eq_key (x, z) then cons (z :: path)
      else fold (paths (z :: path)) (irreducible_preds G X path z);
  in if eq_key (x, y) andalso not (is_edge G (x, x)) then [[]] else paths [] y [] end;


(* all paths *)

fun all_paths G (x, y) =
  let
    val (_, X) = reachable (imm_succs G) [x];
    fun paths path z =
      if not (null path) andalso eq_key (x, z) then [z :: path]
      else if member_keys X z andalso not (member_key path z)
      then maps (paths (z :: path)) (imm_preds G z)
      else [];
  in paths [] y end;


(* maintain acyclic graphs *)

exception CYCLES of key list list;

fun add_edge_acyclic (x, y) G =
  if is_edge G (x, y) then G
  else
    (case irreducible_paths G (y, x) of
      [] => add_edge (x, y) G
    | cycles => raise CYCLES (map (cons x) cycles));

fun add_deps_acyclic (y, xs) = fold (fn x => add_edge_acyclic (x, y)) xs;

fun merge_acyclic eq GG = gen_merge add_edge_acyclic eq GG;

fun topological_order G = minimals G |> all_succs G;


(* maintain transitive acyclic graphs *)

fun add_edge_trans_acyclic (x, y) G =
  add_edge_acyclic (x, y) G
  |> fold_product (curry add_edge) (all_preds G [x]) (all_succs G [y]);

fun merge_trans_acyclic eq (G1, G2) =
  merge_acyclic eq (G1, G2)
  |> fold add_edge_trans_acyclic (diff_edges G1 G2)
  |> fold add_edge_trans_acyclic (diff_edges G2 G1);


(*final declarations of this structure!*)
val fold = fold_graph;

end;

structure Graph = GraphFun(type key = string val ord = fast_string_ord);
structure IntGraph = GraphFun(type key = int val ord = int_ord);
