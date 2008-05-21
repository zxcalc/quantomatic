signature MATCH_CTXT
= sig

structure Vertex : VERTEX
structure Edge : EDGE
type T (* global context holds vertex and edge global match info *)
val empty : T
val vertex_match : Vertex.data -> Vertex.data -> T -> T option
(* match an edge to boundary vertex (in tgt) with another edge (in pat)   *)
val edge_match : Edge.data -> Edge.data -> T -> T option
(* given a set of edges between two internal(strict) vertices in the pat, 
   match the set of edges between two internal vertices in the tgt *) 
val strict_multi_edge_match : Edge.data Edge.NTab.T -> Edge.data Edge.NTab.T 
                              -> T -> T Seq.seq
val subst_in_vertex : T -> Vertex.data -> Vertex.data
val subst_in_edge : T -> Edge.data -> Edge.data
val pretty : T -> Pretty.T
val print : T -> unit
end;


(* Basic case of graphs with no vars and no global context, and just
using data equality function for matching. *)
functor EqMatchUnitEdgeNoCtxtFun(
  structure Vertex : VERTEX
  and Edge : EDGE where type data = unit
) : MATCH_CTXT where type Edge.data = unit
= struct

structure Vertex = Vertex;
structure Edge = Edge;

type T = unit;

val empty = ();

fun vertex_match (n1 : Vertex.data) (n2 : Vertex.data) (m : T) =
    if Vertex.data_eq (n1, n2) then SOME m else NONE;

fun edge_match (e1 : Edge.data) (e2 : Edge.data) (m : T) = SOME m;

(* for multiple edge matching, when edges have no data, count the
   number of edges. *)
fun strict_multi_edge_match etab1 etab2 m = 
    if (Edge.NTab.fold (fn _ => fn i => i + 1) etab1 0) = 
       (Edge.NTab.fold (fn _ => fn i => i + 1) etab2 0) 
    then Seq.single m else Seq.empty;

(* no data, thus these are id functions. *)
fun subst_in_vertex (m : T) (n : Vertex.data) = n; 
fun subst_in_edge (m : T) (e : Edge.data) = e;

fun pretty (m : T) = Pretty.str "";
val print = Pretty.writeln o pretty;

end;

structure StrUnitMatchCtxt = 
EqMatchUnitEdgeNoCtxtFun(structure Vertex = StrVertex and Edge = UnitEdge);


(* signature RG_MATCH
= sig

structure RG_NE : RG_NODES_AND_EDGES

include NE_MATCH sharing NE = RG_NE;

val vars_of_ctxt : T -> VName.N.T
end;
*)
