signature MATCH_CTXT
= sig

structure Vertex : NAME_AND_DATA
structure Edge : NAME_AND_DATA

type T (* global context *)
type match (* matching so far *)

val empty : T
val get_ctxt : match -> T

val init_match : T -> match

val vertex_match : Vertex.data -> Vertex.data -> match -> match option
val edge_match : Edge.data -> Edge.data -> match -> match option

val subst_in_vertex : match -> Vertex.data -> Vertex.data
val subst_in_edge : match -> Edge.data -> Edge.data
end;


(* Basic case of graphs with no vars and no global context, and just
using data equality function for matching. *)
functor EqMatchNoCtxtFun(
  structure Vertex : NAME_AND_DATA
  and Edge : NAME_AND_DATA
) : MATCH_CTXT
= struct

structure Vertex = Vertex;
structure Edge = Edge;

type T = unit;
type match = unit;

fun get_ctxt (m : match) = (() : T);

val empty = ();
fun init_match (c : T) = (() : match);

fun vertex_match (n1 : Vertex.data) (n2 : Vertex.data) (m : match) =
    if Vertex.data_eq (n1, n2) then SOME m else NONE;
    
fun edge_match (e1 : Edge.data) (e2 : Edge.data) (m : match) =
    if Edge.data_eq (e1, e2) then SOME m else NONE;

fun subst_in_vertex (m : match) (n : Vertex.data) = n; 
fun subst_in_edge (m : match) (e : Edge.data) = e;

end;

structure StrUnitMatchCtxt = 
EqMatchNoCtxtFun(structure Vertex = StrVertex and Edge = UnitEdge);


(* signature RG_MATCH
= sig

structure RG_NE : RG_NODES_AND_EDGES

include NE_MATCH sharing NE = RG_NE;

val vars_of_ctxt : T -> VName.N.T
end;
*)
