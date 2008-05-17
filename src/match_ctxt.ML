signature MATCH_CTXT
= sig

structure Vertex : NAME_AND_DATA
structure Edge : NAME_AND_DATA

type ctxt (* global context *)
type T (* matching so far *)

val get_ctxt : T -> ctxt

val init : ctxt -> T

val vertex_match : Vertex.data -> Vertex.data -> T -> T option
val edge_match : Edge.data -> Edge.data -> T -> T option

val subst_in_vertex : T -> Vertex.data -> Vertex.data
val subst_in_edge : T -> Edge.data -> Edge.data
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

type ctxt = unit;
type T = unit;

fun get_ctxt (m : T) = (() : ctxt);

fun init (c : ctxt) = (() : T);

fun vertex_match (n1 : Vertex.data) (n2 : Vertex.data) (m : T) =
    if Vertex.data_eq (n1, n2) then SOME m else NONE;
    
fun edge_match (e1 : Edge.data) (e2 : Edge.data) (m : T) =
    if Edge.data_eq (e1, e2) then SOME m else NONE;

fun subst_in_vertex (m : T) (n : Vertex.data) = n; 
fun subst_in_edge (m : T) (e : Edge.data) = e;

end;

structure StrUnitMatchCtxt = 
EqMatchNoCtxtFun(structure Vertex = StrVertex and Edge = UnitEdge);


(* signature RG_MATCH
= sig

structure RG_NE : RG_NODES_AND_EDGES

include NE_MATCH sharing NE = RG_NE;

val vars_of_ctxt : ctxt -> VName.N.T
end;
*)
