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
                              -> T -> ((Edge.name Edge.NTab.T) * T) Seq.seq
(* given edges between a var-vertex in the pat, match them
   non-strictly with those incident to a matching source in the
   corresponding target vertex *)
val multi_edge_match : Edge.data Edge.NTab.T -> Edge.data Edge.NTab.T 
                       -> T -> ((Edge.NSet.T * (* unmatched tgt *)
                                 (Edge.name Edge.NTab.T)) (* matched pat -> tgt *)
                                * T) (* edge/vertex data instantiation *)
                                 Seq.seq
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

(* m is ignored because there is no edge data *)
fun edge_match (e1 : Edge.data) (e2 : Edge.data) (m : T) = SOME m;

exception too_many_pat_edges_exp of string;

(* tries to match new pattern edge with (first) one in the target set,
raises exception if target set is empty - we handle this and deal with
it when we want to. *)
fun matchup_pat_edges pes tes = 
    let fun match_more (e1,ed) (ematches,tens2) = 
            (case Edge.NSet.pull_local_bot tens2 of
               NONE => raise too_many_pat_edges_exp "matchup_pat_edges"
             | SOME (tn,tens3) => 
               (Edge.NTab.ins (e1, tn) ematches, tens3))
    in
      Edge.NTab.fold match_more pes (Edge.NTab.empty, Edge.NTab.get_names tes)
    end;

(* given a set of edges between two internal(strict) vertices in the pat, 
   match the set of edges between two internal vertices in the tgt *) 
fun strict_multi_edge_match pes tes m = 
    let val (matched_ens,tns) = matchup_pat_edges pes tes
    in if (Edge.NSet.is_empty tns) then Seq.single (matched_ens,m)
       else Seq.empty
    end handle too_many_pat_edges_exp _ => Seq.empty;

(* given edges between a var-vertex in the pat, match them
   non-strictly with those incident to a matching source in the
   corresponding target vertex *)
(* given a set of edges between two internal(strict) vertices in the pat, 
   match the set of edges between two internal vertices in the tgt *) 
fun multi_edge_match pes tes m = 
    let val (matched_ens,leftover_tns) = matchup_pat_edges pes tes
    in Seq.single ((leftover_tns,matched_ens),m) end 
    handle too_many_pat_edges_exp _ => Seq.empty;

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
