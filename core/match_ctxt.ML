signature MATCH_CTXT
= sig

structure BVertex : BVERTEX
structure Edge : EDGE
type T (* global context holds vertex and edge global match info *)
val empty : T
(* internal data matching *)
val vertex_idata_match : BVertex.IData.data -> BVertex.IData.data -> T -> T option
(* match an edge to boundary vertex (in tgt) with another edge (in pat)   *)
val edge_match : Edge.data -> Edge.data -> T -> T option
(* given edges between two vertices in pat and tgt, match pat to tgt *)
val multi_edge_match :
    Edge.data Edge.NTab.T (* pat edges *)
    -> Edge.data Edge.NTab.T (* tgt edges *)
    -> T -> ((Edge.NSet.T * (* unmatched tgt *)
              (Edge.name Edge.NTab.T)) (* matched: pat -> tgt *)
             * T) (* edge/vertex data instantiation *)
              Seq.seq
val subst_in_vertex : T -> BVertex.IData.data -> BVertex.IData.data
val subst_in_edge : T -> Edge.data -> Edge.data
val pretty : T -> Pretty.T
val print : T -> unit
end;


(* Basic case of graphs with no vars and no global context, and just
using data equality function for matching. *)
functor EqMatchUnitEdgeNoCtxtFun(
  structure BVertex : BVERTEX
  and Edge : EDGE where type data = unit
) : MATCH_CTXT where type Edge.data = unit
= struct

structure BVertex = BVertex;
structure Edge = Edge;

type T = unit;

val empty = ();

fun vertex_idata_match (d1 : BVertex.IData.data) (d2 : BVertex.IData.data) 
                       (m : T) =
    if BVertex.IData.data_eq (d1, d2) then SOME m else NONE;

(* m is ignored because there is no edge data *)
fun edge_match (e1 : Edge.data) (e2 : Edge.data) (m : T) = SOME m;

(* tries to match new pattern edge with (first) one in the target set,
raises exception if target set is empty - we handle this give NONE *)
exception too_many_pat_edges_exp of string;
fun matchup_pat_edges pes tes = 
    let fun match_more (e1,ed) (tens2,ematches) = 
            (case Edge.NSet.pull_local_bot tens2 of
               NONE => raise too_many_pat_edges_exp "matchup_pat_edges"
             | SOME (tn,tens3) => 
               (tens3, Edge.NTab.ins (e1, tn) ematches))
    in SOME (Edge.NTab.fold 
               match_more pes (Edge.NTab.get_nameset tes, Edge.NTab.empty))
    end handle too_many_pat_edges_exp _ => NONE;

(* given:
     pes: a set of edges between two vertices in the pat, 
     tes: a set of edges between two vertices in the tgt 
   return: 
     seq of matches using up all edges in pes
*) 
fun multi_edge_match pes tes m = 
    (case matchup_pat_edges pes tes
      of NONE => Seq.empty
       | SOME (leftover_tns, matched_ens) => 
         Seq.single ((leftover_tns,matched_ens), m))

(* no data, thus these are id functions. *)
fun subst_in_vertex (m : T) (n : BVertex.IData.data) = n; 
fun subst_in_edge (m : T) (e : Edge.data) = e;

fun pretty (m : T) = Pretty.str "";
val print = Pretty.writeln o pretty;

end;

