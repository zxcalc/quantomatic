(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Match context: generic notion of matching for data that can occur in 
    vertices and edges. Allows global state changes to happen as part of 
    matching. i.e. variables in pattern get instantiated by matching 
 
    TODO: extend to unificiation
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature MATCH_CTXT_SHARING
= sig
  type T
  structure G : OGRAPH_SHARING
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature MATCH_CTXT
= sig

structure G : OGRAPH
type T (* global context holds vertex and edge match information *)
structure Sharing : MATCH_CTXT_SHARING
sharing type Sharing.T = T;
sharing Sharing.G = G.SharingOGraph;

val empty : T
(* internal data matching *)
val vertex_idata_match : G.OVertex.IData.data -> G.OVertex.IData.data -> T -> T option
(* match an edge to boundary vertex (in tgt) with another edge (in pat)   *)
val edge_match : G.Edge.data -> G.Edge.data -> T -> T option

(* Assumed Properties: 
     (edge_match d1 d2 mctxt = SOME mctxt2) ==> 
       (edge_match d1 d2 mctxt2 = SOME mctxt2)
*)

(* apply a matching information to other data *)
val subst_in_vertex_data : T -> G.OVertex.data -> G.OVertex.data
val subst_in_vertex_idata : T -> G.OVertex.IData.data -> G.OVertex.IData.data
val subst_in_edge_data : T -> G.Edge.data -> G.Edge.data

(* *)
val pretty : T -> Pretty.T
val print : T -> unit

end;



(* Simple instance of graph context data: no vars, no global context, unit data
   on edges, and using vertex data equality for verticex matching. *)
functor EqMatchUnitEdgeNoCtxtFun(
  G : OGRAPH where type Edge.data = unit
) : MATCH_CTXT where type G.Edge.data = unit
= struct

structure G = G;
structure OVertex = G.OVertex;
structure Edge = G.Edge;

type T = unit;

val empty = ();

fun vertex_idata_match (d1 : OVertex.IData.data) (d2 : OVertex.IData.data) 
                       (m : T) =
    if OVertex.IData.data_eq (d1, d2) then SOME m else NONE;

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
fun subst_in_vertex_idata (m : T) (n : OVertex.IData.data) = n; 
fun subst_in_vertex_data (m : T) (n : OVertex.data) = n; 
fun subst_in_edge_data (m : T) (e : Edge.data) = e;

fun pretty (m : T) = Pretty.str "";
val print = Pretty.writeln o pretty;


structure Sharing 
= struct 
type T = T;
structure G = G.SharingOGraph;
end;

end;

