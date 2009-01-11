(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  matching and substitution for Basic Quantum Expressions (with angles) *)
structure AngleMatchCtxt 
: MATCH_CTXT
where type BVertex.IData.data = RGGraph.RGVertex.qnd (* = qnd *)
= struct

structure Edge = UnitEdge; 
structure V = RGGraph.RGVertex; 
structure BVertex = V;

(* instantiations *)
					(* instantiations for vertex data varaiables *)
type T = AngleExpr.T VName.NTab.T
val empty = VName.NTab.empty;

(* info *)
fun get_vdata_vars V.Hnd = VName.NSet.empty
	| get_vdata_vars (V.Znd a) = AngleExpr.get_vars a
	| get_vdata_vars (V.Xnd a) = AngleExpr.get_vars a;

(* vertex match if angle-expresions match *)
fun vertex_idata_match (e1 as V.Hnd) (e2 as V.Hnd) m = SOME m
	| vertex_idata_match (V.Znd a1) (V.Znd a2) m = 
		AngleExpr.match a1 a2 m
	| vertex_idata_match (V.Xnd a1) (V.Xnd a2) m = 
		AngleExpr.match a1 a2 m
	| vertex_idata_match _ _ _ = NONE;

(* edges allways match - they have no data, and we ignore direction *)
fun edge_match ed1 ed2 m = SOME m; 


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
       | SOME (matched_ens,leftover_tns) => 
         Seq.single ((matched_ens,leftover_tns), m))

(* subst in vertex uses angle var instantiations *)
(* implicit: m a *)
val subst_in_angle = AngleExpr.subs;
fun subst_in_vertex m (d as V.Hnd) = d
	| subst_in_vertex m (V.Znd a) = V.Znd (subst_in_angle m a)
	| subst_in_vertex m (V.Xnd a) = V.Znd (subst_in_angle m a);
val subst_in_edge = (K I : T -> Edge.data -> Edge.data);

fun pretty m = 
    Pretty.list 
      "[" "]"
      (map (fn (n,i) => 
               Pretty.block [VName.pretty_name n, Pretty.str "=", 
                             AngleExpr.pretty i])
           (VName.NTab.list_of m));
val print = Pretty.writeln o pretty;
end;
