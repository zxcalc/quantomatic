(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  matching and substitution for Basic Quantum Expressions *)
structure AngleMatchCtxt 
: MATCH_CTXT
where type Vertex.data = RGGraph.IVertex.data
= struct
structure Vertex = RGGraph.IVertex;
structure Edge = UnitEdge; 
structure V = RGGraph.RGVertex; 

(* instantiations *)
					(* instantiations for vertex data varaiables *)
type T = AngleExpr.T VName.NTab.T
val empty = VName.NTab.empty;

(* info *)
fun get_vdata_vars V.Hnd = VName.NSet.empty
	| get_vdata_vars (V.Znd a) = AngleExpr.get_vars a
	| get_vdata_vars (V.Xnd a) = AngleExpr.get_vars a;

(* vertex match if angle-expresions match *)
fun vertex_match (e1 as V.Hnd) (e2 as V.Hnd) m = SOME m
	| vertex_match (V.Znd a1) (V.Znd a2) m = 
		AngleExpr.match a1 a2 m
	| vertex_match (V.Xnd a1) (V.Xnd a2) m = 
		AngleExpr.match a1 a2 m
	| vertex_match _ _ _ = NONE;

(* edges allways match - they have no data *)
fun edge_match ed1 ed2 m = SOME m; 


(* tries to match new pattern edge with (first) one in the target set,
raises exception if target set is empty - we handle this and deal with
it when we want to. *)
exception too_many_pat_edges_exp of string;
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
           (VName.NTab.dest m));
val print = Pretty.writeln o pretty;
end;
