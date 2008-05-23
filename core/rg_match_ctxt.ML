(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  matching and substitution for Basic Quantum Expressions *)
structure AngleMatchCtxt 
: MATCH_CTXT
= struct
structure Vertex = RGVertex; 
structure Edge = UnitEdge; 
  
(* instantiations *)
					(* instantiations for vertex data varaiables *)
type T = AngleExpr.T VName.NTab.T
val empty = VName.NTab.empty;

(* info *)
fun get_vdata_vars Vertex.Hnd = VName.NSet.empty
	| get_vdata_vars (Vertex.Znd a) = AngleExpr.get_vars a
	| get_vdata_vars (Vertex.Xnd a) = AngleExpr.get_vars a;

(* vertex match if angle-expresions match *)
fun vertex_match (e1 as Vertex.Hnd) (e2 as Vertex.Hnd) m = SOME m
	| vertex_match (Vertex.Znd a1) (Vertex.Znd a2) m = 
		AngleExpr.match a1 a2 m
	| vertex_match (Vertex.Xnd a1) (Vertex.Xnd a2) m = 
		AngleExpr.match a1 a2 m
	| vertex_match _ _ _ = NONE;

(* edges allways match - they have no data *)
fun edge_match ed1 ed2 m = SOME m; 


exception too_many_pat_edges_exp of string;
fun matchup_pat_edges pes tes = 
    let fun match_more (e1,ed) (ems,tens2) = 
            (case Edge.NSet.pull_local_bot tens2 of
               NONE => raise too_many_pat_edges_exp "strict_multi_edge_match"
             | SOME (tn,tens3) => 
               (Edge.NTab.ins (e1, tn) ems, tens3))
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
    let val (matched_ens,tns) = matchup_pat_edges pes tes
    in Seq.single (matched_ens,m) end handle too_many_pat_edges_exp _ => Seq.empty;

(* subst in vertex uses angle var instantiations *)
(* implicit: m a *)
val subst_in_angle = AngleExpr.subs;
fun subst_in_vertex m (d as Vertex.Hnd) = d
	| subst_in_vertex m (Vertex.Znd a) = Vertex.Znd (subst_in_angle m a)
	| subst_in_vertex m (Vertex.Xnd a) = Vertex.Znd (subst_in_angle m a);
val subst_in_edge = (K I : T -> Edge.data -> Edge.data);

fun pretty m = 
    Pretty.list 
      "[" "]"
      (map (fn (n,i) => 
               Pretty.block [VName.pretty n, Pretty.str "=", 
                             AngleExpr.pretty i])
           (VName.NTab.dest m));
val print = Pretty.writeln o pretty;
end;
