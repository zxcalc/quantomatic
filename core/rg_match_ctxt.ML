(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  matching and substitution for Basic Quantum Expressions (with angles) *)
structure AngleMatchCtxt 
: MATCH_CTXT
where type G.BVertex.IData.data = RGGraph.RGVertex.qnd (* = qnd *)
= struct

structure G = RGGraph;
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

(* subst in vertex uses angle var instantiations *)
(* implicit: m a *)
val subst_in_angle = AngleExpr.subs;

fun subst_in_vertex_idata m (V.Znd a) = V.Znd (subst_in_angle m a)
	| subst_in_vertex_idata m (V.Xnd a) = V.Xnd (subst_in_angle m a)
  | subst_in_vertex_idata m d = d;

fun subst_in_vertex_data m (V.IVert d) = V.IVert (subst_in_vertex_idata m d)
  | subst_in_vertex_data m d = d;

val subst_in_edge_data = (K I : T -> Edge.data -> Edge.data);

fun pretty m = 
    Pretty.list 
      "[" "]"
      (map (fn (n,i) => 
               Pretty.block [VName.pretty_name n, Pretty.str "=", 
                             AngleExpr.pretty i])
           (VName.NTab.list_of m));
val print = Pretty.writeln o pretty;


structure Sharing 
= struct 
type T = T;
structure G = G.SharingBGraph;
end;

end;


structure MCtxt = AngleMatchCtxt;
