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

(* for multiple edge matching, when edges have no data, count the
   number of edges. *)
fun strict_multi_edge_match etab1 etab2 m = 
    if (Edge.NTab.fold (fn _ => fn i => i + 1) etab1 0) = 
       (Edge.NTab.fold (fn _ => fn i => i + 1) etab2 0) 
    then Seq.single m else Seq.empty;

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
