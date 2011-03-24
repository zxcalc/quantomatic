
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*   *)
functor OGraphParam(
  structure Param : GRAPH_PARAM 
  structure OVertex : OVERTEX
    sharing Param.Vertex.SharingWithoutData = OVertex.SharingWithoutData
    sharing type Param.subst = OVertex.subst
    sharing type Param.Vertex.data = OVertex.IData.data
) : GRAPH_PARAM (* sharing GRAPH_PARAM.Vertex = OVertex *)
= struct 
  
  structure Vertex = OVertex;
  structure Edge = Param.Edge;
  
  (* internal name data *)
  type T = Param.T;

  val rename_vname = Param.rename_vname;
  val rename_ename = Param.rename_ename;
  
  (* rename internal names in the dependency structure *)
  structure CRnmINames = Param.CRnmINames;
  (* rename vertices in the dependency structure *)
  structure CRnmVNames = Param.CRnmVNames;
  (* rename edges in the dependency structure *)
  structure CRnmENames = Param.CRnmENames;
  (* empty name-dependency information *)
  val empty = Param.empty;

  fun add_vertex vd vn vdep = 
      (case OVertex.idata_of vd of NONE => vdep
          | SOME idata => Param.add_vertex idata vn vdep);
  val add_edge = Param.add_edge;

  val delete_vertex = Param.delete_vertex;
  val delete_edge = Param.delete_edge;

  fun replace_vertex vd vn vdep = 
      vdep |> delete_vertex vn
           |> add_vertex vd vn;
  val replace_edge = Param.replace_edge;

  fun of_vertex vd vn = add_vertex vd vn Param.empty;
  val of_edge = Param.of_edge;

  (* renaming internal names within a vertex *)
  structure VDataCRnmINames : COMPOUND_RENAMING
  = EmbedCompoundRenaming(
    structure Crnm = Param.VDataCRnmINames
    type T = OVertex.data
    val update = OVertex.apply_to_idata
    val try_to_get = OVertex.idata_of
  );

  (* rename internal names within an edge data *)
  structure EDataCRnmINames = Param.EDataCRnmINames;

  (* substitutions for variables in vertex/edge data 
     (usually, we only have data on vertices) *)
  type subst = Param.subst; (* = Vertex.subst = OVertex.subst *)

  val empty_subst = Param.empty_subst; (* : IName.NTab.empty; *)
  val compose_subst = Param.compose_subst; 
      (* : Name.NTab.map_all (AngleExpr.subs u2) u1; *)

  fun try_match_vertex d1 d2 u = Vertex.match_data (d1,d2) u;
  val try_match_edge = Param.try_match_edge;

  (* IMPROVE: what data do we want here? should it be add_exp *)
  exception match_exp of unit;

  (* raises match_exp *)
  fun match_vertex d1 d2 u = 
      (case try_match_vertex d1 d2 u 
         of NONE    => raise match_exp ()
          | SOME u' => u'); 
  
  fun match_edge d1 d2 u =
      (case try_match_edge d1 d2 u 
         of NONE    => raise match_exp ()
          | SOME u' => u'); 

  fun subst_in_vertex u vd = Vertex.subst_in_data u vd;
  val subst_in_edge = Param.subst_in_edge

  val pretty_subst = Param.pretty_subst;
  val print_subst = Param.print_subst;

  structure Sharing = 
  struct 
    structure Vertex = Vertex.SharingWithData;
    structure Edge = Edge.EdgeSharingWithData;
    type T = T;
    type inames_renaming = CRnmINames.renaming; (* = IName.Rnm.renaming *)
    type subst = subst;
  end;
end;


