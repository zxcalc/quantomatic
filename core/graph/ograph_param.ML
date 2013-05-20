(* *)
signature OGRAPH_PARAM_SHARING
= sig 
  include GRAPH_PARAM_SHARING;
  structure OVData : UNIFIABLE_DATA_SHARING;
  sharing VData = OVData; (* note importance of direction. *)
end;

signature OGRAPH_PARAM
= sig
  include GRAPH_PARAM;
  structure OVData : OVERTEX_DATA

  (* syntax convenience for sharing types *)
  structure OSharing : OGRAPH_PARAM_SHARING
  sharing Sharing.VData = OVData.Sharing
  sharing Sharing = OSharing;
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*   *)
functor OGraphParam(
  structure Param : GRAPH_PARAM 
  structure OVData : OVERTEX_DATA  (* constructed separately, needed *)
    sharing type Param.subst = OVData.subst
    sharing type Param.VData.data = OVData.IData.data
) : OGRAPH_PARAM (* sharing GRAPH_PARAM.Vertex = OVertex *)
= struct 
  
  structure VData = OVData;
  structure OVData = OVData;
  structure EData = Param.EData;
  
  (* internal name data *)
  type T = Param.T;

  
  (* pretty printing *)
  val pretty = Param.pretty;
  val print = Param.print;

  val get_xnames = Param.get_xnames;

  val dep_vnames_of_xname = Param.dep_vnames_of_xname;
  val dep_enames_of_xname = Param.dep_enames_of_xname;
  
  (*val subst_xname_in_vdata = 
      OVData.apply_to_idata oo Param.subst_xname_in_vdata;
  val subst_xname_in_edata = Param.subst_xname_in_edata;*)
  
  (*val rename_xname = Param.rename_xname;*)
  val rename_vname = Param.rename_vname;
  val rename_ename = Param.rename_ename;
  (*val rename_xs = Param.rename_xs*)
  val rename_vertices = Param.rename_vertices
  val rename_edges = Param.rename_edges
  
  (* rename internal names in the dependency structure *)
  (*structure CRnmX = Param.CRnmX;
  (* rename vertices in the dependency structure *)
  structure CRnmV = Param.CRnmV;
  (* rename edges in the dependency structure *)
  structure CRnmE = Param.CRnmE;*)
  (* empty name-dependency information *)
  val empty = Param.empty;

  fun add_vertex vd vn vdep = 
      (case OVData.idata_of vd of NONE => vdep
          | SOME idata => Param.add_vertex idata vn vdep);
  val add_edge = Param.add_edge;

  val delete_vname = Param.delete_vname;
  val delete_ename = Param.delete_ename;

  fun replace_vertex vd vn vdep = 
      vdep |> delete_vname vn
           |> add_vertex vd vn;
  val replace_edge = Param.replace_edge;

  fun of_vertex vd vn = add_vertex vd vn Param.empty;
  val of_edge = Param.of_edge;

  (* renaming internal names within a vertex *)
  (* structure VDataCRnmX : COMPOUND_RENAMING
  = EmbedCompoundRenaming(
    structure Crnm = Param.VDataCRnmX
    type T = OVData.data
    val update = OVData.apply_to_idata
    val try_to_get = OVData.idata_of
  );
  *)
  (* rename internal names within an edge data *)
  (* structure EDataCRnmX = Param.EDataCRnmX; *)

  (* substitutions for variables in vertex/edge data 
     (usually, we only have data on vertices) *)
  type subst = Param.subst; (* = VData.subst = OVData.subst *)

  val empty_subst = Param.empty_subst; (* : IName.NTab.empty; *)
  val compose_subst = Param.compose_subst; 
      (* : Name.NTab.map_all (LinratAngleExpr.subs u2) u1; *)

  fun try_match_vertex d1 d2 u = OVData.match_data (d1,d2) u;
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

  fun subst_in_vertex u vd = OVData.subst_in_data u vd;
  val subst_in_edge = Param.subst_in_edge

  val pretty_subst = Param.pretty_subst;
  val print_subst = Param.print_subst;

  val pretty_theory_name = Param.pretty_theory_name;

  structure Sharing = 
  struct 
    structure VData = OVData.Sharing;
    structure EData = EData.Sharing;
    type T = T;
    type subst = subst;
  end;
  structure OSharing =
  struct
    open Sharing;
    structure OVData = OVData.Sharing;
  end;

end;


