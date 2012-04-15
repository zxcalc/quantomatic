
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
structure SUBSTR_LINRAT_GraphParam
: GRAPH_PARAM
= struct

  structure VData = SUBSTR_LINRAT_InternVData;

  (* edges have unit data, and are ignored as we use Vertex subst type *)
  structure EData = 
    IgnoredUnifiableData(structure D = UnitData structure S = SUBSTR_LINRAT_InternVData
      val default_data = ());
  
  (* vertex, and vertex data variable dependencies *)
  structure VDep = VXBRel;
  
  (* internal name data *)
  type T = VDep.T;

  (* IMPROVE: what data do we want here? should it be add_exp *)
  exception match_exp of unit;
  exception unknown_type_exp of string;

  fun get_xnames vdep = VDep.get_codset vdep;
  fun get_vnames vdep = VDep.get_domset vdep;

  fun try_rename1_xname n1 n2 vdep = VDep.try_rename1_cod n1 n2 vdep;
  fun try_rename1_vname n1 n2 vdep = VDep.try_rename1_dom n1 n2 vdep;

  fun rename_xname n1 n2 vdep = 
      (case VDep.try_rename1_cod n1 n2 vdep of NONE => 
         raise X.Rnm.invalid_renaming_exp "SUBSTR_LINRAT_GraphParam.rename_xname"
       | SOME x => x);
       
  fun rename_vname n1 n2 vdep = 
      (case VDep.try_rename1_dom n1 n2 vdep of NONE => 
         raise V.Rnm.invalid_renaming_exp "SUBSTR_LINRAT_GraphParam.rename_vname"
       | SOME x => x);

  fun rename_ename n1 n2 vdep = vdep;
  
  (* rename names in T *)

  (* rename internal names in the dependency structure *)
  structure CRnmX : COMPOUND_RENAMING
  = CompoundRenamingFun(
    struct structure Nm = X
           type obj = T
           val get_nameset = get_xnames
           val try_rename1 = try_rename1_xname
    end);

  (* rename vertices in the dependency structure *)
  structure CRnmV : COMPOUND_RENAMING
  = CompoundRenamingFun(
    struct structure Nm = V
           type obj = T
           val get_nameset = get_vnames
           val try_rename1 = try_rename1_vname
    end);

  (* rename edges in the dependency structure *)
  structure CRnmE : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = E type obj = T);

  (* empty name-dependency information *)
  val empty = VDep.empty;

  fun add_vertex vd vn vdep = 
      (case SUBSTR_LINRAT_InternVData.angle_of_data vd of NONE => vdep
          | SOME angle => VDep.add_to_dom vn (LinratAngleExpr.get_vars angle) vdep);
  fun add_edge ed en vdep = vdep;

  fun delete_vname vn vdep = VDep.del_dom vn vdep;
  fun delete_ename en vdep = vdep;

  fun replace_vertex vd vn vdep = 
      vdep |> delete_vname vn
           |> add_vertex vd vn;
  fun replace_edge ed en vdep = vdep;

  fun of_vertex vd vn = add_vertex vd vn VDep.empty;
  fun of_edge en ed = empty;

  (* renaming internal names within a vertex *)
  structure VDataCRnmX : COMPOUND_RENAMING
  = struct 
    type obj = VData.data
    type renaming = X.Rnm.T

    val empty_renaming = CRnmX.empty_renaming

    fun rename rnm vd = 
        (case SUBSTR_LINRAT_InternVData.angle_of_data vd of NONE => (rnm,vd)
         | SOME angle => 
           let val (rnm',angle') = LinratAngleExpr.CRnm.rename rnm angle
           in (rnm', SUBSTR_LINRAT_InternVData.map_to_angle (K angle') vd) end)

    fun mk_renaming vd rnm = 
        (case SUBSTR_LINRAT_InternVData.angle_of_data vd of NONE => rnm
         | SOME angle => LinratAngleExpr.CRnm.mk_renaming angle rnm);

    fun do_renaming rnm vd = 
        (case SUBSTR_LINRAT_InternVData.angle_of_data vd of NONE => vd
         | SOME angle => 
           SUBSTR_LINRAT_InternVData.map_to_angle (LinratAngleExpr.CRnm.do_renaming rnm) vd);

    val restrict_to_avoiding = X.Rnm.restrict_to_avoiding;
  end;

  (* rename internal names within an edge data *)
  structure EDataCRnmX : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = X type obj = EData.data);

  (* substitutions for variables in vertex/edge data 
     (usually, we only have data on vertices) *)
  type subst = VData.subst; (* : LinratAngleExpr.T IName.NTab.T; *)

  val empty_subst = VData.empty_subst; (* : IName.NTab.empty; *)
  fun compose_subst u1 u2 = VData.compose_subst (u1,u2); 
      (* : Name.NTab.map_all (LinratAngleExpr.subs u2) u1; *)

  fun try_match_vertex d1 d2 u = VData.match_data (d1,d2) u;
  fun try_match_edge e1 e2 u = SOME u;

  (* raises match_exp *)
  fun match_vertex d1 d2 u = 
      (case try_match_vertex d1 d2 u 
         of NONE    => raise match_exp ()
          | SOME u' => u'); 
  
  fun match_edge d1 d2 u =
      (case try_match_edge d1 d2 u 
         of NONE    => raise match_exp ()
          | SOME u' => u'); 

  fun subst_in_vertex u vd = VData.subst_in_data u vd;
  fun subst_in_edge u ed = ed;

  val pretty_subst = VData.pretty_subst;
  val print_subst = Pretty.writeln o pretty_subst;

  val pretty_theory_name = Pretty.str "substr_linrat";

  structure Sharing = 
  struct 
    structure VData = VData.Sharing;
    structure EData = EData.Sharing;
    type T = T;
    type xnames_renaming = CRnmX.renaming; (* = IName.Rnm.renaming *)
    type subst = subst;
  end;
end;


structure SUBSTR_LINRAT_OVData = OVertexData(SUBSTR_LINRAT_InternVData);
structure SUBSTR_LINRAT_OGraphParam = OGraphParam(
  structure Param = SUBSTR_LINRAT_GraphParam 
        and OVData = SUBSTR_LINRAT_OVData);

structure SUBSTR_LINRAT_OVertexComponentData_Param = OVertexComponentData_Param(
  structure VertexDataParam = SUBSTR_LINRAT_VertexComponentData_Param
  structure OVData = SUBSTR_LINRAT_OVData);

structure SUBSTR_LINRAT_OVertexComponentData = ElementComponentData(SUBSTR_LINRAT_OVertexComponentData_Param);

