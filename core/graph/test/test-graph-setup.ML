(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
structure Test_GraphParam
: GRAPH_PARAM
= struct

  structure VData = Test_InternVData;

  (* edges have unit data, and are ignored as we use Vertex subst type *)
  structure EData = 
    IgnoredUnifiableData(structure D = UnitData 
                         structure S = Test_InternVData 
                         val default_data = ());
  
  (* vertex, and vertex data variable dependencies *)
  structure VDep = VXBRel;
  
  (* internal name data *)
  type T = VDep.T;

  (* pretty printing *)
  val pretty = VDep.pretty;
  val print = VDep.print;
  
  (* IMPROVE: what data do we want here? should it be add_exp *)
  exception match_exp of unit;
  exception unknown_type_exp of string;

  fun subst_xname_in_vdata n1 n2 d =
      Test_InternVData.map_to_angle 
        (untryify (LinratAngleExpr.try_rename1 n1 n2)) d 
      handle tryify_failed () => 
        raise X.Rnm.invalid_renaming_exp "subst_xname_in_vdata";
  fun subst_xname_in_edata _ _ d = d;
  
  fun get_xnames vdep = VDep.get_codset vdep;
  fun get_vnames vdep = VDep.get_domset vdep;

  fun dep_vnames_of_xname vdep n = VDep.codf vdep n;
  fun dep_enames_of_xname _    _ = E.NSet.empty;

  fun try_rename1_xname n1 n2 vdep = VDep.try_rename1_cod n1 n2 vdep;
  fun try_rename1_vname n1 n2 vdep = VDep.try_rename1_dom n1 n2 vdep;

  fun rename_xname n1 n2 vdep = 
      (case VDep.try_rename1_cod n1 n2 vdep of NONE => 
         raise V.Rnm.invalid_renaming_exp "Test_GraphParam.rename_xname"
       | SOME x => x);

  fun rename_vname n1 n2 vdep = 
      (case VDep.try_rename1_dom n1 n2 vdep of NONE => 
         raise V.Rnm.invalid_renaming_exp "Test_GraphParam.rename_vname"
       | SOME x => x);

  fun rename_ename _ _ vdep = vdep;
  
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
      (case Test_InternVData.angle_of_data vd of NONE => vdep
          | SOME angle => VDep.add_to_dom vn (LinratAngleExpr.get_vars angle) vdep);
  fun add_edge _ _ vdep = vdep;

  fun delete_vname vn vdep = VDep.del_dom vn vdep;
  fun delete_ename _ vdep = vdep;

  fun vertex_type_name (Test_InternVData.Xnd _) = "X"
    | vertex_type_name (Test_InternVData.Znd _) = "Z"
    | vertex_type_name (Test_InternVData.Hnd) = "hadamard"
  fun vertex_default_data "X" = (Test_InternVData.Xnd LinratAngleExpr.zero)
    | vertex_default_data "Z" = (Test_InternVData.Znd LinratAngleExpr.zero)
    | vertex_default_data "hadamard" = (Test_InternVData.Hnd)
    | vertex_default_data s = raise unknown_type_exp s
  fun edge_type_name () = "unit"
  fun edge_default_data "unit" = ()
    | edge_default_data s = raise unknown_type_exp s

  fun replace_vertex vd vn vdep = 
      vdep |> delete_vname vn
           |> add_vertex vd vn;
  fun replace_edge _ _ vdep = vdep;

  fun of_vertex vd vn = add_vertex vd vn VDep.empty;
  fun of_edge _ _ = empty;
  
  (* substitutions for variables in vertex/edge data 
     (usually, we only have data on vertices) *)
  type subst = VData.subst; (* : LinratAngleExpr.T X.NTab.T; *)

  val empty_subst = VData.empty_subst; (* : X.NTab.empty; *)
  fun compose_subst u1 u2 = VData.compose_subst (u1,u2); 
      (* : Name.NTab.map_all (LinratAngleExpr.subs u2) u1; *)

  fun try_match_vertex d1 d2 u = VData.match_data (d1,d2) u;
  fun try_match_edge _ _ u = SOME u;

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
  fun subst_in_edge _ ed = ed;

  val pretty_subst = VData.pretty_subst;
  val print_subst = Pretty.writeln o pretty_subst;

  val pretty_theory_name = Pretty.str "red_green";

  structure Sharing = 
  struct 
    structure VData = VData.Sharing;
    structure EData = EData.Sharing;
    type T = T;
    type subst = subst;
  end;
end;


structure Test_OVData = OVertexData(Test_InternVData);
structure Test_OGraphParam = OGraphParam(
  structure Param = Test_GraphParam 
        and OVData = Test_OVData);

structure Test_OVertexComponentData_Param = OVertexComponentData_Param(
  structure VertexDataParam = Test_VertexComponentData_Param
  structure OVData = Test_OVData);
structure Test_OVertexComponentData = ElementComponentData(Test_OVertexComponentData_Param);

(* Basic structure for testing graphs later... *)
structure DB_Test_Graph 
= struct 
  structure BasicBangGraph = BasicBangGraphFun(
    structure Param = Test_OGraphParam
    type data = unit (* no extra inherrited data for graphs *)
    val empty_data = ());
  structure BBGraph = BangGraphTools(BasicBangGraph);
  open BBGraph;
end;

structure Test_Graph : BANG_GRAPH = DB_Test_Graph;


(* ML utility code for building test graphs *)
structure Test_MkG = 
struct
  structure IVData = Test_InternVData;
  structure G = Test_Graph;
  structure OVData = Test_Graph.OVData;
  structure GIso = BangGraphIso(Test_Graph);

  val bvert = OVData.WVert;
  val parse_angle = LinratAngleExpr.parse;
  val zero_angle = LinratAngleExpr.zero;
  val var_angle = LinratAngleExpr.mk_var o X.mk;
  fun var_iangle i v = LinratAngleExpr.mk (0,1) [(v,i)];
  val add_angles = LinratAngleExpr.add_expr;
  val pi_angle = LinratAngleExpr.pi;
  fun mkZ a = OVData.NVert (IVData.Znd a);
  fun mkX a = OVData.NVert (IVData.Xnd a);
  val mkH = OVData.NVert IVData.Hnd;
  val undir_edge = (G.UnDirected,());
  val dir_edge = (G.Directed,());
  val no_extra_names = (V.NSet.empty, E.NSet.empty, B.NSet.empty);

  (* FIXME: move into Testing in isaplib *)
  fun test_force s f v =
    case Testing.test s f v of
         NONE => raise Testing.failed_tests_exp (!Testing.tests_failed_ref)
       | SOME x => x
end;

structure Test_BBox_Util =
struct
  open Test_MkG
  fun num_bboxes g = B.NSet.cardinality (G.get_bboxes g)
  
  fun assert_n_bboxes g n () =
    if (num_bboxes g) = n
    then () else raise ERROR(
      "expected: " ^ Int.toString n ^ " bboxes, "^
      "got: " ^ Int.toString (num_bboxes g))
end

