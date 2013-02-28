(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  node vertex data for red-green vertex (i.e. before Graph Vertex 
    rather than OGraph VData.) *)
signature TEST_RG_UNIFIABLE_DATA =    
sig
  datatype qnd = Xnd of LinratAngleExpr.T (* Red: defined using H of Z *) 
               | Znd of LinratAngleExpr.T (* Green *)
               | Hnd; (* Hadamard node *)
  include UNIFIABLE_DATA
  sharing type data = qnd;
  val angle_of_data : data -> LinratAngleExpr.T option;
  val map_to_angle : (LinratAngleExpr.T -> LinratAngleExpr.T) -> data -> data; 
end;

(* reg-green vertex data *)
structure Test_RG_InternVData : TEST_RG_UNIFIABLE_DATA
= struct
  exception unimplemented_exp of string;

  datatype qnd = Xnd of LinratAngleExpr.T (* Red: defined using H of Z *) 
               | Znd of LinratAngleExpr.T (* Green *)
               | Hnd; (* Hadamard node *)
  type data = qnd; 

  fun data_ord (Hnd, Hnd) = EQUAL
    | data_ord (Hnd, _) = LESS
    | data_ord (_, Hnd) = GREATER
    | data_ord (Znd a, Znd b) = LinratAngleExpr.ord (a,b)
    | data_ord (Znd _, _) = LESS
    | data_ord (_, Znd _) = GREATER
    | data_ord (Xnd a, Xnd b) = LinratAngleExpr.ord (a,b);
  fun data_eq (n1, n2) = case data_ord (n1, n2) of EQUAL => true | _ => false;
      
  fun pretty_data (Xnd a) = 
      Pretty.block [Pretty.str "X(", LinratAngleExpr.pretty a, Pretty.str ")"]  
    | pretty_data (Znd a) = 
      Pretty.block [Pretty.str "Z(", LinratAngleExpr.pretty a, Pretty.str ")"]
    | pretty_data Hnd = Pretty.str "H";
  val print_data = Pretty.writeln o pretty_data;
  
  type subst = LinratAngleExpr.match;
  val empty_subst = LinratAngleExpr.empty_match;
  fun compose_subst (u1,u2) = LinratAngleExpr.compose_matches u1 u2;
  fun match_data ((Hnd), (Hnd)) m = SOME m
    | match_data ((Znd a1),(Znd a2)) m = 
      LinratAngleExpr.match a1 a2 m
    | match_data ((Xnd a1),(Xnd a2)) m = 
      LinratAngleExpr.match a1 a2 m
    | match_data _ _ = NONE;
  fun unify_data (x,y) m = 
      raise unimplemented_exp "Test_VData.unify_data: has not yet been written.";
  fun subst_in_data _ Hnd = Hnd
    | subst_in_data u (Xnd a) = Xnd (LinratAngleExpr.subst u a)
    | subst_in_data u (Znd a) = Znd (LinratAngleExpr.subst u a);
    
  val pretty_subst = LinratAngleExpr.pretty_match;
  val print_subst = Pretty.writeln o pretty_subst;

  fun map_to_angle f (Xnd angle) = (Xnd (f angle))
    | map_to_angle f (Znd angle) = (Xnd (f angle))
    | map_to_angle _ x = x;

  fun angle_of_data (Xnd angle) = SOME angle
    | angle_of_data (Znd angle) = SOME angle
    | angle_of_data _ = NONE;
    
  val default_data = Xnd LinratAngleExpr.zero;
  
  structure Sharing = struct type data = data; type subst = subst; end
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
structure Test_RG_GraphParam
: GRAPH_PARAM
= struct

  structure VData = Test_RG_InternVData;

  (* edges have unit data, and are ignored as we use Vertex subst type *)
  structure EData = 
    IgnoredUnifiableData(structure D = UnitData 
                         structure S = Test_RG_InternVData 
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
      Test_RG_InternVData.map_to_angle 
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
         raise V.Rnm.invalid_renaming_exp "Test_RG_GraphParam.rename_xname"
       | SOME x => x);

  fun rename_vname n1 n2 vdep = 
      (case VDep.try_rename1_dom n1 n2 vdep of NONE => 
         raise V.Rnm.invalid_renaming_exp "Test_RG_GraphParam.rename_vname"
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
      (case Test_RG_InternVData.angle_of_data vd of NONE => vdep
          | SOME angle => VDep.add_to_dom vn (LinratAngleExpr.get_vars angle) vdep);
  fun add_edge _ _ vdep = vdep;

  fun delete_vname vn vdep = VDep.del_dom vn vdep;
  fun delete_ename _ vdep = vdep;

  fun vertex_type_name (Test_RG_InternVData.Xnd _) = "X"
    | vertex_type_name (Test_RG_InternVData.Znd _) = "Z"
    | vertex_type_name (Test_RG_InternVData.Hnd) = "hadamard"
  fun vertex_default_data "X" = (Test_RG_InternVData.Xnd LinratAngleExpr.zero)
    | vertex_default_data "Z" = (Test_RG_InternVData.Znd LinratAngleExpr.zero)
    | vertex_default_data "hadamard" = (Test_RG_InternVData.Hnd)
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


structure Test_RG_OVData = OVertexData(Test_RG_InternVData);

(* Basic structure for testing graphs later... *)
structure Test_RG_Graph : BANG_GRAPH
= struct 
  structure BasicBangGraph = BasicBangGraphFun(
    structure Param = OGraphParam(
      structure Param = Test_RG_GraphParam 
      and OVData = Test_RG_OVData
    );
    type data = unit (* no extra inherrited data for graphs *)
    val empty_data = ());
  structure BBGraph = BangGraphTools(BasicBangGraph);
  open BBGraph;
end;

(* ML utility code for building test graphs *)
structure Test_RG_MkG = 
struct
  structure IVData = Test_RG_InternVData;
  structure G = Test_RG_Graph;
  structure OVData = Test_RG_Graph.OVData;
  structure GIso = BangGraphIso(Test_RG_Graph);

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

structure Test_RG_BBox_Util =
struct
  open Test_RG_MkG
  fun num_bboxes g = B.NSet.cardinality (G.get_bboxes g)
  
  fun assert_n_bboxes g n () =
    if (num_bboxes g) = n
    then () else raise ERROR(
      "expected: " ^ Int.toString n ^ " bboxes, "^
      "got: " ^ Int.toString (num_bboxes g))
end

structure Test_RG_Rule = Rule(Test_RG_Graph);
structure Test_RG_Ruleset = Ruleset(Test_RG_Rule);
structure Test_RG_MatchState =
  OGraphMatchState(
    structure G = Test_RG_MkG.G
    type data = unit
    val empty_data = ()
  );
structure Test_RG_BGMatchState =
  BangGraphMatchState(
    structure G = Test_RG_MkG.G
    type data = unit
    val empty_data = ()
  );
structure Test_RG_InnerMatcher =
  SimpleInnerMatchSearch(structure MatchState = Test_RG_MatchState);
structure Test_RG_BGInnerMatcher =
  SimpleInnerMatchSearch(structure MatchState = Test_RG_BGMatchState);
structure Test_RG_ConcreteMatcher =
  ConcreteMatchSearch(structure InnerMatchSearch = Test_RG_InnerMatcher);
structure Test_RG_GreedyMatcher =
  GreedyMatchSearch(
    structure InnerMatchSearch = Test_RG_BGInnerMatcher
    structure BGMatchState = Test_RG_BGMatchState
  );
(* set to 4 for really detailed messages *)
(*
Test_RG_InnerMatcher.Log.level_ref := 2;
Test_RG_BGInnerMatcher.Log.level_ref := 2;
Test_RG_ConcreteMatcher.Log.level_ref := 2;
Test_RG_GreedyMatcher.Log.level_ref := 2;
*)

structure Test_RG_MatchUtil =
struct
  fun assert_n_match n mseq () = let
    val mlist = Seq.list_of mseq
    val nm = length mlist
  in if nm = n then I
     else
       (List.map Test_RG_MatchState.print mlist;
        raise ERROR ("expected " ^ Int.toString n ^
                     " matching(s), got " ^ Int.toString nm))
  end
end;

structure Test_RG_BGMatchUtil =
struct
  structure G = Test_RG_MkG.G;
  structure Rule = Rule(G);

  fun assert_n_match n mseq () = let
    val mlist = Seq.list_of mseq
    val nm = length mlist
  in if nm = n then I
     else
       (List.map Test_RG_BGMatchState.print mlist;
        raise ERROR ("expected " ^ Int.toString n ^
                     " matching(s), got " ^ Int.toString nm))
  end

  fun assert_boundary_coherent (g1,g2) = let
    (* easiest just to try to make a rule out of it *)
    val _ = Rule.mk (g1,g2)
  in
    (g1,g2)
  end
end;

structure Test_VertexComponentData_Param
: ELEMENT_COMPONENT_DATA_PARAM where type data = Test_RG_InternVData.data
= struct 
  structure VData = Test_RG_InternVData;
  type data = VData.data;
  exception unknown_type_exp of string;
  exception unexpected_data_exp;

  fun type_of (VData.Xnd _) = "X"
    | type_of (VData.Znd _) = "Z"
    | type_of (VData.Hnd)   = "hadamard";

  fun cdata_of (VData.Xnd a) = (ComponentData.LinratAngleExpr a)
    | cdata_of (VData.Znd a) = (ComponentData.LinratAngleExpr a)
    | cdata_of (VData.Hnd)   = ComponentData.Unit;

  fun default_data "X" = (VData.Xnd LinratAngleExpr.zero)
    | default_data "Z" = (VData.Znd LinratAngleExpr.zero)
    | default_data "hadamard" = (VData.Hnd)
    | default_data s = raise unknown_type_exp s

  fun unwrapUpd upd a =
    let
      fun unwrapLinrat (ComponentData.LinratAngleExpr e) = e
        | unwrapLinrat _ = raise unexpected_data_exp
    in
      unwrapLinrat (upd (ComponentData.LinratAngleExpr a))
    end;

  fun update upd (VData.Xnd a) = (VData.Xnd (unwrapUpd upd a))
    | update upd (VData.Znd a) = (VData.Znd (unwrapUpd upd a))
    | update _   (VData.Hnd)   = VData.Hnd;
end;

structure Test_OVertexComponentData_Param = OVertexComponentData_Param(
  structure VertexDataParam = Test_VertexComponentData_Param
  structure OVData = Test_RG_OVData);
structure Test_OVertexComponentData = ElementComponentData(Test_OVertexComponentData_Param);

structure Test_IO_Interface_v2 : IO_INTERFACE_XML = IO_Interface_Xml(
  structure Ruleset = Test_RG_Ruleset
  structure VertexData = Test_OVertexComponentData;
  structure EdgeData = EmptyElementComponentData;
);



