(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
structure Test_GraphParam
: GRAPH_PARAM
= struct

  structure Vertex = Test_InternVertex;

  (* edges have unit data, and are ignored as we use Vertex subst type *)
  structure UnifyableEdgeData = 
    IgnoredUnifiableData(structure D = UnitData structure S = Test_InternVertex);
  structure EdgeStrNm = StrNmData(structure D = UnifyableEdgeData 
                             val default_data = ());
  structure Edge = EdgeFun(EdgeStrNm);

  structure IName = LinratExpr.VName;
  
  (* vertex, and vertex data variable dependencies *)
  structure VDep = NameBRelFun(structure Dom = Vertex and Cod = IName);
  
  (* internal name data *)
  type T = VDep.T;

  (* IMPROVE: what data do we want here? should it be add_exp *)
  exception match_exp of unit;
  exception unknown_type_exp of string;

  fun get_IName vdep = VDep.get_codset vdep;
  fun get_vnames vdep = VDep.get_domset vdep;

  fun try_rename1_iname n1 n2 vdep = VDep.try_rename1_cod n1 n2 vdep;
  fun try_rename1_vname n1 n2 vdep = VDep.try_rename1_dom n1 n2 vdep;

  fun rename_vname n1 n2 vdep = 
      (case VDep.try_rename1_dom n1 n2 vdep of NONE => 
         raise Vertex.Rnm.invalid_renaming_exp "Test_GraphParam.rename_vname"
       | SOME x => x);

  fun rename_ename n1 n2 vdep = vdep;
  
  (* rename names in T *)

  (* rename internal names in the dependency structure *)
  structure CRnmINames : COMPOUND_RENAMING
  = CompoundRenamingFun(
    struct structure Nm = IName
           type obj = T
           val get_nameset = get_IName
           val try_rename1 = try_rename1_iname
    end);

  (* rename vertices in the dependency structure *)
  structure CRnmVNames : COMPOUND_RENAMING
  = CompoundRenamingFun(
    struct structure Nm = Vertex
           type obj = T
           val get_nameset = get_vnames
           val try_rename1 = try_rename1_vname
    end);

  (* rename edges in the dependency structure *)
  structure CRnmENames : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = Edge type obj = T);

  (* empty name-dependency information *)
  val empty = VDep.empty;

  fun add_vertex vd vn vdep = 
      (case Test_VertexData.angle_of_data vd of NONE => vdep
          | SOME angle => VDep.add_to_dom vn (LinratExpr.get_vars angle) vdep);
  fun add_edge ed en vdep = vdep;

  fun delete_vertex vn vdep = VDep.del_dom vn vdep;
  fun delete_edge en vdep = vdep;

  fun vertex_type_name (Test_VertexData.Xnd _) = "X"
    | vertex_type_name (Test_VertexData.Znd _) = "Z"
    | vertex_type_name (Test_VertexData.Hnd) = "hadamard"
  fun vertex_default_data "X" = (Test_VertexData.Xnd LinratExpr.zero)
    | vertex_default_data "Z" = (Test_VertexData.Znd LinratExpr.zero)
    | vertex_default_data "hadamard" = (Test_VertexData.Hnd)
    | vertex_default_data s = raise unknown_type_exp s
  fun edge_type_name () = "unit"
  fun edge_default_data "unit" = ()
    | edge_default_data s = raise unknown_type_exp s

  fun replace_vertex vd vn vdep = 
      vdep |> delete_vertex vn
           |> add_vertex vd vn;
  fun replace_edge ed en vdep = vdep;

  fun of_vertex vd vn = add_vertex vd vn VDep.empty;
  fun of_edge en ed = empty;

  (* renaming internal names within a vertex *)
  structure VDataCRnmINames : COMPOUND_RENAMING
  = struct 
    type obj = Vertex.data
    type renaming = IName.Rnm.T

    val empty_renaming = CRnmINames.empty_renaming

    fun rename rnm vd = 
        (case Test_VertexData.angle_of_data vd of NONE => (rnm,vd)
         | SOME angle => 
           let val (rnm',angle') = LinratExpr.CRnm.rename rnm angle
           in (rnm', Test_VertexData.map_to_angle (K angle') vd) end)

    fun mk_renaming vd rnm = 
        (case Test_VertexData.angle_of_data vd of NONE => rnm
         | SOME angle => LinratExpr.CRnm.mk_renaming angle rnm);

    fun do_renaming rnm vd = 
        (case Test_VertexData.angle_of_data vd of NONE => vd
         | SOME angle => 
           Test_VertexData.map_to_angle (LinratExpr.CRnm.do_renaming rnm) vd);

    val restrict_to_avoiding = IName.Rnm.restrict_to_avoiding;
  end;

  (* rename internal names within an edge data *)
  structure EDataCRnmINames : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = IName type obj = Edge.data);

  (* substitutions for variables in vertex/edge data 
     (usually, we only have data on vertices) *)
  type subst = Vertex.subst; (* : LinratExpr.T IName.NTab.T; *)

  val empty_subst = Vertex.empty_subst; (* : IName.NTab.empty; *)
  fun compose_subst u1 u2 = Vertex.compose_subst (u1,u2); 
      (* : Name.NTab.map_all (LinratExpr.subs u2) u1; *)

  fun try_match_vertex d1 d2 u = Vertex.match_data (d1,d2) u;
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

  fun subst_in_vertex u vd = Vertex.subst_in_data u vd;
  fun subst_in_edge u ed = ed;

  val pretty_subst = Vertex.pretty_subst;
  val print_subst = Pretty.writeln o pretty_subst;

  val pretty_theory_name = Pretty.str "red_green";

  structure Sharing = 
  struct 
    structure Vertex = Vertex.SharingWithData;
    structure Edge = Edge.EdgeSharingWithData;
    type T = T;
    type inames_renaming = CRnmINames.renaming; (* = IName.Rnm.renaming *)
    type subst = subst;
  end;
end;


structure Test_OVertex = OVertex(Test_InternVertex);
structure Test_OGraphParam = OGraphParam(
  structure Param = Test_GraphParam 
        and OVertex = Test_OVertex);

structure Test_OVertexComponentData_Param = OVertexComponentData_Param(
  structure VertexDataParam = Test_VertexComponentData_Param
  structure OVertex = Test_OVertex);
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
  structure VD = Test_VertexData;
  structure V = Test_Graph.OVertex;
  structure E = Test_Graph.Edge;
  structure G = Test_Graph;

  val bvert = V.EVert;
  val zero_angle = LinratExpr.zero;
  val var_angle = LinratExpr.mk_var;
  fun var_iangle i v = LinratExpr.mk (0,1) [(v,i)];
  val add_angles = LinratExpr.add_expr;
  val pi_angle = LinratExpr.pi;
  fun mkZ a = V.IVert (VD.Znd a);
  fun mkX a = V.IVert (VD.Xnd a);
  val mkH = V.IVert VD.Hnd;
  val undir_edge = (G.UnDirected,());
  val dir_edge = (G.Directed,());
  val no_extra_names = (V.NSet.empty, E.NSet.empty, G.BBox.NSet.empty);
end;




(* test some basic graph constructions *)
local

open Test_MkG;

(* a simple graph, and some basic manipulations *)
val g = G.empty;
val (n_a, g) = g |> G.add_vertex bvert;
val (n_b, g) = g |> G.add_vertex bvert;
val (e0, g) = g |> G.add_edge undir_edge n_a n_b;
val (e1, g) = g |> G.add_edge undir_edge n_a n_b;
val n_c = (V.mk "n_c");
val g = g |> G.rename_vname n_a n_c;
val e3 = (E.mk "e_c");
val g = g |> G.rename_ename e0 e3;

(* 3 CNOTs = swap example *)
val g2 = G.empty;
val (in1, g2)  = g2 |> G.add_named_vertex (V.mk "in1") bvert;
val (in2, g2)  = g2 |> G.add_named_vertex (V.mk "in2") bvert;  
val (out1, g2)  = g2 |> G.add_named_vertex (V.mk "out1") bvert;
val (out2, g2)  = g2 |> G.add_named_vertex (V.mk "out2") bvert;

val (green1, g2) = g2 |> G.add_named_vertex (V.mk "green1") (mkZ zero_angle);
val (green2, g2) = g2 |> G.add_named_vertex (V.mk "green2") (mkZ zero_angle);
val (green3, g2) = g2 |> G.add_named_vertex (V.mk "green3") (mkZ zero_angle);

val (red1, g2) = g2 |> G.add_named_vertex (V.mk "red1") (mkX zero_angle);
val (red2, g2) = g2 |> G.add_named_vertex (V.mk "red2") (mkX zero_angle);
val (red3, g2) = g2 |> G.add_named_vertex (V.mk "red3") (mkX zero_angle);

val (e0, g2) = g2 |> G.add_edge undir_edge in1 green1;
val (e0, g2) = g2 |> G.add_edge undir_edge in2 red1;
val (e0, g2) = g2 |> G.add_edge undir_edge red1 green1;
val (e0, g2) = g2 |> G.add_edge undir_edge red1 green2;
val (e0, g2) = g2 |> G.add_edge undir_edge red2 green1;
val (e0, g2) = g2 |> G.add_edge undir_edge red2 green2;
val (e0, g2) = g2 |> G.add_edge undir_edge red3 green2;
val (e0, g2) = g2 |> G.add_edge undir_edge red2 green3;
val (e0, g2) = g2 |> G.add_edge undir_edge red3 green3;
val (e0, g2) = g2 |> G.add_edge undir_edge out1 green3;
val (e0, g2) = g2 |> G.add_edge undir_edge out2 red3;

(* just a swap *)

val g3 = G.empty;
val (in1, g3)  = g3 |> G.add_named_vertex (V.mk "in1") bvert;
val (in2, g3)  = g3 |> G.add_named_vertex (V.mk "in2") bvert;

val (out1, g3)  = g3 |> G.add_named_vertex (V.mk "out1")  
					       bvert;
val (out2, g3)  = g3 |> G.add_named_vertex (V.mk "out2")
					       bvert;
val (e0, g3) = g3 |> G.add_edge undir_edge in1 out2;
val (e0, g3) = g3 |> G.add_edge undir_edge in2 out1;


(*  A MC pattern for a CNOT *)
val g4 = G.empty;
val (in1, g4)  = g4 |> G.add_named_vertex (V.mk "in1") bvert;
val (in2, g4)  = g4 |> G.add_named_vertex (V.mk "in2") bvert;

val (out1, g4)  = g4 |> G.add_named_vertex (V.mk "out1") bvert;
val (out2, g4)  = g4 |> G.add_named_vertex (V.mk "out2") bvert;

val (green1, g4) = g4 |> G.add_named_vertex (V.mk "green1") (mkZ zero_angle);
val (green2, g4) = g4 |> G.add_named_vertex (V.mk "green2") (mkZ zero_angle);
val (green3, g4) = g4 |> G.add_named_vertex (V.mk "green3") (mkZ zero_angle);
val (green4, g4) = g4 |> G.add_named_vertex (V.mk "green4") (mkZ zero_angle);
val (green5, g4) = g4 |> G.add_named_vertex (V.mk "green5") (mkZ zero_angle);
val (green6, g4) = g4 |> G.add_named_vertex (V.mk "green6") (mkZ zero_angle);
val (green7, g4) = g4 |> G.add_named_vertex (V.mk "green7") (mkZ zero_angle);
val (green8, g4) = g4 |> G.add_named_vertex (V.mk "green8") (mkZ zero_angle);
val (green9, g4) = g4 |> G.add_named_vertex (V.mk "green9") (mkZ zero_angle);
val (green10, g4) = g4 |> G.add_named_vertex (V.mk "green10") (mkZ zero_angle);

val (H1, g4) = g4 |> G.add_named_vertex (V.mk "H1") mkH;
val (H2, g4) = g4 |> G.add_named_vertex (V.mk "H2") mkH;
val (H3, g4) = g4 |> G.add_named_vertex (V.mk "H3") mkH;

val (e0, g4) = g4 |> G.add_edge undir_edge in1 green2;
val (e0, g4) = g4 |> G.add_edge undir_edge in2 green3;
val (e0, g4) = g4 |> G.add_edge undir_edge green3 green1;
val (e0, g4) = g4 |> G.add_edge undir_edge green2 out1;
val (e0, g4) = g4 |> G.add_edge undir_edge green2 H1;
val (e0, g4) = g4 |> G.add_edge undir_edge H1 green5;
val (e0, g4) = g4 |> G.add_edge undir_edge green5 green4;
val (e0, g4) = g4 |> G.add_edge undir_edge green5 green6;
val (e0, g4) = g4 |> G.add_edge undir_edge green3 H2;
val (e0, g4) = g4 |> G.add_edge undir_edge H2 green6;
val (e0, g4) = g4 |> G.add_edge undir_edge green6 green7;
val (e0, g4) = g4 |> G.add_edge undir_edge green7 green8;
val (e0, g4) = g4 |> G.add_edge undir_edge green7 H3;
val (e0, g4) = g4 |> G.add_edge undir_edge H3 green9;
val (e0, g4) = g4 |> G.add_edge undir_edge green9 green10;
val (e0, g4) = g4 |> G.add_edge undir_edge green9 out2;

(* Plugging *)

in
(* val (g,g2,g3,g4) = (g,g2,g3,g4) *)
val _ = Pretty.writeln (Pretty.str "Tests Passed: test/graph.ML.");
end

(* 
in 
val _ = Pretty.writeln (Pretty.str "Tests passed: red-green graphs.");
end;
*)
(* For debugging...

PolyML.exception_trace (fn () => 
  
) ;

*)
