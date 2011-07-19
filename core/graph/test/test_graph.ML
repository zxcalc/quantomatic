
structure Test_OVertex = OVertex(RG_InternVertex);
structure Test_OGraphParam = OGraphParam(
  structure Param = RG_GraphParam 
        and OVertex = Test_OVertex);

structure Test_OVertexComponentData_Param = OVertexComponentData_Param(
  structure VertexDataParam = RG_VertexComponentData_Param
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
  structure VD = RG_VertexData;
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
