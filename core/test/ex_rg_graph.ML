structure V = RGGraph.BVertex;
val bvert = V.BVert ();
val zero_angle = AngleExpr.zero;
val pi_angle = AngleExpr.pi;
fun mkZ a = V.IVert (RGVertex.Znd a);
fun mkX a = V.IVert (RGVertex.Xnd a);
val mkH = V.IVert RGVertex.Hnd;
val edge = ();
structure E = RGGraph.Edge;

(* a simple graph, and some basic manipulations *)
val g = RGGraph.empty;
val (n_a, g) = g |> RGGraph.add_vertex bvert;
val (n_b, g) = g |> RGGraph.add_vertex bvert;
val (e0, g) = g |> RGGraph.add_edge edge n_a n_b;
val (e1, g) = g |> RGGraph.add_edge edge n_a n_b;
val n_c = "n_c";
val g = g |> RGGraph.rename_vname n_a n_c;
val e3 = "e_c";
val g = g |> RGGraph.rename_ename e0 e3;


RGGraph.print g;

(* 3 CNOTs = swap example *)
val g2 = RGGraph.empty;
val (in1, g2)  = g2 |> RGGraph.add_named_vertex "in1" bvert;
val (in2, g2)  = g2 |> RGGraph.add_named_vertex "in2" bvert;  
val (out1, g2)  = g2 |> RGGraph.add_named_vertex "out1" bvert;
val (out2, g2)  = g2 |> RGGraph.add_named_vertex "out2" bvert;

val (green1, g2) = g2 |> RGGraph.add_named_vertex "green1" (mkZ zero_angle);
val (green2, g2) = g2 |> RGGraph.add_named_vertex "green2" (mkZ zero_angle);
val (green3, g2) = g2 |> RGGraph.add_named_vertex "green3" (mkZ zero_angle);

val (red1, g2) = g2 |> RGGraph.add_named_vertex "red1" (mkX zero_angle);
val (red2, g2) = g2 |> RGGraph.add_named_vertex "red2" (mkX zero_angle);
val (red3, g2) = g2 |> RGGraph.add_named_vertex "red3" (mkX zero_angle);

val (e0, g2) = g2 |> RGGraph.add_edge edge in1 green1;
val (e0, g2) = g2 |> RGGraph.add_edge edge in2 red1;
val (e0, g2) = g2 |> RGGraph.add_edge edge red1 green1;
val (e0, g2) = g2 |> RGGraph.add_edge edge red1 green2;
val (e0, g2) = g2 |> RGGraph.add_edge edge red2 green1;
val (e0, g2) = g2 |> RGGraph.add_edge edge red2 green2;
val (e0, g2) = g2 |> RGGraph.add_edge edge red3 green2;
val (e0, g2) = g2 |> RGGraph.add_edge edge red2 green3;
val (e0, g2) = g2 |> RGGraph.add_edge edge red3 green3;
val (e0, g2) = g2 |> RGGraph.add_edge edge out1 green3;
val (e0, g2) = g2 |> RGGraph.add_edge edge out2 red3;

(* just a swap *)

val g3 = RGGraph.empty;
val (in1, g3)  = g3 |> RGGraph.add_named_vertex "in1" bvert;
val (in2, g3)  = g3 |> RGGraph.add_named_vertex "in2" bvert;

val (out1, g3)  = g3 |> RGGraph.add_named_vertex "out1"  
					       bvert;
val (out2, g3)  = g3 |> RGGraph.add_named_vertex "out2"  
					       bvert;
val (e0, g3) = g3 |> RGGraph.add_edge edge in1 out2;
val (e0, g3) = g3 |> RGGraph.add_edge edge in2 out1;


(*  A MC pattern for a CNOT *)
val g4 = RGGraph.empty;
val (in1, g4)  = g4 |> RGGraph.add_named_vertex "in1" bvert;
val (in2, g4)  = g4 |> RGGraph.add_named_vertex "in2" bvert;

val (out1, g4)  = g4 |> RGGraph.add_named_vertex "out1" bvert;
val (out2, g4)  = g4 |> RGGraph.add_named_vertex "out2" bvert;

val (green1, g4) = g4 |> RGGraph.add_named_vertex "green1" (mkZ zero_angle);
val (green2, g4) = g4 |> RGGraph.add_named_vertex "green2" (mkZ zero_angle);
val (green3, g4) = g4 |> RGGraph.add_named_vertex "green3" (mkZ zero_angle);
val (green4, g4) = g4 |> RGGraph.add_named_vertex "green4" (mkZ zero_angle);
val (green5, g4) = g4 |> RGGraph.add_named_vertex "green5" (mkZ zero_angle);
val (green6, g4) = g4 |> RGGraph.add_named_vertex "green6" (mkZ zero_angle);
val (green7, g4) = g4 |> RGGraph.add_named_vertex "green7" (mkZ zero_angle);
val (green8, g4) = g4 |> RGGraph.add_named_vertex "green8" (mkZ zero_angle);
val (green9, g4) = g4 |> RGGraph.add_named_vertex "green9" (mkZ zero_angle);
val (green10, g4) = g4 |> RGGraph.add_named_vertex "green10" (mkZ zero_angle);

val (H1, g4) = g4 |> RGGraph.add_named_vertex "H1" mkH;
val (H2, g4) = g4 |> RGGraph.add_named_vertex "H2" mkH;
val (H3, g4) = g4 |> RGGraph.add_named_vertex "H3" mkH;

val (e0, g4) = g4 |> RGGraph.add_edge edge in1 green2;
val (e0, g4) = g4 |> RGGraph.add_edge edge in2 green3;
val (e0, g4) = g4 |> RGGraph.add_edge edge green3 green1;
val (e0, g4) = g4 |> RGGraph.add_edge edge green2 out1;
val (e0, g4) = g4 |> RGGraph.add_edge edge green2 H1;
val (e0, g4) = g4 |> RGGraph.add_edge edge H1 green5;
val (e0, g4) = g4 |> RGGraph.add_edge edge green5 green4;
val (e0, g4) = g4 |> RGGraph.add_edge edge green5 green6;
val (e0, g4) = g4 |> RGGraph.add_edge edge green3 H2;
val (e0, g4) = g4 |> RGGraph.add_edge edge H2 green6;
val (e0, g4) = g4 |> RGGraph.add_edge edge green6 green7;
val (e0, g4) = g4 |> RGGraph.add_edge edge green7 green8;
val (e0, g4) = g4 |> RGGraph.add_edge edge green7 H3;
val (e0, g4) = g4 |> RGGraph.add_edge edge H3 green9;
val (e0, g4) = g4 |> RGGraph.add_edge edge green9 green10;
val (e0, g4) = g4 |> RGGraph.add_edge edge green9 out2;


(* For debugging...

PolyML.exception_trace (fn () => 
  
) ;

*)
