structure V = RGGraph.BVertex;
val bvert = V.BVert ();
val zero_angle = AngleExpr.zero;
val pi_angle = AngleExpr.pi;
fun mkZ a = V.IVert (RGVertex.Znd a);
fun mkX a = V.IVert (RGVertex.Xnd a);
val mkH = V.IVert RGVertex.Hnd;
val edge = ();
structure E = RGGraph.Edge;

(* Pattern: (B - G - B) *)
val g = RGGraph.empty;
val (b1, g) = g |> RGGraph.add_vertex bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (b2, g) = g |> RGGraph.add_vertex bvert;
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge n1 b2;
RGGraph.print g;

(* For debugging...

RGGraphDotWriter.output_to_file "foo.dot" g;


PolyML.exception_trace (fn () => 
  
);

*)
