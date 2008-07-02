local 

structure V = RGGraph.BVertex;
structure E = RGGraph.Edge;

val bvert = V.BVert ();
val zero_angle = AngleExpr.zero;
val pi_angle = AngleExpr.pi;
fun mkZ a = V.IVert (RGVertex.Znd a);
fun mkX a = V.IVert (RGVertex.Xnd a);
val mkH = V.IVert RGVertex.Hnd;
val edge = ();

(* Pattern: (B - G - B) *)
val g = RGGraph.empty;
val (b1, g) = g |> RGGraph.add_vertex bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (b2, g) = g |> RGGraph.add_vertex bvert;
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge n1 b2;
val g1 = g;
val _ = RGGraph.print g1;

val g = RGGraph.empty;
val (b1, g) = g |> RGGraph.add_vertex bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (b2, g) = g |> RGGraph.add_vertex bvert;
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge n1 b2;
val (_,g2) = RGGraph.rename_apart g1 g;

val _ = RGGraph.print g1;
val _ = RGGraph.print g2;

val [m1,m2] = Seq.list_of (MatchState.match g1 g2 RGVertex.NTab.empty);
val _ = map Match.print [m1,m2];

in
val _ = Pretty.writeln (Pretty.str "Tests passed: rewriting with rules.")
end;


(* For debugging... *)
(*
    RGGraphDotWriter.output_to_file "foo.dot" g;
    PolyML.exception_trace (fn () =>     );
*)
