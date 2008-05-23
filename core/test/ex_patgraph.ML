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
val g1 = g;
RGGraph.print g1;


val g = RGGraph.empty;
val (b1, g) = g |> RGGraph.add_vertex bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (b2, g) = g |> RGGraph.add_vertex bvert;
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge n1 b2;
val g2 = RGGraph.rename_apart g1 g;

RGGraph.print g2;
RGGraph.print g1;



open MatchState;

val initmatching = Match.empty;
val pat = g1;
val tgt = g2;
val initagenda = [];
val init_st = (init initmatching pat tgt initagenda);
val st = init_st;

val Progress l = do_next_match_choices pat tgt st;
map print l;

raise ERROR "stop";

val st = hd l;
val Progress l = do_next_match_choices pat tgt st;


val SOME ((n,tn),st2) = pop_agenda st;
print st2;
val st = st2;

val (d,edgens) = Graph.get_vertex pat n;
val (td,tedgens) = Graph.get_vertex tgt tn;

val BVertex.IVert d = d;
val BVertex.IVert td = td;

val SOME st2 = match_vertex (n,d) (tn,td) st;

val l = Seq.list_of (MatchState.match g1 g2 []);

(Seq.list_of (match_2vertices_with_edges pat tgt n st2 tn));


val n = "Va";
val tn = "Vd";

print st;

val l = Seq.list_of (MatchState.match g1 g2 []);



(* For debugging...

RGGraphDotWriter.output_to_file "foo.dot" g;


PolyML.exception_trace (fn () => 
  
);

*)
