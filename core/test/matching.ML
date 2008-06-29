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
RGGraph.print g1;


val g = RGGraph.empty;
val (b1, g) = g |> RGGraph.add_vertex bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (b2, g) = g |> RGGraph.add_vertex bvert;
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge n1 b2;
val (_,g2) = RGGraph.rename_apart g1 g;

RGGraph.print g1;
RGGraph.print g2;

val [m1,m2] = Seq.list_of (MatchState.match g1 g2 RGVertex.NTab.empty);
map Match.print [m1,m2];


(* For debugging... *)
(*
    RGGraphDotWriter.output_to_file "foo.dot" g;
    PolyML.exception_trace (fn () =>     );
*)




(* debug stuff *)
(* 

raise ERROR "stop";


open MatchState;

val initmatching = Match.empty;
val pat = g1;
RGGraph.print pat;
val tgt = g2;
RGGraph.print tgt;
val initagenda = Vertex.NTab.empty;
val init_st = (init initmatching pat tgt initagenda);
val st = init_st;
print init_st;

val Progress sq = do_next_match_choices st;
val l = Seq.list_of sq;
map print l;

val st = hd l;
print st;

val SOME ((n,tn),st2) = pop_agenda st;
print st2;



raise ERROR "stop";

pop_agenda st;



val SOME Va = (Vertex.NSet.get_local_bot (get_unm_pat_vs st));
val [Vd, Ve, Vf] = Seq.list_of (get_tgt_vnseq st);

val n = Va;
val tn = Ve;

val SOME st2 = match_vertex (n,d) (tn,td) st;

match_2vertices_with_edges 

(* ERROR: only gives two results, should be three! *)
val Progress sq = do_next_match_choices st;
val l = Seq.list_of sq;
map print l;

raise ERROR "stop";

val st = hd l;
val Progress sq = do_next_match_choices st;


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
*)
