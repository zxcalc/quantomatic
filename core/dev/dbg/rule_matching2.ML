structure V = RGGraph.BVertex;
structure E = RGGraph.Edge;
val bvert = V.BVert ();
val zero_angle = LinratExpr.zero;
val var_angle = LinratExpr.mk_var;
val add_angles = LinratExpr.add_expr;
val pi_angle = LinratExpr.pi;
fun mkZ a = V.IVert (RGVertex.Znd a);
fun mkX a = V.IVert (RGVertex.Xnd a);
val mkH = V.IVert RGVertex.Hnd;
val edge = ();

(* X isometry + stuff should still be matched by isometry  *)
val g = RGGraph.empty;
val (b1, g) = g |> RGGraph.add_vertex bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n3, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n4, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge n1 n2;
val (e2, g) = g |> RGGraph.add_edge edge n1 n2;
val (e3, g) = g |> RGGraph.add_edge edge n2 n3;
val (e4, g) = g |> RGGraph.add_edge edge n3 n4;
val _ = RGGraph.print g;
val L = TheoryHack.apply_all_rules g

open DB_Rule;

val rule = TheoryHack.x_isometry_rule;
val pat = (get_lhs rule);
val tgt = g;

val (rn,lhs2) = RGGraph.rename_apart g (get_lhs rule);
val (rn2, rhs2) = RGGraph.rename rn (get_rhs rule)

val rule = rule |> set_lhs lhs2
                |> set_rhs rhs2;

Rule.print rule;
RGGraph.print g;
RGGraph.print lhs2;

open DB_MatchSearch;

val pat = lhs2;
val tgt = g;

RGGraph.print tgt;
RGGraph.print pat;

raise ERROR "stop";

val init_st = (init Match.empty pat tgt Vertex.NTab.empty);
val st = init_st;
MatchSearch.print st;
(* match boundary "i" to 4 tgt bverts *)
val (Progress s) = MatchSearch.do_next_match_choices st;
val (L as [st1,st2]) = Seq.list_of s;
map MatchSearch.print L;
(* first two are ok, second two not *)
val matches = Seq.list_of (Seq.make (match_search (Seq.of_list [st3,st4])));

