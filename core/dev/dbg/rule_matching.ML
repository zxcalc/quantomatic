(*   *)
val rule = TheoryHack.bialgebra1_rule;

Rule.print rule;
RGGraph.print g;

open DB_Rule;

val tgt = (get_lhs rule);
val g = tgt;

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


val init_st = (init Match.empty pat tgt V.NTab.empty);
val st = init_st;
(* match boundary "i" to 4 tgt bverts *)
val (Progress s) = do_next_match_choices st;
val (L as [st1,st2,st3,st4]) = Seq.list_of s;
map print L;
(* first two are ok, second two not *)
val matches = Seq.list_of (Seq.make (match_search (Seq.of_list [st3,st4])));


raise ERROR "stop";

val NONE = pop_agenda st;
val SOME pvn = (V.NSet.get_local_bot (get_unm_pat_vs st));


val SOME ((n,tn),st2) = 

 no_extra_tgt_edges st



 val _ = Pretty.writeln (Pretty.str "Test passed: rule_matching.");

(* end; (* structure *) *)






(* For debugging... *)
(*
    RGGraphDotWriter.output_to_file "foo.dot" g;
    PolyML.exception_trace (fn () =>     );
*)


(* debug stuff *)
(* 

raise ERROR "stop";


open DB_MatchSearch;

val initmatching = Match.empty;
val pat = g1;
G.print pat;
val tgt = g2;
G.print tgt;
val initagenda = V.NTab.empty;
val init_st = (init initmatching pat tgt initagenda);
val st = init_st;
print st;

(* starting point for matching; first vertex *)
val SOME pn = (V.NSet.get_local_bot (get_unm_pat_vs st));
V.print_name pn;

(* possible things it can match *)
val tns = Seq.list_of (get_tgt_vnseq st);
map V.print_name tns;

val Progress sq = do_next_match_choices st;
val l = Seq.list_of sq;
map print l;

val st = hd l;


val [st1,st2,st3,st4] = l;

val st = st1;
val SOME ((pn,tn),st) = pop_agenda st;
V.print_name pn;
V.print_name tn;
print st;

val pnv = (pn,pd);
val tnv = (tn,td);

val (n,V.IVert d) = pnv;
val (tn,V.IVert td) = tnv;

val m = Match.get_mctxt (get_match st);
val V.Xnd a1 = d;
val V.Xnd a2 = td;
LinratAngleExpr.match a1 a2 m;

MCtxt.print m;

LinratAngleExpr.print a1;
LinratAngleExpr.print a2;
LinratAngleExpr.eq (LinratAngleExpr.parse "a") a2;
val (c1,t1) = a1;
val (c2,t2) = a2;
val (a3 as (c3,t3)) = (LinratAngleExpr.parse "a");

X.NTab.list_of t1;
X.NTab.list_of t2;
X.NTab.list_of t3;

(LinratAngleExpr.parse "a")


MCtxt.vertex_idata_match d td (Match.get_mctxt m)

val [tn1,tn2,tn3] = Seq.list_of (get_tgt_vnseq st);

val tn = tn2;
val SOME pn = (V.NSet.get_local_bot (get_unm_pat_vs st));
V.print_name pn;
V.print_name tn;


(* LOOP *)


val SOME ((pn,tn),st) = pop_agenda st2;
V.print_name pn;
V.print_name tn;
print st;

val st = st |> match_lfvertex pn tn;
print st;
val [st2,st3] = Seq.list_of (edgematch_adj_verticies (pn,pd) (tn,td) (pns,tns) (ltes,(plfs,st)));


val SOME st2 = (match_vertex (pn,pd) (tn,td) st)

val [(ltes,(plfs,st))] = (ltes,(plfs,st2))
                            |> match_self_loops pd tn lpes 
                            |> Seq.list_of;

edgematch_adj_verticies (pn,pd) (tn,td) (pns,tns)

val from_pd = pd;
val from_tn = tn;

val SOME ((teh, more_tehs)) = H.NSet.pull_local_bot left_over_tehs;
H.print_name teh;

raise ERROR "stop";

pop_agenda st;



val SOME Va = (V.NSet.get_local_bot (get_unm_pat_vs st));
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

val BVData.IVert d = d;
val BVData.IVert td = td;

val SOME st2 = match_vertex (n,d) (tn,td) st;

val l = Seq.list_of (MatchSearch.match g1 g2 []);

(Seq.list_of (match_2vertices_with_edges pat tgt n st2 tn));


val n = "Va";
val tn = "Vd";

print st;

val l = Seq.list_of (MatchSearch.match g1 g2 []);
*)
