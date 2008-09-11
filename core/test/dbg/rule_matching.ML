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

open DB_MatchState;

val pat = lhs2;
val tgt = g;

RGGraph.print tgt;
RGGraph.print pat;


val init_st = (init Match.empty pat tgt Vertex.NTab.empty);
val st = init_st;
(* match boundary "i" to 4 tgt bverts *)
val (Progress s) = do_next_match_choices st;
val (L as [st1,st2,st3,st4]) = Seq.list_of s;
map print L;
(* first two are ok, second two not *)
val matches = Seq.list_of (Seq.make (match_search (Seq.of_list [st3,st4])));


raise ERROR "stop";

val NONE = pop_agenda st;
val SOME pvn = (Vertex.NSet.get_local_bot (get_unm_pat_vs st));


val SOME ((n,tn),st2) = 

 no_extra_tgt_edges st



 val _ = Pretty.writeln (Pretty.str "Test passed: rule_matching.");

(* end; (* structure *) *)


