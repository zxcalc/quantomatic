(* Provide graph construction context *)
open MkG;

(* bialg *)
val pat = RGGraphLoaderSaver.load_graph "/Users/ldixon/Desktop/bialg.xml";

(* ladder *)
val tgt = RGGraphLoaderSaver.load_graph "/Users/ldixon/Desktop/g.xml";

val (rn,pat) = (G.rename_apart tgt pat);
val _ = G.print pat;
val _ = G.print tgt;

val ms as [m1,m2] = Seq.list_of (MatchSearch.match pat tgt V.NTab.empty);
val _ = Match.print m1;
val _ = map Match.print ms;


(*
(* *)
RGGraph.print pat;
RGGraph.print tgt;

open DB_MatchSearch;

val init_st = (init Match.empty pat tgt V.NTab.empty);
val st = init_st;

(* toplevel *)
val st = init_st;
val Progress xs = (do_next_match_choices st);
val [st1,st2,st3,st4] = Seq.list_of xs;
print st2;
val st = st2;
val Progress xs = (do_next_match_choices st);
val [st1,st2] = Seq.list_of xs;
print st2;
val st = st2;
val Progress xs = (do_next_match_choices st);
val [st1,st2] = Seq.list_of xs;
print st1;
val st = st1;
val Progress xs = (do_next_match_choices st);
val [st1] = Seq.list_of xs;
print st1;
val st = st1;
val Progress xs = (do_next_match_choices st);
val [st1] = Seq.list_of xs;
print st1;
val st = st1;
(* r -> i *)


val Progress xs = (do_next_match_choices st); (* FAILS *)
val [st1] = Seq.list_of xs;
print st1;
val st = st1;
(* n -> h *)
val Progress xs = (do_next_match_choices st);
val [st1] = Seq.list_of xs;
print st1;
val st = st1;

(* *)
val SOME ((pn,tn),st2) = pop_agenda st;
V.string_of_name pn; (* r *)
V.string_of_name tn; (* i *)

      val pat = get_unm_pat st;
      val tgt = get_unm_tgt st;
      val pd = RGGraph.get_vertex_data pat pn;
      val td = RGGraph.get_vertex_data tgt tn;

val SOME st2 = match_vertex (pn,pd) (tn,td) st;
val st = st2;

val (from_pv as (from_pn,from_pd)) = (pn,pd); 
val (from_tv as (from_tn,from_td)) =  (tn,td); 
val (ignore_arity,st) = (0, match_lfvertex pn (tn,td) st);




(* or ...*)
val SOME (UnBBoxed pn) = get_next_pat_vertex st;
val [tn1,tn2,tn3] = Seq.list_of (get_unbboxed_tgt_vnseq st);
val tn = tn3;

(* then ... *)
      val pat = get_unm_pat st;
      val tgt = get_unm_tgt st;
      val pd = RGGraph.get_vertex_data pat pn;
      val td = RGGraph.get_vertex_data tgt tn;

val SOME st2 = match_vertex (pn,pd) (tn,td) st;
val st = st2;

(from_pv as (from_pn,from_pd)) (* pat vertex *)
      (from_tv as (from_tn,from_td)) (* tgt vertex *)
      (ignore_arity,st) = (* state of matching so far *)


val [_,_,(ignore_arity, st)] = Seq.list_of (expand_pat_bboxes (pn,pd) (tn,td) st);

(* *)
val [(ignore_arity, st)] = Seq.list_of (match_self_loops (pn,pd) (tn,td) (ignore_arity, st));
val [(ltes,st)] = Seq.list_of (st |> match_all_pat_self_loops lpes ltes);


val (from_pv as (from_pn,from_pd)) = (pn,pd)
val (from_tv as (from_tn,from_td)) = (tn,td);
val (ignore_arity,st) = (0,st2);


(* into graph ... *)



(* *)
val (Progress s) = do_next_match_choices st;
val l = Seq.list_of s;
val st = hd l;
val (Progress s) = do_next_match_choices st;
val l = Seq.list_of s;

val SOME ((pn,tn),st2) = pop_agenda st;

val st = st2;

val SOME st2 = match_vertex (pn,pd) (tn,td) st;

val st = st2;

val [(ignore_arity, st)] = Seq.list_of (expand_pat_bboxes (pn,pd) (tn,td) st);
val [(ignore_arity, st)] = Seq.list_of (match_self_loops (pn,pd) (tn,td) (ignore_arity, st));

val [(ltes,st)] = Seq.list_of (st |> match_all_pat_self_loops lpes ltes);

val pnpd = (pn,pd);
val tntd = (tn,td);
*)
(*

(* match boundary "i" to 4 tgt bverts *)
(* 
val (Progress s) = do_next_match_choices st;
*)

val NONE = pop_agenda st;
val SOME (UnBBoxed pvn) = get_next_pat_vertex st;

val [tn1,tn2,tn3] = Seq.list_of (get_unbboxed_tgt_vnseq st);
val tn = tn1;

(* match_2vertices_with_edges pvn st *)

val (from_pv as (from_pn,from_pd)) = (pn,pd)
val (from_tv as (from_tn,from_td)) = (tn,td);
val (ignore_arity,st) = (0,st2);

(* 
fun edgematch_adj_verticies 
      (from_pv as (from_pn,from_pd)) 
      (from_tv as (from_tn,from_td)) 
      (ignore_arity,st) = *)

val ((to_tn,tes)::more_tns) = V.NTab.list_of tns; 

(* 
fun strict_edgematch_adj_tgt_vertex 
*)

val st = st2;

(* ***********************************************************************
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
*)
*)
