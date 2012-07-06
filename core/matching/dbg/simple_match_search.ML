(* code for debugging simple match search *)

PolyML.SaveState.loadState "/Users/ldixon/work/IsaP-trunk/quantomatic/core-in-dev/.polysave/matching/filter.ML.save";

(* create initial context *)
  open MkG;
  structure S = DB_SimpleMatchSearch;
  fun match g1 g2 = 
      let val (rn,g2') = G.rename_apart g1 g2;
      in Seq.list_of (S.match no_extra_names g1 g2' V.NTab.empty)
      end;

(***** INSERT GRAPHS HERE ********)
(* X spider, self-loop lhs *)
val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (n1, g) = g |> G.add_vertex (mkX (var_angle "a"));
val (_, g) = g |> G.add_edge undir_edge n1 n1;
val (_, g) = g |> G.add_edge undir_edge b1 n1;
val (bbn, g) = g |> G.add_bbox;
val g = g |> G.move_to_bbox bbn (V.NSet.single b1);
val x_1loop_spider = g;

(* self-loop on X(b) and a dangling edge *)
val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (n1, g) = g |> G.add_vertex (mkX (var_angle "b"));
val (_, g) = g |> G.add_edge undir_edge n1 n1;
val (_, g) = g |> G.add_edge undir_edge b1 n1;
val xVb_loop_and_edge = g;

val pat = x_1loop_spider;
val tgt = xVb_loop_and_edge;
val ms = match pat tgt;
val _ = map Match.print ms;
val [] = ms;
(**********************************)

(* handle the error and look at the exception *)
(*  THE BIT THAT GOES WRONG...
val ms = PolyML.exception_trace (fn () => match pat tgt);
val _ = 
val [m1,m2] = ms;
*)

(**********************************)
(* setup *)
val (rn,tgt) = G.rename_apart pat tgt;
val st0 = S.init (Match.init no_extra_names pat tgt) pat tgt V.NTab.empty;
val st = st0;
S.print st;

open S;

(* top level loop *)
val (S.Progress sts) = S.do_next_match_choices st;

(* do_next_match_choices *)
val SOME (st,sts) = Seq.pull sts;
S.print st;

  (* if nothing on the agenda *)
  val SOME (S.UnBBoxed pn) = S.get_next_pat_vertex st;
  val tns = S.get_unbboxed_tgt_vnseq st;
  val SOME (tn,tns) = Seq.pull tns;
  val sts = Seq.list_of (S.match_2vertices_with_edges pn st tn);

  (* if something on the agenda *)
  val SOME ((pn,tn),st) = S.pop_agenda st;
  val sts = Seq.list_of (S.match_2vertices_with_edges pn st tn);

(* match_2vertices_with_edges *)

      val pat = S.get_unm_pat st;
      val tgt = S.get_unm_tgt st;
      val pd = G.get_vertex_data pat pn;
      val td = G.get_vertex_data tgt tn;
val st1 = st;
val [st] =
      st |> update_match (Match.match_vertex (pn,pd) (tn,td))
         |> expand_adj_pat_bboxes pn tn |> Seq.list_of;

val st =
      st |> update_match (Match.match_vertex (pn,pd) (tn,td))
         |> expand_adj_pat_bboxes pn tn
         |> Seq.maps (edgematch_all_adj (pn,tn))
         (* remove pattern vertex, ignore now redudnent info *)
         |> Seq.map (del_pat_vertex pn);
