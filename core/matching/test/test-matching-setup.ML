structure Test_ConcreteMatcher =
  ConcreteMatchSearch(Test_OGraph);

structure Test_GreedyMatcher =
  GreedyMatchSearch(Test_Bang_Graph);
(* set to 4 for really detailed messages *)
(*
Test_ConcreteMatcher.Log.level_ref := 2;
Test_GreedyMatcher.Log.level_ref := 2;
*)

signature TEST_MATCH_SEARCH = MATCH_SEARCH
    where type Graph.nvdata = Test_Graph_Data.nvdata
      and type Graph.edata  = Test_Graph_Data.edata
      and type Graph.psubst = Test_Graph_Data.psubst
      and type Graph.subst  = Test_Graph_Data.subst

signature TEST_BG_MATCH_SEARCH = BG_MATCH_SEARCH
    where type Graph.nvdata = Test_Graph_Data.nvdata
      and type Graph.edata  = Test_Graph_Data.edata
      and type Graph.psubst = Test_Graph_Data.psubst
      and type Graph.subst  = Test_Graph_Data.subst
      and type Match.Graph.T = Test_Bang_Graph.T


signature MATCH_TESTER =
sig
  structure G : TEST_OGRAPH
  type match
  val find_n_matches : int -> (G.T * G.T)
                    -> match list;
  val find_n_correct_matches : int -> (G.T * G.T)
                            -> match list;
end;

functor Test_Match_Tools(
  Matcher : TEST_MATCH_SEARCH
) =
struct

  structure M = Matcher;
  structure G = M.Graph
  structure GTools = Test_OGraph_Tools(G);
  open GTools;
  structure Match = M.Match
  type match = Match.T

  (* TODO: fix with iso implementation *)
  fun assert_match_correct orig_pat orig_tgt tgt_verts m = let
    val pat = Match.get_pat m
    val tgt = Match.get_tgt m
    val vmap = Match.get_vmap m

    (*val _ = assert_iso_graph_up_to_wires "Pattern correct" orig_pat pat
    val _ = assert_iso_graph_up_to_wires "Target correct" orig_tgt tgt*)
    (* FIXME: normalisation
    val _ = Testing.assert "T subset of tgt verts"
        (V.NSet.sub_set tgt_verts (G.get_vertices tgt))
    val _ = Testing.assert "T does not intersect cod vmap"
        (V.NSet.is_empty (V.NSet.intersect tgt_verts
                                           (VVInj.get_cod_set vmap)))
    *)

  in m end;

  fun assert_n_match n mseq () = let
    val mlist = Seq.list_of mseq
    val nm = length mlist
  in
    if nm = n then mlist
    else (List.map Match.print mlist;
          raise ERROR ("expected " ^ Int.toString n ^
                       " matching(s), got " ^ Int.toString nm))
  end;

  fun find_n_matches n (pat,tgt) =
    assert_n_match n (M.match pat tgt) ();

  fun find_n_correct_matches n (pat,tgt) =
    map (assert_match_correct pat tgt (G.get_vertices tgt))
    (assert_n_match n (M.match pat tgt) ());
end;

functor Test_Bang_Match_Tools(
  BGMatcher : TEST_BG_MATCH_SEARCH
) =
struct
  structure BGTools = Test_Bang_Graph_Tools(BGMatcher.Graph);
  open BGTools;
  structure M = BGMatcher;
  structure Match = M.Match;
  type match = Match.T
  structure G = Match.Graph

  (* TODO: re-introduce iso checks *)
  fun assert_match_correct orig_pat orig_tgt tgt_verts m = let
    val init_pat = Match.get_init_pat m
    val pat = Match.get_pat m
    val replayed_pat = Test_Bang_Graph.replay_bbox_ops (Match.get_bbox_ops m) init_pat
    val tgt = Match.get_tgt m
    val vmap = Match.get_vmap m

    (*val _ = assert_iso_graph_up_to_wires "Pattern correct" full_pat pat
    val _ = assert_iso_graph_up_to_wires "Target correct" orig_tgt tgt*)
    (* FIXME: normalisation
    val _ = Testing.assert "T subset of tgt verts"
        (V.NSet.sub_set tgt_verts (G.get_vertices tgt))
    val _ = Testing.assert "T does not intersect cod vmap"
        (V.NSet.is_empty (V.NSet.intersect tgt_verts
                                           (VVInj.get_cod_set vmap)))
    *)
  in m end;

  fun assert_n_match n mseq () = let
    val mlist = Seq.list_of mseq
    val nm = length mlist
  in
    if nm = n then mlist
    else (List.map Match.print mlist;
          raise ERROR ("expected " ^ Int.toString n ^
                       " matching(s), got " ^ Int.toString nm))
  end;

  fun assert_n_wild_bboxes n ms = let
    val ws = Match.get_wild_bboxes ms
  in
    Testing.assert ((Int.toString n)^" wild !-boxes") (B.NSet.cardinality ws = n)
  end

  fun find_n_matches n (pat,tgt) = assert_n_match n (M.match pat tgt) ()

  fun find_n_correct_matches n (pat,tgt) =
    map (assert_match_correct pat tgt (G.get_vertices tgt))
    (assert_n_match n (M.match pat tgt) ())
end;
