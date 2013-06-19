structure Test_ConcreteMatcher =
  ConcreteMatchSearch(Test_OGraph);

structure Test_BBoxOperableGraph =
  ReplayableBBoxOperableGraph(Test_Bang_Graph);

structure Test_GreedyMatcher =
  GreedyMatchSearch(Test_BBoxOperableGraph);
(* set to 4 for really detailed messages *)
(*
Test_ConcreteMatcher.Log.level_ref := 2;
Test_GreedyMatcher.Log.level_ref := 2;
*)


signature MATCH_TESTER =
sig
  structure G : OGRAPH
      where type EData.data = Test_Graph_Data.edata
        and type OVData.IData.data = Test_Graph_Data.vdata
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
  structure GTools = Test_OGraph_Tools(M.G);
  open GTools;
  structure Match = M.Match
  type match = Match.T
  structure G = Match.G

  fun assert_match_correct orig_pat orig_tgt tgt_verts m = let
    val pat = Match.get_pat m
    val tgt = Match.get_tgt m
    val vmap = Match.get_vmap m

    val _ = assert_iso_graph_up_to_wires "Pattern correct" orig_pat pat
    val _ = assert_iso_graph_up_to_wires "Target correct" orig_tgt tgt
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
  BGMatcher : BG_MATCH_SEARCH
    where type G.EData.data = Test_Graph_Data.edata
      and type G.OVData.IData.data = Test_Graph_Data.vdata
      and type Match.BBoxOperableGraph.T = Test_BBoxOperableGraph.T
      and type Match.BBoxOperableGraph.G.T = Test_BBoxOperableGraph.G.T
) =
struct
  structure BGTools = Test_Bang_Graph_Tools(BGMatcher.G);
  open BGTools;
  structure M = BGMatcher;
  structure Match = M.Match;
  type match = Match.T
  structure G = Match.G

  fun assert_match_correct orig_pat orig_tgt tgt_verts m = let
    val bbox_oper = Match.get_pat m
    val full_pat = Test_BBoxOperableGraph.replay_bbox_ops bbox_oper orig_pat
    val pat = Test_BBoxOperableGraph.get_current_graph bbox_oper
    val tgt = Match.get_tgt m
    val vmap = Match.get_vmap m

    val _ = assert_iso_graph_up_to_wires "Pattern correct" full_pat pat
    val _ = assert_iso_graph_up_to_wires "Target correct" orig_tgt tgt
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

  fun find_n_matches n (pat,tgt) = let
    val oper = Test_BBoxOperableGraph.init pat V.NSet.empty V.NSet.empty
  in
    assert_n_match n (M.match oper tgt) ()
  end;

  fun find_n_correct_matches n (pat,tgt) = let
    val oper = Test_BBoxOperableGraph.init pat V.NSet.empty V.NSet.empty
  in
    map (assert_match_correct pat tgt (G.get_vertices tgt))
    (assert_n_match n (M.match oper tgt) ())
  end;
end;
