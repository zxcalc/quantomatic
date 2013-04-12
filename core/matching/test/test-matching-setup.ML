structure Test_MatchState =
  OGraphMatchState(
    structure G = Test_OGraph
    type data = unit
    val empty_data = ()
  );
structure Test_InnerMatcher =
  SimpleInnerMatchSearch(structure MatchState = Test_MatchState);
structure Test_ConcreteMatcher =
  ConcreteMatchSearch(structure InnerMatchSearch = Test_InnerMatcher);

structure Test_BGMatchState =
  BangGraphMatchState(
    structure G = Test_Bang_Graph
    type data = unit
    val empty_data = ()
  );
structure Test_BGInnerMatcher =
  SimpleInnerMatchSearch(structure MatchState = Test_BGMatchState);
structure Test_GreedyMatcher =
  GreedyMatchSearch(
    structure InnerMatchSearch = Test_BGInnerMatcher
    structure BGMatchState = Test_BGMatchState
  );
(* set to 4 for really detailed messages *)
(*
Test_InnerMatcher.Log.level_ref := 2;
Test_BGInnerMatcher.Log.level_ref := 2;
Test_ConcreteMatcher.Log.level_ref := 2;
Test_GreedyMatcher.Log.level_ref := 2;
*)

functor Test_Match_Tools(
  Matcher : MATCH_SEARCH where type G.EData.data = Test_Graph_Data.edata
                           and type G.OVData.IData.data = Test_Graph_Data.vdata
) =
struct
  structure GTools = Test_OGraph_Tools(Matcher.G);
  open GTools;
  structure M = Matcher;
  structure MS = M.MatchState;

  fun assert_match_correct orig_pat orig_tgt m = let
    val pat = MS.get_pat m
    val tgt = MS.get_tgt m
    val tgt_verts = MS.get_tgt_verts m
    val vmap = MS.get_vmap m
    val vmapf = VInjEndo.domf vmap
    val emap = MS.get_emap m
    val emapf = EInjEndo.domf emap
    val subst = MS.get_match_subst m

    val _ = assert_iso_graph_up_to_wires "Pattern correct" orig_pat pat
    val _ = assert_iso_graph_up_to_wires "Target correct" orig_tgt tgt
    val _ = Testing.assert "T subset of tgt verts"
        (V.NSet.sub_set tgt_verts (G.get_vnames tgt))
    val _ = Testing.assert "vmap total"
        (V.NSet.eq (G.get_vnames pat) (VInjEndo.get_domset vmap))
    val _ = Testing.assert "emap total"
        (E.NSet.eq (G.get_enames pat) (EInjEndo.get_domset emap))
    val _ = Testing.assert "vmap image in tgt"
        (V.NSet.sub_set (VInjEndo.get_codset vmap) (G.get_vnames tgt))
    val _ = Testing.assert "emap image in tgt"
        (E.NSet.sub_set (EInjEndo.get_codset emap) (G.get_enames tgt))
    val _ = Testing.assert "T does not intersect cod vmap"
        (V.NSet.is_empty (V.NSet.intersect tgt_verts
                                           (VInjEndo.get_codset vmap)))

    fun assert_edge_coherent e = let
      val ((dir,_),(s,t)) = G.get_edge pat e
      val e' = emapf e
      val exp_s' = vmapf s
      val exp_t' = vmapf t
      val ((dir',_),(real_s',real_t')) = G.get_edge tgt e'
    in
      case (dir,dir')
        of (G.Directed,G.Directed) =>
            if not (V.name_eq (exp_s',real_s'))
            then raise ERROR ("Edge "^(E.dest e)^"->"^(E.dest e')^
                   ": expected source vertex "^(V.dest s)^" to map to "^
                   (V.dest exp_s')^" instead of "^(V.dest real_s'))
            else if not (V.name_eq (exp_t',real_t'))
            then raise ERROR ("Edge "^(E.dest e)^"->"^(E.dest e')^
                   ": expected target vertex "^(V.dest t)^" to map to "^
                   (V.dest exp_t')^" instead of "^(V.dest real_t'))
            else I
         | (G.UnDirected,G.UnDirected) =>
            if not (V.NSet.eq (V.NSet.of_list [exp_s',exp_t'])
                              (V.NSet.of_list [real_s',real_t']))
            then raise ERROR ("Edge "^(E.dest e)^"->"^(E.dest e')^
                   ": expected end vertices {"^(V.dest s)^","^(V.dest t)^
                   "} to map to {"^(V.dest exp_s')^","^(V.dest exp_t')^
                   "} instead of {"^(V.dest real_s')^","^(V.dest real_t')^"}")
            else I
         | (G.Directed,G.UnDirected) =>
            raise ERROR ("Edge "^(E.dest e)^"->"^(E.dest e')^
                   ": "^(E.dest e)^" is directed, but "^(E.dest e')^
                   " is undirected")
         | (G.UnDirected,G.Directed) =>
            raise ERROR ("Edge "^(E.dest e)^"->"^(E.dest e')^
                   ": "^(E.dest e)^" is undirected, but "^(E.dest e')^
                   " is directed")
    end;
    val _ = Testing.assert "vmap,emap coherent"
        (E.NSet.fold assert_edge_coherent (EInjEndo.get_domset emap) true)

    fun assert_vdata_match v = let
      val v' = vmapf v
      val vdata = G.get_vertex_data pat v
      val tgt_vdata' = G.get_vertex_data tgt v'
      val subst_vdata' = G.Param.subst_in_vertex subst vdata
      val str_data = Pretty.string_of o G.Param.VData.pretty_data
    in
      if G.Param.VData.data_eq (tgt_vdata',subst_vdata')
      then I
      else raise ERROR ("Vertex "^(V.dest v)^
           ": subst("^(str_data vdata)^") = "^(str_data subst_vdata')^
           " but target vertex "^(V.dest v')^" has data "
           ^(str_data tgt_vdata'))
    end;
    val _ = Testing.assert "vertex data subst correct"
        (V.NSet.fold assert_vdata_match (VInjEndo.get_domset vmap) true)

    fun assert_edata_match e = let
      val e' = emapf e
      val edata = G.get_edge_data pat e
      val tgt_edata' = G.get_edge_data tgt e'
      val subst_edata' = G.Param.subst_in_edge subst edata
      val str_data = Pretty.string_of o G.Param.EData.pretty_data
    in
      if G.Param.EData.data_eq (tgt_edata',subst_edata')
      then I
      else raise ERROR ("Edge "^(E.dest e)^
           ": subst("^(str_data edata)^") = "^(str_data subst_edata')^
           " but target vertex "^(E.dest e')^" has data "
           ^(str_data tgt_edata'))
    end;
    val _ = Testing.assert "edge data subst correct"
        (E.NSet.fold assert_edata_match (EInjEndo.get_domset emap) true)
  in m end;

  fun assert_n_match n mseq () = let
    val mlist = Seq.list_of mseq
    val nm = length mlist
  in
    if nm = n then mlist
    else (List.map MS.print mlist;
          raise ERROR ("expected " ^ Int.toString n ^
                       " matching(s), got " ^ Int.toString nm))
  end;

  fun find_n_matches n (pat,tgt) =
    assert_n_match n (M.match pat tgt) ();

  fun find_n_correct_matches n (pat,tgt) =
    map (assert_match_correct pat tgt)
    (assert_n_match n (M.match pat tgt) ());
end;

functor Test_Bang_Match_Tools(
  BGMatcher : BG_MATCH_SEARCH where type G.EData.data = Test_Graph_Data.edata
                                and type G.OVData.IData.data = Test_Graph_Data.vdata
) =
struct
  structure MTools = Test_Match_Tools(BGMatcher);
  open MTools;
  structure Tools = Test_Bang_Graph_Tools(BGMatcher.BG);
  open Tools;
  structure M = BGMatcher;
  structure MS = M.BGMatchState;

  fun assert_n_warnings n ms = let
    val ws = MS.get_warnings ms
  in
    Testing.assert ((Int.toString n)^" warnings") (length ws = n)
  end

  fun assert_match_correct orig_pat orig_tgt m = let
    val full_pat = MS.replay_pat_bbox_ops m orig_pat
  in
    MTools.assert_match_correct full_pat orig_tgt m
  end;

  fun find_n_correct_matches n (pat,tgt) =
    map (assert_match_correct pat tgt)
    (assert_n_match n (M.match pat tgt) ());
end;

