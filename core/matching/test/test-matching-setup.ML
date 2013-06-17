structure Test_MatchState =
  OGraphMatchState(
    structure Graph = Test_OGraph
    type data = unit
    val empty_data = ()
  );
structure Test_InnerMatcher =
  SimpleInnerMatchSearch(structure MatchState = Test_MatchState);
structure Test_ConcreteMatcher =
  ConcreteMatchSearch(structure InnerMatchSearch = Test_InnerMatcher);

structure Test_BGMatchState =
  BangGraphMatchState(
    structure Graph = Test_Bang_Graph
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

signature TEST_MATCH_SEARCH = MATCH_SEARCH
    where type Graph.Data.nvdata = Test_Graph_Data.nvdata
      and type Graph.Data.edata  = Test_Graph_Data.edata
      and type Graph.Data.psubst = Test_Graph_Data.psubst
      and type Graph.Data.subst  = Test_Graph_Data.subst

signature TEST_BG_MATCH_SEARCH = BG_MATCH_SEARCH
    where type Graph.Data.nvdata = Test_Graph_Data.nvdata
      and type Graph.Data.edata  = Test_Graph_Data.edata
      and type Graph.Data.psubst = Test_Graph_Data.psubst
      and type Graph.Data.subst  = Test_Graph_Data.subst

functor Test_Match_Tools(
  Matcher : TEST_MATCH_SEARCH
) =
struct
  structure GTools = Test_OGraph_Tools(structure G = Matcher.Graph);
  open GTools;
  structure M = Matcher;
  structure MS = M.MatchState;

  fun assert_match_correct orig_pat orig_tgt m = let
    val pat = MS.get_pat m
    val tgt = MS.get_tgt m
    val full_tgt = MS.get_full_tgt m
    val vmap = MS.get_vmap m
    val vmapf = VVInj.get vmap
    val emap = MS.get_emap m
    val emapf = EEInj.get emap
    val subst = MS.get_match_psubst m

    (*val _ = assert_iso_graph_up_to_wires "Pattern correct" orig_pat pat
    val _ = assert_iso_graph_up_to_wires "Target correct" orig_tgt full_tgt *)
    val _ = Testing.assert "tgt subgraph of full_tgt"
        (* FIXME: check properly; put subgraph test in graph.ML? *)
        (*        should also be full *)
        (V.NSet.sub_set (G.get_vertices tgt) (G.get_vertices full_tgt))
    val _ = Testing.assert "vmap total"
        (V.NSet.eq (G.get_vertices pat) (VVInj.get_dom_set vmap))
    val _ = Testing.assert "emap total"
        (E.NSet.eq (G.get_edges pat) (EEInj.get_dom_set emap))
    val _ = Testing.assert "vmap image in tgt"
        (V.NSet.sub_set (VVInj.get_cod_set vmap) (G.get_vertices tgt))
    val _ = Testing.assert "emap image in tgt"
        (E.NSet.sub_set (EEInj.get_cod_set emap) (G.get_edges tgt))

    fun assert_edge_coherent e = let
      val (dir,(s,t)) =
        (G.get_edge_dir pat e, (G.get_edge_source pat e, G.get_edge_target pat e))
      val e' = emapf e
      val exp_s' = vmapf s
      val exp_t' = vmapf t
      val (dir',(real_s',real_t')) =
        (G.get_edge_dir tgt e', (G.get_edge_source tgt e', G.get_edge_target tgt e'))
    in
      case (dir,dir')
        of (Directed,Directed) =>
            if not (V.name_eq (exp_s',real_s'))
            then raise ERROR ("Edge "^(E.dest e)^"->"^(E.dest e')^
                   ": expected source vertex "^(V.dest s)^" to map to "^
                   (V.dest exp_s')^" instead of "^(V.dest real_s'))
            else if not (V.name_eq (exp_t',real_t'))
            then raise ERROR ("Edge "^(E.dest e)^"->"^(E.dest e')^
                   ": expected target vertex "^(V.dest t)^" to map to "^
                   (V.dest exp_t')^" instead of "^(V.dest real_t'))
            else I
         | (Undirected,Undirected) =>
            if not (V.NSet.eq (V.NSet.of_list [exp_s',exp_t'])
                              (V.NSet.of_list [real_s',real_t']))
            then raise ERROR ("Edge "^(E.dest e)^"->"^(E.dest e')^
                   ": expected end vertices {"^(V.dest s)^","^(V.dest t)^
                   "} to map to {"^(V.dest exp_s')^","^(V.dest exp_t')^
                   "} instead of {"^(V.dest real_s')^","^(V.dest real_t')^"}")
            else I
         | (Directed,Undirected) =>
            raise ERROR ("Edge "^(E.dest e)^"->"^(E.dest e')^
                   ": "^(E.dest e)^" is directed, but "^(E.dest e')^
                   " is undirected")
         | (Undirected,Directed) =>
            raise ERROR ("Edge "^(E.dest e)^"->"^(E.dest e')^
                   ": "^(E.dest e)^" is undirected, but "^(E.dest e')^
                   " is directed")
    end;
    val _ = Testing.assert "vmap,emap coherent"
        (E.NSet.fold assert_edge_coherent (EEInj.get_dom_set emap) true)

    fun assert_vdata_match v = let
      val v' = vmapf v
      val vdata = G.get_vertex_data pat v
      val tgt_vdata' = G.get_vertex_data tgt v'
      val subst_vdata' =
        case vdata of G.NVert nvdata => G.NVert (G.Data.subst_in_nvdata subst nvdata)
                    | G.WVert => G.WVert
      val str_data = Pretty.string_of o G.pretty_vdata
    in
      if G.vdata_eq (tgt_vdata',subst_vdata')
      then I
      else raise ERROR ("Vertex "^(V.dest v)^
           ": subst("^(str_data vdata)^") = "^(str_data subst_vdata')^
           " but target vertex "^(V.dest v')^" has data "
           ^(str_data tgt_vdata'))
    end;
    val _ = Testing.assert "vertex data subst correct"
        (V.NSet.fold assert_vdata_match (VVInj.get_dom_set vmap) true)

    fun assert_edata_match e = let
      val e' = emapf e
      val edata = G.get_edge_data pat e
      val tgt_edata' = G.get_edge_data tgt e'
      val subst_edata' = G.Data.subst_in_edata subst edata
      val str_data = Pretty.string_of o G.pretty_edata
    in
      if G.edata_eq (tgt_edata',subst_edata')
      then I
      else raise ERROR ("Edge "^(E.dest e)^
           ": subst("^(str_data edata)^") = "^(str_data subst_edata')^
           " but target vertex "^(E.dest e')^" has data "
           ^(str_data tgt_edata'))
    end;
    val _ = Testing.assert "edge data subst correct"
        (E.NSet.fold assert_edata_match (EEInj.get_dom_set emap) true)
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
  BGMatcher : TEST_BG_MATCH_SEARCH
) =
struct
  structure MTools = Test_Match_Tools(BGMatcher);
  open MTools;
  structure Tools = Test_Bang_Graph_Tools(structure BG = BGMatcher.BG);
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

