(* A wrapper for matching bang graphs against concrete graphs
 * see docs/matching_algo for details
 *
 * This implements the "Lazy !-Box Expansion" algorithm
 *
 * This wrapper greedily matches the whole concrete part of the
 * graph, and then picks an arbitrary top-level !-box and tries
 * both expanding it and killing it.
 *)
functor GreedyMatchSearch(
  BBoxOperableGraph : BBOX_OPERABLE_GRAPH
) : BG_MATCH_SEARCH =
struct

  structure Log : LOG = Log(val init_level = 0);

  structure BBoxOperableGraph = BBoxOperableGraph;
  structure G = BBoxOperableGraph.G;
  structure InnerMatchState = OGraphMatchState(G);
  structure MatchState = BangGraphMatchState(
    structure InnerMatchState = InnerMatchState
    structure BBoxOperableGraph = BBoxOperableGraph
  );
  structure InnerMatchSearch = SimpleInnerMatchSearch(InnerMatchState);
  structure Match = BangGraphMatch(BBoxOperableGraph);

  fun log_p pretty_f lvl name =
    Log.logf lvl (fn g => Pretty.string_of
        (Pretty.chunks [Pretty.str (name^":"),(pretty_f g)]))
  val log_graph = log_p G.pretty
  val log_v_nset = log_p V.NSet.pretty

  (* TODO: it is probably worth optimising this some more *)
  fun doomedf ms pv = let
    val pat = InnerMatchState.get_pat ms
    val tgt = InnerMatchState.get_tgt ms
    val tv = VInjEndo.domf (InnerMatchState.get_vmap ms) pv
    val parity = G.get_arity pat pv
    val tarity = G.get_arity tgt tv
    fun has_bbox es = let
      fun other_end_in_bbox e =
        G.is_bboxed pat (G.edge_get_other_vertex pat e pv)
    in
      E.NSet.exists other_end_in_bbox es
    end
    val need_more_ins = Arity.get_in parity < Arity.get_in tarity
    val need_more_outs = Arity.get_out parity < Arity.get_out tarity
    val need_more_undir = Arity.get_undir parity < Arity.get_undir tarity
    fun chk_bbox (inctyp,v,(e,(dirtyp,_))) (nmins,nmouts,nmundir) =
      (* we can ignore selfloops, as the graph is normalised *)
      case (inctyp,dirtyp)
        of (G.SelfLoop,_) => raise ERROR ("Unexpected self-loop in normed graph ("^
                V.string_of_name pv^","^E.string_of_name e^")")
         | (_,G.UnDirected) => (nmins,nmouts,nmundir andalso not (G.is_bboxed pat v))
         | (G.Incoming,G.Directed) => (nmins andalso not (G.is_bboxed pat v),nmouts,nmundir)
         | (G.Outgoing,G.Directed) => (nmins,nmouts andalso not (G.is_bboxed pat v),nmundir)
  in
    (need_more_ins orelse need_more_outs orelse need_more_undir)
    andalso
    (let val (nmins,nmouts,nmundir) =
      G.fold_adj chk_bbox pat pv (need_more_ins,need_more_outs,need_more_undir)
     in nmins orelse nmouts orelse nmundir end)
  end

  fun get_next_bbox g =
    B.NSet.get_exists (not o (G.bbox_has_parent g)) (G.get_bboxes g)

  val is_total = InnerMatchState.is_total o MatchState.get_inner_state;
  val get_current_pat = InnerMatchState.get_pat o MatchState.get_inner_state;
  fun match_bare_wires ms = let
    val ims = MatchState.get_inner_state ms
  in
    Seq.map (fn ims => MatchState.set_inner_state ims ms)
            (InnerMatchSearch.match_bare_wires ims)
  end;
  fun match_pending ms = let
    val ims = MatchState.get_inner_state ms
  in
    Seq.map (fn ims => MatchState.set_inner_state ims ms)
            (InnerMatchSearch.match_pending doomedf ims)
  end;

  fun finish_match ms =
      Seq.filter is_total
        (match_bare_wires ms)

  fun kill_pat_bbox b ms = let
    val _ = Log.logf 2 (fn () => "Killing !-box "^(B.string_of_name b)) ()
  in
    MatchState.kill_pat_bbox b ms
  end
  fun expand_pat_bbox b ms = let
    val _ = Log.logf 2 (fn () => "Expanding !-box "^(B.string_of_name b)) ()
  in
    MatchState.expand_pat_bbox b ms
  end

  fun match_loop ms' = let
    fun kill_and_expand b ms = let
      val do_kill = choose_next_bbox o (kill_pat_bbox b)
      val do_copy = match_loop o MatchState.schedule_new_concrete
                               o (expand_pat_bbox b)
    in
      Seq.append (Seq.maps do_kill (Seq.single ms))
                 (Seq.maps do_copy (Seq.single ms))
    end
    and choose_next_bbox ms =
      case get_next_bbox (get_current_pat ms) of
           NONE => finish_match ms
         | SOME b => kill_and_expand b ms
  in
    Seq.maps choose_next_bbox
      (Seq.map MatchState.kill_impossible_bboxes
        (match_pending ms'))
  end

  fun match_from_ms ms = let
    val ims = MatchState.get_inner_state ms
  in
    Match.mk (MatchState.get_pat ms)
             (InnerMatchState.get_tgt ims)
             (InnerMatchState.get_vmap ims)
             (InnerMatchState.get_emap ims)
             (BInjEndo.empty)
             (InnerMatchState.get_match_subst ims)
             (MatchState.get_wild_bboxes ms)
  end;

  fun find_matches ms = Seq.map match_from_ms (match_loop ms);

  fun match_normalised pat tgt = let
    val _ = log_graph 3 "GreedyMatchSearch: normalised pattern"
                        (BBoxOperableGraph.get_current_graph pat)
    val _ = log_graph 3 "GreedyMatchSearch: normalised target" tgt
    val ms = MatchState.init pat tgt
           |> MatchState.schedule_concrete_pat
           |> MatchState.update_inner_state (InnerMatchState.schedule_all_tgt)
  in
    find_matches ms
  end
  fun match pat tgt =
    match_normalised (BBoxOperableGraph.normalise pat) (G.normalise tgt)

  fun match_subgraph_normalised pat tgt tgt_verts = let
    val _ = log_graph 3 "GreedyMatchSearch: normalised pattern"
                        (BBoxOperableGraph.get_current_graph pat)
    val _ = log_graph 3 "GreedyMatchSearch: normalised target" tgt
    val _ = log_v_nset 4 "GreedyMatchSearch: target vertices" tgt_verts
    val ms = MatchState.init pat tgt
           |> MatchState.schedule_concrete_pat
           |> MatchState.update_inner_state (InnerMatchState.schedule_tgt_subgraph tgt_verts)
  in
    find_matches ms
  end
  fun match_subgraph pat tgt =
    match_subgraph_normalised (BBoxOperableGraph.normalise pat) (G.normalise tgt)

  structure Sharing =
  struct
    structure G = G.SharingOGraph
    structure Match = Match.Sharing
  end
end
