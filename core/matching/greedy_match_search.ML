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
    structure InnerMatchSearch : INNER_MATCH_SEARCH
    (* This arg is required to make typing work sanely. *)
    (* It must be the same structure as the one in InnerMatchSearch *)
    structure BGMatchState : BANG_GRAPH_MATCH_STATE
    sharing InnerMatchSearch.MatchState.Sharing = BGMatchState.Sharing
) : BG_MATCH_SEARCH =
struct

  structure Log : LOG = Log(val init_level = 5);

  structure BGMatchState = BGMatchState
  structure BG = BGMatchState.BG
  structure MatchState = InnerMatchSearch.MatchState
  structure G = InnerMatchSearch.G

  fun log_p pretty_f lvl name =
    Log.logf lvl (fn g => Pretty.string_of
        (Pretty.chunks [Pretty.str (name^":"),(pretty_f g)]))
  val log_graph = log_p G.pretty
  val log_v_nset = log_p V.NSet.pretty

  (* TODO: it is probably worth optimising this some more *)
  fun doomedf ms pv = let
    val pat = BGMatchState.get_pat ms
    val tgt = BGMatchState.get_tgt ms
    val tv = VInjEndo.domf (BGMatchState.get_vmap ms) pv
    val parity = BG.get_arity pat pv
    val tarity = BG.get_arity tgt tv
    fun has_bbox es = let
      fun other_end_in_bbox e =
        BG.is_bboxed pat (BG.edge_get_other_vertex pat e pv)
    in
      E.NSet.exists other_end_in_bbox es
    end
    val need_more_ins = Arity.get_in parity < Arity.get_in tarity
    val need_more_outs = Arity.get_out parity < Arity.get_out tarity
    val need_more_undir = Arity.get_undir parity < Arity.get_undir tarity
    fun chk_bbox (inctyp,v,(e,(dirtyp,_))) (nmins,nmouts,nmundir) =
      (* we can ignore selfloops, as the graph is normalised *)
      case (inctyp,dirtyp)
        of (BG.SelfLoop,_) => raise ERROR ("Unexpected self-loop in normed graph ("^
                V.string_of_name pv^","^E.string_of_name e^")")
         | (_,BG.UnDirected) => (nmins,nmouts,nmundir andalso not (BG.is_bboxed pat v))
         | (BG.Incoming,BG.Directed) => (nmins andalso not (BG.is_bboxed pat v),nmouts,nmundir)
         | (BG.Outgoing,BG.Directed) => (nmins,nmouts andalso not (BG.is_bboxed pat v),nmundir)
  in
    (need_more_ins orelse need_more_outs orelse need_more_undir)
    andalso
    (let val (nmins,nmouts,nmundir) =
      BG.fold_adj chk_bbox pat pv (need_more_ins,need_more_outs,need_more_undir)
     in nmins orelse nmouts orelse nmundir end)
  end

  fun get_next_bbox g =
    B.NSet.get_exists (not o (BG.bbox_has_parent g)) (BG.get_bboxes g)

  fun finish_match ms =
      Seq.filter BGMatchState.is_total
        (InnerMatchSearch.match_bare_wires ms)

  fun kill_pat_bbox b ms = let
    val _ = Log.logf 2 (fn () => "Killing !-box "^(B.string_of_name b)) ()
  in
    BGMatchState.kill_pat_bbox b ms
  end
  fun expand_pat_bbox b ms = let
    val _ = Log.logf 2 (fn () => "Expanding !-box "^(B.string_of_name b)) ()
  in
    BGMatchState.expand_pat_bbox b ms
  end

  fun match_loop ms' = let
    fun kill_and_expand b ms = let
      val do_kill = choose_next_bbox o (kill_pat_bbox b)
      val do_copy = match_loop o BGMatchState.schedule_new_concrete
                               o (expand_pat_bbox b)
    in
      Seq.append (Seq.maps do_kill (Seq.single ms))
                 (Seq.maps do_copy (Seq.single ms))
    end
    and choose_next_bbox ms =
      case get_next_bbox (BGMatchState.get_pat ms) of
           NONE => finish_match ms
         | SOME b => kill_and_expand b ms
  in
    Seq.maps choose_next_bbox
    (Seq.map BGMatchState.kill_impossible_bboxes
        (InnerMatchSearch.match_pending doomedf ms'))
  end

  fun match_normalised_avoiding_tracking (avoid,track) pat tgt = let
    val _ = log_graph 3 "GreedyMatchSearch: normalised pattern" pat
    val _ = log_graph 3 "GreedyMatchSearch: normalised target" tgt
    val _ = log_v_nset 3 "GreedyMatchSearch: avoids" avoid
    val _ = log_v_nset 3 "GreedyMatchSearch: tracked" track
    val ms = BGMatchState.init_and_schedule_concrete pat tgt
              |> BGMatchState.set_pat_avoids avoid
              |> BGMatchState.set_pat_tracked track
  in
    match_loop ms
  end
  fun match_avoiding_tracking at pat tgt = let
    val pat' = G.normalise pat
    val tgt' = G.normalise tgt
  in match_normalised_avoiding_tracking at pat' tgt' end

  fun match_subgraph_normalised_avoiding_tracking (avoid,track) pat tgt tgt_verts = let
    val pat = G.normalise pat
    val tgt = G.normalise tgt
    val _ = log_graph 3 "GreedyMatchSearch: normalised pattern" pat
    val _ = log_graph 3 "GreedyMatchSearch: normalised target" tgt
    val _ = log_v_nset 3 "GreedyMatchSearch: avoids" avoid
    val _ = log_v_nset 3 "GreedyMatchSearch: tracked" track
    val _ = log_v_nset 4 "GreedyMatchSearch: target vertices" tgt_verts
    val ms = BGMatchState.init_subgraph_and_schedule_concrete pat tgt tgt_verts
              |> BGMatchState.set_pat_avoids avoid
              |> BGMatchState.set_pat_tracked track
  in
    match_loop ms
  end
  fun match_subgraph_avoiding_tracking at pat tgt = let
    val pat' = G.normalise pat
    val tgt' = G.normalise tgt
  in match_subgraph_normalised_avoiding_tracking at pat' tgt' end

  val match = match_avoiding_tracking (V.NSet.empty, V.NSet.empty)
  val match_subgraph = match_subgraph_avoiding_tracking (V.NSet.empty, V.NSet.empty)
  val match_normalised = match_normalised_avoiding_tracking (V.NSet.empty, V.NSet.empty)
  val match_subgraph_normalised = match_subgraph_normalised_avoiding_tracking (V.NSet.empty, V.NSet.empty)

  structure Sharing =
  struct
    structure G = G.SharingOGraph
    structure MatchState = MatchState.Sharing
  end
end
