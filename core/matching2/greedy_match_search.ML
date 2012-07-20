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

  structure Log : LOG = Log(val init_level = 0);

  structure BGMatchState = BGMatchState
  structure BG = BGMatchState.BG
  structure MatchState = InnerMatchSearch.MatchState
  structure G = InnerMatchSearch.G

  fun log_p pretty_f lvl name =
    Log.logf lvl (fn g => Pretty.string_of
        (Pretty.chunks [Pretty.str (name^":"),(pretty_f g)]))
  val log_graph = log_p G.pretty
  val log_v_nset = log_p V.NSet.pretty

  (* FIXME *)
  fun doomedf _ _ = false

  fun get_next_bbox g = B.NSet.get_local_bot (BG.get_bboxes g)

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
