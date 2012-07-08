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
) : OUTER_BG_MATCH_SEARCH =
struct
  structure InnerMatchSearch = InnerMatchSearch
  structure BGMatchState = BGMatchState
  structure BG = BGMatchState.BG
  structure MatchState = InnerMatchSearch.MatchState
  structure G = InnerMatchSearch.G

  (* FIXME *)
  fun doomedf _ _ = false

  fun get_next_bbox g = B.NSet.get_local_bot (BG.get_bboxes g)

  fun finish_match ms =
      Seq.filter BGMatchState.is_total
        (InnerMatchSearch.match_bare_wires ms)

  fun match_loop ms' = let
    fun kill_and_expand b ms = let
      val do_kill = choose_next_bbox o (BGMatchState.kill_pat_bbox b)
      val do_copy = match_loop o BGMatchState.schedule_new_concrete
                               o (BGMatchState.expand_pat_bbox b)
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

  (* We just put everything in the match state, and for each match
   * provided by the inner loop, we match the bare wires, and discard any
   * incomplete matches (as we have nothing left to add to the pattern) *)
  fun match_avoiding_names names pat tgt = let
    val pat = G.normalise pat
    val tgt = G.normalise tgt
    val ms = BGMatchState.init_and_schedule_concrete pat tgt |>
    BGMatchState.set_names names
  in
    match_loop ms
  end

  val match = match_avoiding_names (V.NSet.empty, E.NSet.empty)

  structure Sharing =
  struct
    structure G = G.SharingOGraph
    structure InnerMatchSearch = InnerMatchSearch.Sharing
    structure MatchState = MatchState.Sharing
  end
end
