(* A wrapper for matching concrete graphs against concrete graphs *)
(* see docs/matching_algo for details *)
(* This wrapper is very simple, as the InnerMatchSearch does all the work *)
functor MJB_ConcreteMatchSearch(
    structure InnerMatchSearch : INNER_MATCH_SEARCH
) : MATCH_SEARCH =
struct
  structure Log : LOG = Log(val init_level = 0);

  structure MatchState = InnerMatchSearch.MatchState
  structure G = InnerMatchSearch.G
  
  (*For ease of use*)
  structure Future = MJB_Simple_Future;
  structure Par_Seq = MJB_Safe_Parallel_Seq;
  
  (* We can't provide any information that the inner match search
   * does not already know *)
  fun concrete_doomedf _ _ = false

  (* We just put everything in the match state, and for each match
   * provided by the inner loop, we match the bare wires, and discard any
   * incomplete matches (as we have nothing left to add to the pattern) *)
  fun do_match ms =
    let
      val data = Future.new_future_data (SOME "<-Concrete wrapper")
      val sq = InnerMatchSearch.match_pending concrete_doomedf ms in
    Par_Seq.sequential_seq_of (
    Par_Seq.filter (fn _ => fn m => MatchState.is_total m)
       (Par_Seq.maps (fn fd => fn m =>
        Par_Seq.of_sequential_seq fd (InnerMatchSearch.match_bare_wires m))
        (Par_Seq.of_sequential_seq data sq)))
    end

  fun log_p pretty_f lvl name =
    Log.logf lvl (fn g => Pretty.string_of
        (Pretty.chunks [Pretty.str (name^":"),(pretty_f g)]))
  val log_graph = log_p G.pretty
  val log_v_nset = log_p V.NSet.pretty
  
  fun match_normalised pat tgt = let
    val _ = log_graph 3 "ConcreteMatchSearch: normalised pattern" pat
    val _ = log_graph 3 "ConcreteMatchSearch: normalised target" tgt
    val ms = MatchState.init_and_schedule_all pat tgt
  in
    do_match ms
  end
  fun match pat tgt = let
    val pat' = G.normalise pat
    val tgt' = G.normalise tgt
  in match_normalised pat' tgt' end
  
  fun match_subgraph pat tgt tgt_verts = let
    val _ = log_graph 3 "ConcreteMatchSearch: normalised pattern" pat
    val _ = log_graph 3 "ConcreteMatchSearch: normalised target" tgt
    val _ = log_v_nset 4 "ConcreteMatchSearch: target vertices" tgt_verts
    val ms = MatchState.init_subgraph_and_schedule_all pat tgt tgt_verts
  in
    do_match ms
  end
  fun match_subgraph_normalised pat tgt = let
    val pat = G.normalise pat
    val tgt = G.normalise tgt
  in match_subgraph_normalised pat tgt end
  
  structure Sharing =
  struct
    structure G = G.SharingOGraph
    structure MatchState = MatchState.Sharing
  end
end
