(* A wrapper for matching concrete graphs against concrete graphs *)
(* see docs/matching_algo for details *)
(* This wrapper is very simple, as the InnerMatchSearch does all the work *)
functor ConcreteMatchSearch(
    structure InnerMatchSearch : INNER_MATCH_SEARCH
) : MATCH_SEARCH =
struct

  structure Log : LOG = Log(val init_level = 0);

  structure MatchState = InnerMatchSearch.MatchState
  structure Graph = InnerMatchSearch.Graph
  
  (* We can't provide any information that the inner match search
   * does not already know *)
  fun concrete_doomedf _ _ = false

  (* We just put everything in the match state, and for each match
   * provided by the inner loop, we match the bare wires, and discard any
   * incomplete matches (as we have nothing left to add to the pattern) *)
  fun do_match ms = 
    Seq.filter MatchState.is_total
       (Seq.maps InnerMatchSearch.match_bare_wires
        (InnerMatchSearch.match_pending concrete_doomedf ms))

  fun log_p pretty_f lvl name =
    Log.logf lvl (fn g => Pretty.string_of
        (Pretty.chunks [Pretty.str (name^":"),(pretty_f g)]))
  val log_graph = log_p Graph.pretty
  val log_v_nset = log_p V.NSet.pretty
  
  fun match_normalised pat tgt = let
    val _ = log_graph 3 "ConcreteMatchSearch: normalised pattern" pat
    val _ = log_graph 3 "ConcreteMatchSearch: normalised target" tgt
    val ms = MatchState.init_and_schedule_all pat tgt
  in
    do_match ms
  end
  fun match pat tgt = let
    val pat' = Graph.normalise pat
    val tgt' = Graph.normalise tgt
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
    val pat = Graph.normalise pat
    val tgt = Graph.normalise tgt
  in match_subgraph_normalised pat tgt end
  
  structure Sharing =
  struct
    structure Graph = Graph.Sharing
    structure MatchState = MatchState.Sharing
  end
end
