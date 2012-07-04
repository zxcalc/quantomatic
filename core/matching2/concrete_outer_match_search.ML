(* A wrapper for matching concrete graphs against concrete graphs *)
(* see docs/matching_algo for details *)
(* This wrapper is very simple, as the InnerMatchSearch does all the work *)
functor ConcreteOuterMatchSearch(
    structure InnerMatchSearch : INNER_MATCH_SEARCH
) : OUTER_MATCH_SEARCH =
struct
  structure InnerMatchSearch = InnerMatchSearch
  structure MatchState = InnerMatchSearch.MatchState
  structure G = InnerMatchSearch.G
  
  (* We can't provide any information that the inner match search
   * does not already know *)
  fun concrete_doomedf _ _ = false
  
  (* We just put everything in the match state, and for each match
   * provided by the inner loop, we match the bare wires, and discard any
   * incomplete matches (as we have nothing left to add to the pattern) *)
  fun match_avoiding_names names pat tgt = let
    val pat = G.normalise pat
    val tgt = G.normalise tgt
    val ms = MatchState.init_and_schedule_all pat tgt |> MatchState.set_names names
  in Seq.filter MatchState.is_total
       (Seq.maps InnerMatchSearch.match_bare_wires
        (InnerMatchSearch.match_pending concrete_doomedf ms))
  end
  
  val match = match_avoiding_names (V.NSet.empty, E.NSet.empty)
  
  structure Sharing =
  struct
    structure G = G.SharingOGraph
    structure InnerMatchSearch = InnerMatchSearch.Sharing
    structure MatchState = MatchState.Sharing
  end
end
