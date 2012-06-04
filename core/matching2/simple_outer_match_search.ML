functor SimpleOuterMatchSearch(
    structure InnerMatchSearch : INNER_MATCH_SEARCH
) : OUTER_MATCH_SEARCH =
struct
  structure InnerMatchSearch = InnerMatchSearch
  structure MatchState = InnerMatchSearch.MatchState
  structure G = InnerMatchSearch.G
  
  (* TODO: actually schedule things to be matched and do wires *)
  fun match_avoiding_names names pat tgt = let
    val ms = MatchState.empty
      |> MatchState.set_names names
      |> MatchState.set_pat (G.normalise pat)
      |> MatchState.set_tgt (G.normalise tgt)
  in InnerMatchSearch.match_concrete ms
  end
  
  val match = match_avoiding_names (V.NSet.empty, E.NSet.empty)
  
  structure Sharing =
  struct
    structure G = G.SharingOGraph
    structure InnerMatchSearch = InnerMatchSearch.Sharing
    structure MatchState = MatchState.Sharing
  end
end