functor SimpleOuterMatchSearch(
    structure InnerMatchSearch : INNER_MATCH_SEARCH
) : OUTER_MATCH_SEARCH =
struct
  structure InnerMatchSearch = InnerMatchSearch
  structure MatchState = InnerMatchSearch.MatchState
  structure G = InnerMatchSearch.G
  
  fun concrete_doomedf ms nv = false
  
  fun match_avoiding_names names pat tgt = let
    val pat = G.normalise pat
    val tgt = G.normalise tgt
    val ms = MatchState.init_and_schedule_all pat tgt |> MatchState.set_names names
  in Seq.filter MatchState.is_total
       (Seq.maps InnerMatchSearch.match_bare_wires (InnerMatchSearch.match_concrete concrete_doomedf ms))
  end
  
  val match = match_avoiding_names (V.NSet.empty, E.NSet.empty)
  
  structure Sharing =
  struct
    structure G = G.SharingOGraph
    structure InnerMatchSearch = InnerMatchSearch.Sharing
    structure MatchState = MatchState.Sharing
  end
end