signature INNER_MATCH_SEARCH_SHARING =
sig
  structure G : OGRAPH_SHARING
  structure MatchState : OGRAPH_MATCH_STATE_SHARING
end

signature INNER_MATCH_SEARCH =
sig
  structure G : OGRAPH
  structure MatchState : OGRAPH_MATCH_STATE
  structure Sharing : INNER_MATCH_SEARCH_SHARING
    sharing Sharing.G = G.SharingOGraph
    sharing Sharing.MatchState = MatchState.Sharing
end