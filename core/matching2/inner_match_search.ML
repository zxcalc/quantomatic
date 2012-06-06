signature INNER_MATCH_SEARCH_SHARING =
sig
  structure G : OGRAPH_SHARING
  structure MatchState : OGRAPH_MATCH_STATE_SHARING
    sharing MatchState.G = G
end

signature INNER_MATCH_SEARCH =
sig
  structure G : OGRAPH
  structure MatchState : OGRAPH_MATCH_STATE
  structure Sharing : INNER_MATCH_SEARCH_SHARING
    sharing Sharing.G = G.SharingOGraph
    sharing Sharing.MatchState = MatchState.Sharing
    
  (* do as much concrete matching as possible and return a lazy list of updated match states *)
  val match_concrete : MatchState.T -> MatchState.T Seq.seq
  
  (* match the given set of bare wires, expanded edge points where necessary *)
  val match_bare_wires : MatchState.T -> MatchState.T Seq.seq
end