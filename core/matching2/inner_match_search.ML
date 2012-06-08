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
    
  (* Do as much concrete matching as possible and return a lazy list of updated match states. *)
  val match_concrete : MatchState.T -> MatchState.T Seq.seq
  
  (* Match all the bare wires in u_bare_wires, expanding edge points where necessary. *)
  val match_bare_wires : MatchState.T -> MatchState.T Seq.seq
  
  (* Match all circles. This can be done deterministically, as we don't care which circles go where. *)
  val match_circles : MatchState.T -> MatchState.T option
  
  
  (*** These are exposed mainly for debugging ***)
  val u_circles_in_tgt : MatchState.T -> V.NSet.T
end