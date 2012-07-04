(* The inner matching loop interface *)
(* see docs/matching_algo for details *)
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

  (* Match all queued unmatched circles and node vertices, and extend the
   * matchings of any scheduled partially-matched node vertices as much as
   * possible using queued unmatched wire vertices.
   *
   * Returns a lazy list of updated match states.
   *
   * The first argument is a "doomed" function, which should return
   * true if it is known that matching for the given vertex will
   * ultimately fail.
   *
   * Note that this will _not_ match bare wires.
   *)
  val match_pending : (MatchState.T -> V.name -> bool)
                      -> MatchState.T
                      -> MatchState.T Seq.seq

  (* Match all the bare wires in u_bare_wires, expanding edge points where
   * necessary. *)
  val match_bare_wires : MatchState.T -> MatchState.T Seq.seq
  
  (* Match all circles.
   *
   * Note that match_pending will do this anyway.
   *)
  val match_circles : MatchState.T -> MatchState.T Seq.seq


  (*** Uncomment for debug ***)
  (*val u_circles_in_tgt : MatchState.T -> V.NSet.T*)

  structure Sharing : INNER_MATCH_SEARCH_SHARING
    sharing Sharing.G = G.SharingOGraph
    sharing Sharing.MatchState = MatchState.Sharing
end
