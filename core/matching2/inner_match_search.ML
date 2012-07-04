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
    
  (* Do as much concrete matching as possible and return a lazy list of updated match states. The first argument is a "doomed"
     function, which lets the inner loop know if matching for the given vertex will ultimately fail. *)
  val match_concrete : (MatchState.T -> V.name -> bool) -> MatchState.T -> MatchState.T Seq.seq
  
  (* Match all the bare wires in u_bare_wires, expanding edge points where necessary. *)
  val match_bare_wires : MatchState.T -> MatchState.T Seq.seq
  
  (* Match all circles.*)
  val match_circles : MatchState.T -> MatchState.T Seq.seq
    
  
  (*** Uncomment for debug ***)
  (*val u_circles_in_tgt : MatchState.T -> V.NSet.T*)
  
  structure Sharing : INNER_MATCH_SEARCH_SHARING
    sharing Sharing.G = G.SharingOGraph
    sharing Sharing.MatchState = MatchState.Sharing
end

signature INNER_BG_MATCH_SEARCH =
sig
  include INNER_MATCH_SEARCH;
  structure BG : BANG_GRAPH;
  structure BGMatchState : BANG_GRAPH_MATCH_STATE;
  sharing type G.T = BG.T;
  sharing type BGMatchState.T = MatchState.T;
end
