(* The outer matching loop interface *)
(* see docs/matching_algo for details *)
signature OUTER_MATCH_SEARCH_SHARING =
sig
  structure G : OGRAPH_SHARING
  structure InnerMatchSearch : INNER_MATCH_SEARCH_SHARING
  structure MatchState : OGRAPH_MATCH_STATE_SHARING
  
  sharing MatchState = InnerMatchSearch.MatchState
  sharing InnerMatchSearch.G = G
end

signature OUTER_MATCH_SEARCH =
sig
  structure MatchState : OGRAPH_MATCH_STATE
  structure InnerMatchSearch : INNER_MATCH_SEARCH
  structure G : OGRAPH
  structure Sharing : OUTER_MATCH_SEARCH_SHARING
    sharing Sharing.G = G.SharingOGraph
    sharing Sharing.InnerMatchSearch = InnerMatchSearch.Sharing
    sharing Sharing.MatchState = MatchState.Sharing
    
  (* Searches for matches *)
  val match_avoiding_names : (V.NSet.T * E.NSet.T) (* names to avoid *)
    -> G.T                   (* pattern graph *)
    -> G.T                   (* target graph *)
    -> MatchState.T Seq.seq  (* (lazy) list of matches *)
      
  val match : G.T -> G.T -> MatchState.T Seq.seq
end

signature OUTER_BG_MATCH_SEARCH =
sig
  include OUTER_MATCH_SEARCH;
  structure BG : BANG_GRAPH;
  structure BGMatchState : BANG_GRAPH_MATCH_STATE;
  sharing G.SharingOGraph = BG.SharingOGraph;
  sharing BGMatchState.G.SharingOGraph = G.SharingOGraph;
  sharing MatchState.Sharing = BGMatchState.Sharing
end
