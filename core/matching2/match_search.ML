(* The matching interface *)
(* see docs/matching_algo for details *)
signature MATCH_SEARCH_SHARING =
sig
  structure G : OGRAPH_SHARING
  structure MatchState : OGRAPH_MATCH_STATE_SHARING
end

signature MATCH_SEARCH =
sig
  structure MatchState : OGRAPH_MATCH_STATE
  structure G : OGRAPH
  structure Sharing : MATCH_SEARCH_SHARING
    sharing Sharing.G = G.SharingOGraph
    sharing Sharing.MatchState = MatchState.Sharing
  
  val match : G.T           (* pattern graph *)
    -> G.T                  (* target graph *)
    -> MatchState.T Seq.seq (* (lazy) list of matches *)
  
  val match_subgraph : G.T  (* pattern graph *)
    -> G.T                  (* target graph *)
    -> V.NSet.T             (* target subgraph verts *)
    -> MatchState.T Seq.seq (* (lazy) list of matches *)
end

signature BG_MATCH_SEARCH =
sig
  include MATCH_SEARCH;
  structure BG : BANG_GRAPH;
  structure BGMatchState : BANG_GRAPH_MATCH_STATE;
  sharing G.SharingOGraph = BG.SharingOGraph;
  sharing BGMatchState.G.SharingOGraph = G.SharingOGraph;
  sharing MatchState.Sharing = BGMatchState.Sharing
  
  (* Searches for matches *)
  val match_avoiding_tracking
     : (V.NSet.T *          (* names to avoid *)
        V.NSet.T)           (* names to track *)
    -> G.T                  (* pattern graph *)
    -> G.T                  (* target graph *)
    -> MatchState.T Seq.seq (* (lazy) list of matches *)
  
  (* Searches for matches *)
  val match_subgraph_avoiding_tracking
     : (V.NSet.T *          (* names to avoid *)
        V.NSet.T)           (* names to track *)
    -> G.T                  (* pattern graph *)
    -> G.T                  (* target graph *)
    -> V.NSet.T             (* target subgraph verts *)
    -> MatchState.T Seq.seq (* (lazy) list of matches *)
end
