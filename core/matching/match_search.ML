(* The matching interface *)
(* see docs/matching_algo for details *)
signature MATCH_SEARCH_SHARING =
sig
  structure Graph : OGRAPH_SHARING
  structure MatchState : OGRAPH_MATCH_STATE_SHARING
  sharing Graph = MatchState.Graph
end

signature MATCH_SEARCH =
sig
  structure Log : LOG;

  structure MatchState : OGRAPH_MATCH_STATE
  structure Graph : OGRAPH
  structure Sharing : MATCH_SEARCH_SHARING
    sharing Sharing.Graph = Graph.Sharing
    sharing Sharing.MatchState = MatchState.Sharing
  
  val match : Graph.T           (* pattern graph *)
    -> Graph.T                  (* target graph *)
    -> MatchState.T Seq.seq (* (lazy) list of matches *)
  
  val match_subgraph : Graph.T  (* pattern graph *)
    -> Graph.T                  (* target graph *)
    -> V.NSet.T             (* target subgraph verts *)
    -> MatchState.T Seq.seq (* (lazy) list of matches *)
  
  (* these versions are for efficiency if you can guarantee
   * that the graphs are already normalised
   *)
  val match_normalised : Graph.T (* pattern graph (normalised) *)
    -> Graph.T                   (* target graph (normalised) *)
    -> MatchState.T Seq.seq  (* (lazy) list of matches *)
  
  val match_subgraph_normalised : Graph.T (* pattern graph (normalised) *)
    -> Graph.T                            (* target graph (normalised) *)
    -> V.NSet.T                       (* target subgraph verts *)
    -> MatchState.T Seq.seq           (* (lazy) list of matches *)
end

signature BG_MATCH_SEARCH =
sig
  include MATCH_SEARCH;
  structure BG : BANG_GRAPH;
  structure BGMatchState : BANG_GRAPH_MATCH_STATE;
  sharing Graph.Sharing = BG.Sharing
  sharing BGMatchState.Graph.Sharing = Graph.Sharing;
  sharing MatchState.Sharing = BGMatchState.Sharing
  
  (* Searches for matches *)
  val match_avoiding_tracking
     : (V.NSet.T *          (* names to avoid *)
        V.NSet.T)           (* names to track *)
    -> Graph.T                  (* pattern graph *)
    -> Graph.T                  (* target graph *)
    -> MatchState.T Seq.seq (* (lazy) list of matches *)
  
  (* Searches for matches *)
  val match_subgraph_avoiding_tracking
     : (V.NSet.T *          (* names to avoid *)
        V.NSet.T)           (* names to track *)
    -> Graph.T                  (* pattern graph *)
    -> Graph.T                  (* target graph *)
    -> V.NSet.T             (* target subgraph verts *)
    -> MatchState.T Seq.seq (* (lazy) list of matches *)
  
  (* Searches for matches *)
  val match_normalised_avoiding_tracking
     : (V.NSet.T *          (* names to avoid *)
        V.NSet.T)           (* names to track *)
    -> Graph.T                  (* pattern graph (normalised) *)
    -> Graph.T                  (* target graph (normalised) *)
    -> MatchState.T Seq.seq (* (lazy) list of matches *)
  
  (* Searches for matches *)
  val match_subgraph_normalised_avoiding_tracking
     : (V.NSet.T *          (* names to avoid *)
        V.NSet.T)           (* names to track *)
    -> Graph.T                  (* pattern graph (normalised) *)
    -> Graph.T                  (* target graph (normalised) *)
    -> V.NSet.T             (* target subgraph verts *)
    -> MatchState.T Seq.seq (* (lazy) list of matches *)
end
