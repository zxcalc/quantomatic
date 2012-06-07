functor SimpleInnerMatchSearch(
    structure G : OGRAPH
) : INNER_MATCH_SEARCH
= struct
  structure G = G
  structure MatchState = OGraphMatchState(structure G = G)
    
  (* do as much concrete matching as possible and return a lazy list of updated match states *)
  fun match_concrete m = Seq.empty
  
  (* match the given set of bare wires, expanded edge points where necessary *)
  fun match_bare_wires m = Seq.empty
  
  structure Sharing =
  struct
    structure G = G.SharingOGraph
    structure MatchState = MatchState.Sharing
  end
end