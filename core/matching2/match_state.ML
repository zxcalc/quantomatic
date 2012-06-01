signature OGRAPH_MATCH_STATE
= sig
  structure G : OGRAPH
end

functor OGraphMatchState(structure G : OGRAPH) : OGRAPH_MATCH_STATE
= struct
  structure G = G
end
