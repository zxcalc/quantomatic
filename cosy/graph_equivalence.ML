signature GRAPH_EQUIVALENCE =
sig
  structure Graph : BANG_GRAPH
  val eq : Graph.T * Graph.T -> bool
end


