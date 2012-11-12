signature GRAPH_EQUIV =
sig
  structure Graph : BANG_GRAPH
  type T
  
  (* compute data that is used to check graph equivalence. This function may
     also do some renaming of the graph to put it in some canonical form (e.g.
     wrt. the ordering of inputs and outputs). *)
  val compute_equiv_data : Graph.T -> Graph.T * T
  val to_string : T -> string
  val eq : T * T -> bool
end


