functor WeightedArityMetric(
  structure Graph : BANG_GRAPH
  val weight_for_data : Graph.VData.data -> int
) : METRIC =
struct
  type T = int
  structure Graph = Graph
  val ord = int_ord
  val bottom = 0

  val name = "Weighted Arity Metric"
  val desc = "Description Goes here"

 
  (*fun weight_for_arity 0 0 = 10
      | weight_for_arity 1 0 = 20
      | weight_for_arity 0 1 = 23
      | weight_for_arity 1 1 = 30
      | weight_for_arity 2 0 = 50
      | weight_for_arity 0 2 = 53
      | weight_for_arity 2 1 = 80
      | weight_for_arity 1 2 = 83
      | weight_for_arity 3 0 = 90
      | weight_for_arity 0 3 = 93
      | weight_for_arity 2 2 = 110
      | weight_for_arity 3 1 = 120
      | weight_for_arity 1 3 = 123
      | weight_for_arity 4 0 = 130
      | weight_for_arity 0 4 = 133
      | weight_for_arity n m = (20 * (5 + n)) + (21 * m)*)
  
  fun weight_for_arity 0 0 l = 10 + l
    | weight_for_arity 1 0 l = 20 + l
    | weight_for_arity 0 1 l = 22 + l
    | weight_for_arity 1 1 l = 30 + l
    | weight_for_arity 2 0 l = 40 + l
    | weight_for_arity 0 2 l = 44 + l
    | weight_for_arity 2 1 l = 50 + l
    | weight_for_arity 1 2 l = 55 + l
    | weight_for_arity m n l = 20*(m*m + n*n) + 20*(m + n)*(m + n) + l
  
  (*fun weight_for_arity m n l = 2*(m*m + n*n) + 2*(m + n)*(m + n) + l*)
  
  fun compute graph = let
    fun vert_weights v tot = let
      val loops = E.NTab.cardinality (Graph.get_self_loops graph v)
      val ins = E.NSet.cardinality (Graph.in_enames graph v) - loops
      val outs = E.NSet.cardinality (Graph.out_enames graph v) - loops
      val dat = fst (Graph.get_vertex graph v)
    in (weight_for_data dat * weight_for_arity ins outs loops) + tot
    end
  in V.NSet.fold vert_weights (Graph.get_vnames graph) 0
  end

  val ord_graph = ord o (apfst compute) o (apsnd compute)
end

