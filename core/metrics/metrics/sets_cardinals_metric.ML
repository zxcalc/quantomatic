functor SetsCardinalsMetric(
  structure Graph : BANG_GRAPH
) : METRIC =
struct
  type T = int * int 
  structure Graph = Graph
 
  val name = "Sets Cardinals Metric"
  val desc = "Metric based on the cardinals of the V set and the E set."

  val ord = (prod_ord int_ord int_ord)
  val bottom = (0,0)
 
  fun compute graph = let
    val n_verts = V.NSet.cardinality (Graph.get_vnames graph)
    val n_edges = E.NSet.cardinality (Graph.get_enames graph)
  in
    (n_verts, n_edges)
  end 

  val ord_graph = ord o (apfst compute) o (apsnd compute)
end

