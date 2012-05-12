functor EdgeComplexityMetric(
  structure Graph : BANG_GRAPH
) : METRIC =
struct
  type T = int * int * int
  structure Graph = Graph
 
  val name = "Edge Complexity Metric"
  val desc = "Description Goes Here"

  fun ord ((a,b,c), (d,e,f)) = (prod_ord int_ord (prod_ord int_ord int_ord)) ((a,(b,c)), (d,(e,f)))
  val bottom = (0,0,0)
  
  fun compute graph = let
    fun vert_weight v = let
      val edges = E.NSet.cardinality (E.NSet.union_merge 
                                      (Graph.in_enames graph v)
                                      (Graph.out_enames graph v))
      in if edges > 2 then edges else 0
      end
    fun vfld v (ec,count) = (ec + vert_weight v, count+1)
    val (ec,verts) = V.NSet.fold vfld (Graph.get_vnames graph) (0,0)
    val edges = (E.NSet.cardinality o Graph.get_enames) graph
  in (ec, verts, edges)
  end
  
  val ord_graph = ord o (apfst compute) o (apsnd compute)
end

