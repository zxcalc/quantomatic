signature GRAPH_METRIC =
sig
  type T
  structure Graph : BANG_GRAPH
  val compute : Graph.T -> T
  val ord : T * T -> order
  val bottom : T
  
  (* ASSUMED: ord_graph = ord o (apfst compute) o (apsnd compute) *)
  val ord_graph : Graph.T * Graph.T -> order
end

functor EdgeComplexityMetric(
  structure Graph : BANG_GRAPH
) =
struct
  type T = int * int * int
  structure Graph = Graph
  
  fun ord ((a,b,c), (d,e,f)) = (prod_ord int_ord (prod_ord int_ord int_ord)) ((a,(b,c)), (d,(e,f)))
  val bottom = (0,0,0)
  
  fun compute graph = let
    fun vert_weight v = let
      val edges = Graph.Edge.NSet.cardinality (Graph.Edge.NSet.union_merge 
                                                (Graph.in_enames graph v)
                                                (Graph.out_enames graph v))
      in if edges > 2 then edges else 0
      end
    fun vfld v (ec,count) = (ec + vert_weight v, count+1)
    val (ec,verts) = Graph.Vertex.NSet.fold vfld (Graph.get_vnames graph) (0,0)
    val edges = (Graph.Edge.NSet.cardinality o Graph.get_enames) graph
  in (ec, verts, edges)
  end
  
  val ord_graph = ord o (apfst compute) o (apsnd compute)
end

structure GHZW_ECMetric = EdgeComplexityMetric(structure Graph = GHZW_Theory.Graph)

