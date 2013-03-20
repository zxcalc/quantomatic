functor EdgeComplexityMetric(
  structure MT : METRIC_TYPE
) : METRIC =
struct
  structure MT = MT;
  structure Graph : BANG_GRAPH = MT.Graph;
  datatype metric_type = datatype MT.metric_type;
  datatype metric_rec = datatype MT.metric_rec;
  type T = metric_type

  val name = "Edge Complexity Metric"
  val desc = "Description Goes Here"

  fun ord (Tup3 (a,b,c), Tup3 (d,e,f)) = SOME ((prod_ord int_ord (prod_ord
    int_ord int_ord)) ((a,(b,c)), (d,(e,f))))
     | ord _ = NONE
  val bottom = Tup3 (0,0,0)

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
  in SOME (Tup3 (ec, verts, edges))
  end
  
  fun internal_ord (SOME a, SOME b) = ord (a,b)
    | internal_ord _ = NONE
  val ord_graph = internal_ord o (apfst compute) o (apsnd compute)

  val metric_record = MR {desc = desc, 
                          compute = MT.to_foreign_type o compute,
                          ord_graph = ord_graph,
                          ord = ord o 
                            (fn (a,b) => (MT.from_foreign_type a,
                          MT.from_foreign_type a)),
                          bottom = MT.to_foreign_type_safe bottom}
end

