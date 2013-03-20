functor SetsCardinalsMetric(
  structure MT: METRIC_TYPE
) : METRIC =
struct
  structure MT = MT;
  structure Graph = MT.Graph;
  datatype metric_type = datatype MT.metric_type;
  datatype metric_rec = datatype MT.metric_rec;
  type T = metric_type



  val name = "Sets Cardinals Metric"
  val desc = "Metric based on the cardinals of the V set and the E set."

  fun ord (Tup2 (a,b), Tup2 (c,d)) = 
          SOME ((prod_ord int_ord int_ord) ((a,b), (c,d)))
    | ord _ = NONE
  
  val bottom = Tup2 (0,0)
 
  fun compute graph = let
    val n_verts = V.NSet.cardinality (Graph.get_vnames graph)
    val n_edges = E.NSet.cardinality (Graph.get_enames graph)
  in
    SOME (Tup2 (n_verts, n_edges))
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

