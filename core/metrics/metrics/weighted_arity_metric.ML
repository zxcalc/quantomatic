functor WeightedArityMetric(
  structure MT : METRIC_TYPE
  val weight_for_data : MT.Graph.VData.data -> int
) : METRIC =
struct
  structure MT = MT;
  structure Graph = MT.Graph;
  datatype metric_type = datatype MT.metric_type;
  datatype metric_rec = datatype MT.metric_rec;
  type T = metric_type



  fun ord (Tup1 a, Tup1 b) = SOME (int_ord (a,b))
    | ord _ = NONE
  val bottom = Tup1 0

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
  in SOME (Tup1 (V.NSet.fold vert_weights (Graph.get_vnames graph) 0))
  end

  fun internal_ord ((SOME a), (SOME b)) = ord (a,b)
  val ord_graph = internal_ord o (apfst compute) o (apsnd compute)
  val metric_record = MR {desc = desc, 
                          compute = MT.to_foreign_type o compute,
                          ord_graph = ord_graph,
                          ord = ord o 
                            (fn (a,b) => (MT.from_foreign_type a, MT.from_foreign_type a)),
                          bottom = MT.to_foreign_type_safe bottom}
end

