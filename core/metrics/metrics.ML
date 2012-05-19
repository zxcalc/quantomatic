signature METRICS = 
sig

   type T;
  
   val init : T;

   structure Graph : BANG_GRAPH; 
   structure MT : METRIC_TYPE;

   type metric_rec = MT.metric_rec;
   type foreign_type = MT.foreign_type;
   val set_current_metric : string -> T -> T;

   val get_metrics_names : T -> string list;
   val get_metrics_descs : T -> string list;
   val get_current_metric_name : T -> string;

   val get_current_desc : T -> string;
   val get_current_ord : T -> ((MT.foreign_type * MT.foreign_type) -> order option);
   val get_current_compute : T -> (Graph.T -> MT.foreign_type option );
   val get_current_ord_graph : T -> ((Graph.T * Graph.T) -> order option); 
   val get_current_bottom : T -> MT.foreign_type;

    
   exception no_such_metric_exp of string;

end;


functor MetricsController (
  structure Graph : BANG_GRAPH
) : METRICS =
  struct

    structure Graph = Graph;
    structure MT = MetricType(structure Graph = Graph);

    type foreign_type = MT.foreign_type;

    exception no_such_metric_exp of string;

    datatype metric_rec = datatype MT.metric_rec;
    (* This is were new metrics should be added: get a new structure and
     * register desc, compute, ord_graph, ord and bottom *)

    structure SCMet : METRIC = SetsCardinalsMetric(structure MT = MT); 
    (* FIXME: Some metrics may be applied to only 1 type of Graphical Theory.
     * This should be taken into account by the metric functor, not here.
     * Some computations may fail or return different results depending on the
     * theory *)
    structure WAMet : METRIC = WeightedArityMetric(structure MT = MT
                                                   fun weight_for_data _ = 5);
    structure ECMet : METRIC = EdgeComplexityMetric(structure MT = MT); 

    val metrics_list = [(SCMet.name, SCMet.metric_record), 
                        (WAMet.name, WAMet.metric_record),
                        (ECMet.name, ECMet.metric_record)
                       ];

    val init_metrics = Symtab.make metrics_list;
    val first_metric = case (Symtab.min_key init_metrics) of
                        SOME a => a
                        | NONE => "None"

    datatype T = MC of { metrics : metric_rec Symtab.table,
               current_metric : string};

    val init = MC {
      metrics = init_metrics,
      current_metric = first_metric
    };

    fun update_metrics f (MC {metrics= m, current_metric=c}) =
      MC {metrics = f m, current_metric = c};
    fun update_current_metric f (MC {metrics=m, current_metric=c}) =
      MC {metrics = m, current_metric = f c};
    val (set_metrics, set_current_metric_int) =
      (update_metrics o K, update_current_metric o K) 

    fun get_metrics (MC {metrics=m,...}) = m
    fun get_current_metric (MC {current_metric=c,...}) = c

   
    fun set_current_metric nm mc = let
      val () = case Symtab.lookup (get_metrics mc) nm of
        SOME a => ()
      | NONE => raise no_such_metric_exp nm 
    in
      set_current_metric_int nm mc 
    end;

    fun get_metrics_names mc = 
      Symtab.keys (get_metrics mc)

    fun get_metrics_descs mc = 
        List.map (MT.get_mr_desc o snd) (Symtab.dest (get_metrics mc))

   val get_current_metric_name = get_current_metric;
  
   fun get_current_metric_elem getter mc = 
     let
        val cur_met = get_current_metric mc;
        val record = case (Symtab.lookup (get_metrics mc) cur_met) of
              SOME a => a
              | NONE => raise no_such_metric_exp cur_met
     in
        getter record 
     end;

  val get_current_desc = get_current_metric_elem MT.get_mr_desc;
  val get_current_ord = get_current_metric_elem MT.get_mr_ord;
  val get_current_ord_graph = get_current_metric_elem MT.get_mr_ord_graph;
  val get_current_compute = get_current_metric_elem MT.get_mr_compute;
  val get_current_bottom = get_current_metric_elem MT.get_mr_bottom;
  
  end

