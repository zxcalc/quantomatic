(* Signatures for metrics on graphs.
 * (inspired by /cosy/metric.ML) *)

signature METRIC =
sig

  type T
 
  structure Graph : BANG_GRAPH
  val name : string
  val desc : string

  val compute : Graph.T -> T
  val ord : (T * T) -> order
  val ord_graph : (Graph.T * Graph.T) -> order
  val bottom : T;

end;

signature METRICS = 
sig

   type T;
   
   structure Graph : BANG_GRAPH; 

   structure M : METRIC;

   val curent_metric : string;

   val get_metrics_names : T -> string list;
   val get_metrics_descs : T -> string list;

   val set_current_metric : string -> T -> T;
   val get_current_metric_name : T -> string;
   val get_current_metric_desc : T -> string;
   val current_ord : T -> ((M.T * M.T) -> order);
   val current_compute : T -> ((Graph.T * Graph.T) -> M.T);
   val current_ord_graph : T -> (Graph.T -> Graph.T -> order); 
   val current_botton : T -> M.T;

end;

(* Metrics functor: Takes a Graph structure and registers metrics.
 * This is where new metrics should be hooked up to the core. *)
