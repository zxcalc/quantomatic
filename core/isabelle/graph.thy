theory graph
imports Main expressions 
uses
(* Graphs *)
 "../../core/graph/arity.ML" (* arity of vertices (in,out,undir) *)
 "../../core/graph/graph_param.ML"
 "../../core/graph/basic_graph.ML" (* basic graphs, just the data *)
 "../../core/graph/graph.ML" (* graphs with interesting functions *)
 "../../core/graph/graph_iso.ML" (* isomorphism between graphs *)
 "../../core/graph/overtex.ML" (* open graphs *)
 "../../core/graph/ograph_param.ML"
 "../../core/graph/ograph.ML"
 "../../core/graph/bang_graph.ML" (* bang box graphs *)
 "../../core/graph/bang_graph_iso.ML" (* isomorphism between !graphs *)

 "../../core/graph/graph_annotations.ML" (* graph annotations *)

(* Metrics *)
 "../../core/metrics/metric_type.ML" (* Utils to handle int tuples *)
 "../../core/metrics/metrics/metric.ML" (* METRIC signature *)
 "../../core/metrics/metrics/edge_complexity_metric.ML"
 "../../core/metrics/metrics/weighted_arity_metric.ML"
 "../../core/metrics/metrics/sets_cardinals_metric.ML"
 "../../core/metrics/metrics.ML" (* Metrics on graphs *)

begin

end;

