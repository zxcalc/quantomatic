theory graph
imports Main expressions  
uses

(* Graphs *)
 "../../core/graph/arity.ML" (* arity of vertices (in,out,undir) *)
(* neighbourhood data for non-commutative vertices *)
 "../../core/graph/nhd.ML"

 "../../core/graph/graph_data.ML"
 "../../core/graph/ograph.sig.ML"
 "../../core/graph/bang_graph.sig.ML"
 "../../core/graph/bang_graph.ML"

 "../../core/graph/graph_annotations.ML" (* graph annotations *)


(* I/O *)
 "../../core/io/graph_json.ML"
 "../../core/io/graph_annotations_json.ML"
 "../../core/io/graph_dot_output.ML"

(* new, combined IO struct *)
 "../../core/io/graph_json_io.ML"

(*
 * Misc stuff
 *)
 "../../core/stub_run.ML" (* ? *)
(* hilbert space stuff: depends on OGRAPH *)
 "../../core/expressions/hilb.ML"


(* Metrics *)
 "../../core/metrics/metric_type.ML" (* Utils to handle int tuples *)
 "../../core/metrics/metrics/metric.ML" (* METRIC signature *)
 "../../core/metrics/metrics/edge_complexity_metric.ML"
 "../../core/metrics/metrics/weighted_arity_metric.ML"
 "../../core/metrics/metrics/sets_cardinals_metric.ML"
 "../../core/metrics/metrics.ML" (* Metrics on graphs *)


begin

end;

