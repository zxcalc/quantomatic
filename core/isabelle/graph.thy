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

(* I/O *)
 "../../core/io/graph_json.ML"
 "../../core/io/graph_annotations_json.ML"
 "../../core/io/graph_dot_output.ML"

(* Metrics *)
 "../../core/metrics/metric_type.ML" (* Utils to handle int tuples *)
 "../../core/metrics/metrics/metric.ML" (* METRIC signature *)
 "../../core/metrics/metrics/edge_complexity_metric.ML"
 "../../core/metrics/metrics/weighted_arity_metric.ML"
 "../../core/metrics/metrics/sets_cardinals_metric.ML"
 "../../core/metrics/metrics.ML" (* Metrics on graphs *)

(*
 * Rules
 *)
 "../../core/rewriting/rule.ML"
 (* I/O *)
 "../../core/io/rule_json.ML"

(*
 * Rulesets
 *)
 "../../core/theories/ruleset.ML"
 "../../core/theories/ruleset_annotations.ML"

(* I/O *)
 "../../core/io/ruleset_json.ML"
 "../../core/io/ruleset_annotations_json.ML"

begin

end;

