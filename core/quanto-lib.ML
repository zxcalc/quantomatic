(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*                         Compile quantomatic core                        *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

(* 
 * Utility Code
 *)

(* IO Utils *)
PolyML.Project.make "io/input.ML";
PolyML.Project.make "io/output.ML";
PolyML.Project.make "io/json_io.ML";
PolyML.Project.make "io/file_io.ML";


(*
 * Names
 *)
PolyML.Project.make "names.ML"; (* defines basic names used in Quantomatic *)


(*
 * Expressions for use in graph component data
 *)
PolyML.Project.make "expressions/lex.ML";
PolyML.Project.make "expressions/coeff.ML";
PolyML.Project.make "expressions/matrix.ML";
PolyML.Project.make "expressions/expr.ML";
PolyML.Project.make "expressions/linrat_expr.ML";
PolyML.Project.make "expressions/linrat_angle_expr.ML";
PolyML.Project.make "expressions/semiring.ML";
PolyML.Project.make "expressions/tensor.ML";
PolyML.Project.make "expressions/linrat_angle_matcher.ML";
PolyML.Project.make "expressions/linrat_matcher.ML";

PolyML.Project.make "expressions/alg.ML"; (* algebraic expression utils *)

(* I/O *)
PolyML.Project.make "io/linrat_json.ML";



(*
 * Graphs
 *)

(* arity of vertices (in,out,undir) *)
PolyML.Project.make "graph/arity.ML";
(* neighbourhood data for non-commutative vertices *)
PolyML.Project.make "graph/nhd.ML"; 


PolyML.Project.make "graph/graph_data.ML";
PolyML.Project.make "graph/ograph.sig.ML";
PolyML.Project.make "graph/bang_graph.sig.ML";
PolyML.Project.make "graph/bang_graph.ML";

PolyML.Project.make "graph/graph_annotations.ML"; (* graph annotations *)

(* I/O *)
PolyML.Project.make "io/graph_json.ML";
PolyML.Project.make "io/graph_annotations_json.ML";
PolyML.Project.make "io/graph_dot_output.ML";

(* new, combined IO struct *)
PolyML.Project.make "io/graph_json_io.ML";


(*
 * Misc stuff
 *)
(* hilbert space stuff: depends on OGRAPH *)
(*PolyML.Project.make "expressions/hilb.ML";*)

(*
 * Metrics
 *)

(*PolyML.Project.make "metrics/metric_type.ML"; (* Utils to handle int tuples *)
PolyML.Project.make "metrics/metrics/metric.ML"; (* METRIC signature *)
PolyML.Project.make "metrics/metrics/edge_complexity_metric.ML";
PolyML.Project.make "metrics/metrics/weighted_arity_metric.ML";
PolyML.Project.make "metrics/metrics/sets_cardinals_metric.ML";
PolyML.Project.make "metrics/metrics.ML"; (* Metrics on graphs *) *)


(*
 * Matching
 *)
PolyML.Project.make "matching/match.ML";
PolyML.Project.make "matching/bg_match.ML";
PolyML.Project.make "matching/match_state.ML";

(* piece-by-piece matching utility *)
PolyML.Project.make "matching/progressive_match_search.ML";

(* signature for outer (e.g. !-box) matching *)
PolyML.Project.make "matching/match_search.ML";
(* wrappers for inner_match_search *)
(* pattern-free wrapper (concrete graphs onto concrete graphs) *)
PolyML.Project.make "matching/concrete_match_search.ML";
(* naive pattern-graph wrapper *)
PolyML.Project.make "matching/greedy_match_search.ML";
PolyML.Project.make "matching/bang_graph_homeomorphism_search.ML";


(*
 * Rules
 *)
PolyML.Project.make "rewriting/rule.ML";

(* I/O *)
PolyML.Project.make "io/rule_json.ML";

(* new, combined rule IO struct *)
PolyML.Project.make "io/rule_json_io.ML";


(*
 * Rulesets
 *)
PolyML.Project.make "theories/ruleset.ML";
PolyML.Project.make "theories/ruleset_annotations.ML";

(* Ruleset I/O *)
PolyML.Project.make "io/ruleset_json.ML";
PolyML.Project.make "io/ruleset_annotations_json.ML";

PolyML.Project.make "io/ruleset_json_io.ML";



(*
 * Rewriting
 *)
(* substitution of a matched subgraph for another graph *)
PolyML.Project.make "rewriting/rewriter.ML";
(* substitution used to provide rewriting with rulesets *)
PolyML.Project.make "rewriting/ruleset_rewriter.ML";
(* Heuristic derived data structures *)
PolyML.Project.make "rewriting/heuristic/distancematrix.ML"; (* distance matrix *)
(* I/O *)
PolyML.Project.make "io/rewrite_json.ML";

