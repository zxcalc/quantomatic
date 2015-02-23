theory core
imports lib
begin

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*                         Compile quantomatic core                        *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

(* 
 * Utility Code
 *)

(* IO Utils *)
ML_file "io/input.ML";
ML_file "io/output.ML";
ML_file "io/json_io.ML";
ML_file "io/file_io.ML";


(*
 * Names
 *)
ML_file "names.ML"; (* defines basic names used in Quantomatic *)


(*
 * Expressions for use in graph component data
 *)
ML_file "expressions/lex.ML";
ML_file "expressions/coeff.ML";
ML_file "expressions/matrix.ML";
ML_file "expressions/expr.ML";
ML_file "expressions/linrat_expr.ML";
ML_file "expressions/linrat_angle_expr.ML";
ML_file "expressions/semiring.ML";
ML_file "expressions/tensor.ML";
ML_file "expressions/linrat_angle_matcher.ML";
ML_file "expressions/linrat_matcher.ML";

ML_file "expressions/alg.ML"; (* algebraic expression utils *)

(* I/O *)
ML_file "io/linrat_json.ML";



(*
 * Graphs
 *)

(* arity of vertices (in,out,undir) *)
ML_file "graph/arity.ML";
(* neighbourhood data for non-commutative vertices *)
ML_file "graph/nhd.ML"; 


ML_file "graph/graph_data.ML";
ML_file "graph/ograph.sig.ML";
ML_file "graph/bang_graph.sig.ML";
ML_file "graph/bang_graph.ML";

ML_file "graph/graph_annotations.ML"; (* graph annotations *)

(* I/O *)
ML_file "io/graph_json.ML";
ML_file "io/graph_annotations_json.ML";
ML_file "io/graph_dot_output.ML";

(* new, combined IO struct *)
ML_file "io/graph_json_io.ML";



(*
 * Matching
 *)
ML_file "matching/match.ML";
ML_file "matching/bg_match.ML";
ML_file "matching/match_state.ML";

(* piece-by-piece matching utility *)
ML_file "matching/progressive_match_search.ML";

(* signature for outer (e.g. !-box) matching *)
ML_file "matching/match_search.ML";
(* wrappers for inner_match_search *)
(* pattern-free wrapper (concrete graphs onto concrete graphs) *)
ML_file "matching/concrete_match_search.ML";
(* naive pattern-graph wrapper *)
ML_file "matching/greedy_match_search.ML";
ML_file "matching/bang_graph_homeomorphism_search.ML";


(*
 * Rules
 *)
ML_file "rewriting/rule.ML";

(* I/O *)
ML_file "io/rule_json.ML";

(* new, combined rule IO struct *)
ML_file "io/rule_json_io.ML";


(*
 * Rulesets
 *)
ML_file "theories/ruleset.ML";
ML_file "theories/ruleset_annotations.ML";

(* Ruleset I/O *)
ML_file "io/ruleset_json.ML";
ML_file "io/ruleset_annotations_json.ML";

ML_file "io/ruleset_json_io.ML";



(*
 * Rewriting
 *)
(* substitution of a matched subgraph for another graph *)
ML_file "rewriting/rewriter.ML";
(* substitution used to provide rewriting with rulesets *)
ML_file "rewriting/ruleset_rewriter.ML";
(* Heuristic derived data structures *)
ML_file "rewriting/heuristic/distancematrix.ML"; (* distance matrix *)
(* I/O *)
ML_file "io/rewrite_json.ML";

(*
 * Theories
 *)
(* construction of everything in a graphical theory from just param *)
ML_file "io/graph_component_io.ML";
ML_file "theories/graphical_theory.ML";
ML_file "io/graphical_theory_io.ML";

ML_file "dnets/DNetsLib.ML";
ML_file "dnets/Literal.ML";
ML_file "dnets/Contour.ML";
ML_file "dnets/ContourList.ML";
ML_file "dnets/TopDNet.ML";

(*Testing.make_test "dnets/test.ML"; *)

end
