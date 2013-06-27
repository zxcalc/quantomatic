theory io  
imports Main graph 
uses

(* Quantomatic CORE *)

(* Generic output/input tools *)
 "../../core/io/input.ML"
 "../../core/io/output.ML"
 "../../core/io/json_io.ML"

(* Expressions *)
 "../../core/io/linrat_json.ML"

(* Graph Component Data *)
(* component data is a bit of a hack, and has I/O interdependencies *)
 "../../core/theories/component_data.ML"

(* boilerplate data functors for enumerated datatypes *)
 "../../core/theories/enum_data.ML"

(* Graphs *)
 "../../core/io/graph_json.ML"
 "../../core/io/graph_annotations_json.ML"

(* Rules *)
 "../../core/io/rule_json.ML"

(* Rulesets *)
 "../../core/io/ruleset_json.ML"
 "../../core/io/ruleset_annotations_json.ML"

(* Lists of rewrites *)
 "../../core/io/rewrite_json.ML"

(* Simple dot output for graphs *)
 "../../core/io/graph_dot_output.ML"

(* matching *)
 "../../core/matching/match_state.ML"
 "../../core/matching/bang_graph_match_state.ML"
(* signature for inner (concrete) matching *)
 "../../core/matching/inner_match_search.ML"
(* signature for outer (e.g. !-box) matching *)
 "../../core/matching/match_search.ML"
(* simple inner loop for the matching algo *)
 "../../core/matching/simple_inner_match_search.ML"
(* wrappers for inner_match_search *)
(* pattern-free wrapper (concrete graphs onto concrete graphs) *)
 "../../core/matching/concrete_match_search.ML"
(* naive pattern-graph wrapper *)
 "../../core/matching/greedy_match_search.ML"

(*
 * Rewriting
 *)

(* substitution of a matched subgraph for another graph *)
 "../../core/rewriting/graph_subst.ML"
(* substitution used to provide rewriting with rulesets *)
 "../../core/rewriting/ruleset_rewriter.ML" 

(* Heuristic derived data structures *)
 "../../core/rewriting/heuristic/distancematrix.ML" (* distance matrix *)











(* construction of everything in a graphical theory from just param *)
 "../../core/theories/graphical_theory.ML"
 "../../core/io/graphical_theory_io.ML"

(*
 "../../core/io/output_graph_dot.ML"
*)
begin

end;
