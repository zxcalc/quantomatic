theory match
imports graph  
uses

(*
 * Matching
 *)
 "../../core/matching/match.ML"
 "../../core/matching/bg_match.ML"
 "../../core/matching/match_state.ML"

(* piece-by-piece matching utility *)
 "../../core/matching/progressive_match_search.ML"

(* signature for outer (e.g. !-box) matching *)
 "../../core/matching/match_search.ML"
(* wrappers for inner_match_search *)
(* pattern-free wrapper (concrete graphs onto concrete graphs) *)
 "../../core/matching/concrete_match_search.ML"
(* naive pattern-graph wrapper *)
 "../../core/matching/greedy_match_search.ML"
 "../../core/matching/bang_graph_homeomorphism_search.ML"



(*
 * Rules
 *)
 "../../core/rewriting/rule.ML"
 (* I/O *)
 "../../core/io/rule_json.ML"

(* new, combined rule IO struct *)
 "../../core/io/rule_json_io.ML"

(*
 * Rulesets
 *)
 "../../core/theories/ruleset.ML"
 "../../core/theories/ruleset_annotations.ML"

(* Ruleset I/O *)
 "../../core/io/ruleset_json.ML"
 "../../core/io/ruleset_annotations_json.ML"

 "../../core/io/ruleset_json_io.ML"

begin

end;

