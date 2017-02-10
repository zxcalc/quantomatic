theory rewriting
imports match  
uses
(*
 * Rewriting
 *)
(* substitution of a matched subgraph for another graph *)
 "../../core/rewriting/rewriter.ML"
(* substitution used to provide rewriting with rulesets *)
 "../../core/rewriting/ruleset_rewriter.ML"
(* Heuristic derived data structures *)
 "../../core/rewriting/heuristic/distancematrix.ML" (* distance matrix *)
(* I/O *)
 "../../core/io/rewrite_json.ML"

(* construction of everything in a graphical theory from just param *)
 "../../core/io/graph_component_io.ML"
 "../../core/theories/graphical_theory.ML"
 "../../core/io/graphical_theory_io.ML"



begin

end;

