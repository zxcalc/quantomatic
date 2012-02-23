theory io
imports Main graph
uses

(* Quantomatic CORE *)

(* Generic output/input tools *)
 "../../core/io/xml_parse_utils.ML"
 "../../core/io/xml_output_utils.ML"
 "../../core/io/input_generic.ML"
 "../../core/io/output_generic.ML"
 "../../core/io/input_string.ML"
 "../../core/io/output_string.ML"
 "../../core/io/input_linrat.ML"
 "../../core/io/output_linrat.ML"
 "../../core/io/reader.ML"
 "../../core/io/writer.ML"

(* component data needs to go before I/O for graphs *)
 "../../core/theories/component_data.ML"

(* I/O for graphs *)
 "../../core/io/output_user_data.ML"
 "../../core/io/input_graph_v2.ML"
 "../../core/io/output_graph_v2.ML"

(* basic definition of a rewrite rule (as a pair of graphs) *)
 "../../core/rewriting/rule.ML"
 "../../core/io/output_rule.ML"
 "../../core/io/input_rule.ML"

(* rule sets *)
 "../../core/theories/ruleset.ML"
 "../../core/io/input_ruleset.ML"
 "../../core/io/output_ruleset.ML"

(* package all IO stuff into one place *)
 "../../core/io/io_interface.ML"

(* Simple dot output for graphs *)
 "../../core/io/output_graph_dot.ML"

(* matching *)
 "../../core/matching/bbox_match.ML" (* match info for bbox graphs *)
 "../../core/matching/match.ML" (* a graph matching *)

(* signature for rule match search *)
 "../../core/matching/rule_match_search.ML"
(* naive match search implementation, find symmetric cases *)
 "../../core/matching/simple_match_search.ML"
 "../../core/matching/simple_rule_match_search.ML"
(* searching for matches, but avoiding symmetric ones *)
 "../../core/matching/symmetry_rule_match_search.ML"
(* substitution of a matched subgraph for another graph *)
 "../../core/rewriting/graph_subst.ML"
(* substitution used to provide rewriting with rulesets *)
 "../../core/rewriting/ruleset_rewriting.ML"

(* Heuristic derived data structures *)
 "../../core/rewriting/heuristic/distancematrix.ML" (* distance matrix *)
 "../../core/matching/filter.ML" (* incremental match filter *)

(* construction of everything in a graphical theory from just param *)
 "../../core/theories/graphical_theory.ML"


(* OK TO HERE *)
(*
(* red-green specific vertices, graphs and matching *)
 "../../core/theories/red_green/vertex.ML"

 (* testing required in input_graph_v1 *)
 "../../core/theories/test/vertex-test.ML"

(* I/O for old RG-graphs, depends on defining RG_VertexData *)
 "../../core/io/input_graph_v1.ML"
 "../../core/io/output_graph_v1.ML"
*)
begin

end;
