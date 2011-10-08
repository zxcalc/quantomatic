theory io
imports Main graph
uses

(* Quantomatic CORE *)

(* Generic output/input tools *)
 "../../../../quantomatic/core/io/xml_parse_utils.ML"
 "../../../../quantomatic/core/io/xml_output_utils.ML"
 "../../../../quantomatic/core/io/input_generic.ML"
 "../../../../quantomatic/core/io/output_generic.ML"
 "../../../../quantomatic/core/io/input_linrat.ML"
 "../../../../quantomatic/core/io/output_linrat.ML"
 "../../../../quantomatic/core/io/reader.ML"
 "../../../../quantomatic/core/io/writer.ML"

(* component data needs to go before I/O for graphs *)
 "../../../../quantomatic/core/theories/component_data.ML"

(* I/O for graphs *)
 "../../../../quantomatic/core/io/input_graph_v2.ML"
 "../../../../quantomatic/core/io/output_graph_v2.ML"

(* basic definition of a rewrite rule (as a pair of graphs) *)
 "../../../../quantomatic/core/rewriting/rule.ML"
 "../../../../quantomatic/core/io/output_rule.ML"
 "../../../../quantomatic/core/io/input_rule.ML"

(* rule sets *)
 "../../../../quantomatic/core/theories/ruleset.ML"
 "../../../../quantomatic/core/io/input_ruleset.ML"
 "../../../../quantomatic/core/io/output_ruleset.ML"

(* package all IO stuff into one place *)
 "../../../../quantomatic/core/io/io_interface.ML"

(* Simple dot output for graphs *)
 "../../../../quantomatic/core/io/output_graph_dot.ML"

(* matching *)
 "../../../../quantomatic/core/matching/bbox_match.ML" (* match info for bbox graphs *)
 "../../../../quantomatic/core/matching/match.ML" (* a graph matching *)

(* signature for rule match search *)
 "../../../../quantomatic/core/matching/rule_match_search.ML"
(* naive match search implementation, find symmetric cases *)
 "../../../../quantomatic/core/matching/simple_match_search.ML"
 "../../../../quantomatic/core/matching/simple_rule_match_search.ML"
(* searching for matches, but avoiding symmetric ones *)
 "../../../../quantomatic/core/matching/symmetry_rule_match_search.ML"
(* substitution of a matched subgraph for another graph *)
 "../../../../quantomatic/core/rewriting/graph_subst.ML"
(* substitution used to provide rewriting with rulesets *)
 "../../../../quantomatic/core/rewriting/ruleset_rewriting.ML"

(* Heuristic derived data structures *)
 "../../../../quantomatic/core/rewriting/heuristic/distancematrix.ML" (* distance matrix *)
 "../../../../quantomatic/core/matching/filter.ML" (* incremental match filter *)

(* construction of everything in a graphical theory from just param *)
 "../../../../quantomatic/core/theories/graphical_theory.ML"

(* red-green specific vertices, graphs and matching *)
 "../../../../quantomatic/core/theories/red_green/vertex.ML"

 (* testing required in input_graph_v1 *)
 "../../../../quantomatic/core/theories/test/vertex-test.ML"

(* I/O for old RG-graphs, depends on defining RG_VertexData *)
 "../../../../quantomatic/core/io/input_graph_v1.ML"
 "../../../../quantomatic/core/io/output_graph_v1.ML"
begin

end;
