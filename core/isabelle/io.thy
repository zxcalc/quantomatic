theory io  
imports Main graph 
uses

(* TODO(ldixon): push the edits to xml.ML back to Isabelle -
re-loading over Isabelle's library means that quanto's XML structure,
and anything that uses it, will be incompatible with the one that is
loaded over it. Alternatively, make an xml2 library. *)
"~~/contrib/isaplib/General/xml.ML"

(* Quantomatic CORE *)

(* Generic output/input tools *)
 "../../core/io/xml_parse_utils.ML"
 "../../core/io/xml_output_utils.ML"
 "../../core/io/input.ML"
 "../../core/io/output.ML"
 "../../core/io/json_io.ML"
 "../../core/io/xml_io.ML"
(*
 "../../core/io/input_xml.ML"
 "../../core/io/output_xml.ML"
 "../../core/io/input_linrat.ML"
 "../../core/io/output_linrat.ML" *)
 "../../core/io/reader.ML"
 "../../core/io/writer.ML"

(* Expressions *)
 "../../core/io/linrat_json.ML"
 "../../core/io/linrat_xml.ML"

(* Graph Component Data *)
(* component data is a bit of a hack, and has I/O interdependencies *)
 "../../core/theories/component_data.ML"

(* boilerplate data functors for enumerated datatypes *)
 "../../core/theories/enum_data.ML"

(* Graphs *)
 "../../core/io/graph_xml_v2_input.ML"
 "../../core/io/graph_xml_v2_output.ML"
 "../../core/io/graph_json.ML"
 "../../core/io/graph_annotations_xml_input.ML"
 "../../core/io/graph_annotations_xml_output.ML"
 "../../core/io/graph_annotations_json.ML"

(* Rules *)
 "../../core/io/rule_xml_output.ML"
 "../../core/io/rule_xml_input.ML"
 "../../core/io/rule_json.ML"

(* Rulesets *)
 "../../core/io/ruleset_xml_input.ML"
 "../../core/io/ruleset_xml_output.ML"
 "../../core/io/ruleset_json.ML"
 "../../core/io/ruleset_annotations_xml_input.ML"
 "../../core/io/ruleset_annotations_xml_output.ML"
 "../../core/io/ruleset_annotations_json.ML"

(* Lists of rewrites *)
 "../../core/io/rewrite_json.ML"

(* Simple dot output for graphs *)
 "../../core/io/graph_dot_output.ML"

(* Package all IO stuff into one place *)
 "../../core/io/io_interface.ML"

(* TO HERE *)


(* basic definition of a rewrite rule (as a pair of graphs) *)
 "../../core/rewriting/rule.ML"
 "../../core/io/output_rule.ML"
 "../../core/io/input_rule.ML"

(* rule sets *)
 "../../core/theories/ruleset.ML"
 "../../core/io/input_ruleset.ML"
 "../../core/io/output_ruleset.ML"
 "../../core/theories/ruleset_annotations.ML"
 "../../core/io/input_ruleset_annotations.ML"
 "../../core/io/output_ruleset_annotations.ML"


(* package all IO stuff into one place *)
 "../../core/io/io_interface.ML"

(* Simple dot output for graphs *)
 "../../core/io/output_graph_dot.ML"


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




(* substitution of a matched subgraph for another graph *)
 "../../core/rewriting/graph_subst.ML"
(* substitution used to provide rewriting with rulesets *)
 "../../core/rewriting/ruleset_rewriter.ML" 

(* Heuristic derived data structures *)
 "../../core/rewriting/heuristic/distancematrix.ML" (* distance matrix *)

(* construction of everything in a graphical theory from just param *)
 "../../core/theories/graphical_theory.ML"

begin

end;
