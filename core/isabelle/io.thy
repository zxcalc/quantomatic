theory io  
imports Main graph 
uses

(*
 * Matching
 *)
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
 * Deprecated XML I/O stuff
 *)
"~~/contrib/isaplib/General/xml.ML"
 "../../core/io/xml_parse_utils.ML"
 "../../core/io/xml_output_utils.ML"
 "../../core/io/xml_io.ML"
 "../../core/io/reader.ML"
 "../../core/io/writer.ML"
 "../../core/io/linrat_xml.ML"

(* Graph Component Data *)
(* component data is a bit of a hack, and has I/O interdependencies *)
 "../../core/theories/component_data.ML"

(* boilerplate data functors for enumerated datatypes *)
 "../../core/theories/enum_data.ML"

(* data for strings *)
 "../../core/theories/string_data.ML"

(* Graphs *)
 "../../core/io/graph_xml_v2_input.ML"
 "../../core/io/graph_xml_v2_output.ML"
 "../../core/io/graph_annotations_xml_input.ML"
 "../../core/io/graph_annotations_xml_output.ML"
 "../../core/io/rule_xml_output.ML"
 "../../core/io/rule_xml_input.ML"
 "../../core/io/ruleset_annotations_xml_input.ML"
 "../../core/io/ruleset_annotations_xml_output.ML"
 "../../core/io/ruleset_xml_input.ML"
 "../../core/io/ruleset_xml_output.ML"
 "../../core/io/io_interface_xml.ML"

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
 "../../core/theories/graphical_theory.ML"
 "../../core/io/graphical_theory_io.ML"


begin

end;
