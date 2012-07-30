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

(* Storing UI data on graphs, vertices, !-Boxes and edges *)
 "../../core/interface/user_data.ML"

(* I/O for graphs *)
 "../../core/io/input_user_data.ML"
 "../../core/io/output_user_data.ML"
 "../../core/io/input_graph_v2.ML"
 "../../core/io/output_graph_v2.ML"
 "../../core/io/input_ruleset_user_data.ML"

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
