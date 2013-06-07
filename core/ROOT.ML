(*
 *  ROOT file to compile all libraries and quantomatic core
 *)


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*                          Compile isaplib code                           *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

val rootDir = OS.FileSys.getDir();

(* Include isaplib *)
OS.FileSys.chDir (rootDir ^ "/../../isaplib/");
use "ROOT.ML";
OS.FileSys.chDir rootDir;

PolyML.Project.depend_on_files
  [
   rootDir ^ "/../../isaplib/ROOT.ML",
   rootDir ^ "/ROOT.ML"
  ];



(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*                              Local setup                                *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

(** Useful settings for debugging:   **)

(* PolyML.Compiler.debug := true;     *)
(* PolyML.Project.Log.level_ref := 4; *)
(* Testing.Log.level_ref := 3;        *)
(* print_depth 5                      *)

PolyML.Project.optional_make "local_config.ML";


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*                         Compile quantomatic code                        *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

(* 
 * Utility Code
 *)

(* IO Utils *)
PolyML.Project.make "io/input.ML";
PolyML.Project.make "io/output.ML";
PolyML.Project.make "io/json_io.ML";

(*
 * Names
 *)
PolyML.Project.make "names.ML"; (* defines basic names used in Quantomatic *)


(*
 * Expressions for use in graph component data
 *)
PolyML.Project.make "expressions/lex.ML";
PolyML.Project.make "expressions/coeff.ML";
PolyML.Project.make "expressions/expr.ML"; 
PolyML.Project.make "expressions/linrat_angle_expr.ML";

(* I/O *)
PolyML.Project.make "io/linrat_json.ML";

Testing.make_test "expressions/test/linrat_angle_expr-test.ML";
Testing.make_test "io/test/linrat-json-test.ML";


(*
 * Graphs
 *)
PolyML.Project.make "graph/arity.ML"; (* arity of vertices (in,out,undir) *)
PolyML.Project.make "graph/graph_param.ML";
PolyML.Project.make "graph/basic_graph.ML"; (* basic graphs, just the data *)
PolyML.Project.make "graph/graph.ML"; (* graphs with interesting functions *)
PolyML.Project.make "graph/graph_iso.ML"; (* isomorphism between graphs *)
PolyML.Project.make "graph/overtex.ML"; (* open graphs *)
PolyML.Project.make "graph/ograph_param.ML";
PolyML.Project.make "graph/ograph.ML";
PolyML.Project.make "graph/bang_graph.ML"; (* bang box graphs *)
PolyML.Project.make "graph/bang_graph_iso.ML"; (* isomorphism between !graphs *)
PolyML.Project.make "graph/graph_annotations.ML"; (* graph annotations *)

(* I/O *)
PolyML.Project.make "io/graph_json.ML";
PolyML.Project.make "io/graph_annotations_json.ML";
PolyML.Project.make "io/graph_dot_output.ML";

Testing.make_test "graph/test/test-graph-setup.ML";
Testing.make_test "graph/test/test-graph.ML";
Testing.make_test "graph/test/test-ograph.ML";
Testing.make_test "graph/test/test-bang-graph.ML";
Testing.make_test "graph/test/test-bang-graph-iso.ML";

Testing.make_test "io/test/graph-json-test.ML";


(*
 * Metrics
 *)
PolyML.Project.make "metrics/metric_type.ML"; (* Utils to handle int tuples *)
PolyML.Project.make "metrics/metrics/metric.ML"; (* METRIC signature *)
PolyML.Project.make "metrics/metrics/edge_complexity_metric.ML";
PolyML.Project.make "metrics/metrics/weighted_arity_metric.ML";
PolyML.Project.make "metrics/metrics/sets_cardinals_metric.ML";
PolyML.Project.make "metrics/metrics.ML"; (* Metrics on graphs *)


(*
 * Rules
 *)
PolyML.Project.make "rewriting/rule.ML";

(* I/O *)
PolyML.Project.make "io/rule_json.ML";

Testing.make_test "rewriting/test/rule-test.ML";


(*
 * Rulesets
 *)
PolyML.Project.make "theories/ruleset.ML";
PolyML.Project.make "theories/ruleset_annotations.ML";

(* I/O *)
PolyML.Project.make "io/ruleset_json.ML";
PolyML.Project.make "io/ruleset_annotations_json.ML";

Testing.make_test "theories/test/ruleset-test.ML";


(*
 * Matching
 *)
PolyML.Project.make "matching/bbox_operator.ML";
PolyML.Project.make "matching/match_state.ML";
PolyML.Project.make "matching/bang_graph_match_state.ML";
(* signature for inner (concrete) matching *)
PolyML.Project.make "matching/inner_match_search.ML";
(* signature for outer (e.g. !-box) matching *)
PolyML.Project.make "matching/match_search.ML";
(* simple inner loop for the matching algo *)
PolyML.Project.make "matching/simple_inner_match_search.ML";
(* wrappers for inner_match_search *)
(* pattern-free wrapper (concrete graphs onto concrete graphs) *)
PolyML.Project.make "matching/concrete_match_search.ML";
(* naive pattern-graph wrapper *)
PolyML.Project.make "matching/greedy_match_search.ML";

Testing.make_test "matching/test/test-matching-setup.ML";
Testing.make_test "matching/test/test-bang-match-state.ML";
Testing.make_test "matching/test/test-string-graph-matching.ML";
Testing.make_test "matching/test/test-bang-graph-matching.ML";


(*
 * Deprecated XML I/O stuff
 *)
PolyML.Project.make "io/xml_parse_utils.ML";
PolyML.Project.make "io/xml_output_utils.ML";
PolyML.Project.make "io/xml_io.ML";
PolyML.Project.make "io/reader.ML";
PolyML.Project.make "io/writer.ML";

PolyML.Project.make "io/linrat_xml.ML";

(* Graph Component Data *)
(* component data is a bit of a hack, and has I/O interdependencies *)
PolyML.Project.make "theories/component_data.ML";
(* boilerplate data functors for enumerated datatypes *)
PolyML.Project.make "theories/enum_data.ML";

PolyML.Project.make "io/graph_xml_v2_input.ML";
PolyML.Project.make "io/graph_xml_v2_output.ML";
PolyML.Project.make "io/graph_annotations_xml_input.ML";
PolyML.Project.make "io/graph_annotations_xml_output.ML";
PolyML.Project.make "io/rule_xml_output.ML";
PolyML.Project.make "io/rule_xml_input.ML";
PolyML.Project.make "io/ruleset_annotations_xml_input.ML";
PolyML.Project.make "io/ruleset_annotations_xml_output.ML";
PolyML.Project.make "io/ruleset_xml_input.ML";
PolyML.Project.make "io/ruleset_xml_output.ML";

PolyML.Project.make "io/io_interface_xml.ML";


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

Testing.make_test "rewriting/test/rewriter-test.ML";
Testing.make_test "rewriting/test/ruleset_rewriter-tests.ML";


(*
 * Theories
 *)
(* construction of everything in a graphical theory from just param *)
(* NB: currently relies on XML I/O stuff *)
PolyML.Project.make "theories/graphical_theory.ML";
PolyML.Project.make "io/graphical_theory_io.ML";

(* red-green specific vertices, graphs and matching *)
(* graph-derived expressions for R-G graphs *)
PolyML.Project.make "expressions/alg.ML"; (* algebraic expression utils *)
PolyML.Project.make "expressions/hilb.ML"; (* hilbert space stuff *)
PolyML.Project.make "theories/red_green/vertex.ML";
PolyML.Project.make "theories/red_green/graph.ML";
PolyML.Project.make "theories/red_green/theory.ML";
PolyML.Project.make "theories/red_green/io.ML";

(* ghz-w specific vertices, graphs, and matching *)
PolyML.Project.make "theories/ghz_w/vertex.ML";
PolyML.Project.make "theories/ghz_w/graph.ML";
PolyML.Project.make "theories/ghz_w/theory.ML";
PolyML.Project.make "theories/ghz_w/io.ML";

(* Graphs having vertices with strings as data, substring as matching *)
PolyML.Project.make "theories/substrings/vertex.ML";
PolyML.Project.make "theories/substrings/graph.ML";
PolyML.Project.make "theories/substrings/theory.ML";
PolyML.Project.make "theories/substrings/io.ML";

(* Graphs having strings as types, linrat as data and both substrings and linrat
 * as matching *)
PolyML.Project.make "theories/substr_linrat/vertex.ML";
PolyML.Project.make "theories/substr_linrat/graph.ML";
PolyML.Project.make "theories/substr_linrat/theory.ML";
PolyML.Project.make "theories/substr_linrat/io.ML";
Testing.make_test "theories/substr_linrat/test/test.ML";

(* rgb specific vertices, graphs, and matching *)
PolyML.Project.make "theories/red_green_blue/vertex.ML";
PolyML.Project.make "theories/red_green_blue/graph.ML";
PolyML.Project.make "theories/red_green_blue/theory.ML";
PolyML.Project.make "theories/red_green_blue/io.ML";

(* petri specific vertices, graphs, and matching *)
PolyML.Project.make "theories/petri/vertex.ML";
PolyML.Project.make "theories/petri/graph.ML";
PolyML.Project.make "theories/petri/theory.ML";
PolyML.Project.make "theories/petri/io.ML";

(* Tactics as Graphs in Isabelle *)
PolyML.Project.make "theories/isaplanner_rtechn/vertex.ML";
PolyML.Project.make "theories/isaplanner_rtechn/graph.ML";
PolyML.Project.make "theories/isaplanner_rtechn/theory.ML";
PolyML.Project.make "theories/isaplanner_rtechn/io.ML";


(*
 * Decisions nets
 *)
PolyML.Project.make "dnets/DNetsLib.ML";
PolyML.Project.make "dnets/Literal.ML";
PolyML.Project.make "dnets/Contour.ML";
PolyML.Project.make "dnets/ContourList.ML";
PolyML.Project.make "dnets/TopDNet.ML";

Testing.make_test "dnets/test.ML"; 

(* Performance tests for dnets *)
(* Testing.make_test "dnets/perf.ML"; *)


(*
 * Controller / Interface
 *)
(* Overall controller for theories *)
PolyML.Project.make "interface/controller_state.ML"; (* control state for quanto *)
PolyML.Project.make "interface/controller.ML"; (* commands *)
PolyML.Project.make "interface/controller_registry.ML"; (* theory lists *)

(* interface protocol/console *)
PolyML.Project.make "interface/console_commands.ML"; (* console commands and help *)
PolyML.Project.make "interface/console_lexer.ML"; (* lexer for quanto console *)
PolyML.Project.make "interface/console.ML"; (* generic protocol using commands *)
PolyML.Project.make "interface/console_interface.ML"; (* generic protocol using commands *)
PolyML.Project.make "interface/protocol.ML"; (* protocol for tools *)


(*
 * Top-level testing
 *)
Testing.make_test "test/regression-tests.ML";
(* Metrics tests depend on Str_Theory *)
Testing.make_test "metrics/test/test.ML";
Testing.make_test "example_code/ROOT.ML";



(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*                               Update heaps                              *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

PolyML.Project.Log.log 2 "checking if heap is newer than edits...";

if PolyML.Project.heap_is_newer_than_edits "heaps/quanto_child.heap"
then PolyML.Project.Log.log 2 "NO"
else 
 (PolyML.Project.Log.log 2 "YES...building child heap...";
  PolyML.Project.ensure_at_latest_heap ();
  PolyML.Project.save_project_child_state "heaps/quanto_child.heap");
  
PolyML.Project.Log.log 2 "done.";

