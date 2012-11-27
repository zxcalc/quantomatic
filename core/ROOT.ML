(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  ROOT file to compile all libraries and quantomatic core  *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(**** Compile isaplib libraries ****)

val rootDir = OS.FileSys.getDir();

(* Include isaplib *)
OS.FileSys.chDir (rootDir ^ "/../../isaplib/");
use "ROOT.ML";
OS.FileSys.chDir rootDir;

print_depth 3;

PolyML.Project.Log.level_ref := 4;
PolyML.Project.depend_on_files
  [
   rootDir ^ "/../../isaplib/ROOT.ML",
   rootDir ^ "/ROOT.ML"
  ];

(** For debugging, turn on debug flag: **)
(* PolyML.Compiler.debug := true; *)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(**** generic testing stuff / move to isaplib? ****)
PolyML.Project.optional_make "local_config.ML";

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(**** Compile quantomatic code ****)
PolyML.Project.make "names.ML"; (* defines basic names used in Quantomatic *)

(* linear arithmetic expressions: data inside verticies *)
PolyML.Project.make "expressions/lex.ML";
PolyML.Project.make "expressions/coeff.ML";
PolyML.Project.make "expressions/expr.ML"; 
PolyML.Project.make "expressions/linrat_angle_expr.ML";

(* Testing *)
Testing.make_test "expressions/test/linrat_angle_expr-test.ML";

(* Graphs *)
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

(* Metrics *)
PolyML.Project.make "metrics/metric_type.ML"; (* Utils to handle int tuples *)
PolyML.Project.make "metrics/metrics/metric.ML"; (* METRIC signature *)
PolyML.Project.make "metrics/metrics/edge_complexity_metric.ML";
PolyML.Project.make "metrics/metrics/weighted_arity_metric.ML";
PolyML.Project.make "metrics/metrics/sets_cardinals_metric.ML";
PolyML.Project.make "metrics/metrics.ML"; (* Metrics on graphs *)
(*  *)
(* PolyML.Project.make "layout/ograph_layout.ML"; *)

(* Generic output/input tools *)
PolyML.Project.make "io/xml_parse_utils.ML";
PolyML.Project.make "io/xml_output_utils.ML";
PolyML.Project.make "io/input.ML";
PolyML.Project.make "io/output.ML";
PolyML.Project.make "io/json_io.ML";
PolyML.Project.make "io/xml_io.ML";
PolyML.Project.make "io/linrat_json.ML";
PolyML.Project.make "io/linrat_xml.ML";
PolyML.Project.make "io/reader.ML";
PolyML.Project.make "io/writer.ML";

Testing.make_test "io/test/linrat-json-test.ML";

(* component data needs to go before I/O for graphs *)
PolyML.Project.make "theories/component_data.ML";

(* boilerplate data functors for enumerated datatypes *)
PolyML.Project.make "theories/enum_data.ML";

(* I/O for graphs *)
PolyML.Project.make "io/graph_xml_v2_input.ML";
PolyML.Project.make "io/graph_xml_v2_output.ML";
PolyML.Project.make "io/graph_json.ML";
PolyML.Project.make "io/graph_annotations_xml_input.ML";
PolyML.Project.make "io/graph_annotations_xml_output.ML";
PolyML.Project.make "io/graph_annotations_json.ML";

(* Testing *)
(* *)
Testing.make_test "theories/test/vertex-test.ML";
(* These aren't tests, but they are used only by tests. *)
Testing.make_test "io/graph_xml_v1_input.ML";
Testing.make_test "io/graph_xml_v1_output.ML";
(* *)
Testing.make_test "graph/test/test-graph-setup.ML";
Testing.make_test "graph/test/test-graph.ML";
Testing.make_test "graph/test/test-ograph.ML";
Testing.make_test "graph/test/test-bang-graph.ML";
Testing.make_test "graph/test/test-bang-graph-iso.ML";

Testing.make_test "io/test/graph-json-test.ML";

(* basic definition of a rewrite rule (as a pair of graphs) *)
PolyML.Project.make "rewriting/rule.ML";
PolyML.Project.make "io/rule_xml_output.ML";
PolyML.Project.make "io/rule_xml_input.ML";
PolyML.Project.make "io/rule_json.ML";

(* rule sets *)
PolyML.Project.make "theories/ruleset.ML";
PolyML.Project.make "theories/ruleset_annotations.ML";

PolyML.Project.make "io/ruleset_xml_input.ML";
PolyML.Project.make "io/ruleset_xml_output.ML";
PolyML.Project.make "io/ruleset_json.ML";
PolyML.Project.make "io/ruleset_annotations_xml_input.ML";
PolyML.Project.make "io/ruleset_annotations_xml_output.ML";
PolyML.Project.make "io/ruleset_annotations_json.ML";

(* rewrites *)
PolyML.Project.make "io/rewrite_json.ML";

(* package all IO stuff into one place *)
PolyML.Project.make "io/io_interface.ML";

(* Simple dot output for graphs *)
PolyML.Project.make "io/graph_dot_output.ML";

(* Testing *)
Testing.make_test "theories/test/ruleset-test.ML";
Testing.make_test "io/test/graph_io-test.ML";


(* matching *)
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


(* substitution of a matched subgraph for another graph *)
PolyML.Project.make "rewriting/graph_subst.ML";
(* substitution used to provide rewriting with rulesets *)
PolyML.Project.make "rewriting/ruleset_rewriter.ML";


(* Testing *)
Testing.make_test "matching/test/test-matching-setup.ML";
Testing.make_test "matching/test/test-bang-match-state.ML";
Testing.make_test "matching/test/test-matching-concrete.ML";
Testing.make_test "matching/test/test-matching-greedy.ML";

Testing.make_test "rewriting/test/graph_subst-test.ML";
Testing.make_test "rewriting/test/ruleset_rewriting-test.ML";

(* Heuristic derived data structures *)
PolyML.Project.make "rewriting/heuristic/distancematrix.ML"; (* distance matrix *)

(* construction of everything in a graphical theory from just param *)
PolyML.Project.make "theories/graphical_theory.ML";
PolyML.Project.make "io/graphical_theory_io.ML";

(* more expression code; this time expresions derived from graphs, 
   e.g. for interaction with Mathematica/Maple/etc. *)
(* These depend on the red-green theory for now *)
PolyML.Project.make "expressions/alg.ML"; (* algebraic expression utils *)
PolyML.Project.make "expressions/hilb.ML"; (* hilbert space stuff *)

(* red-green specific vertices, graphs and matching *)
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

(*  *)
PolyML.Project.make "theories/isaplanner_rtechn/vertex.ML";
PolyML.Project.make "theories/isaplanner_rtechn/graph.ML";
PolyML.Project.make "theories/isaplanner_rtechn/theory.ML";
PolyML.Project.make "theories/isaplanner_rtechn/io.ML";

(* Overall controller for theories *)
(* PolyML.Project.make "interface/controller_state.ML"; (* control state for quanto *)
PolyML.Project.make "interface/controller.ML"; (* commands *)
PolyML.Project.make "interface/controller_registry.ML"; (* theory lists *)
*)
(* OLD interface protocol/console *)
(* PolyML.Project.make "interface/console_commands.ML"; (* console commands and help *)
PolyML.Project.make "interface/console_lexer.ML"; (* lexer for quanto console *)
PolyML.Project.make "interface/console.ML"; (* generic protocol using commands *)
PolyML.Project.make "interface/console_interface.ML"; (* generic protocol using commands *)
PolyML.Project.make "interface/protocol.ML"; (* protocol for tools *)
*)

(* new modular controller *)
PolyML.Project.make "json_interface/controller_util.ML";
PolyML.Project.make "json_interface/controller_module.ML";
PolyML.Project.make "json_interface/modules/test.ML";
PolyML.Project.make "json_interface/controller.ML";
PolyML.Project.make "json_interface/controller_registry.ML";
PolyML.Project.make "json_interface/protocol.ML";
PolyML.Project.make "json_interface/run.ML";

(* Testing *)
Testing.make_test "io/test/load_ruleset-test.ML";
Testing.make_test "rewriting/test/rule-test.ML";
Testing.make_test "test/regression-tests.ML";

(* Test to make sure example code for playing with graphs at the ML level 
   are working. *)
Testing.make_test "theories/substr_linrat/test/test.ML";

(* Testing for Metrics *)
Testing.make_test "metrics/test/test.ML";
Testing.make_test "example_code/ROOT.ML";

PolyML.Project.Log.log 2 "checking if heap is newer than edits...";

if PolyML.Project.heap_is_newer_than_edits "heaps/quanto_child.heap"
then PolyML.Project.Log.log 2 "NO"
else 
 (PolyML.Project.Log.log 2 "YES...building child heap...";
  PolyML.Project.ensure_at_latest_heap ();
  PolyML.Project.save_project_child_state "heaps/quanto_child.heap");
  
PolyML.Project.Log.log 2 "done.";
