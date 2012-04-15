(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  ROOT file to compile all libraries and quantomatic core  *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(**** Compile isaplib libraries ****)

val rootDir = OS.FileSys.getDir(); 
val _ = OS.FileSys.chDir (rootDir ^ "/../../isaplib/"); 
val _ = use "ROOT.ML"; 
val _ = OS.FileSys.chDir rootDir;
print_depth 3;

val rootDir = OS.FileSys.getDir(); 
PolyML.Project.Log.level_ref := 4;
PolyML.Project.depend_on_files 
  [(* this file *)
   rootDir ^ "/../../isaplib/ROOT.ML",
   rootDir ^ "/ROOT.ML"
   (* TODO: isaplib? *)
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

(*  *)
(* PolyML.Project.make "layout/ograph_layout.ML"; *)

(* Generic output/input tools *)
PolyML.Project.make "io/xml_parse_utils.ML";
PolyML.Project.make "io/xml_output_utils.ML";
PolyML.Project.make "io/input_generic.ML";
PolyML.Project.make "io/output_generic.ML";
PolyML.Project.make "io/input_string.ML";
PolyML.Project.make "io/output_string.ML";
PolyML.Project.make "io/input_linrat.ML";
PolyML.Project.make "io/output_linrat.ML";
PolyML.Project.make "io/reader.ML";
PolyML.Project.make "io/writer.ML";

(* component data needs to go before I/O for graphs *)
PolyML.Project.make "theories/component_data.ML";

(* Storing UI data on graphs, vertices, !-Boxes and edges *)
PolyML.Project.make "interface/user_data.ML";

(* I/O for graphs *)
PolyML.Project.make "io/input_user_data.ML";
PolyML.Project.make "io/output_user_data.ML";
PolyML.Project.make "io/input_graph_v2.ML";
PolyML.Project.make "io/output_graph_v2.ML";
PolyML.Project.make "io/input_ruleset_user_data.ML";

(* Testing *)
(* *)
Testing.make_test "theories/test/vertex-test.ML";
(* These arn't tests, but they are used only by tests. *)
Testing.make_test "io/input_graph_v1.ML";
Testing.make_test "io/output_graph_v1.ML";
(* *)
Testing.make_test "graph/test/graph-test.ML";

(* basic definition of a rewrite rule (as a pair of graphs) *)
PolyML.Project.make "rewriting/rule.ML";
PolyML.Project.make "io/output_rule.ML";
PolyML.Project.make "io/input_rule.ML";

(* rule sets *)
PolyML.Project.make "theories/ruleset.ML";
PolyML.Project.make "io/input_ruleset.ML";
PolyML.Project.make "io/output_ruleset.ML";

(* package all IO stuff into one place *)
PolyML.Project.make "io/io_interface.ML";

(* Simple dot output for graphs *)
PolyML.Project.make "io/output_graph_dot.ML";

(* Testing *)
Testing.make_test "theories/test/ruleset-test.ML";
Testing.make_test "io/test/graph_io-test.ML";

(* matching *)
PolyML.Project.make "matching/bbox_match.ML"; (* match info for bbox graphs *)
PolyML.Project.make "matching/match.ML"; (* a graph matching *)
Testing.make_test "matching/test/match-test.ML"; (* unit/regression tests *)
(* signature for rule match search *)
PolyML.Project.make "matching/rule_match_search.ML";
(* naive match search implementation, find symmetric cases *)
PolyML.Project.make "matching/simple_match_search.ML";
PolyML.Project.make "matching/simple_rule_match_search.ML";
(* searching for matches, but avoiding symmetric ones *)
PolyML.Project.make "matching/symmetry_rule_match_search.ML";
(* substitution of a matched subgraph for another graph *)
PolyML.Project.make "rewriting/graph_subst.ML";
(* substitution used to provide rewriting with rulesets *)
PolyML.Project.make "rewriting/ruleset_rewriting.ML";

(* Testing *)
Testing.make_test "matching/test/simple_rule_match_search-test.ML";
Testing.make_test "matching/test/simple_match_search-test.ML";
Testing.make_test "rewriting/test/ruleset_rewriting-test.ML";
Testing.make_test "matching/test/symmetry_match_search-test.ML";

(* Heuristic derived data structures *)
PolyML.Project.make "rewriting/heuristic/distancematrix.ML"; (* distance matrix *)
PolyML.Project.make "matching/filter.ML"; (* incremental match filter *)

(* construction of everything in a graphical theory from just param *)
PolyML.Project.make "theories/graphical_theory.ML";

(* Overall controller for theories *)
PolyML.Project.make "interface/controller_state.ML"; (* control state for quanto *)
PolyML.Project.make "interface/controller.ML"; (* commands *)

(* interface protocol/console *)
PolyML.Project.make "interface/console_commands.ML"; (* console commands and help *)
PolyML.Project.make "interface/console_lexer.ML"; (* lexer for quanto console *)
PolyML.Project.make "interface/console.ML"; (* generic protocol using commands *)
PolyML.Project.make "interface/console_interface.ML"; (* generic protocol using commands *)
PolyML.Project.make "interface/protocol.ML"; (* protocol for tools *)
PolyML.Project.make "interface/run.ML";

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

(*  *)
PolyML.Project.make "theories/isaplanner_rtechn/vertex.ML";
PolyML.Project.make "theories/isaplanner_rtechn/graph.ML";
PolyML.Project.make "theories/isaplanner_rtechn/theory.ML";
PolyML.Project.make "theories/isaplanner_rtechn/io.ML";

(* Testing *)
Testing.make_test "io/test/load_ruleset-test.ML";
Testing.make_test "rewriting/test/rule-test.ML";

(* Test to make sure example code for playing with graphs at the ML level 
   are working. *)
Testing.make_test "theories/substr_linrat/test/test.ML";

Testing.make_test "example_code/ROOT.ML";

PolyML.Project.Log.log 2 "checking if heap is newer than edits...";

if PolyML.Project.heap_is_newer_than_edits "heaps/quanto_child.heap"
then PolyML.Project.Log.log 2 "NO"
else 
 (PolyML.Project.Log.log 2 "YES...building child heap...";
  PolyML.Project.ensure_at_latest_heap ();
  PolyML.Project.save_project_child_state "heaps/quanto_child.heap");
  
PolyML.Project.Log.log 2 "done.";
