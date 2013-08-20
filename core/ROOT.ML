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

(*PolyML.Project.Log.level_ref := 4;*)
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
(*                         Compile quantomatic core                        *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

(* 
 * Utility Code
 *)

(* IO Utils *)
PolyML.Project.make "io/input.ML";
PolyML.Project.make "io/output.ML";
PolyML.Project.make "io/json_io.ML";
PolyML.Project.make "io/file_io.ML";

(*
 * Names
 *)
PolyML.Project.make "names.ML"; (* defines basic names used in Quantomatic *)


(*
 * Expressions for use in graph component data
 *)
PolyML.Project.make "expressions/lex.ML";
PolyML.Project.make "expressions/coeff.ML";
PolyML.Project.make "expressions/matrix.ML";
PolyML.Project.make "expressions/expr.ML"; 
PolyML.Project.make "expressions/linrat_angle_expr.ML";
PolyML.Project.make "expressions/semiring.ML";
PolyML.Project.make "expressions/tensor.ML";
PolyML.Project.make "expressions/linrat_matcher.ML";

PolyML.Project.make "expressions/alg.ML"; (* algebraic expression utils *)

(* I/O *)
PolyML.Project.make "io/linrat_json.ML";

Testing.make_test "expressions/test/linrat_angle_expr-test.ML";
Testing.make_test "expressions/test/linrat_matcher-test.ML";
Testing.make_test "io/test/linrat-json-test.ML";


(*
 * Graphs
 *)
PolyML.Project.make "graph/arity.ML"; (* arity of vertices (in,out,undir) *)

PolyML.Project.make "graph/graph_data.ML";
PolyML.Project.make "graph/ograph.sig.ML";
PolyML.Project.make "graph/bang_graph.sig.ML";
PolyML.Project.make "graph/bang_graph.ML";
PolyML.Project.make "graph/bang_graph_iso.ML";

PolyML.Project.make "graph/graph_annotations.ML"; (* graph annotations *)

(* I/O *)
PolyML.Project.make "io/graph_json.ML";
PolyML.Project.make "io/graph_annotations_json.ML";
PolyML.Project.make "io/graph_dot_output.ML";

Testing.make_test "graph/test/test-graph-setup.ML";
Testing.make_test "graph/test/test-ograph.ML";
Testing.make_test "graph/test/test-bang-graph.ML";

Testing.make_test "io/test/graph-json-test.ML";

(*
 * Misc stuff
 *)
PolyML.Project.make "stub_run.ML"; (* ? *)
(* hilbert space stuff: depends on OGRAPH *)
PolyML.Project.make "expressions/hilb.ML";

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
 * Matching
 *)
PolyML.Project.make "matching/match.ML";
PolyML.Project.make "matching/bg_match.ML";
PolyML.Project.make "matching/match_state.ML";

(* piece-by-piece matching utility *)
PolyML.Project.make "matching/progressive_match_search.ML";

(* signature for outer (e.g. !-box) matching *)
PolyML.Project.make "matching/match_search.ML";
(* wrappers for inner_match_search *)
(* pattern-free wrapper (concrete graphs onto concrete graphs) *)
PolyML.Project.make "matching/concrete_match_search.ML";
(* naive pattern-graph wrapper *)
PolyML.Project.make "matching/greedy_match_search.ML";
PolyML.Project.make "matching/bang_graph_homeomorphism_search.ML";

Testing.make_test "matching/test/test-matching-setup.ML";
Testing.make_test "matching/test/test-string-graph-matching.ML";
Testing.make_test "matching/test/test-bang-graph-matching.ML";
Testing.make_test "matching/test/test-bang-graph-homeomorphism-search.ML";


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

(* Ruleset I/O *)
PolyML.Project.make "io/ruleset_json.ML";
PolyML.Project.make "io/ruleset_annotations_json.ML";

Testing.make_test "theories/test/ruleset-test.ML";


(*
 * Rewriting
 *)
(* a structure which stores a rule and a single application of the rule *)
PolyML.Project.make "rewriting/rewrite.ML";
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
PolyML.Project.make "theories/graphical_theory.ML";
PolyML.Project.make "io/graphical_theory_io.ML";

(* string vertex/edge graphs *)
PolyML.Project.make "theories/string_ve/theory.ML";
PolyML.Project.make "theories/string_ve/io.ML";
PolyML.Project.make "theories/string_ve/test/test.ML";

(* red-green specific vertices, graphs and matching *)
(* graph-derived expressions for R-G graphs *)
PolyML.Project.make "theories/red_green/theory.ML";
PolyML.Project.make "theories/red_green/io.ML";
Testing.make_test "theories/red_green/test/test.ML";

(* ghz-w specific vertices, graphs, and matching *)
PolyML.Project.make "theories/ghz_w/theory.ML";
PolyML.Project.make "theories/ghz_w/io.ML";
Testing.make_test "theories/ghz_w/test/test.ML";

(* Graphs having vertices with strings as data, substring as matching *)
PolyML.Project.make "theories/substrings/theory.ML";
PolyML.Project.make "theories/substrings/io.ML";
Testing.make_test "theories/substrings/test/test.ML";

(* Graphs having strings as types, linrat as data and both substrings and linrat
 * as matching *)
PolyML.Project.make "theories/substr_linrat/theory.ML";
PolyML.Project.make "theories/substr_linrat/io.ML";
Testing.make_test "theories/substr_linrat/test/test.ML";

(* rgb specific vertices, graphs, and matching *)
PolyML.Project.make "theories/red_green_blue/theory.ML";
PolyML.Project.make "theories/red_green_blue/io.ML";
Testing.make_test "theories/red_green_blue/test/test.ML";

(* petri specific vertices, graphs, and matching *)
PolyML.Project.make "theories/petri/theory.ML";
PolyML.Project.make "theories/petri/io.ML";
Testing.make_test "theories/petri/test/test.ML";

(* Tactics as Graphs in Isabelle *)
PolyML.Project.make "theories/isaplanner_rtechn/theory.ML";
PolyML.Project.make "theories/isaplanner_rtechn/io.ML";
Testing.make_test "theories/isaplanner_rtechn/test/test.ML";


(*
 * Descrimination nets
 *)
PolyML.Project.make "dnets/DNetsLib.ML";
PolyML.Project.make "dnets/Literal.ML";
PolyML.Project.make "dnets/Contour.ML";
PolyML.Project.make "dnets/ContourList.ML";
PolyML.Project.make "dnets/TopDNet.ML";

Testing.make_test "dnets/test.ML"; 

(* Performance tests for dnets *)
(*Testing.make_test "dnets/perf.ML";*)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*                          Compile quantocosy                             *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)


(* Include cosy *)
val rootDir = OS.FileSys.getDir();
OS.FileSys.chDir (rootDir ^ "/../cosy/");
use "ROOT.ML";
OS.FileSys.chDir rootDir;
PolyML.Project.depend_on_files [rootDir ^ "/../cosy/ROOT.ML"];



(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*                          Compile the controller                         *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)


(* Overall controller for theories *)
PolyML.Project.make "interface/controller_state.ML"; (* control state for quanto *)
PolyML.Project.make "interface/controller.ML"; (* commands *)
PolyML.Project.make "interface/controller_registry.ML"; (* theory lists *)

(* interface protocol/console *)
PolyML.Project.make "interface/control_interface.ML"; (* generic interface for run_in_textstreams *)

PolyML.Project.make "interface/console_commands.ML"; (* console commands and help *)
PolyML.Project.make "interface/console_lexer.ML"; (* lexer for quanto console *)
PolyML.Project.make "interface/console.ML"; (* generic protocol using commands *)
PolyML.Project.make "interface/console_interface.ML"; (* generic protocol using commands *)
PolyML.Project.make "interface/protocol.ML"; (* protocol for tools *)



(* new modular controller *)

PolyML.Project.make "json_interface/controller_util.ML";
PolyML.Project.make "json_interface/controller_module.ML";
PolyML.Project.make "json_interface/modules/test.ML";
PolyML.Project.make "json_interface/controller.ML";
PolyML.Project.make "json_interface/controller_registry.ML";
PolyML.Project.make "json_interface/protocol.ML";
PolyML.Project.make "json_interface/run.ML";


(*
 * Top-level testing
 *)
Testing.make_test "test/old-rg-ruleset-rewriter-tests.ML";
Testing.make_test "test/regression-tests.ML";
(* Metrics tests depend on Substrings_Theory *)
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

