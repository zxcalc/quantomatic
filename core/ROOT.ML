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


(* quanto-lib contains all of core graph/rule/match/rewrite
 * functionality of quantomatic *)

PolyML.Project.make "quanto-lib.ML";


(* tests from quanto-lib files *)

Testing.make_test "expressions/test/linrat_angle_expr-test.ML";
Testing.make_test "expressions/test/linrat_matcher-test.ML";
Testing.make_test "io/test/linrat-json-test.ML";
Testing.make_test "graph/test/test-graph-setup.ML";
Testing.make_test "graph/test/test-nhd.ML";
Testing.make_test "graph/test/test-ograph.ML";
Testing.make_test "graph/test/test-bang-graph.ML";
Testing.make_test "io/test/graph-json-test.ML";
Testing.make_test "matching/test/test-matching-setup.ML";
Testing.make_test "matching/test/test-sg-to-sg-matching.ML";
Testing.make_test "matching/test/test-bg-to-sg-matching.ML";
Testing.make_test "matching/test/test-bg-to-bg-matching.ML";
Testing.make_test "matching/test/test-concrete-matcher.ML";
Testing.make_test "matching/test/test-greedy-matcher.ML";
Testing.make_test "matching/test/test-bang-graph-homeomorphism-search.ML";
Testing.make_test "rewriting/test/rule-test.ML";
Testing.make_test "theories/test/ruleset-test.ML";
Testing.make_test "rewriting/test/rewriter-test.ML";
Testing.make_test "rewriting/test/ruleset_rewriter-tests.ML";

(*
 * Theories
 *)
(* construction of everything in a graphical theory from just param *)
PolyML.Project.make "io/graph_component_io.ML";
PolyML.Project.make "theories/graphical_theory.ML";
PolyML.Project.make "io/graphical_theory_io.ML";

(* string vertex/edge graphs *)
PolyML.Project.make "theories/string_ve/data.ML";
PolyML.Project.make "theories/string_ve/io.ML";
PolyML.Project.make "theories/string_ve/theory.ML";
PolyML.Project.make "theories/string_ve/test/test.ML";

(* red-green specific vertices, graphs and matching *)
(* graph-derived expressions for R-G graphs *)
PolyML.Project.make "theories/red_green/data.ML";
PolyML.Project.make "theories/red_green/io.ML";
PolyML.Project.make "theories/red_green/theory.ML";
Testing.make_test "theories/red_green/test/test.ML";
Testing.make_test "theories/red_green/rg_mathematica.ML";

(* ghz-w specific vertices, graphs, and matching *)
PolyML.Project.make "theories/ghz_w/data.ML";
PolyML.Project.make "theories/ghz_w/io.ML";
PolyML.Project.make "theories/ghz_w/theory.ML";
Testing.make_test "theories/ghz_w/test/test.ML";

(* Graphs having vertices with strings as data, substring as matching *)
PolyML.Project.make "theories/substrings/data.ML";
PolyML.Project.make "theories/substrings/io.ML";
PolyML.Project.make "theories/substrings/theory.ML";
Testing.make_test "theories/substrings/test/test.ML";

(* Graphs having strings as types, linrat as data and both substrings and linrat
 * as matching *)
PolyML.Project.make "theories/substr_linrat/data.ML";
PolyML.Project.make "theories/substr_linrat/io.ML";
PolyML.Project.make "theories/substr_linrat/theory.ML";
Testing.make_test "theories/substr_linrat/test/test.ML";

(* rgb specific vertices, graphs, and matching *)
PolyML.Project.make "theories/red_green_blue/data.ML";
PolyML.Project.make "theories/red_green_blue/io.ML";
PolyML.Project.make "theories/red_green_blue/theory.ML";
Testing.make_test "theories/red_green_blue/test/test.ML";

(* petri specific vertices, graphs, and matching *)
PolyML.Project.make "theories/petri/data.ML";
PolyML.Project.make "theories/petri/io.ML";
PolyML.Project.make "theories/petri/theory.ML";
Testing.make_test "theories/petri/test/test.ML";

(* Tactics as Graphs in Isabelle *)
PolyML.Project.make "theories/isaplanner_rtechn/data.ML";
PolyML.Project.make "theories/isaplanner_rtechn/io.ML";
PolyML.Project.make "theories/isaplanner_rtechn/theory.ML";
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
(* PolyML.Project.make "interface/controller_state.ML"; (* control state for quanto *)
PolyML.Project.make "interface/controller.ML"; (* commands *)
PolyML.Project.make "interface/controller_registry.ML"; (* theory lists *)

(* interface protocol/console *)
PolyML.Project.make "interface/control_interface.ML"; (* generic interface for run_in_textstreams *)

PolyML.Project.make "interface/console_commands.ML"; (* console commands and help *)
PolyML.Project.make "interface/console_lexer.ML"; (* lexer for quanto console *)
PolyML.Project.make "interface/console.ML"; (* generic protocol using commands *)
PolyML.Project.make "interface/console_interface.ML"; (* generic protocol using commands *)
PolyML.Project.make "interface/protocol.ML"; (* protocol for tools *) *)



(* new modular controller *)

PolyML.Project.make "json_interface/controller_util.ML";
PolyML.Project.make "json_interface/controller_module.ML";
PolyML.Project.make "json_interface/modules/test.ML";
PolyML.Project.make "json_interface/modules/rewrite.ML";
PolyML.Project.make "json_interface/modules/simplify.ML";
PolyML.Project.make "json_interface/controller.ML";
PolyML.Project.make "json_interface/controller_registry.ML";
PolyML.Project.make "json_interface/protocol.ML";
PolyML.Project.make "json_interface/run.ML";


(* some combinators and shorthand functions for simprocs *)
PolyML.Project.make "rewriting/simp_util.ML";
PolyML.Project.make "theories/red_green/rg_simp_util.ML";


(*
 * Top-level testing
 *)
Testing.make_test "test/old-rg-ruleset-rewriter-tests.ML";
Testing.make_test "test/regression-tests.ML";
(* Metrics tests depend on Substrings_Theory *)
(*Testing.make_test "metrics/test/test.ML";*)
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

