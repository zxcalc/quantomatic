(* build the core *)

use "isabelle_env.ML";
(*use_thy "lib.thy";
use_thy "core.thy";
use_thy "theories.thy";
use_thy "controller.thy";*)

(* replaced isabelle-parser-based includes with
 * sed-generated ML files *)
use "lib.thy.ML";
use "core.thy.ML";
use "theories.thy.ML";
use "controller.thy.ML";


(* core tests *)

Testing.make_test "expressions/test/linrat_angle_expr-test.ML";
Testing.make_test "expressions/test/linrat_expr-test.ML";
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

(* theory tests *)

Testing.make_test "theories/red_green/test/test.ML";
Testing.make_test "theories/red_green/rg_mathematica.ML";
Testing.make_test "theories/ghz_w/test/test.ML";
Testing.make_test "theories/substrings/test/test.ML";
Testing.make_test "theories/substr_linrat/test/test.ML";
Testing.make_test "theories/red_green_blue/test/test.ML";
Testing.make_test "theories/petri/test/test.ML";
Testing.make_test "theories/isaplanner_rtechn/test/test.ML";
Testing.make_test "theories/rational_pair/test/test.ML";

(*PolyML.Project.make "dnets/DNetsLib.ML";
PolyML.Project.make "dnets/Literal.ML";
PolyML.Project.make "dnets/Contour.ML";
PolyML.Project.make "dnets/ContourList.ML";
PolyML.Project.make "dnets/TopDNet.ML";
Testing.make_test "dnets/test.ML";*) 

(* top-level and regression tests *)

Testing.make_test "test/old-rg-ruleset-rewriter-tests.ML";
Testing.make_test "test/regression-tests.ML";
(* Metrics tests depend on Substrings_Theory *)
(*Testing.make_test "metrics/test/test.ML";*)
(*Testing.make_test "example_code/ROOT.ML";*)

