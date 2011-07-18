(*** TESTS ***)
PolyML.SaveState.loadState "heaps/quanto.polyml-heap";


(* TESTING: test_graph structure and test construction tools *)
PolyML.Project.make "graph/test/test_graph.ML";

(* TESTING; ruleset and io construction *)
PolyML.Project.make "theories/test/test_ruleset.ML";
PolyML.Project.make "io/test/test_graph_io.ML";

(* TESTING: matching *)
PolyML.Project.make "matching/test/matching.ML";
PolyML.Project.make "matching/test/matching2.ML";
PolyML.Project.make "rewriting/test/ruleset_rewriting.ML";
PolyML.Project.make "matching/test/simple_match_search.ML";
PolyML.Project.make "matching/test/test_symmetry_match_search.ML";

(* unit tests *)
PolyML.Project.use_root "test/ROOT.ML";

(*
PolyML.Project.make "test/dbg/rule_matching3-bbox-v1.ML"; 
*)
