(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  ROOT file to compile all libraries and quantomatic core  *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(**** Compile isaplib libraries ****)

val rootDir = OS.FileSys.getDir();
(OS.FileSys.chDir (rootDir ^ "/../../isaplib/"); 
use "ROOT.ML"; 
OS.FileSys.chDir rootDir);
print_depth 3;

(* move to library? is this used? *)
fun and_try f1 f2 x = (case f1 x of NONE => NONE | SOME x2 => f2 x2);

(** For debugging, turn on debug flag: **)
(* PolyML.Compiler.debug := true; *)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(**** Compile quantomatic code ****)

(* Graphs *)
PolyML.Project.make "graph/arity.ML"; (* arity of vertices (in,out,undir) *)
PolyML.Project.make "graph/graph_param.ML";
PolyML.Project.make "graph/basic_graph.ML"; (* basic graphs, just the data *)
PolyML.Project.make "graph/graph.ML"; (* graphs with interesting functions *)
PolyML.Project.make "graph/graph_iso.ML"; (* isomorphism between graphs *)
PolyML.Project.make "graph/overtex.ML"; (* open graphs *)
PolyML.Project.make "graph/ograph.ML";
PolyML.Project.make "graph/bang_graph.ML"; (* bang box graphs *)
PolyML.Project.make "graph/bang_graph_iso.ML"; (* isomorphism between !graphs *)

(* linear arithmetic expressions: data inside verticies *)
PolyML.Project.make "expressions/lex.ML";
PolyML.Project.make "expressions/coeff.ML";
PolyML.Project.make "expressions/expr.ML"; 
PolyML.Project.make "expressions/linrat_expr.ML";

(* red-green specific vertices, graphs and matching *)
PolyML.Project.make "theories/red_green/rg_vertex.ML";
PolyML.Project.make "theories/red_green/rg_graph.ML";

(* output/input tools *)
PolyML.Project.make "io/input.ML";
PolyML.Project.make "io/output.ML";
PolyML.Project.make "io/xml_utils.ML";
PolyML.Project.make "io/reader.ML";
PolyML.Project.make "io/writer.ML";

PolyML.Project.make "io/input_graph_xml.ML";
PolyML.Project.make "io/output_graph_XML.ML";
PolyML.Project.make "io/loadsave_graph.ML";

(* basic definition of a rewrite rule (as a pair of graphs) *)
PolyML.Project.make "rewriting/rule.ML";
PolyML.Project.make "io/output_rule_XML.ML";
PolyML.Project.make "io/input_rule_xml.ML";

(* matching *)
PolyML.Project.make "matching/bbox_match.ML"; (* match info for bbox graphs *)
PolyML.Project.make "matching/match.ML"; (* a graph matching *)

(* naive match search implementation, find symmetric cases *)
PolyML.Project.make "matching/simple_match_search.ML";
PolyML.Project.make "theories/red_green/rg_simple_matchsearch.ML";

PolyML.Project.make "matching/symmetry_match_search.ML";

(* Heuristic derived data structures *)
PolyML.Project.make "heuristic/distancematrix.ML"; (* distance matrix *)
PolyML.Project.make "matching/filter.ML"; (* incremental match filter *)

(* **** *)
(* automorphism reduced search for matches *)
PolyML.Project.make "matching/autocut_match_search.ML"; 

(* **** *)
(* generic rules *)
PolyML.Project.make "rewriting/rewrite.ML";

(* **** *)
(* rule sets *)
PolyML.Project.make "theories/ruleset.ML";
PolyML.Project.make "io/output_ruleset_XML.ML";
PolyML.Project.make "io/input_ruleset_XML.ML";


(* more expression code; this time expresions derived from graphs *)
PolyML.Project.make "expressions/alg.ML"; (* algebraic expression utils *)
PolyML.Project.make "expressions/hilb.ML"; (* hilbert space stuff *)

(* interface protocol *)
PolyML.Project.make "interface/controller.ML";

(* unit tests *)
PolyML.Project.make "test/ROOT.ML"; 

(* top level run function for controller/interface protocol *)
val run = Controller.init;

(*
PolyML.Project.make "test/dbg/rule_matching3-bbox-v1.ML"; 
*)
