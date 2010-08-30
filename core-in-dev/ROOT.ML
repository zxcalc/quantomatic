(* basic values; and basic isaplanner libs *)
val rootDir = OS.FileSys.getDir();
(OS.FileSys.chDir (rootDir ^ "/../../isaplib/"); 
use "ROOT.ML"; 
OS.FileSys.chDir rootDir);
print_depth 3;

(* turn on debug flag *)
(* PolyML.Compiler.debug := true; *)

(* load all the files. *)
PolyML.Project.make "expressions/lex.ML";

(* basic graphs *)
PolyML.Project.make "graph/graph.ML";
(* distance matrix *)
PolyML.Project.make "heuristic/distancematrix.ML";
(* open graphs *)
PolyML.Project.make "graph/overtex.ML";
PolyML.Project.make "graph/ograph.ML";
(* vertex and edge matching and global context *)
PolyML.Project.make "matching/match_ctxt.ML";
(* bang box graphs and bbox matching information *)
PolyML.Project.make "graph/bang_graph.ML";
PolyML.Project.make "graph/bbox_match.ML";
(* linear arithmetic expressions *)
PolyML.Project.make "expressions/coeff.ML";
PolyML.Project.make "expressions/expr.ML"; 
PolyML.Project.make "expreessions/linrat_expr.ML";

(* red-green specific vertices, graphs and matching *)
PolyML.Project.make "rg_vertex.ML";
PolyML.Project.make "rg_graph.ML";
PolyML.Project.make "rg_match_ctxt.ML";
(* PolyML.Project.make "edge_match.ML"; -- not used *) 

(* generic matching and match state for bbox graphs *)
PolyML.Project.make "matching/match.ML";
PolyML.Project.make "matching/matchstate.ML"; 

(* generic rules *)
PolyML.Project.make "rewriting/rule.ML";
(* PolyML.Project.make "spider.ML"; -- no longer used *)

(* rule sets *)
PolyML.Project.make "theories/ruleset.ML";

(* output/input tools *)
PolyML.Project.make "io/output.ML";
PolyML.Project.make "io/output_XML.ML";
PolyML.Project.make "io/output_rule_XML.ML";
(*PolyML.Project.make "output_dot.ML";*)
PolyML.Project.make "io/xml_utils.ML";
PolyML.Project.make "io/input.ML";
PolyML.Project.make "io/input_graph_xml.ML";
PolyML.Project.make "io/input_rule_xml.ML";
PolyML.Project.make "io/reader.ML";
PolyML.Project.make "io/writer.ML";
PolyML.Project.make "io/loadsave.ML";
(*PolyML.Project.make "theory_hack.ML";*)

(* general algebraic things *)
PolyML.Project.make "expressions/alg.ML";
(* hilbert space stuff *)
PolyML.Project.make "expressions/hilb.ML";

(* interface protocol *)
PolyML.Project.make "interface/controller.ML";

(* unit tests *)
PolyML.Project.make "test/ROOT.ML"; 

(* top level run function for controller/interface protocol *)
val run = Controller.init;

(*
PolyML.Project.make "test/dbg/rule_matching3-bbox-v1.ML"; 
*)
