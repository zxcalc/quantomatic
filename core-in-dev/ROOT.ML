(* basic values; and basic isaplanner libs *)
val rootDir = OS.FileSys.getDir();
(OS.FileSys.chDir (rootDir ^ "/../../isaplib/"); 
use "ROOT.ML"; 
OS.FileSys.chDir rootDir);
print_depth 3;

(* turn on debug flag *)
(* PolyML.Compiler.debug := true; *)

(* load all the files. *)
PolyML.Project.make "lex.ML";

(* basic graphs *)
PolyML.Project.make "graph.ML";
(* distance matrix *)
PolyML.Project.make "distancematrix.ML";
(* open graphs *)
PolyML.Project.make "overtex.ML";
PolyML.Project.make "ograph.ML";
(* vertex and edge matching and global context *)
PolyML.Project.make "match_ctxt.ML";
(* bang box graphs and bbox matching information *)
PolyML.Project.make "bang_graph.ML";
PolyML.Project.make "bbox_match.ML";
(* linear arithmetic expressions *)
PolyML.Project.make "coeff.ML";
PolyML.Project.make "expr.ML"; 
PolyML.Project.make "linrat_expr.ML";

(* red-green specific vertices, graphs and matching *)
PolyML.Project.make "rg_vertex.ML";
PolyML.Project.make "rg_graph.ML";
PolyML.Project.make "rg_match_ctxt.ML";
(* PolyML.Project.make "edge_match.ML"; -- not used *) 

(* generic matching and match state for bbox graphs *)
PolyML.Project.make "match.ML";
PolyML.Project.make "matchstate.ML"; 

(* generic rules *)
PolyML.Project.make "rule.ML";
(* PolyML.Project.make "spider.ML"; -- no longer used *)

(* rule sets *)
PolyML.Project.make "ruleset.ML";

(* output/input tools *)
PolyML.Project.make "output.ML";
PolyML.Project.make "output_XML.ML";
PolyML.Project.make "output_rule_XML.ML";
(*PolyML.Project.make "output_dot.ML";*)
PolyML.Project.make "xml_utils.ML";
PolyML.Project.make "input.ML";
PolyML.Project.make "input_graph_xml.ML";
PolyML.Project.make "input_rule_xml.ML";
PolyML.Project.make "reader.ML";
PolyML.Project.make "writer.ML";
PolyML.Project.make "loadsave.ML";
(*PolyML.Project.make "theory_hack.ML";*)

(* general algebraic things *)
PolyML.Project.make "alg.ML";
(* hilbert space stuff *)
PolyML.Project.make "hilb.ML";

(* interface protocol *)
PolyML.Project.make "controller.ML";

(* unit tests *)
PolyML.Project.make "tests.ML"; 

(* top level run function for controller/interface protocol *)
val run = Controller.init;

(*
PolyML.Project.make "test/dbg/rule_matching3-bbox-v1.ML"; 
*)
