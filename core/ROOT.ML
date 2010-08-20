(* basic values *)
val rootDir = OS.FileSys.getDir();
(OS.FileSys.chDir (rootDir ^ "/../../isaplib/"); 
use "ROOT.ML"; 
OS.FileSys.chDir rootDir);
print_depth 3;

(* turn on debug flag *)
(* PolyML.Compiler.debug := true; *)

(* load all the files. *)
PolyML.Project.make "lex.ML";
PolyML.Project.make "graph.ML";
PolyML.Project.make "bvertex.ML";
PolyML.Project.make "bgraph.ML";
PolyML.Project.make "match_ctxt.ML"; (* vertex and edge matching and global context *)
PolyML.Project.make "bang_graph.ML";
PolyML.Project.make "bbox_match.ML";
PolyML.Project.make "coeff.ML";
PolyML.Project.make "expr.ML"; 
PolyML.Project.make "linrat_expr.ML";
PolyML.Project.make "rg_vertex.ML";
PolyML.Project.make "rg_graph.ML";
PolyML.Project.make "rg_match_ctxt.ML";
(* PolyML.Project.make "edge_match.ML"; -- not used *) 
PolyML.Project.make "match.ML";
PolyML.Project.make "matchstate.ML"; 
PolyML.Project.make "rule.ML";
(* PolyML.Project.make "spider.ML"; -- no longer used *)
PolyML.Project.make "ruleset.ML";
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
PolyML.Project.make "alg.ML";
PolyML.Project.make "hilb.ML";
PolyML.Project.make "controller.ML";




PolyML.Project.make "tests.ML"; 

val run = Controller.init;
(*
PolyML.Project.make "test/dbg/rule_matching3-bbox-v1.ML"; 
*)
