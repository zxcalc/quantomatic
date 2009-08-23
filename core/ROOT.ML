(* basic values *)
val rootDir = OS.FileSys.getDir();
(* print rootDir; *)
val heapName = (rootDir ^ "/../../isaplib/heaps/all.polyml-heap");

(* load the background heap, or compile from src if no heap *)
val _ = if ((OS.FileSys.fileId heapName; true) handle OS.SysErr e => false)
then (PolyML.SaveState.loadState heapName)
else 
  (OS.FileSys.chDir (rootDir ^ "/../../isaplib/"); 
   use "ROOT.ML"; 
   OS.FileSys.chDir rootDir);

(* load all the files. *)
use "lex.ML";
use "alg.ML";
use "graph.ML";
use "bvertex.ML";
use "bgraph.ML";
use "match_ctxt.ML"; (* vertex and edge matching and global context *)
use "bang_graph.ML";
use "coeff.ML";
use "expr.ML"; 
(* use "lex.ML"; *)
use "linrat_expr.ML"; 
use "rg_vertex.ML";
use "rg_graph.ML";
use "rg_match_ctxt.ML";
use "bbox_match.ML";
(* use "edge_match.ML"; -- not used *)
use "match.ML";
use "matchstate.ML";
use "rule.ML";
use "spider.ML";
use "output.ML";
use "output_XML.ML";
use "output_rule_XML.ML";
(*use "output_dot.ML";*)
use "xml_utils.ML";
use "input.ML";
use "input_graph_xml.ML";
use "input_rule_xml.ML";
use "reader.ML";
use "writer.ML";
use "loadsave.ML";
(*use "theory_hack.ML";*)
use "theory_new.ML";
use "hilb.ML";
use "controller.ML";

val run = Controller.init;
