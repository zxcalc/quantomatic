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
PolyML.Project.make "io/xml_parse_utils.ML";
PolyML.Project.make "io/input_generic.ML";
PolyML.Project.make "io/output_generic.ML";

PolyML.Project.make "io/output_linrat.ML";

PolyML.Project.make "io/reader.ML";
PolyML.Project.make "io/writer.ML";

PolyML.Project.make "io/input_graph.ML";
PolyML.Project.make "io/output_graph.ML";

(* basic definition of a rewrite rule (as a pair of graphs) *)
PolyML.Project.make "rewriting/rule.ML";
PolyML.Project.make "io/output_rule.ML";
PolyML.Project.make "io/input_rule.ML";

(* rule sets *)
PolyML.Project.make "theories/ruleset.ML";
PolyML.Project.make "io/input_ruleset.ML";
PolyML.Project.make "io/output_ruleset.ML";

(* TESTING *)
PolyML.Project.make "io/test/rg_io.ML";

(* package all IO stuff into one place *)
PolyML.Project.make "io/io_interface.ML";

(* matching *)
PolyML.Project.make "matching/bbox_match.ML"; (* match info for bbox graphs *)
PolyML.Project.make "matching/match.ML"; (* a graph matching *)

(* naive match search implementation, find symmetric cases *)
PolyML.Project.make "matching/simple_match_search.ML";
(* substitution of a matched subgraph for another graph *)
PolyML.Project.make "rewriting/graph_subst.ML";
(* substitution used to provide rewriting with rulesets *)
PolyML.Project.make "rewriting/ruleset_rewriting.ML";
(* particular test cases *)
PolyML.Project.make "theories/red_green/rg_ruleset_rewriting.ML";
PolyML.Project.make "matching/test/simple_match_search.ML";

(* searching for matches, but avoiding symmetric ones *)
PolyML.Project.make "matching/symmetry_match_search.ML";

(* Heuristic derived data structures *)
PolyML.Project.make "heuristic/distancematrix.ML"; (* distance matrix *)
PolyML.Project.make "matching/filter.ML"; (* incremental match filter *)

(* more expression code; this time expresions derived from graphs, 
   e.g. for interaction with Mathematica/Maple/etc. *)
PolyML.Project.make "expressions/alg.ML"; (* algebraic expression utils *)
PolyML.Project.make "expressions/hilb.ML"; (* hilbert space stuff *)

(* interface protocol/console *)
PolyML.Project.make "interface/controller_state.ML"; (* control state for quanto *)
PolyML.Project.make "interface/command_lexer.ML"; (* lexer for quanto console *)
PolyML.Project.make "interface/commands.ML"; (* quanto commands and help *)
(* **** *)
PolyML.Project.make "interface/console_interface.ML"; (* generic protocol using commands *)

(* unit tests *)
(* PolyML.Project.make "test/ROOT.ML"; *) 

(* top level run function for controller/interface protocol *)
val run = ConsoleInterface.run_in_stdio;

(*
PolyML.Project.make "test/dbg/rule_matching3-bbox-v1.ML"; 
*)
