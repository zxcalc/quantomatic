(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  ROOT file to compile all libraries and quantomatic core  *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(**** Compile isaplib libraries ****)

val rootDir = OS.FileSys.getDir();
(OS.FileSys.chDir (rootDir ^ "/../../isaplib/"); 
use "ROOT.ML"; 
OS.FileSys.chDir rootDir);
print_depth 3;

PolyML.Project.Log.level_ref := 4;
PolyML.Project.depend_on_files 
  [(* this file *)
   rootDir ^ "/../../isaplib/ROOT.ML",
   rootDir ^ "/ROOT.ML"
   (* TODO: isaplib? *)
  ];

(** For debugging, turn on debug flag: **)
(* PolyML.Compiler.debug := true; *)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(**** generic testing stuff / move to isaplib? ****)
PolyML.Project.make "test/testing.ML";

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(**** Compile quantomatic code ****)

(* Graphs *)
PolyML.Project.make "graph/arity.ML"; (* arity of vertices (in,out,undir) *)
PolyML.Project.make "graph/graph_param.ML";
PolyML.Project.make "graph/basic_graph.ML"; (* basic graphs, just the data *)
PolyML.Project.make "graph/graph.ML"; (* graphs with interesting functions *)
PolyML.Project.make "graph/graph_iso.ML"; (* isomorphism between graphs *)
PolyML.Project.make "graph/overtex.ML"; (* open graphs *)
PolyML.Project.make "graph/ograph_param.ML";
PolyML.Project.make "graph/ograph.ML";
PolyML.Project.make "graph/bang_graph.ML"; (* bang box graphs *)
PolyML.Project.make "graph/bang_graph_iso.ML"; (* isomorphism between !graphs *)

(* linear arithmetic expressions: data inside verticies *)
PolyML.Project.make "expressions/lex.ML";
PolyML.Project.make "expressions/coeff.ML";
PolyML.Project.make "expressions/expr.ML"; 
PolyML.Project.make "expressions/linrat_expr.ML";

(* Generic output/input tools *)
PolyML.Project.make "io/xml_parse_utils.ML";
PolyML.Project.make "io/xml_output_utils.ML";
PolyML.Project.make "io/input_generic.ML";
PolyML.Project.make "io/output_generic.ML";
PolyML.Project.make "io/input_linrat.ML";
PolyML.Project.make "io/output_linrat.ML";
PolyML.Project.make "io/reader.ML";
PolyML.Project.make "io/writer.ML";
PolyML.Project.make "io/input_graph_v2.ML";
PolyML.Project.make "io/output_graph_v2.ML";

(* red-green specific vertices, graphs and matching *)
PolyML.Project.make "theories/red_green/vertex.ML";
PolyML.Project.make "theories/red_green/graph.ML";


(* TESTING: test_graph structure and test construction tools *)
PolyML.Project.make "graph/test/test_graph.ML";

(* basic definition of a rewrite rule (as a pair of graphs) *)
PolyML.Project.make "rewriting/rule.ML";
PolyML.Project.make "io/output_rule.ML";
PolyML.Project.make "io/input_rule.ML";

(* rule sets *)
PolyML.Project.make "theories/ruleset.ML";
PolyML.Project.make "io/input_ruleset.ML";
PolyML.Project.make "io/output_ruleset.ML";

(* package all IO stuff into one place *)
PolyML.Project.make "io/io_interface.ML";

(* I/O for old RG-graphs, depends on defining RG_VertexData *)
PolyML.Project.make "io/input_graph_v1.ML";
PolyML.Project.make "io/output_graph_v1.ML";

(* Simple dot output for graphs *)
PolyML.Project.make "io/output_graph_dot.ML";

(* TESTING; ruleset and io construction *)
PolyML.Project.make "theories/test/test_ruleset.ML";
PolyML.Project.make "io/test/test_graph_io.ML";

(* matching *)
PolyML.Project.make "matching/bbox_match.ML"; (* match info for bbox graphs *)
PolyML.Project.make "matching/match.ML"; (* a graph matching *)
(* naive match search implementation, find symmetric cases *)
PolyML.Project.make "matching/simple_match_search.ML";
(* substitution of a matched subgraph for another graph *)
PolyML.Project.make "rewriting/graph_subst.ML";
(* substitution used to provide rewriting with rulesets *)
PolyML.Project.make "rewriting/ruleset_rewriting.ML";
(* TESTS *)
PolyML.Project.make "matching/test/matching.ML";
PolyML.Project.make "matching/test/matching2.ML";
PolyML.Project.make "rewriting/test/ruleset_rewriting.ML";
PolyML.Project.make "matching/test/simple_match_search.ML";

(* construction of everything in a graphical theory from just param *)
PolyML.Project.make "theories/graphical_theory.ML";

(* construct the red-green IO *)
PolyML.Project.make "theories/red_green/io.ML";

(* searching for matches, but avoiding symmetric ones *)
PolyML.Project.make "matching/symmetry_match_search.ML";

(* Heuristic derived data structures *)
PolyML.Project.make "rewriting/heuristic/distancematrix.ML"; (* distance matrix *)
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

PolyML.Project.make "dev/convert_old_files.ML";

(* unit tests *)
PolyML.Project.use_root "test/ROOT.ML";

(* ghz-w specific vertices, graphs, and matching *)
PolyML.Project.make "theories/ghz_w/vertex.ML";
PolyML.Project.make "theories/ghz_w/graph.ML";
PolyML.Project.make "theories/ghz_w/theory.ML";
PolyML.Project.make "theories/ghz_w/io.ML";

(* rgb specific vertices, graphs, and matching *)
PolyML.Project.make "theories/red_green_blue/vertex.ML";
PolyML.Project.make "theories/red_green_blue/graph.ML";
PolyML.Project.make "theories/red_green_blue/theory.ML";
PolyML.Project.make "theories/red_green_blue/io.ML";


PolyML.Project.Log.log 2 "checking if heap is newer than edits...";

if PolyML.Project.heap_is_newer_than_edits "heaps/quanto_child.heap"
then PolyML.Project.Log.log 2 "NO"
else 
 (PolyML.Project.Log.log 2 "YES...building child heap...";
  PolyML.Project.ensure_at_latest_heap ();
  PolyML.Project.save_project_child_state "heaps/quanto_child.heap");
  
PolyML.Project.Log.log 2 "done."



(*
PolyML.Project.make "test/dbg/rule_matching3-bbox-v1.ML"; 
*)
