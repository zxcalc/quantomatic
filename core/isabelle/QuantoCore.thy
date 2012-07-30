theory QuantoCore
imports Main io
uses

(* Overall controller for theories *)
 "../../core/interface/controller_state.ML" (* control state for quanto *)
 "../../core/interface/controller.ML" (* commands *)

(* interface protocol/console *)
 "../../core/interface/console_commands.ML" (* console commands and help *)
 "../../core/interface/console_lexer.ML" (* lexer for quanto console *)
 "../../core/interface/console.ML" (* generic protocol using commands *)
 "../../core/interface/console_interface.ML" (* generic protocol using commands *)
 "../../core/interface/protocol.ML" (* protocol for tools *)
 "../../core/interface/run.ML" 

(* more expression code; this time expresions derived from graphs, 
   e.g. for interaction with Mathematica/Maple/etc. *)
(* These depend on the red-green theory for now *)
(* more expression code; this time expresions derived from graphs, 
   e.g. for interaction with Mathematica/Maple/etc. *)
(* These depend on the red-green theory for now *)
 "../../core/expressions/alg.ML" (* algebraic expression utils *)
 "../../core/expressions/hilb.ML" (* hilbert space stuff *)

(* red-green specific vertices, graphs and matching *)
 "../../core/theories/red_green/vertex.ML"
 "../../core/theories/red_green/graph.ML"
 "../../core/theories/red_green/theory.ML"
 "../../core/theories/red_green/io.ML"

(* ghz-w specific vertices, graphs, and matching *)
 "../../core/theories/ghz_w/vertex.ML"
 "../../core/theories/ghz_w/graph.ML"
 "../../core/theories/ghz_w/theory.ML"
 "../../core/theories/ghz_w/io.ML"

(* Graphs having vertices with strings as data, substring as matching *)
 "../../core/theories/substrings/vertex.ML"
 "../../core/theories/substrings/graph.ML"
 "../../core/theories/substrings/theory.ML"
 "../../core/theories/substrings/io.ML"

(* Graphs having strings as types, linrat as data and both substrings and linrat
 * as matching *)
 "../../core/theories/substr_linrat/vertex.ML"
 "../../core/theories/substr_linrat/graph.ML"
 "../../core/theories/substr_linrat/theory.ML"
 "../../core/theories/substr_linrat/io.ML"

(* rgb specific vertices, graphs, and matching *)
 "../../core/theories/red_green_blue/vertex.ML"
 "../../core/theories/red_green_blue/graph.ML"
 "../../core/theories/red_green_blue/theory.ML"
 "../../core/theories/red_green_blue/io.ML"


(* rgb specific vertices, graphs, and matching *)
 "../../core/theories/isaplanner_rtechn/vertex.ML"
 "../../core/theories/isaplanner_rtechn/graph.ML"
 "../../core/theories/isaplanner_rtechn/theory.ML"
 "../../core/theories/isaplanner_rtechn/io.ML"


begin

end
