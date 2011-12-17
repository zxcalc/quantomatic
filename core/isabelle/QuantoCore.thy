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

(* red-green specific vertices, graphs and matching *)
 "../../core/theories/red_green/graph.ML"
 "../../core/theories/red_green/io.ML"

(* more expression code; this time expresions derived from graphs, 
   e.g. for interaction with Mathematica/Maple/etc. *)
(* These depend on the red-green theory for now *)
 "../../core/expressions/alg.ML" (* algebraic expression utils *)
 "../../core/expressions/hilb.ML" (* hilbert space stuff *)

(* ghz-w specific vertices, graphs, and matching *)
 "../../core/theories/ghz_w/vertex.ML"
 "../../core/theories/ghz_w/graph.ML"
 "../../core/theories/ghz_w/theory.ML"
 "../../core/theories/ghz_w/io.ML"

(* rgb specific vertices, graphs, and matching *)
 "../../core/theories/red_green_blue/vertex.ML"
 "../../core/theories/red_green_blue/graph.ML"
 "../../core/theories/red_green_blue/theory.ML"
 "../../core/theories/red_green_blue/io.ML"

begin

end
