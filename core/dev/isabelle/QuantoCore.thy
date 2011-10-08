theory QuantoCore
imports Main io
uses

(* Overall controller for theories *)
 "../../../../quantomatic/core/interface/controller_state.ML" (* control state for quanto *)
 "../../../../quantomatic/core/interface/controller.ML" (* commands *)

(* interface protocol/console *)
 "../../../../quantomatic/core/interface/console_commands.ML" (* console commands and help *)
 "../../../../quantomatic/core/interface/console_lexer.ML" (* lexer for quanto console *)
 "../../../../quantomatic/core/interface/console.ML" (* generic protocol using commands *)
 "../../../../quantomatic/core/interface/console_interface.ML" (* generic protocol using commands *)
 (* FIXME: lex error "../../../../quantomatic/core/interface/protocol.ML" *) (* protocol for tools *)
 (* FIXME: depends on protocol "../../../../quantomatic/core/interface/run.ML" *)

(* red-green specific vertices, graphs and matching *)
 "../../../../quantomatic/core/theories/red_green/graph.ML"
 "../../../../quantomatic/core/theories/red_green/io.ML"

(* more expression code; this time expresions derived from graphs, 
   e.g. for interaction with Mathematica/Maple/etc. *)
(* These depend on the red-green theory for now *)
 "../../../../quantomatic/core/expressions/alg.ML" (* algebraic expression utils *)
 "../../../../quantomatic/core/expressions/hilb.ML" (* hilbert space stuff *)

(* ghz-w specific vertices, graphs, and matching *)
 "../../../../quantomatic/core/theories/ghz_w/vertex.ML"
 "../../../../quantomatic/core/theories/ghz_w/graph.ML"
 "../../../../quantomatic/core/theories/ghz_w/theory.ML"
 "../../../../quantomatic/core/theories/ghz_w/io.ML"

(* rgb specific vertices, graphs, and matching *)
 "../../../../quantomatic/core/theories/red_green_blue/vertex.ML"
 "../../../../quantomatic/core/theories/red_green_blue/graph.ML"
 "../../../../quantomatic/core/theories/red_green_blue/theory.ML"
 "../../../../quantomatic/core/theories/red_green_blue/io.ML"

begin

use "../../../../quantomatic/core/interface/protocol.ML";

end
