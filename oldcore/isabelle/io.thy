theory io  
imports theories 
uses


(*
 * Descrimination nets
 *)
 "../../core/dnets/DNetsLib.ML"
 "../../core/dnets/Literal.ML"
 "../../core/dnets/Contour.ML"
 "../../core/dnets/ContourList.ML"
 "../../core/dnets/TopDNet.ML"

(* SKIPPING QUANTOCOSY *)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*                          Compile the controller                         *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)


(* Overall controller for theories *)
 "../../core/interface/controller_state.ML" (* control state for quanto *)
 "../../core/interface/controller.ML" (* commands *)
 "../../core/interface/controller_registry.ML"  (* theory lists *)

(* interface protocol/console *)
 "../../core/interface/control_interface.ML" (* generic interface for run_in_textstreams *)

 "../../core/interface/console_commands.ML"  (* console commands and help *)
 "../../core/interface/console_lexer.ML" (* lexer for quanto console *)
 "../../core/interface/console.ML" (* generic protocol using commands *)
 "../../core/interface/console_interface.ML" (* generic protocol using commands *)
 "../../core/interface/protocol.ML" (* protocol for tools *)


(* new modular controller *)

 "../../core/json_interface/controller_util.ML"
 "../../core/json_interface/controller_module.ML"
 "../../core/json_interface/modules/test.ML"
 "../../core/json_interface/modules/rewrite.ML"
 "../../core/json_interface/modules/simplify.ML"
 "../../core/json_interface/controller.ML"
 "../../core/json_interface/controller_registry.ML"
 "../../core/json_interface/protocol.ML"
 "../../core/json_interface/run.ML"

(* some combinators and shorthand functions for simprocs *)
 "../../core/rewriting/simp_util.ML"
 "../../core/theories/red_green/rg_simp_util.ML"



begin

end;
