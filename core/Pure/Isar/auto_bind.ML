(*  Title:      Pure/Isar/auto_bind.ML
    Author:     Markus Wenzel, TU Muenchen

Automatic bindings of Isar text elements.
*)

signature AUTO_BIND =
sig
  val thesisN: string
  val thisN: string
  val assmsN: string
  val goal: Proof.context -> term list -> (indexname * term option) list
  val facts: Proof.context -> term list -> (indexname * term option) list
  val no_facts: (indexname * term option) list
end;

structure Auto_Bind: AUTO_BIND =
struct

(** bindings **)

val thesisN = "thesis";
val thisN = "this";
val assmsN = "assms";

fun strip_judgment ctxt = Object_Logic.drop_judgment ctxt o Logic.strip_assums_concl;

fun statement_binds ctxt name prop =
  [((name, 0), SOME (fold_rev Term.abs (Logic.strip_params prop) (strip_judgment ctxt prop)))];


(* goal *)

fun goal ctxt [prop] = statement_binds ctxt thesisN prop
  | goal _ _ = [((thesisN, 0), NONE)];


(* facts *)

fun get_arg ctxt prop =
  (case strip_judgment ctxt prop of
    _ $ t => SOME (fold_rev Term.abs (Logic.strip_params prop) t)
  | _ => NONE);

fun facts _ [] = []
  | facts ctxt props =
      let val prop = List.last props
      in [(Syntax_Ext.dddot_indexname, get_arg ctxt prop)] @ statement_binds ctxt thisN prop end;

val no_facts = [(Syntax_Ext.dddot_indexname, NONE), ((thisN, 0), NONE)];

end;
