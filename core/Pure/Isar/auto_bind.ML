(*  Title:      Pure/Isar/auto_bind.ML
    Author:     Markus Wenzel, TU Muenchen

Automatic bindings of Isar text elements.
*)

signature AUTO_BIND =
sig
  val thesisN: string
  val thisN: string
  val assmsN: string
  val goal: theory -> term list -> (indexname * term option) list
  val facts: theory -> term list -> (indexname * term option) list
  val no_facts: (indexname * term option) list
end;

structure Auto_Bind: AUTO_BIND =
struct

(** bindings **)

val thesisN = "thesis";
val thisN = "this";
val assmsN = "assms";

fun strip_judgment thy = Object_Logic.drop_judgment thy o Logic.strip_assums_concl;

fun statement_binds thy name prop =
  [((name, 0), SOME (fold_rev Term.abs (Logic.strip_params prop) (strip_judgment thy prop)))];


(* goal *)

fun goal thy [prop] = statement_binds thy thesisN prop
  | goal _ _ = [((thesisN, 0), NONE)];


(* facts *)

fun get_arg thy prop =
  (case strip_judgment thy prop of
    _ $ t => SOME (fold_rev Term.abs (Logic.strip_params prop) t)
  | _ => NONE);

fun facts _ [] = []
  | facts thy props =
      let val prop = List.last props
      in [(Syntax_Ext.dddot_indexname, get_arg thy prop)] @ statement_binds thy thisN prop end;

val no_facts = [(Syntax_Ext.dddot_indexname, NONE), ((thisN, 0), NONE)];

end;
