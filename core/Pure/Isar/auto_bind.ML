(*  Title:      Pure/Isar/auto_bind.ML
    Author:     Markus Wenzel, TU Muenchen

Automatic bindings of Isar text elements.
*)

signature AUTO_BIND =
sig
  val thesisN: string
  val thisN: string
  val thatN: string
  val assmsN: string
  val abs_params: term -> term -> term
  val goal: Proof.context -> term list -> (indexname * term option) list
  val dddot: indexname
  val facts: Proof.context -> term list -> (indexname * term option) list
  val no_facts: indexname list
end;

structure Auto_Bind: AUTO_BIND =
struct

(** bindings **)

val thesisN = "thesis";
val thisN = "this";
val thatN = "that";
val assmsN = "assms";

fun strip_judgment ctxt = Object_Logic.drop_judgment ctxt o Logic.strip_assums_concl;

fun abs_params prop = fold_rev Term.abs (Logic.strip_params prop);

fun statement_binds ctxt name prop =
  [((name, 0), SOME (abs_params prop (strip_judgment ctxt prop)))];


(* goal *)

fun goal ctxt [prop] = statement_binds ctxt thesisN prop
  | goal _ _ = [((thesisN, 0), NONE)];


(* dddot *)

val dddot = ("dddot", 0);

val _ =
  Theory.setup (Sign.parse_translation
    [("_DDDOT", fn _ => fn ts => Term.list_comb (Syntax.var dddot, ts))]);


(* facts *)

fun get_arg ctxt prop =
  (case strip_judgment ctxt prop of
    _ $ t => SOME (abs_params prop t)
  | _ => NONE);

fun facts ctxt props =
  (case try List.last props of
    NONE => []
  | SOME prop => [(dddot, get_arg ctxt prop)] @ statement_binds ctxt thisN prop);

val no_facts = [dddot, (thisN, 0)];

end;
