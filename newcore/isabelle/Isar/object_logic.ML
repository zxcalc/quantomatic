(*  Title:      Pure/Isar/object_logic.ML
    Author:     Markus Wenzel, TU Muenchen

Specifics about common object-logics.
*)

signature OBJECT_LOGIC =
sig
  val get_base_sort: theory -> sort option
  val add_base_sort: sort -> theory -> theory
  val add_judgment: binding * typ * mixfix -> theory -> theory
  val add_judgment_cmd: binding * string * mixfix -> theory -> theory
  val judgment_name: theory -> string
  val is_judgment: theory -> term -> bool
  val drop_judgment: theory -> term -> term
  val fixed_judgment: theory -> string -> term
  val ensure_propT: theory -> term -> term
  val dest_judgment: cterm -> cterm
  val judgment_conv: conv -> conv
  val elim_concl: thm -> term option
  val declare_atomize: attribute
  val declare_rulify: attribute
  val atomize_term: theory -> term -> term
  val atomize: Proof.context -> conv
  val atomize_prems: Proof.context -> conv
  val atomize_prems_tac: Proof.context -> int -> tactic
  val full_atomize_tac: Proof.context -> int -> tactic
  val rulify_term: theory -> term -> term
  val rulify_tac: Proof.context -> int -> tactic
  val rulify: Proof.context -> thm -> thm
  val rulify_no_asm: Proof.context -> thm -> thm
  val rule_format: attribute
  val rule_format_no_asm: attribute
end;

structure Object_Logic: OBJECT_LOGIC =
struct

(** theory data **)

datatype data = Data of
 {base_sort: sort option,
  judgment: string option,
  atomize_rulify: thm list * thm list};

fun make_data (base_sort, judgment, atomize_rulify) =
  Data {base_sort = base_sort, judgment = judgment, atomize_rulify = atomize_rulify};

structure Data = Theory_Data
(
  type T = data;
  val empty = make_data (NONE, NONE, ([], []));
  val extend = I;

  fun merge_opt eq (SOME x, SOME y) =
        if eq (x, y) then SOME x else error "Attempt to merge different object-logics"
    | merge_opt _ data = merge_options data;

  fun merge
     (Data {base_sort = base_sort1, judgment = judgment1, atomize_rulify = (atomize1, rulify1)},
      Data {base_sort = base_sort2, judgment = judgment2, atomize_rulify = (atomize2, rulify2)}) =
    make_data (merge_opt (op =) (base_sort1, base_sort2), merge_opt (op =) (judgment1, judgment2),
      (Thm.merge_thms (atomize1, atomize2), Thm.merge_thms (rulify1, rulify2)));
);

fun map_data f = Data.map (fn (Data {base_sort, judgment, atomize_rulify}) =>
  make_data (f (base_sort, judgment, atomize_rulify)));

fun get_data thy = Data.get thy |> (fn Data args => args);



(** generic treatment of judgments -- with a single argument only **)

(* base_sort *)

val get_base_sort = #base_sort o get_data;

fun add_base_sort S = map_data (fn (base_sort, judgment, atomize_rulify) =>
  if is_some base_sort then error "Attempt to redeclare object-logic base sort"
  else (SOME S, judgment, atomize_rulify));


(* add judgment *)

local

fun gen_add_judgment add_consts (b, T, mx) thy =
  let val c = Sign.full_name thy b in
    thy
    |> add_consts [(b, T, mx)]
    |> (fn thy' => Theory.add_deps_global c (c, Sign.the_const_type thy' c) [] thy')
    |> map_data (fn (base_sort, judgment, atomize_rulify) =>
        if is_some judgment then error "Attempt to redeclare object-logic judgment"
        else (base_sort, SOME c, atomize_rulify))
  end;

in

val add_judgment = gen_add_judgment Sign.add_consts;
val add_judgment_cmd = gen_add_judgment Sign.add_consts_cmd;

end;


(* judgments *)

fun judgment_name thy =
  (case #judgment (get_data thy) of
    SOME name => name
  | _ => raise TERM ("Unknown object-logic judgment", []));

fun is_judgment thy (Const (c, _) $ _) = c = judgment_name thy
  | is_judgment _ _ = false;

fun drop_judgment thy (Abs (x, T, t)) = Abs (x, T, drop_judgment thy t)
  | drop_judgment thy (tm as (Const (c, _) $ t)) =
      if (c = judgment_name thy handle TERM _ => false) then t else tm
  | drop_judgment _ tm = tm;

fun fixed_judgment thy x =
  let  (*be robust wrt. low-level errors*)
    val c = judgment_name thy;
    val aT = TFree (Name.aT, []);
    val T =
      the_default (aT --> propT) (Sign.const_type thy c)
      |> Term.map_type_tvar (fn ((x, _), S) => TFree (x, S));
    val U = Term.domain_type T handle Match => aT;
  in Const (c, T) $ Free (x, U) end;

fun ensure_propT thy t =
  let val T = Term.fastype_of t
  in if T = propT then t else Const (judgment_name thy, T --> propT) $ t end;

fun dest_judgment ct =
  if is_judgment (Thm.theory_of_cterm ct) (Thm.term_of ct)
  then Thm.dest_arg ct
  else raise CTERM ("dest_judgment", [ct]);

fun judgment_conv cv ct =
  if is_judgment (Thm.theory_of_cterm ct) (Thm.term_of ct)
  then Conv.arg_conv cv ct
  else raise CTERM ("judgment_conv", [ct]);


(* elimination rules *)

fun elim_concl rule =
  let
    val thy = Thm.theory_of_thm rule;
    val concl = Thm.concl_of rule;
    val C = drop_judgment thy concl;
  in
    if Term.is_Var C andalso
      exists (fn prem => concl aconv Logic.strip_assums_concl prem) (Thm.prems_of rule)
    then SOME C else NONE
  end;



(** treatment of meta-level connectives **)

(* maintain rules *)

val get_atomize = #1 o #atomize_rulify o get_data;
val get_rulify = #2 o #atomize_rulify o get_data;

fun add_atomize th = map_data (fn (base_sort, judgment, (atomize, rulify)) =>
  (base_sort, judgment, (Thm.add_thm th atomize, rulify)));

fun add_rulify th = map_data (fn (base_sort, judgment, (atomize, rulify)) =>
  (base_sort, judgment, (atomize, Thm.add_thm th rulify)));

val declare_atomize = Thm.declaration_attribute (fn th => Context.mapping (add_atomize th) I);
val declare_rulify = Thm.declaration_attribute (fn th => Context.mapping (add_rulify th) I);

val _ = Theory.setup (fold add_rulify Drule.norm_hhf_eqs);


(* atomize *)

fun atomize_term thy =
  drop_judgment thy o Raw_Simplifier.rewrite_term thy (get_atomize thy) [];

fun atomize ctxt =
  Raw_Simplifier.rewrite ctxt true (get_atomize (Proof_Context.theory_of ctxt));

fun atomize_prems ctxt ct =
  if Logic.has_meta_prems (Thm.term_of ct) then
    Conv.params_conv ~1 (Conv.prems_conv ~1 o atomize) ctxt ct
  else Conv.all_conv ct;

val atomize_prems_tac = CONVERSION o atomize_prems;
val full_atomize_tac = CONVERSION o atomize;


(* rulify *)

fun rulify_term thy = Raw_Simplifier.rewrite_term thy (get_rulify thy) [];
fun rulify_tac ctxt = rewrite_goal_tac ctxt (get_rulify (Proof_Context.theory_of ctxt));

fun gen_rulify full ctxt =
  Conv.fconv_rule (Raw_Simplifier.rewrite ctxt full (get_rulify (Proof_Context.theory_of ctxt)))
  #> Drule.gen_all #> Thm.strip_shyps #> Drule.zero_var_indexes;

val rulify = gen_rulify true;
val rulify_no_asm = gen_rulify false;

val rule_format = Thm.rule_attribute (rulify o Context.proof_of);
val rule_format_no_asm = Thm.rule_attribute (rulify_no_asm o Context.proof_of);

end;
