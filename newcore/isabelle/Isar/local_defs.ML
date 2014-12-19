(*  Title:      Pure/Isar/local_defs.ML
    Author:     Makarius

Local definitions.
*)

signature LOCAL_DEFS =
sig
  val cert_def: Proof.context -> term -> (string * typ) * term
  val abs_def: term -> (string * typ) * term
  val expand: cterm list -> thm -> thm
  val def_export: Assumption.export
  val add_defs: ((binding * mixfix) * (Thm.binding * term)) list -> Proof.context ->
    (term * (string * thm)) list * Proof.context
  val add_def: (binding * mixfix) * term -> Proof.context -> (term * thm) * Proof.context
  val fixed_abbrev: (binding * mixfix) * term -> Proof.context ->
    (term * term) * Proof.context
  val export: Proof.context -> Proof.context -> thm -> (thm list * thm list) * thm
  val export_cterm: Proof.context -> Proof.context -> cterm -> (thm list * thm list) * cterm
  val contract: Proof.context -> thm list -> cterm -> thm -> thm
  val print_rules: Proof.context -> unit
  val defn_add: attribute
  val defn_del: attribute
  val meta_rewrite_conv: Proof.context -> conv
  val meta_rewrite_rule: Proof.context -> thm -> thm
  val unfold: Proof.context -> thm list -> thm -> thm
  val unfold_goals: Proof.context -> thm list -> thm -> thm
  val unfold_tac: Proof.context -> thm list -> tactic
  val fold: Proof.context -> thm list -> thm -> thm
  val fold_tac: Proof.context -> thm list -> tactic
  val derived_def: Proof.context -> bool -> term ->
    ((string * typ) * term) * (Proof.context -> thm -> thm)
end;

structure Local_Defs: LOCAL_DEFS =
struct

(** primitive definitions **)

(* prepare defs *)

fun cert_def ctxt eq =
  let
    fun err msg =
      cat_error msg ("The error(s) above occurred in definition:\n" ^
        quote (Syntax.string_of_term ctxt eq));
    val ((lhs, _), eq') = eq
      |> Sign.no_vars ctxt
      |> Primitive_Defs.dest_def ctxt Term.is_Free (Variable.is_fixed ctxt) (K true)
      handle TERM (msg, _) => err msg | ERROR msg => err msg;
  in (Term.dest_Free (Term.head_of lhs), eq') end;

val abs_def = Primitive_Defs.abs_def #>> Term.dest_Free;

fun mk_def ctxt args =
  let
    val (bs, rhss) = split_list args;
    val Ts = map Term.fastype_of rhss;
    val (xs, _) = Proof_Context.add_fixes (map2 (fn b => fn T => (b, SOME T, NoSyn)) bs Ts) ctxt;
    val lhss = ListPair.map Free (xs, Ts);
  in map Logic.mk_equals (lhss ~~ rhss) end;


(* export defs *)

val head_of_def =
  Term.dest_Free o Term.head_of o #1 o Logic.dest_equals o Term.strip_all_body;


(*
  [x, x == a]
       :
      B x
  -----------
      B a
*)
fun expand defs =
  Drule.implies_intr_list defs
  #> Drule.generalize ([], map (#1 o head_of_def o Thm.term_of) defs)
  #> funpow (length defs) (fn th => Drule.reflexive_thm RS th);

val expand_term = Envir.expand_term_frees o map (abs_def o Thm.term_of);

fun def_export _ defs = (expand defs, expand_term defs);


(* add defs *)

fun add_defs defs ctxt =
  let
    val ((xs, mxs), specs) = defs |> split_list |>> split_list;
    val (bs, rhss) = specs |> split_list;
    val eqs = mk_def ctxt (xs ~~ rhss);
    val lhss = map (fst o Logic.dest_equals) eqs;
  in
    ctxt
    |> Proof_Context.add_fixes (map2 (fn x => fn mx => (x, NONE, mx)) xs mxs) |> #2
    |> fold Variable.declare_term eqs
    |> Proof_Context.add_assms_i def_export (map2 (fn b => fn eq => (b, [(eq, [])])) bs eqs)
    |>> map2 (fn lhs => fn (name, [th]) => (lhs, (name, th))) lhss
  end;

fun add_def (var, rhs) ctxt =
  let val ([(lhs, (_, th))], ctxt') = add_defs [(var, (Thm.empty_binding, rhs))] ctxt
  in ((lhs, th), ctxt') end;


(* fixed_abbrev *)

fun fixed_abbrev ((x, mx), rhs) ctxt =
  let
    val T = Term.fastype_of rhs;
    val ([x'], ctxt') = ctxt
      |> Variable.declare_term rhs
      |> Proof_Context.add_fixes [(x, SOME T, mx)];
    val lhs = Free (x', T);
    val _ = cert_def ctxt' (Logic.mk_equals (lhs, rhs));
    fun abbrev_export _ _ = (I, Envir.expand_term_frees [((x', T), rhs)]);
    val (_, ctxt'') = Assumption.add_assms abbrev_export [] ctxt';
  in ((lhs, rhs), ctxt'') end;


(* specific export -- result based on educated guessing *)

(*
  [xs, xs == as]
        :
       B xs
  --------------
       B as
*)
fun export inner outer =    (*beware of closure sizes*)
  let
    val exp = Assumption.export false inner outer;
    val exp_term = Assumption.export_term inner outer;
    val asms = Assumption.local_assms_of inner outer;
  in
    fn th =>
      let
        val th' = exp th;
        val defs_asms = asms |> map (Thm.assume #> (fn asm =>
          (case try (head_of_def o Thm.prop_of) asm of
            NONE => (asm, false)
          | SOME x =>
              let val t = Free x in
                (case try exp_term t of
                  NONE => (asm, false)
                | SOME u =>
                    if t aconv u then (asm, false)
                    else (Drule.abs_def (Drule.gen_all asm), true))
              end)));
      in (pairself (map #1) (List.partition #2 defs_asms), th') end
  end;

(*
  [xs, xs == as]
        :
     TERM b xs
  --------------  and  --------------
     TERM b as          b xs == b as
*)
fun export_cterm inner outer ct =
  export inner outer (Drule.mk_term ct) ||> Drule.dest_term;

fun contract ctxt defs ct th =
  th COMP (Raw_Simplifier.rewrite ctxt true defs ct COMP_INCR Drule.equal_elim_rule2);



(** defived definitions **)

(* transformation via rewrite rules *)

structure Rules = Generic_Data
(
  type T = thm list;
  val empty = [];
  val extend = I;
  val merge = Thm.merge_thms;
);

fun print_rules ctxt =
  Pretty.writeln (Pretty.big_list "definitional rewrite rules:"
    (map (Display.pretty_thm_item ctxt) (Rules.get (Context.Proof ctxt))));

val defn_add = Thm.declaration_attribute (Rules.map o Thm.add_thm);
val defn_del = Thm.declaration_attribute (Rules.map o Thm.del_thm);


(* meta rewrite rules *)

fun meta_rewrite_conv ctxt =
  Raw_Simplifier.rewrite_cterm (false, false, false) (K (K NONE))
    (empty_simpset ctxt
      addsimps (Rules.get (Context.Proof ctxt))
      |> Raw_Simplifier.add_eqcong Drule.equals_cong);    (*protect meta-level equality*)

val meta_rewrite_rule = Conv.fconv_rule o meta_rewrite_conv;


(* rewriting with object-level rules *)

fun meta f ctxt = f ctxt o map (meta_rewrite_rule ctxt);

val unfold       = meta Raw_Simplifier.rewrite_rule;
val unfold_goals = meta Raw_Simplifier.rewrite_goals_rule;
val unfold_tac   = meta Raw_Simplifier.rewrite_goals_tac;
val fold         = meta Raw_Simplifier.fold_rule;
val fold_tac     = meta Raw_Simplifier.fold_goals_tac;


(* derived defs -- potentially within the object-logic *)

fun derived_def ctxt conditional prop =
  let
    val ((c, T), rhs) = prop
      |> Thm.cterm_of (Proof_Context.theory_of ctxt)
      |> meta_rewrite_conv ctxt
      |> (snd o Logic.dest_equals o Thm.prop_of)
      |> conditional ? Logic.strip_imp_concl
      |> (abs_def o #2 o cert_def ctxt);
    fun prove ctxt' def =
      Goal.prove ctxt' (Variable.add_free_names ctxt' prop []) [] prop
        (fn {context = ctxt'', ...} =>
          ALLGOALS
            (CONVERSION (meta_rewrite_conv ctxt'') THEN'
              rewrite_goal_tac ctxt'' [def] THEN'
              resolve_tac [Drule.reflexive_thm]))
      handle ERROR msg => cat_error msg "Failed to prove definitional specification";
  in (((c, T), rhs), prove) end;

end;
