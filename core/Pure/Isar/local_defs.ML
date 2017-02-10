(*  Title:      Pure/Isar/local_defs.ML
    Author:     Makarius

Local definitions.
*)

signature LOCAL_DEFS =
sig
  val cert_def: Proof.context -> (string -> Position.T list) -> term -> (string * typ) * term
  val abs_def: term -> (string * typ) * term
  val expand: cterm list -> thm -> thm
  val def_export: Assumption.export
  val define: ((binding * mixfix) * (Thm.binding * term)) list -> Proof.context ->
    (term * (string * thm)) list * Proof.context
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
  val abs_def_rule: Proof.context -> thm -> thm
  val unfold_abs_def_raw: Config.raw
  val unfold_abs_def: bool Config.T
  val unfold: Proof.context -> thm list -> thm -> thm
  val unfold_goals: Proof.context -> thm list -> thm -> thm
  val unfold_tac: Proof.context -> thm list -> tactic
  val unfold0: Proof.context -> thm list -> thm -> thm
  val unfold0_goals: Proof.context -> thm list -> thm -> thm
  val unfold0_tac: Proof.context -> thm list -> tactic
  val fold: Proof.context -> thm list -> thm -> thm
  val fold_tac: Proof.context -> thm list -> tactic
  val derived_def: Proof.context -> (string -> Position.T list) -> {conditional: bool} ->
    term -> ((string * typ) * term) * (Proof.context -> thm -> thm)
end;

structure Local_Defs: LOCAL_DEFS =
struct

(** primitive definitions **)

(* prepare defs *)

fun cert_def ctxt get_pos eq =
  let
    fun err msg =
      cat_error msg ("The error(s) above occurred in definition:\n" ^
        quote (Syntax.string_of_term ctxt eq));
    val ((lhs, _), args, eq') = eq
      |> Sign.no_vars ctxt
      |> Primitive_Defs.dest_def ctxt
        {check_head = Term.is_Free,
         check_free_lhs = not o Variable.is_fixed ctxt,
         check_free_rhs = if Variable.is_body ctxt then K true else Variable.is_fixed ctxt,
         check_tfree = K true}
      handle TERM (msg, _) => err msg | ERROR msg => err msg;
    val _ =
      Context_Position.reports ctxt
        (maps (fn Free (x, _) => Syntax_Phases.reports_of_scope (get_pos x) | _ => []) args);
  in (Term.dest_Free (Term.head_of lhs), eq') end;

val abs_def = Primitive_Defs.abs_def #>> Term.dest_Free;

fun mk_def ctxt args =
  let
    val (bs, rhss) = split_list args;
    val Ts = map Term.fastype_of rhss;
    val (xs, _) = ctxt
      |> Context_Position.set_visible false
      |> Proof_Context.add_fixes (map2 (fn b => fn T => (b, SOME T, NoSyn)) bs Ts);
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


(* define *)

fun define defs ctxt =
  let
    val ((xs, mxs), specs) = defs |> split_list |>> split_list;
    val (bs, rhss) = specs |> split_list;
    val eqs = mk_def ctxt (xs ~~ rhss);
    val lhss = map (fst o Logic.dest_equals) eqs;
  in
    ctxt
    |> Proof_Context.add_fixes (map2 (fn x => fn mx => (x, NONE, mx)) xs mxs) |> #2
    |> fold Variable.declare_term eqs
    |> Proof_Context.add_assms def_export (map2 (fn b => fn eq => (b, [(eq, [])])) bs eqs)
    |>> map2 (fn lhs => fn (name, [th]) => (lhs, (name, th))) lhss
  end;


(* fixed_abbrev *)

fun fixed_abbrev ((x, mx), rhs) ctxt =
  let
    val T = Term.fastype_of rhs;
    val ([x'], ctxt') = ctxt
      |> Variable.declare_term rhs
      |> Proof_Context.add_fixes [(x, SOME T, mx)];
    val lhs = Free (x', T);
    val _ = cert_def ctxt' (K []) (Logic.mk_equals (lhs, rhs));
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
fun export inner outer th =
  let
    val defs_asms =
      Assumption.local_assms_of inner outer
      |> filter_out (Drule.is_sort_constraint o Thm.term_of)
      |> map (Thm.assume #> (fn asm =>
        (case try (head_of_def o Thm.prop_of) asm of
          NONE => (asm, false)
        | SOME x =>
            let val t = Free x in
              (case try (Assumption.export_term inner outer) t of
                NONE => (asm, false)
              | SOME u =>
                  if t aconv u then (asm, false)
                  else (Drule.abs_def (Variable.gen_all outer asm), true))
            end)));
  in (apply2 (map #1) (List.partition #2 defs_asms), Assumption.export false inner outer th) end;

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
    (map (Thm.pretty_thm_item ctxt) (Rules.get (Context.Proof ctxt))));

val defn_add = Thm.declaration_attribute (Rules.map o Thm.add_thm o Thm.trim_context);
val defn_del = Thm.declaration_attribute (Rules.map o Thm.del_thm o Thm.trim_context);


(* meta rewrite rules *)

fun meta_rewrite_conv ctxt =
  Raw_Simplifier.rewrite_cterm (false, false, false) (K (K NONE))
    (ctxt
      |> Raw_Simplifier.init_simpset (Rules.get (Context.Proof ctxt))
      |> Raw_Simplifier.add_eqcong Drule.equals_cong);    (*protect meta-level equality*)

val meta_rewrite_rule = Conv.fconv_rule o meta_rewrite_conv;

fun abs_def_rule ctxt = meta_rewrite_rule ctxt #> Drule.abs_def;


(* unfold object-level rules *)

val unfold_abs_def_raw = Config.declare ("unfold_abs_def", @{here}) (K (Config.Bool true));
val unfold_abs_def = Config.bool unfold_abs_def_raw;

local

fun gen_unfold rewrite ctxt rews =
  let val meta_rews = map (meta_rewrite_rule ctxt) rews in
    if Config.get ctxt unfold_abs_def then
      rewrite ctxt meta_rews #>
      rewrite ctxt (map (perhaps (try Drule.abs_def)) meta_rews)
    else rewrite ctxt meta_rews
  end;

val no_unfold_abs_def = Config.put unfold_abs_def false;

in

val unfold = gen_unfold Raw_Simplifier.rewrite_rule;
val unfold_goals = gen_unfold Raw_Simplifier.rewrite_goals_rule;
val unfold_tac = PRIMITIVE oo unfold_goals;

val unfold0 = unfold o no_unfold_abs_def;
val unfold0_goals = unfold_goals o no_unfold_abs_def;
val unfold0_tac = unfold_tac o no_unfold_abs_def;

end


(* fold object-level rules *)

fun fold ctxt rews = Raw_Simplifier.fold_rule ctxt (map (meta_rewrite_rule ctxt) rews);
fun fold_tac ctxt rews = Raw_Simplifier.fold_goals_tac ctxt (map (meta_rewrite_rule ctxt) rews);


(* derived defs -- potentially within the object-logic *)

fun derived_def ctxt get_pos {conditional} prop =
  let
    val ((c, T), rhs) = prop
      |> Thm.cterm_of ctxt
      |> meta_rewrite_conv ctxt
      |> (snd o Logic.dest_equals o Thm.prop_of)
      |> conditional ? Logic.strip_imp_concl
      |> (abs_def o #2 o cert_def ctxt get_pos);
    fun prove ctxt' def =
      Goal.prove ctxt'
        ((not (Variable.is_body ctxt') ? Variable.add_free_names ctxt' prop) []) [] prop
        (fn {context = ctxt'', ...} =>
          ALLGOALS
            (CONVERSION (meta_rewrite_conv ctxt'') THEN'
              rewrite_goal_tac ctxt'' [def] THEN'
              resolve_tac ctxt'' [Drule.reflexive_thm]))
      handle ERROR msg => cat_error msg "Failed to prove definitional specification";
  in (((c, T), rhs), prove) end;

end;
