(*  Title:      Pure/Isar/interpretation.ML
    Author:     Clemens Ballarin, TU Muenchen
    Author:     Florian Haftmann, TU Muenchen

Locale interpretation.
*)

signature INTERPRETATION =
sig
  type 'a defines = (Attrib.binding * ((binding * mixfix) * 'a)) list
  type 'a rewrites = (Attrib.binding * 'a) list

  (*interpretation in proofs*)
  val interpret: Expression.expression_i -> term rewrites -> Proof.state -> Proof.state
  val interpret_cmd: Expression.expression -> string rewrites -> Proof.state -> Proof.state

  (*interpretation in local theories*)
  val interpretation: Expression.expression_i ->
    term rewrites -> local_theory -> Proof.state
  val interpretation_cmd: Expression.expression ->
    string rewrites -> local_theory -> Proof.state

  (*interpretation into global theories*)
  val global_interpretation: Expression.expression_i ->
    term defines -> term rewrites -> local_theory -> Proof.state
  val global_interpretation_cmd: Expression.expression ->
    string defines -> string rewrites -> local_theory -> Proof.state

  (*interpretation between locales*)
  val sublocale: Expression.expression_i ->
    term defines -> term rewrites -> local_theory -> Proof.state
  val sublocale_cmd: Expression.expression ->
    string defines -> string rewrites -> local_theory -> Proof.state
  val global_sublocale: string -> Expression.expression_i ->
    term defines -> term rewrites -> theory -> Proof.state
  val global_sublocale_cmd: xstring * Position.T -> Expression.expression ->
    string defines -> string rewrites -> theory -> Proof.state

  (*mixed Isar interface*)
  val isar_interpretation: Expression.expression_i ->
    term rewrites -> local_theory -> Proof.state
  val isar_interpretation_cmd: Expression.expression ->
    string rewrites -> local_theory -> Proof.state
end;

structure Interpretation : INTERPRETATION =
struct

(** common interpretation machinery **)

type 'a defines = (Attrib.binding * ((binding * mixfix) * 'a)) list
type 'a rewrites = (Attrib.binding * 'a) list

(* reading of locale expressions with rewrite morphisms *)

local

fun augment_with_def prep_term deps ((name, atts), ((b, mx), raw_rhs)) lthy =
  let
    val rhs = prep_term lthy raw_rhs;
    val lthy' = Variable.declare_term rhs lthy;
    val ((_, (_, def)), lthy'') =
      Local_Theory.define ((b, mx), ((Thm.def_binding_optional b name, atts), rhs)) lthy';
  in (def, lthy'') end;

fun augment_with_defs prep_term [] deps ctxt = ([], ctxt)
      (*quasi-inhomogeneous type: definitions demand local theory rather than bare proof context*)
  | augment_with_defs prep_term raw_defs deps lthy =
      let
        val (_, inner_lthy) =
          Local_Theory.open_target lthy
          ||> fold Locale.activate_declarations deps;
        val (inner_defs, inner_lthy') =
          fold_map (augment_with_def prep_term deps) raw_defs inner_lthy;
        val lthy' =
          inner_lthy'
          |> Local_Theory.close_target;
        val def_eqns =
          map (singleton (Proof_Context.export inner_lthy' lthy') o Thm.symmetric) inner_defs
      in (def_eqns, lthy') end;

fun prep_eqns prep_props prep_attr [] deps ctxt = ([], [])
  | prep_eqns prep_props prep_attr raw_eqns deps ctxt =
      let
        val ctxt' = fold Locale.activate_declarations deps ctxt;
        val eqns =
          (Variable.export_terms ctxt' ctxt o prep_props ctxt' o map snd) raw_eqns;
        val attrss = map (apsnd (map (prep_attr ctxt)) o fst) raw_eqns;
      in (eqns, attrss) end;

fun prep_interpretation prep_expr prep_term prep_props prep_attr
  expression raw_defs raw_eqns initial_ctxt =
  let
    val ((propss, deps, export), expr_ctxt) = prep_expr expression initial_ctxt;
    val (def_eqns, def_ctxt) =
      augment_with_defs prep_term raw_defs deps expr_ctxt;
    val (eqns, attrss) = prep_eqns prep_props prep_attr raw_eqns deps def_ctxt;
    val goal_ctxt = fold Variable.auto_fixes eqns def_ctxt;
    val export' = Variable.export_morphism goal_ctxt expr_ctxt;
  in (((propss, deps, export, export'), (def_eqns, eqns, attrss)), goal_ctxt) end;

in

fun cert_interpretation expression =
  prep_interpretation Expression.cert_goal_expression Syntax.check_term
    Syntax.check_props (K I) expression;

fun read_interpretation expression =
  prep_interpretation Expression.read_goal_expression Syntax.read_term
    Syntax.read_props Attrib.check_src expression;

end;


(* interpretation machinery *)

local

fun meta_rewrite eqns ctxt =
  (map (Local_Defs.abs_def_rule ctxt) (maps snd eqns), ctxt);

fun note_eqns_register pos note activate deps witss def_eqns eqns attrss export export' ctxt =
  let
    val facts =
      (Binding.empty_atts, [(map (Morphism.thm (export' $> export)) def_eqns, [])]) ::
        map2 (fn attrs => fn eqn => (attrs, [([Morphism.thm (export' $> export) eqn], [])]))
          attrss eqns;
    val (eqns', ctxt') = ctxt
      |> note Thm.theoremK facts
      |-> meta_rewrite;
    val dep_morphs =
      map2 (fn (dep, morph) => fn wits =>
        let val morph' = morph
          $> Element.satisfy_morphism (map (Element.transform_witness export') wits)
          $> Morphism.binding_morphism "position" (Binding.set_pos pos)
        in (dep, morph') end) deps witss;
    fun activate' dep_morph ctxt =
      activate dep_morph
        (Option.map (rpair true) (Element.eq_morphism (Proof_Context.theory_of ctxt) eqns'))
        export ctxt;
  in ctxt' |> fold activate' dep_morphs end;

in

fun generic_interpretation prep_interpretation setup_proof note add_registration
    expression raw_defs raw_eqns initial_ctxt =
  let
    val (((propss, deps, export, export'), (def_eqns, eqns, attrss)), goal_ctxt) =
      prep_interpretation expression raw_defs raw_eqns initial_ctxt;
    val pos = Position.thread_data ();
    fun after_qed witss eqns =
      note_eqns_register pos note add_registration deps witss def_eqns eqns attrss export export';
  in setup_proof after_qed propss eqns goal_ctxt end;

end;


(** interfaces **)

(* interpretation in proofs *)

local

fun gen_interpret prep_interpretation expression raw_eqns state =
  let
    val _ = Proof.assert_forward_or_chain state;
    fun lift_after_qed after_qed witss eqns =
      Proof.map_context (after_qed witss eqns) #> Proof.reset_facts;
    fun setup_proof after_qed propss eqns goal_ctxt =
      Element.witness_local_proof_eqs (lift_after_qed after_qed) "interpret"
        propss eqns goal_ctxt state;
  in
    Proof.context_of state
    |> generic_interpretation prep_interpretation setup_proof
      Attrib.local_notes (Context.proof_map ooo Locale.add_registration) expression [] raw_eqns
  end;

in

val interpret = gen_interpret cert_interpretation;
val interpret_cmd = gen_interpret read_interpretation;

end;


(* interpretation in local theories *)

fun interpretation expression =
  generic_interpretation cert_interpretation Element.witness_proof_eqs
    Local_Theory.notes_kind Locale.activate_fragment expression [];

fun interpretation_cmd expression =
  generic_interpretation read_interpretation Element.witness_proof_eqs
    Local_Theory.notes_kind Locale.activate_fragment expression [];


(* interpretation into global theories *)

fun global_interpretation expression =
  generic_interpretation cert_interpretation Element.witness_proof_eqs
    Local_Theory.notes_kind Local_Theory.theory_registration expression;

fun global_interpretation_cmd expression =
  generic_interpretation read_interpretation Element.witness_proof_eqs
    Local_Theory.notes_kind Local_Theory.theory_registration expression;


(* interpretation between locales *)

fun sublocale expression =
  generic_interpretation cert_interpretation Element.witness_proof_eqs
    Local_Theory.notes_kind Local_Theory.locale_dependency expression;

fun sublocale_cmd expression =
  generic_interpretation read_interpretation Element.witness_proof_eqs
    Local_Theory.notes_kind Local_Theory.locale_dependency expression;

local

fun gen_global_sublocale prep_loc prep_interpretation
    raw_locale expression raw_defs raw_eqns thy =
  let
    val lthy = Named_Target.init NONE (prep_loc thy raw_locale) thy;
    fun setup_proof after_qed =
      Element.witness_proof_eqs
        (fn wits => fn eqs => after_qed wits eqs #> Local_Theory.exit);
  in
    lthy |>
      generic_interpretation prep_interpretation setup_proof
        Local_Theory.notes_kind Local_Theory.locale_dependency expression raw_defs raw_eqns
  end;

in

fun global_sublocale expression =
  gen_global_sublocale (K I) cert_interpretation expression;

fun global_sublocale_cmd raw_expression =
  gen_global_sublocale Locale.check read_interpretation raw_expression;

end;


(* mixed Isar interface *)

local

fun register_or_activate lthy =
  if Named_Target.is_theory lthy
  then Local_Theory.theory_registration
  else Locale.activate_fragment;

fun gen_isar_interpretation prep_interpretation expression raw_eqns lthy =
  generic_interpretation prep_interpretation Element.witness_proof_eqs
    Local_Theory.notes_kind (register_or_activate lthy) expression [] raw_eqns lthy;

in

fun isar_interpretation expression =
  gen_isar_interpretation cert_interpretation expression;
fun isar_interpretation_cmd raw_expression =
  gen_isar_interpretation read_interpretation raw_expression;

end;

end;
