(*  Title:      Pure/Isar/overloading.ML
    Author:     Florian Haftmann, TU Muenchen

Overloaded definitions without any discipline.
*)

signature OVERLOADING =
sig
  type improvable_syntax
  val activate_improvable_syntax: Proof.context -> Proof.context
  val map_improvable_syntax: (improvable_syntax -> improvable_syntax)
    -> Proof.context -> Proof.context
  val set_primary_constraints: Proof.context -> Proof.context

  val overloading: (string * (string * typ) * bool) list -> theory -> local_theory
  val overloading_cmd: (string * string * bool) list -> theory -> local_theory
end;

structure Overloading: OVERLOADING =
struct

(* generic check/uncheck combinators for improvable constants *)

type improvable_syntax = ((((string * typ) list * (string * typ) list) *
  ((((string * typ -> (typ * typ) option) * (string * typ -> (typ * term) option)) * bool) *
    (term * term) list)) * bool);

structure Improvable_Syntax = Proof_Data
(
  type T = {
    primary_constraints: (string * typ) list,
    secondary_constraints: (string * typ) list,
    improve: string * typ -> (typ * typ) option,
    subst: string * typ -> (typ * term) option,
    consider_abbrevs: bool,
    unchecks: (term * term) list,
    passed: bool
  };
  fun init _ = {
    primary_constraints = [],
    secondary_constraints = [],
    improve = K NONE,
    subst = K NONE,
    consider_abbrevs = false,
    unchecks = [],
    passed = true
  };
);

fun map_improvable_syntax f = Improvable_Syntax.map (fn {primary_constraints,
    secondary_constraints, improve, subst, consider_abbrevs, unchecks, passed} =>
  let
    val (((primary_constraints', secondary_constraints'),
      (((improve', subst'), consider_abbrevs'), unchecks')), passed')
        = f (((primary_constraints, secondary_constraints),
            (((improve, subst), consider_abbrevs), unchecks)), passed)
  in
   {primary_constraints = primary_constraints', secondary_constraints = secondary_constraints',
    improve = improve', subst = subst', consider_abbrevs = consider_abbrevs',
    unchecks = unchecks', passed = passed'}
  end);

val mark_passed = (map_improvable_syntax o apsnd) (K true);

fun improve_term_check ts ctxt =
  let
    val thy = Proof_Context.theory_of ctxt;

    val {secondary_constraints, improve, subst, consider_abbrevs, passed, ...} =
      Improvable_Syntax.get ctxt;
    val is_abbrev = consider_abbrevs andalso Proof_Context.abbrev_mode ctxt;
    val passed_or_abbrev = passed orelse is_abbrev;
    fun accumulate_improvements (Const (c, ty)) =
          (case improve (c, ty) of
            SOME ty_ty' => Sign.typ_match thy ty_ty'
          | _ => I)
      | accumulate_improvements _ = I;
    val improvements = (fold o fold_aterms) accumulate_improvements ts Vartab.empty;
    val ts' = (map o map_types) (Envir.subst_type improvements) ts;
    fun apply_subst t =
      Envir.expand_term
        (fn Const (c, ty) =>
          (case subst (c, ty) of
            SOME (ty', t') =>
              if Sign.typ_instance thy (ty, ty')
              then SOME (ty', apply_subst t') else NONE
          | NONE => NONE)
        | _ => NONE) t;
    val ts'' = if is_abbrev then ts' else map apply_subst ts';
  in
    if eq_list (op aconv) (ts, ts'') andalso passed_or_abbrev then NONE
    else if passed_or_abbrev then SOME (ts'', ctxt)
    else
      SOME (ts'', ctxt
        |> fold (Proof_Context.add_const_constraint o apsnd SOME) secondary_constraints
        |> mark_passed)
  end;

fun rewrite_liberal thy unchecks t =
  (case try (Pattern.rewrite_term thy unchecks []) t of
    NONE => NONE
  | SOME t' => if t aconv t' then NONE else SOME t');

fun improve_term_uncheck ts ctxt =
  let
    val thy = Proof_Context.theory_of ctxt;
    val {unchecks, ...} = Improvable_Syntax.get ctxt;
    val ts' = map (rewrite_liberal thy unchecks) ts;
  in if exists is_some ts' then SOME (map2 the_default ts ts', ctxt) else NONE end;

fun set_primary_constraints ctxt =
  let val {primary_constraints, ...} = Improvable_Syntax.get ctxt;
  in fold (Proof_Context.add_const_constraint o apsnd SOME) primary_constraints ctxt end;

val activate_improvable_syntax =
  Context.proof_map
    (Syntax_Phases.term_check' 0 "improvement" improve_term_check
    #> Syntax_Phases.term_uncheck' 0 "improvement" improve_term_uncheck)
  #> set_primary_constraints;


(* overloading target *)

structure Data = Proof_Data
(
  type T = ((string * typ) * (string * bool)) list;
  fun init _ = [];
);

val get_overloading = Data.get o Local_Theory.target_of;
val map_overloading = Local_Theory.target o Data.map;

fun operation lthy b =
  get_overloading lthy
  |> get_first (fn ((c, _), (v, checked)) =>
      if Binding.name_of b = v then SOME (c, (v, checked)) else NONE);

fun synchronize_syntax ctxt =
  let
    val overloading = Data.get ctxt;
    fun subst (c, ty) =
      (case AList.lookup (op =) overloading (c, ty) of
        SOME (v, _) => SOME (ty, Free (v, ty))
      | NONE => NONE);
    val unchecks =
      map (fn (c_ty as (_, ty), (v, _)) => (Free (v, ty), Const c_ty)) overloading;
  in 
    ctxt
    |> map_improvable_syntax (K ((([], []), (((K NONE, subst), false), unchecks)), false))
  end;

fun define_overloaded (c, U) (v, checked) (b_def, rhs) =
  Local_Theory.background_theory_result
    (Thm.add_def_global (not checked) true
      (Thm.def_binding_optional (Binding.name v) b_def,
        Logic.mk_equals (Const (c, Term.fastype_of rhs), rhs)))
  ##> map_overloading (filter_out (fn (_, (v', _)) => v' = v))
  ##> Local_Theory.map_contexts (K synchronize_syntax)
  #-> (fn (_, def) => pair (Const (c, U), def));

fun foundation (((b, U), mx), (b_def, rhs)) params lthy =
  (case operation lthy b of
    SOME (c, (v, checked)) =>
      if mx <> NoSyn
      then error ("Illegal mixfix syntax for overloaded constant " ^ quote c)
      else lthy |> define_overloaded (c, U) (v, checked) (b_def, rhs)
  | NONE => lthy |> Generic_Target.theory_foundation (((b, U), mx), (b_def, rhs)) params);

fun pretty lthy =
  let
    val overloading = get_overloading lthy;
    fun pr_operation ((c, ty), (v, _)) =
      Pretty.block (Pretty.breaks
        [Pretty.str v, Pretty.str "==", Proof_Context.pretty_const lthy c,
          Pretty.str "::", Syntax.pretty_typ lthy ty]);
  in Pretty.keyword1 "overloading" :: map pr_operation overloading end;

fun conclude lthy =
  let
    val overloading = get_overloading lthy;
    val _ =
      if null overloading then ()
      else
        error ("Missing definition(s) for parameter(s) " ^
          commas_quote (map (Syntax.string_of_term lthy o Const o fst) overloading));
  in lthy end;

fun gen_overloading prep_const raw_overloading thy =
  let
    val ctxt = Proof_Context.init_global thy;
    val naming = Sign.naming_of thy;
    val _ = if null raw_overloading then error "At least one parameter must be given" else ();
    val overloading = raw_overloading |> map (fn (v, const, checked) =>
      (Term.dest_Const (prep_const ctxt const), (v, checked)));
  in
    thy
    |> Sign.change_begin
    |> Proof_Context.init_global
    |> Data.put overloading
    |> fold (fn ((_, ty), (v, _)) => Variable.declare_names (Free (v, ty))) overloading
    |> activate_improvable_syntax
    |> synchronize_syntax
    |> Local_Theory.init naming
       {define = Generic_Target.define foundation,
        notes = Generic_Target.notes Generic_Target.theory_notes,
        abbrev = Generic_Target.abbrev Generic_Target.theory_abbrev,
        declaration = K Generic_Target.theory_declaration,
        subscription = Generic_Target.theory_registration,
        pretty = pretty,
        exit = conclude #> Local_Theory.target_of #> Sign.change_end_local}
  end;

val overloading = gen_overloading (fn ctxt => Syntax.check_term ctxt o Const);
val overloading_cmd = gen_overloading Syntax.read_term;

end;
