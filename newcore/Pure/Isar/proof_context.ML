(*  Title:      Pure/Isar/proof_context.ML
    Author:     Markus Wenzel, TU Muenchen

The key concept of Isar proof contexts: elevates primitive local
reasoning Gamma |- phi to a structured concept, with generic context
elements.  See also structure Variable and Assumption.
*)

signature PROOF_CONTEXT =
sig
  val theory_of: Proof.context -> theory
  val init_global: theory -> Proof.context
  val get_global: theory -> string -> Proof.context
  type mode
  val mode_default: mode
  val mode_stmt: mode
  val mode_pattern: mode
  val mode_schematic: mode
  val mode_abbrev: mode
  val set_mode: mode -> Proof.context -> Proof.context
  val get_mode: Proof.context -> mode
  val restore_mode: Proof.context -> Proof.context -> Proof.context
  val abbrev_mode: Proof.context -> bool
  val set_stmt: bool -> Proof.context -> Proof.context
  val syntax_of: Proof.context -> Local_Syntax.T
  val syn_of: Proof.context -> Syntax.syntax
  val tsig_of: Proof.context -> Type.tsig
  val set_defsort: sort -> Proof.context -> Proof.context
  val default_sort: Proof.context -> indexname -> sort
  val consts_of: Proof.context -> Consts.T
  val set_syntax_mode: Syntax.mode -> Proof.context -> Proof.context
  val restore_syntax_mode: Proof.context -> Proof.context -> Proof.context
  val map_naming: (Name_Space.naming -> Name_Space.naming) -> Proof.context -> Proof.context
  val naming_of: Proof.context -> Name_Space.naming
  val restore_naming: Proof.context -> Proof.context -> Proof.context
  val full_name: Proof.context -> binding -> string
  val class_space: Proof.context -> Name_Space.T
  val type_space: Proof.context -> Name_Space.T
  val const_space: Proof.context -> Name_Space.T
  val intern_class: Proof.context -> xstring -> string
  val intern_type: Proof.context -> xstring -> string
  val intern_const: Proof.context -> xstring -> string
  val extern_class: Proof.context -> string -> xstring
  val markup_class: Proof.context -> string -> string
  val pretty_class: Proof.context -> string -> Pretty.T
  val extern_type: Proof.context -> string -> xstring
  val markup_type: Proof.context -> string -> string
  val pretty_type: Proof.context -> string -> Pretty.T
  val extern_const: Proof.context -> string -> xstring
  val markup_const: Proof.context -> string -> string
  val pretty_const: Proof.context -> string -> Pretty.T
  val transfer: theory -> Proof.context -> Proof.context
  val background_theory: (theory -> theory) -> Proof.context -> Proof.context
  val background_theory_result: (theory -> 'a * theory) -> Proof.context -> 'a * Proof.context
  val facts_of: Proof.context -> Facts.T
  val facts_of_fact: Proof.context -> string -> Facts.T
  val markup_extern_fact: Proof.context -> string -> Markup.T * xstring
  val pretty_term_abbrev: Proof.context -> term -> Pretty.T
  val pretty_fact: Proof.context -> string * thm list -> Pretty.T
  val check_class: Proof.context -> xstring * Position.T -> class * Position.report list
  val read_class: Proof.context -> string -> class
  val read_typ: Proof.context -> string -> typ
  val read_typ_syntax: Proof.context -> string -> typ
  val read_typ_abbrev: Proof.context -> string -> typ
  val cert_typ: Proof.context -> typ -> typ
  val cert_typ_syntax: Proof.context -> typ -> typ
  val cert_typ_abbrev: Proof.context -> typ -> typ
  val infer_type: Proof.context -> string * typ -> typ
  val inferred_param: string -> Proof.context -> typ * Proof.context
  val inferred_fixes: Proof.context -> (string * typ) list * Proof.context
  val check_type_name: {proper: bool, strict: bool} -> Proof.context ->
    xstring * Position.T -> typ * Position.report list
  val read_type_name: {proper: bool, strict: bool} -> Proof.context -> string -> typ
  val consts_completion_message: Proof.context -> xstring * Position.T list -> string
  val check_const: {proper: bool, strict: bool} -> Proof.context ->
    xstring * Position.T list -> term * Position.report list
  val read_const: {proper: bool, strict: bool} -> Proof.context -> string -> term
  val read_arity: Proof.context -> xstring * string list * string -> arity
  val cert_arity: Proof.context -> arity -> arity
  val allow_dummies: Proof.context -> Proof.context
  val prepare_sortsT: Proof.context -> typ list -> string list * typ list
  val prepare_sorts: Proof.context -> term list -> string list * term list
  val check_tfree: Proof.context -> string * sort -> string * sort
  val read_term_pattern: Proof.context -> string -> term
  val read_term_schematic: Proof.context -> string -> term
  val read_term_abbrev: Proof.context -> string -> term
  val show_abbrevs_raw: Config.raw
  val show_abbrevs: bool Config.T
  val expand_abbrevs: Proof.context -> term -> term
  val cert_term: Proof.context -> term -> term
  val cert_prop: Proof.context -> term -> term
  val def_type: Proof.context -> indexname -> typ option
  val standard_typ_check: Proof.context -> typ list -> typ list
  val standard_term_check_finish: Proof.context -> term list -> term list
  val standard_term_uncheck: Proof.context -> term list -> term list
  val goal_export: Proof.context -> Proof.context -> thm list -> thm list
  val export: Proof.context -> Proof.context -> thm list -> thm list
  val export_morphism: Proof.context -> Proof.context -> morphism
  val norm_export_morphism: Proof.context -> Proof.context -> morphism
  val bind_terms: (indexname * term option) list -> Proof.context -> Proof.context
  val auto_bind_goal: term list -> Proof.context -> Proof.context
  val auto_bind_facts: term list -> Proof.context -> Proof.context
  val match_bind: bool -> (string list * string) list -> Proof.context -> term list * Proof.context
  val match_bind_i: bool -> (term list * term) list -> Proof.context -> term list * Proof.context
  val read_propp: (string * string list) list list -> Proof.context ->
    (term * term list) list list * Proof.context
  val cert_propp: (term * term list) list list -> Proof.context ->
    (term * term list) list list * Proof.context
  val read_propp_schematic: (string * string list) list list -> Proof.context ->
    (term * term list) list list * Proof.context
  val cert_propp_schematic: (term * term list) list list -> Proof.context ->
    (term * term list) list list * Proof.context
  val bind_propp: (string * string list) list list -> Proof.context ->
    (term list list * (Proof.context -> Proof.context)) * Proof.context
  val bind_propp_i: (term * term list) list list -> Proof.context ->
    (term list list * (Proof.context -> Proof.context)) * Proof.context
  val bind_propp_schematic: (string * string list) list list -> Proof.context ->
    (term list list * (Proof.context -> Proof.context)) * Proof.context
  val bind_propp_schematic_i: (term * term list) list list -> Proof.context ->
    (term list list * (Proof.context -> Proof.context)) * Proof.context
  val fact_tac: Proof.context -> thm list -> int -> tactic
  val some_fact_tac: Proof.context -> int -> tactic
  val get_fact_generic: Context.generic -> Facts.ref -> thm list
  val get_fact: Proof.context -> Facts.ref -> thm list
  val get_fact_single: Proof.context -> Facts.ref -> thm
  val get_thms: Proof.context -> xstring -> thm list
  val get_thm: Proof.context -> xstring -> thm
  val note_thmss: string -> (Thm.binding * (thm list * attribute list) list) list ->
    Proof.context -> (string * thm list) list * Proof.context
  val put_thms: bool -> string * thm list option -> Proof.context -> Proof.context
  val read_vars: (binding * string option * mixfix) list -> Proof.context ->
    (binding * typ option * mixfix) list * Proof.context
  val cert_vars: (binding * typ option * mixfix) list -> Proof.context ->
    (binding * typ option * mixfix) list * Proof.context
  val add_fixes: (binding * typ option * mixfix) list -> Proof.context ->
    string list * Proof.context
  val add_assms: Assumption.export ->
    (Thm.binding * (string * string list) list) list ->
    Proof.context -> (string * thm list) list * Proof.context
  val add_assms_i: Assumption.export ->
    (Thm.binding * (term * term list) list) list ->
    Proof.context -> (string * thm list) list * Proof.context
  val dest_cases: Proof.context -> (string * (Rule_Cases.T * bool)) list
  val update_cases: bool -> (string * Rule_Cases.T option) list -> Proof.context -> Proof.context
  val apply_case: Rule_Cases.T -> Proof.context -> (binding * term list) list * Proof.context
  val check_case: Proof.context -> bool ->
    string * Position.T -> binding option list -> Rule_Cases.T
  val type_notation: bool -> Syntax.mode -> (typ * mixfix) list -> Proof.context -> Proof.context
  val notation: bool -> Syntax.mode -> (term * mixfix) list -> Proof.context -> Proof.context
  val generic_type_notation: bool -> Syntax.mode -> (typ * mixfix) list -> morphism ->
    Context.generic -> Context.generic
  val generic_notation: bool -> Syntax.mode -> (term * mixfix) list -> morphism ->
    Context.generic -> Context.generic
  val class_alias: binding -> class -> Proof.context -> Proof.context
  val type_alias: binding -> string -> Proof.context -> Proof.context
  val const_alias: binding -> string -> Proof.context -> Proof.context
  val add_const_constraint: string * typ option -> Proof.context -> Proof.context
  val add_abbrev: string -> binding * term -> Proof.context -> (term * term) * Proof.context
  val revert_abbrev: string -> string -> Proof.context -> Proof.context
  val generic_add_abbrev: string -> binding * term -> Context.generic ->
    (term * term) * Context.generic
  val generic_revert_abbrev: string -> string -> Context.generic -> Context.generic
  val print_syntax: Proof.context -> unit
  val print_abbrevs: Proof.context -> unit
  val pretty_term_bindings: Proof.context -> Pretty.T list
  val pretty_local_facts: Proof.context -> bool -> Pretty.T list
  val print_local_facts: Proof.context -> bool -> unit
  val pretty_cases: Proof.context -> Pretty.T list
  val debug: bool Config.T
  val verbose: bool Config.T
  val pretty_ctxt: Proof.context -> Pretty.T list
  val pretty_context: Proof.context -> Pretty.T list
end;

structure Proof_Context: PROOF_CONTEXT =
struct

val theory_of = Proof_Context.theory_of;
val init_global = Proof_Context.init_global;
val get_global = Proof_Context.get_global;



(** inner syntax mode **)

datatype mode =
  Mode of
   {stmt: bool,                (*inner statement mode*)
    pattern: bool,             (*pattern binding schematic variables*)
    schematic: bool,           (*term referencing loose schematic variables*)
    abbrev: bool};             (*abbrev mode -- no normalization*)

fun make_mode (stmt, pattern, schematic, abbrev) =
  Mode {stmt = stmt, pattern = pattern, schematic = schematic, abbrev = abbrev};

val mode_default   = make_mode (false, false, false, false);
val mode_stmt      = make_mode (true, false, false, false);
val mode_pattern   = make_mode (false, true, false, false);
val mode_schematic = make_mode (false, false, true, false);
val mode_abbrev    = make_mode (false, false, false, true);



(** Isar proof context information **)

type cases = ((Rule_Cases.T * bool) * int) Name_Space.table;
val empty_cases: cases = Name_Space.empty_table Markup.caseN;

datatype data =
  Data of
   {mode: mode,                  (*inner syntax mode*)
    syntax: Local_Syntax.T,      (*local syntax*)
    tsig: Type.tsig * Type.tsig, (*local/global type signature -- local name space / defsort only*)
    consts: Consts.T * Consts.T, (*local/global consts -- local name space / abbrevs only*)
    facts: Facts.T,              (*local facts, based on initial global facts*)
    cases: cases};               (*named case contexts: case, is_proper, running index*)

fun make_data (mode, syntax, tsig, consts, facts, cases) =
  Data {mode = mode, syntax = syntax, tsig = tsig, consts = consts, facts = facts, cases = cases};

structure Data = Proof_Data
(
  type T = data;
  fun init thy =
    make_data (mode_default,
      Local_Syntax.init thy,
      (Type.change_ignore (Sign.tsig_of thy), Sign.tsig_of thy),
      (Consts.change_ignore (Sign.consts_of thy), Sign.consts_of thy),
      Global_Theory.facts_of thy,
      empty_cases);
);

fun rep_data ctxt = Data.get ctxt |> (fn Data rep => rep);

fun map_data f =
  Data.map (fn Data {mode, syntax, tsig, consts, facts, cases} =>
    make_data (f (mode, syntax, tsig, consts, facts, cases)));

fun set_mode mode = map_data (fn (_, syntax, tsig, consts, facts, cases) =>
  (mode, syntax, tsig, consts, facts, cases));

fun map_mode f =
  map_data (fn (Mode {stmt, pattern, schematic, abbrev}, syntax, tsig, consts, facts, cases) =>
    (make_mode (f (stmt, pattern, schematic, abbrev)), syntax, tsig, consts, facts, cases));

fun map_syntax f =
  map_data (fn (mode, syntax, tsig, consts, facts, cases) =>
    (mode, f syntax, tsig, consts, facts, cases));

fun map_tsig f =
  map_data (fn (mode, syntax, tsig, consts, facts, cases) =>
    (mode, syntax, f tsig, consts, facts, cases));

fun map_consts f =
  map_data (fn (mode, syntax, tsig, consts, facts, cases) =>
    (mode, syntax, tsig, f consts, facts, cases));

fun map_facts f =
  map_data (fn (mode, syntax, tsig, consts, facts, cases) =>
    (mode, syntax, tsig, consts, f facts, cases));

fun map_cases f =
  map_data (fn (mode, syntax, tsig, consts, facts, cases) =>
    (mode, syntax, tsig, consts, facts, f cases));

val get_mode = #mode o rep_data;
val restore_mode = set_mode o get_mode;
val abbrev_mode = get_mode #> (fn Mode {abbrev, ...} => abbrev);

fun set_stmt stmt =
  map_mode (fn (_, pattern, schematic, abbrev) => (stmt, pattern, schematic, abbrev));

val syntax_of = #syntax o rep_data;
val syn_of = Local_Syntax.syn_of o syntax_of;
val set_syntax_mode = map_syntax o Local_Syntax.set_mode;
val restore_syntax_mode = map_syntax o Local_Syntax.restore_mode o syntax_of;

val tsig_of = #1 o #tsig o rep_data;
val set_defsort = map_tsig o apfst o Type.set_defsort;
fun default_sort ctxt = the_default (Type.defaultS (tsig_of ctxt)) o Variable.def_sort ctxt;

val consts_of = #1 o #consts o rep_data;
val cases_of = #cases o rep_data;


(* naming *)

val naming_of = Name_Space.naming_of o Context.Proof;
val map_naming = Context.proof_map o Name_Space.map_naming;
val restore_naming = map_naming o K o naming_of;

val full_name = Name_Space.full_name o naming_of;


(* name spaces *)

val class_space = Type.class_space o tsig_of;
val type_space = Type.type_space o tsig_of;
val const_space = Consts.space_of o consts_of;

val intern_class = Name_Space.intern o class_space;
val intern_type = Name_Space.intern o type_space;
val intern_const = Name_Space.intern o const_space;

fun extern_class ctxt = Name_Space.extern ctxt (class_space ctxt);
fun extern_type ctxt = Name_Space.extern ctxt (type_space ctxt);
fun extern_const ctxt = Name_Space.extern ctxt (const_space ctxt);

fun markup_class ctxt c = Name_Space.markup_extern ctxt (class_space ctxt) c |-> Markup.markup;
fun markup_type ctxt c = Name_Space.markup_extern ctxt (type_space ctxt) c |-> Markup.markup;
fun markup_const ctxt c = Name_Space.markup_extern ctxt (const_space ctxt) c |-> Markup.markup;

fun pretty_class ctxt c = Name_Space.markup_extern ctxt (class_space ctxt) c |> Pretty.mark_str;
fun pretty_type ctxt c = Name_Space.markup_extern ctxt (type_space ctxt) c |> Pretty.mark_str;
fun pretty_const ctxt c = Name_Space.markup_extern ctxt (const_space ctxt) c |> Pretty.mark_str;


(* theory transfer *)

fun transfer_syntax thy ctxt = ctxt |>
  map_syntax (Local_Syntax.rebuild thy) |>
  map_tsig (fn tsig as (local_tsig, global_tsig) =>
    let val thy_tsig = Sign.tsig_of thy in
      if Type.eq_tsig (thy_tsig, global_tsig) then tsig
      else (Type.merge_tsig (Context.pretty ctxt) (local_tsig, thy_tsig), thy_tsig)
    end) |>
  map_consts (fn consts as (local_consts, global_consts) =>
    let val thy_consts = Sign.consts_of thy in
      if Consts.eq_consts (thy_consts, global_consts) then consts
      else (Consts.merge (local_consts, thy_consts), thy_consts)
    end);

fun transfer thy = Context.raw_transfer thy #> transfer_syntax thy;

fun background_theory f ctxt = transfer (f (theory_of ctxt)) ctxt;

fun background_theory_result f ctxt =
  let val (res, thy') = f (theory_of ctxt)
  in (res, ctxt |> transfer thy') end;


(* hybrid facts *)

val facts_of = #facts o rep_data;

fun facts_of_fact ctxt name =
  let
    val local_facts = facts_of ctxt;
    val global_facts = Global_Theory.facts_of (theory_of ctxt);
  in
    if Facts.defined local_facts name
    then local_facts else global_facts
  end;

fun markup_extern_fact ctxt name =
  Facts.markup_extern ctxt (facts_of_fact ctxt name) name;



(** pretty printing **)

fun pretty_term_abbrev ctxt = Syntax.pretty_term (set_mode mode_abbrev ctxt);

fun pretty_fact_name ctxt a =
  Pretty.block [Pretty.mark_str (markup_extern_fact ctxt a), Pretty.str ":"];

fun pretty_fact ctxt =
  let
    val pretty_thm = Display.pretty_thm ctxt;
    val pretty_thms = map (Display.pretty_thm_item ctxt);
  in
    fn ("", [th]) => pretty_thm th
     | ("", ths) => Pretty.blk (0, Pretty.fbreaks (pretty_thms ths))
     | (a, [th]) => Pretty.block [pretty_fact_name ctxt a, Pretty.brk 1, pretty_thm th]
     | (a, ths) => Pretty.block (Pretty.fbreaks (pretty_fact_name ctxt a :: pretty_thms ths))
  end;



(** prepare types **)

(* classes *)

fun check_class ctxt (xname, pos) =
  let
    val tsig = tsig_of ctxt;
    val class_space = Type.class_space tsig;

    val name = Type.cert_class tsig (Name_Space.intern class_space xname)
      handle TYPE (msg, _, _) =>
        error (msg ^ Position.here pos ^
          Markup.markup_report (Completion.reported_text
            (Name_Space.completion (Context.Proof ctxt) class_space (xname, pos))));
    val reports =
      if Context_Position.is_reported ctxt pos
      then [(pos, Name_Space.markup class_space name)] else [];
  in (name, reports) end;

fun read_class ctxt text =
  let
    val (c, reports) = check_class ctxt (Symbol_Pos.source_content (Syntax.read_token text));
    val _ = Position.reports reports;
  in c end;


(* types *)

fun read_typ_mode mode ctxt s =
  Syntax.read_typ (Type.set_mode mode ctxt) s;

val read_typ = read_typ_mode Type.mode_default;
val read_typ_syntax = read_typ_mode Type.mode_syntax;
val read_typ_abbrev = read_typ_mode Type.mode_abbrev;


fun cert_typ_mode mode ctxt T =
  Type.cert_typ_mode mode (tsig_of ctxt) T
    handle TYPE (msg, _, _) => error msg;

val cert_typ = cert_typ_mode Type.mode_default;
val cert_typ_syntax = cert_typ_mode Type.mode_syntax;
val cert_typ_abbrev = cert_typ_mode Type.mode_abbrev;



(** prepare terms and propositions **)

(* inferred types of parameters *)

fun infer_type ctxt x =
  Term.fastype_of (singleton (Syntax.check_terms (set_mode mode_schematic ctxt)) (Free x));

fun inferred_param x ctxt =
  let val T = infer_type ctxt (x, dummyT)
  in (T, ctxt |> Variable.declare_term (Free (x, T))) end;

fun inferred_fixes ctxt =
  let
    val xs = map #2 (Variable.dest_fixes ctxt);
    val (Ts, ctxt') = fold_map inferred_param xs ctxt;
  in (xs ~~ Ts, ctxt') end;


(* type names *)

fun check_type_name {proper, strict} ctxt (c, pos) =
  if Lexicon.is_tid c then
    if proper then error ("Not a type constructor: " ^ quote c ^ Position.here pos)
    else
      let
        val reports =
          if Context_Position.is_reported ctxt pos
          then [(pos, Markup.tfree)] else [];
      in (TFree (c, default_sort ctxt (c, ~1)), reports) end
  else
    let
      val ((d, reports), decl) = Type.check_decl (Context.Proof ctxt) (tsig_of ctxt) (c, pos);
      fun err () = error ("Bad type name: " ^ quote d ^ Position.here pos);
      val args =
        (case decl of
          Type.LogicalType n => n
        | Type.Abbreviation (vs, _, _) => if strict then err () else length vs
        | Type.Nonterminal => if strict then err () else 0);
    in (Type (d, replicate args dummyT), reports) end;

fun read_type_name ctxt flags text =
  let
    val (T, reports) =
      check_type_name ctxt flags (Symbol_Pos.source_content (Syntax.read_token text));
    val _ = Position.reports reports;
  in T end;


(* constant names *)

fun consts_completion_message ctxt (c, ps) =
  ps |> map (fn pos =>
    Name_Space.completion (Context.Proof ctxt) (Consts.space_of (consts_of ctxt)) (c, pos)
    |> Completion.reported_text)
  |> implode
  |> Markup.markup_report;

fun check_const {proper, strict} ctxt (c, ps) =
  let
    val _ =
      Name.reject_internal (c, ps) handle ERROR msg =>
        error (msg ^ consts_completion_message ctxt (c, ps));
    fun err msg = error (msg ^ Position.here_list ps);
    val consts = consts_of ctxt;
    val fixed = if proper then NONE else Variable.lookup_fixed ctxt c;
    val (t, reports) =
      (case (fixed, Variable.lookup_const ctxt c) of
        (SOME x, NONE) =>
          let
            val reports = ps
              |> filter (Context_Position.is_reported ctxt)
              |> map (fn pos =>
                (pos, Markup.name x (if Name.is_skolem x then Markup.skolem else Markup.free)));
          in (Free (x, infer_type ctxt (x, dummyT)), reports) end
      | (_, SOME d) =>
          let
            val T = Consts.type_scheme consts d handle TYPE (msg, _, _) => err msg;
            val reports = ps
              |> filter (Context_Position.is_reported ctxt)
              |> map (fn pos => (pos, Name_Space.markup (Consts.space_of consts) d));
          in (Const (d, T), reports) end
      | _ => Consts.check_const (Context.Proof ctxt) consts (c, ps));
    val _ =
      (case (strict, t) of
        (true, Const (d, _)) =>
          (ignore (Consts.the_const consts d) handle TYPE (msg, _, _) => err msg)
      | _ => ());
  in (t, reports) end;

fun read_const ctxt flags text =
  let
    val (xname, pos) = Symbol_Pos.source_content (Syntax.read_token text);
    val (t, reports) = check_const ctxt flags (xname, [pos]);
    val _ = Position.reports reports;
  in t end;


(* type arities *)

local

fun prep_arity prep_tycon prep_sort ctxt (t, Ss, S) =
  let val arity = (prep_tycon ctxt t, map (prep_sort ctxt) Ss, prep_sort ctxt S)
  in Type.add_arity (Context.pretty ctxt) arity (tsig_of ctxt); arity end;

in

val read_arity =
  prep_arity ((#1 o dest_Type) oo read_type_name {proper = true, strict = true}) Syntax.read_sort;

val cert_arity = prep_arity (K I) (Type.cert_sort o tsig_of);

end;


(* read_term *)

fun read_term_mode mode ctxt = Syntax.read_term (set_mode mode ctxt);

val read_term_pattern   = read_term_mode mode_pattern;
val read_term_schematic = read_term_mode mode_schematic;
val read_term_abbrev    = read_term_mode mode_abbrev;


(* local abbreviations *)

local

fun certify_consts ctxt = Consts.certify (Context.pretty ctxt) (tsig_of ctxt)
  (not (abbrev_mode ctxt)) (consts_of ctxt);

fun expand_binds ctxt =
  let
    val Mode {pattern, schematic, ...} = get_mode ctxt;

    fun reject_schematic (t as Var _) =
          error ("Unbound schematic variable: " ^ Syntax.string_of_term ctxt t)
      | reject_schematic (Abs (_, _, t)) = reject_schematic t
      | reject_schematic (t $ u) = (reject_schematic t; reject_schematic u)
      | reject_schematic _ = ();
  in
    if pattern then I
    else Variable.expand_binds ctxt #> (if schematic then I else tap reject_schematic)
  end;

in

fun expand_abbrevs ctxt = certify_consts ctxt #> expand_binds ctxt;

end;

val show_abbrevs_raw = Config.declare ("show_abbrevs", @{here}) (fn _ => Config.Bool true);
val show_abbrevs = Config.bool show_abbrevs_raw;

fun contract_abbrevs ctxt t =
  let
    val thy = theory_of ctxt;
    val consts = consts_of ctxt;
    val Mode {abbrev, ...} = get_mode ctxt;
    val retrieve = Consts.retrieve_abbrevs consts (print_mode_value () @ [""]);
    fun match_abbrev u = Option.map #1 (get_first (Pattern.match_rew thy u) (retrieve u));
  in
    if abbrev orelse not (Config.get ctxt show_abbrevs) orelse not (can Term.type_of t) then t
    else Pattern.rewrite_term_top thy [] [match_abbrev] t
  end;


(* patterns *)

fun prepare_patternT ctxt T =
  let
    val Mode {pattern, schematic, ...} = get_mode ctxt;
    val _ =
      pattern orelse schematic orelse
        T |> Term.exists_subtype
          (fn T as TVar (xi, _) =>
            not (Type_Infer.is_param xi) andalso
              error ("Illegal schematic type variable: " ^ Syntax.string_of_typ ctxt T)
          | _ => false)
  in T end;


local

val dummies =
  Config.bool (Config.declare ("Proof_Context.dummies", @{here}) (K (Config.Bool false)));

fun check_dummies ctxt t =
  if Config.get ctxt dummies then t
  else Term.no_dummy_patterns t handle TERM _ => error "Illegal dummy pattern(s) in term";

fun prepare_dummies ts = #1 (fold_map Term.replace_dummy_patterns ts 1);

in

val allow_dummies = Config.put dummies true;

fun prepare_patterns ctxt =
  let val Mode {pattern, ...} = get_mode ctxt in
    Type_Infer.fixate ctxt #>
    pattern ? Variable.polymorphic ctxt #>
    (map o Term.map_types) (prepare_patternT ctxt) #>
    (if pattern then prepare_dummies else map (check_dummies ctxt))
  end;

end;


(* sort constraints *)

local

fun prepare_sorts_env ctxt tys =
  let
    val tsig = tsig_of ctxt;
    val defaultS = Type.defaultS tsig;

    val dummy_var = ("'_dummy_", ~1);

    fun constraint (xi, raw_S) env =
      let val (ps, S) = Term_Position.decode_positionS raw_S in
        if xi = dummy_var orelse S = dummyS then env
        else
          Vartab.insert (op =) (xi, Type.minimize_sort tsig S) env
            handle Vartab.DUP _ =>
              error ("Inconsistent sort constraints for type variable " ^
                quote (Term.string_of_vname' xi) ^ Position.here_list ps)
      end;

    val env =
      (fold o fold_atyps)
        (fn TFree (x, S) => constraint ((x, ~1), S)
          | TVar v => constraint v
          | _ => I) tys Vartab.empty;

    fun get_sort xi raw_S =
      if xi = dummy_var then
        Type.minimize_sort tsig (#2 (Term_Position.decode_positionS raw_S))
      else
        (case (Vartab.lookup env xi, Variable.def_sort ctxt xi) of
          (NONE, NONE) => defaultS
        | (NONE, SOME S) => S
        | (SOME S, NONE) => S
        | (SOME S, SOME S') =>
            if Type.eq_sort tsig (S, S') then S'
            else
              error ("Sort constraint " ^ Syntax.string_of_sort ctxt S ^
                " inconsistent with default " ^ Syntax.string_of_sort ctxt S' ^
                " for type variable " ^ quote (Term.string_of_vname' xi)));

    fun add_report S pos reports =
      if Position.is_reported pos andalso not (AList.defined (op =) reports pos) then
        (pos, Position.reported_text pos Markup.sorting (Syntax.string_of_sort ctxt S)) :: reports
      else reports;

    fun get_sort_reports xi raw_S =
      let
        val ps = #1 (Term_Position.decode_positionS raw_S);
        val S = get_sort xi raw_S handle ERROR msg => error (msg ^ Position.here_list ps);
      in fold (add_report S) ps end;

    val reports =
      (fold o fold_atyps)
        (fn T =>
          if Term_Position.is_positionT T then I
          else
            (case T of
              TFree (x, raw_S) => get_sort_reports (x, ~1) raw_S
            | TVar (xi, raw_S) => get_sort_reports xi raw_S
            | _ => I)) tys [];

  in (map #2 reports, get_sort) end;

fun replace_sortsT get_sort =
  map_atyps
    (fn T =>
      if Term_Position.is_positionT T then T
      else
        (case T of
          TFree (x, raw_S) => TFree (x, get_sort (x, ~1) raw_S)
        | TVar (xi, raw_S) => TVar (xi, get_sort xi raw_S)
        | _ => T));

in

fun prepare_sortsT ctxt tys =
  let val (sorting_report, get_sort) = prepare_sorts_env ctxt tys
  in (sorting_report, map (replace_sortsT get_sort) tys) end;

fun prepare_sorts ctxt tms =
  let
    val tys = rev ((fold o fold_types) cons tms []);
    val (sorting_report, get_sort) = prepare_sorts_env ctxt tys;
  in (sorting_report, (map o map_types) (replace_sortsT get_sort) tms) end;

fun check_tfree ctxt v =
  let
    val (sorting_report, [TFree a]) = prepare_sortsT ctxt [TFree v];
    val _ = if Context_Position.is_visible ctxt then Output.report sorting_report else ();
  in a end;

end;


(* certify terms *)

local

fun gen_cert prop ctxt t =
  t
  |> expand_abbrevs ctxt
  |> (fn t' =>
      #1 (Sign.certify' prop (Context.pretty ctxt) false (consts_of ctxt) (theory_of ctxt) t')
        handle TYPE (msg, _, _) => error msg | TERM (msg, _) => error msg);

in

val cert_term = gen_cert false;
val cert_prop = gen_cert true;

end;


(* check/uncheck *)

fun def_type ctxt =
  let val Mode {pattern, ...} = get_mode ctxt
  in Variable.def_type ctxt pattern end;

fun standard_typ_check ctxt =
  map (cert_typ_mode (Type.get_mode ctxt) ctxt #> prepare_patternT ctxt);

val standard_term_check_finish = prepare_patterns;

fun standard_term_uncheck ctxt = map (contract_abbrevs ctxt);



(** export results **)

fun common_export is_goal inner outer =
  map (Assumption.export is_goal inner outer) #>
  Variable.export inner outer;

val goal_export = common_export true;
val export = common_export false;

fun export_morphism inner outer =
  Assumption.export_morphism inner outer $>
  Variable.export_morphism inner outer;

fun norm_export_morphism inner outer =
  export_morphism inner outer $>
  Morphism.thm_morphism "Proof_Context.norm_export" (Goal.norm_result outer);



(** term bindings **)

(* simult_matches *)

fun simult_matches ctxt (t, pats) =
  (case Seq.pull (Unify.matchers (theory_of ctxt) (map (rpair t) pats)) of
    NONE => error "Pattern match failed!"
  | SOME (env, _) => Vartab.fold (fn (v, (_, t)) => cons (v, t)) (Envir.term_env env) []);


(* bind_terms *)

val bind_terms = fold (fn (xi, t) => fn ctxt =>
  ctxt
  |> Variable.bind_term (xi, Option.map (cert_term (set_mode mode_default ctxt)) t));


(* auto_bind *)

fun drop_schematic (b as (xi, SOME t)) = if Term.exists_subterm is_Var t then (xi, NONE) else b
  | drop_schematic b = b;

fun auto_bind f ts ctxt = ctxt |> bind_terms (map drop_schematic (f (theory_of ctxt) ts));

val auto_bind_goal = auto_bind Auto_Bind.goal;
val auto_bind_facts = auto_bind Auto_Bind.facts;


(* match_bind(_i) *)

local

fun gen_bind prep_terms gen raw_binds ctxt =
  let
    fun prep_bind (raw_pats, t) ctxt1 =
      let
        val T = Term.fastype_of t;
        val ctxt2 = Variable.declare_term t ctxt1;
        val pats = prep_terms (set_mode mode_pattern ctxt2) T raw_pats;
        val binds = simult_matches ctxt2 (t, pats);
      in (binds, ctxt2) end;

    val ts = prep_terms ctxt dummyT (map snd raw_binds);
    val (binds, ctxt') = apfst flat (fold_map prep_bind (map fst raw_binds ~~ ts) ctxt);
    val binds' =
      if gen then map #1 binds ~~ Variable.exportT_terms ctxt' ctxt (map #2 binds)
      else binds;
    val binds'' = map (apsnd SOME) binds';
    val ctxt'' =
      tap (Variable.warn_extra_tfrees ctxt)
       (if gen then
          ctxt (*sic!*) |> fold Variable.declare_term (map #2 binds') |> bind_terms binds''
        else ctxt' |> bind_terms binds'');
  in (ts, ctxt'') end;

in

fun read_terms ctxt T =
  map (Syntax.parse_term ctxt #> Type.constraint T) #> Syntax.check_terms ctxt;

val match_bind = gen_bind read_terms;
val match_bind_i = gen_bind (fn ctxt => fn _ => map (cert_term ctxt));

end;


(* propositions with patterns *)

local

fun prep_propp mode prep_props args context =
  let
    fun prep (_, raw_pats) (ctxt, prop :: props) =
      let val ctxt' = Variable.declare_term prop ctxt
      in ((prop, prep_props (set_mode mode_pattern ctxt') raw_pats), (ctxt', props)) end;

    val (propp, (context', _)) =
      (fold_map o fold_map) prep args
        (context, prep_props (set_mode mode context) (maps (map fst) args));
  in (propp, context') end;

fun gen_bind_propp mode parse_prop raw_args ctxt =
  let
    val (args, ctxt') = prep_propp mode parse_prop raw_args ctxt;
    val binds = flat (flat (map (map (simult_matches ctxt')) args));
    val propss = map (map #1) args;
    fun gen_binds ctxt0 = ctxt0
      |> bind_terms (map #1 binds ~~
          map (SOME o Term.close_schematic_term) (Variable.export_terms ctxt' ctxt0 (map #2 binds)));
  in ((propss, gen_binds), ctxt' |> bind_terms (map (apsnd SOME) binds)) end;

in

val read_propp           = prep_propp mode_default Syntax.read_props;
val cert_propp           = prep_propp mode_default (map o cert_prop);
val read_propp_schematic = prep_propp mode_schematic Syntax.read_props;
val cert_propp_schematic = prep_propp mode_schematic (map o cert_prop);

val bind_propp             = gen_bind_propp mode_default Syntax.read_props;
val bind_propp_i           = gen_bind_propp mode_default (map o cert_prop);
val bind_propp_schematic   = gen_bind_propp mode_schematic Syntax.read_props;
val bind_propp_schematic_i = gen_bind_propp mode_schematic (map o cert_prop);

end;



(** theorems **)

(* fact_tac *)

local

fun comp_hhf_tac th i st =
  PRIMSEQ (Thm.bicompose {flatten = true, match = false, incremented = true}
    (false, Drule.lift_all (Thm.cprem_of st i) th, 0) i) st;

fun comp_incr_tac [] _ = no_tac
  | comp_incr_tac (th :: ths) i =
      (fn st => comp_hhf_tac (Drule.incr_indexes st th) i st) APPEND comp_incr_tac ths i;

in

fun fact_tac ctxt facts = Goal.norm_hhf_tac ctxt THEN' comp_incr_tac facts;

fun potential_facts ctxt prop =
  Facts.could_unify (facts_of ctxt) (Term.strip_all_body prop);

fun some_fact_tac ctxt = SUBGOAL (fn (goal, i) => fact_tac ctxt (potential_facts ctxt goal) i);

end;


(* get facts *)

local

fun retrieve_global context =
  Facts.retrieve context (Global_Theory.facts_of (Context.theory_of context));

fun retrieve_generic (context as Context.Proof ctxt) arg =
      (Facts.retrieve context (facts_of ctxt) arg handle ERROR local_msg =>
        (retrieve_global context arg handle ERROR _ => error local_msg))
  | retrieve_generic context arg = retrieve_global context arg;

fun retrieve pick context (Facts.Fact s) =
      let
        val ctxt = Context.the_proof context;
        val pos = Syntax.read_token_pos s;
        val prop =
          Syntax.read_prop (ctxt |> set_mode mode_default |> allow_dummies) s
          |> singleton (Variable.polymorphic ctxt);
        fun err msg = error (msg ^ Position.here pos ^ ":\n" ^ Syntax.string_of_term ctxt prop);

        val (prop', _) = Term.replace_dummy_patterns prop (Variable.maxidx_of ctxt + 1);
        fun prove_fact th =
          Goal.prove ctxt [] [] prop' (K (ALLGOALS (fact_tac ctxt [th])));
        val results = map_filter (try prove_fact) (potential_facts ctxt prop');
        val thm =
          (case distinct Thm.eq_thm_prop results of
            [thm] => thm
          | [] => err "Failed to retrieve literal fact"
          | _ => err "Ambiguous specification of literal fact");
      in pick ("", Position.none) [thm] end
  | retrieve pick context (Facts.Named ((xname, pos), ivs)) =
      let
        val thy = Context.theory_of context;
        val (name, thms) =
          (case xname of
            "" => (xname, [Thm.transfer thy Drule.dummy_thm])
          | "_" => (xname, [Thm.transfer thy Drule.asm_rl])
          | _ => retrieve_generic context (xname, pos));
      in pick (name, pos) (Facts.select (Facts.Named ((name, pos), ivs)) thms) end;

in

val get_fact_generic = retrieve (K I);
val get_fact = retrieve (K I) o Context.Proof;
val get_fact_single = retrieve Facts.the_single o Context.Proof;

fun get_thms ctxt = get_fact ctxt o Facts.named;
fun get_thm ctxt = get_fact_single ctxt o Facts.named;

end;


(* facts *)

local

fun update_thms _ (b, NONE) ctxt = ctxt |> map_facts (Facts.del (full_name ctxt b))
  | update_thms flags (b, SOME ths) ctxt = ctxt |> map_facts
      (Facts.add_static (Context.Proof ctxt) flags (b, ths) #> snd);

in

fun note_thmss kind = fold_map (fn ((b, more_atts), raw_facts) => fn ctxt =>
  let
    val name = full_name ctxt b;
    val facts = Global_Theory.name_thmss false name raw_facts;
    fun app (ths, atts) =
      fold_map (Thm.proof_attributes (surround (Thm.kind kind) (atts @ more_atts))) ths;
    val (res, ctxt') = fold_map app facts ctxt;
    val thms = Global_Theory.name_thms false false name (flat res);
    val Mode {stmt, ...} = get_mode ctxt;
  in ((name, thms), ctxt' |> update_thms {strict = false, index = stmt} (b, SOME thms)) end);

fun put_thms index thms ctxt = ctxt
  |> map_naming (K Name_Space.local_naming)
  |> Context_Position.set_visible false
  |> update_thms {strict = false, index = index} (apfst Binding.name thms)
  |> Context_Position.restore_visible ctxt
  |> restore_naming ctxt;

end;



(** basic logical entities **)

(* variables *)

fun declare_var (x, opt_T, mx) ctxt =
  let val T = (case opt_T of SOME T => T | NONE => Mixfix.mixfixT mx)
  in ((x, T, mx), ctxt |> Variable.declare_constraints (Free (x, T))) end;

fun check_var internal b =
  let
    val x = Variable.check_name b;
    val check = if internal then Name.reject_skolem else Name.reject_internal;
    val _ =
      if can check (x, []) andalso Symbol_Pos.is_identifier x then ()
      else error ("Bad name: " ^ Binding.print b);
  in x end;

local

fun prep_vars prep_typ internal =
  fold_map (fn (b, raw_T, mx) => fn ctxt =>
    let
      val x = check_var internal b;
      fun cond_tvars T =
        if internal then T
        else Type.no_tvars T handle TYPE (msg, _, _) => error msg;
      val opt_T = Option.map (cond_tvars o cert_typ ctxt o prep_typ ctxt) raw_T;
      val (_, ctxt') = ctxt |> declare_var (x, opt_T, mx);
    in ((b, opt_T, mx), ctxt') end);

in

val read_vars = prep_vars Syntax.read_typ false;
val cert_vars = prep_vars (K I) true;

end;


(* notation *)

local

fun type_syntax (Type (c, args), mx) =
      SOME (Local_Syntax.Type, (Lexicon.mark_type c, Mixfix.make_type (length args), mx))
  | type_syntax _ = NONE;

fun const_syntax _ (Free (x, T), mx) = SOME (Local_Syntax.Fixed, (x, T, mx))
  | const_syntax ctxt (Const (c, _), mx) =
      (case try (Consts.type_scheme (consts_of ctxt)) c of
        SOME T => SOME (Local_Syntax.Const, (Lexicon.mark_const c, T, mx))
      | NONE => NONE)
  | const_syntax _ _ = NONE;

fun gen_notation syntax add mode args ctxt =
  ctxt |> map_syntax
    (Local_Syntax.update_modesyntax (theory_of ctxt) add mode (map_filter (syntax ctxt) args));

in

val type_notation = gen_notation (K type_syntax);
val notation = gen_notation const_syntax;

fun generic_type_notation add mode args phi =
  let
    val args' = args |> map_filter (fn (T, mx) =>
      let
        val T' = Morphism.typ phi T;
        val similar = (case (T, T') of (Type (c, _), Type (c', _)) => c = c' | _ => false);
      in if similar then SOME (T', mx) else NONE end);
  in Context.mapping (Sign.type_notation add mode args') (type_notation add mode args') end;

fun generic_notation add mode args phi =
  let
    val args' = args |> map_filter (fn (t, mx) =>
      let val t' = Morphism.term phi t
      in if Term.aconv_untyped (t, t') then SOME (t', mx) else NONE end);
  in Context.mapping (Sign.notation add mode args') (notation add mode args') end;

end;


(* aliases *)

fun class_alias b c ctxt = (map_tsig o apfst) (Type.class_alias (naming_of ctxt) b c) ctxt;
fun type_alias b c ctxt = (map_tsig o apfst) (Type.type_alias (naming_of ctxt) b c) ctxt;
fun const_alias b c ctxt = (map_consts o apfst) (Consts.alias (naming_of ctxt) b c) ctxt;


(* local constants *)

fun add_const_constraint (c, opt_T) ctxt =
  let
    fun prepT raw_T =
      let val T = cert_typ ctxt raw_T
      in cert_term ctxt (Const (c, T)); T end;
  in ctxt |> (map_consts o apfst) (Consts.constrain (c, Option.map prepT opt_T)) end;

fun add_abbrev mode (b, raw_t) ctxt =
  let
    val t0 = cert_term (ctxt |> set_mode mode_abbrev) raw_t
      handle ERROR msg => cat_error msg ("in constant abbreviation " ^ Binding.print b);
    val [t] = Variable.exportT_terms (Variable.declare_term t0 ctxt) ctxt [t0];
    val ((lhs, rhs), consts') = consts_of ctxt
      |> Consts.abbreviate (Context.Proof ctxt) (tsig_of ctxt) mode (b, t);
  in
    ctxt
    |> (map_consts o apfst) (K consts')
    |> Variable.declare_term rhs
    |> pair (lhs, rhs)
  end;

fun revert_abbrev mode c = (map_consts o apfst) (Consts.revert_abbrev mode c);

fun generic_add_abbrev mode arg =
  Context.mapping_result (Sign.add_abbrev mode arg) (add_abbrev mode arg);

fun generic_revert_abbrev mode arg =
  Context.mapping (Sign.revert_abbrev mode arg) (revert_abbrev mode arg);


(* fixes *)

fun add_fixes raw_vars ctxt =
  let
    val thy = theory_of ctxt;
    val vars = #1 (cert_vars raw_vars ctxt);
  in
    ctxt
    |> Variable.add_fixes_binding (map #1 vars)
    |-> (fn xs =>
      fold_map declare_var (map2 (fn x => fn (_, T, mx) => (x, T, mx)) xs vars)
      #-> (map_syntax o Local_Syntax.add_syntax thy o map (pair Local_Syntax.Fixed))
      #> pair xs)
  end;



(** assumptions **)

local

fun gen_assms prepp exp args ctxt =
  let
    val cert = Thm.cterm_of (theory_of ctxt);
    val ((propss, _), ctxt1) = prepp (map snd args) ctxt;
    val _ = Variable.warn_extra_tfrees ctxt ctxt1;
    val (premss, ctxt2) = fold_burrow (Assumption.add_assms exp o map cert) propss ctxt1;
  in
    ctxt2
    |> auto_bind_facts (flat propss)
    |> note_thmss "" (map fst args ~~ map (map (fn th => ([th], []))) premss)
  end;

in

val add_assms = gen_assms bind_propp;
val add_assms_i = gen_assms bind_propp_i;

end;



(** cases **)

fun dest_cases ctxt =
  Name_Space.fold_table (fn (a, (c, i)) => cons (i, (a, c))) (cases_of ctxt) []
  |> sort (int_ord o pairself #1) |> map #2;

local

fun update_case _ _ ("", _) res = res
  | update_case _ _ (name, NONE) (cases, index) =
      (Name_Space.del_table name cases, index)
  | update_case context is_proper (name, SOME c) (cases, index) =
      let
        val binding =
          Binding.make (name, Position.none)
          |> not is_proper ? Binding.conceal;
        val (_, cases') = cases
          |> Name_Space.define context false (binding, ((c, is_proper), index));
        val index' = index + 1;
      in (cases', index') end;

fun fix (b, T) ctxt =
  let val ([x], ctxt') = add_fixes [(b, SOME T, NoSyn)] ctxt
  in (Free (x, T), ctxt') end;

in

fun update_cases is_proper args ctxt =
  let
    val context = Context.Proof ctxt |> Name_Space.map_naming (K Name_Space.default_naming);
    val cases = cases_of ctxt;
    val index = Name_Space.fold_table (fn _ => Integer.add 1) cases 0;
    val (cases', _) = fold (update_case context is_proper) args (cases, index);
  in map_cases (K cases') ctxt end;

fun case_result c ctxt =
  let
    val Rule_Cases.Case {fixes, ...} = c;
    val (ts, ctxt') = ctxt |> fold_map fix fixes;
    val Rule_Cases.Case {assumes, binds, cases, ...} = Rule_Cases.apply ts c;
  in
    ctxt'
    |> bind_terms (map drop_schematic binds)
    |> update_cases true (map (apsnd SOME) cases)
    |> pair (assumes, (binds, cases))
  end;

val apply_case = apfst fst oo case_result;

fun check_case ctxt internal (name, pos) fxs =
  let
    val (_, ((Rule_Cases.Case {fixes, assumes, binds, cases}, is_proper), _)) =
      Name_Space.check (Context.Proof ctxt) (cases_of ctxt) (name, pos);
    val _ = if is_proper then () else Context_Position.report ctxt pos Markup.improper;

    val _ = List.app (fn NONE => () | SOME b => ignore (check_var internal b)) fxs;
    fun replace (opt_x :: xs) ((y, T) :: ys) = (the_default y opt_x, T) :: replace xs ys
      | replace [] ys = ys
      | replace (_ :: _) [] =
          error ("Too many parameters for case " ^ quote name ^ Position.here pos);
    val fixes' = replace fxs fixes;
    val binds' = map drop_schematic binds;
  in
    if null (fold (Term.add_tvarsT o snd) fixes []) andalso
      null (fold (fold Term.add_vars o snd) assumes []) then
        Rule_Cases.Case {fixes = fixes', assumes = assumes, binds = binds', cases = cases}
    else error ("Illegal schematic variable(s) in case " ^ quote name ^ Position.here pos)
  end;

end;



(** print context information **)

(* local syntax *)

val print_syntax = Syntax.print_syntax o syn_of;


(* abbreviations *)

fun pretty_abbrevs show_globals ctxt =
  let
    val space = const_space ctxt;
    val (constants, global_constants) =
      pairself (#constants o Consts.dest) (#consts (rep_data ctxt));
    val globals = Symtab.make global_constants;
    fun add_abbr (_, (_, NONE)) = I
      | add_abbr (c, (T, SOME t)) =
          if not show_globals andalso Symtab.defined globals c then I
          else cons (c, Logic.mk_equals (Const (c, T), t));
    val abbrevs = Name_Space.markup_entries ctxt space (fold add_abbr constants []);
  in
    if null abbrevs then []
    else [Pretty.big_list "abbreviations:" (map (pretty_term_abbrev ctxt o #2) abbrevs)]
  end;

val print_abbrevs = Pretty.writeln_chunks o pretty_abbrevs true;


(* term bindings *)

fun pretty_term_bindings ctxt =
  let
    val binds = Variable.binds_of ctxt;
    fun prt_bind (xi, (T, t)) = pretty_term_abbrev ctxt (Logic.mk_equals (Var (xi, T), t));
  in
    if Vartab.is_empty binds then []
    else [Pretty.big_list "term bindings:" (map prt_bind (Vartab.dest binds))]
  end;


(* local facts *)

fun pretty_local_facts ctxt verbose =
  let
    val facts = facts_of ctxt;
    val props = Facts.props facts;
    val local_facts =
      (if null props then [] else [("<unnamed>", props)]) @
      Facts.dest_static verbose [Global_Theory.facts_of (theory_of ctxt)] facts;
  in
    if null local_facts then []
    else
      [Pretty.big_list "local facts:"
        (map #1 (sort_wrt (#1 o #2) (map (`(pretty_fact ctxt)) local_facts)))]
  end;

fun print_local_facts ctxt verbose =
  Pretty.writeln_chunks (pretty_local_facts ctxt verbose);


(* local contexts *)

local

fun pretty_case (name, (fixes, ((asms, (lets, cs)), ctxt))) =
  let
    val prt_term = Syntax.pretty_term ctxt;

    fun prt_let (xi, t) = Pretty.block
      [Pretty.quote (prt_term (Var (xi, Term.fastype_of t))), Pretty.str " =", Pretty.brk 1,
        Pretty.quote (prt_term t)];

    fun prt_asm (b, ts) = Pretty.block (Pretty.breaks
      ((if Binding.is_empty b then []
        else [Binding.pretty b, Pretty.str ":"]) @ map (Pretty.quote o prt_term) ts));

    fun prt_sect _ _ _ [] = []
      | prt_sect head sep prt xs =
          [Pretty.block (Pretty.breaks (head ::
            flat (separate sep (map (single o prt) xs))))];
  in
    Pretty.block (Pretty.fbreaks
      (Pretty.str (name ^ ":") ::
        prt_sect (Pretty.keyword1 "fix") [] (Pretty.str o Binding.name_of o fst) fixes @
        prt_sect (Pretty.keyword1 "let") [Pretty.keyword2 "and"] prt_let
          (map_filter (fn (xi, SOME t) => SOME (xi, t) | _ => NONE) lets) @
        (if forall (null o #2) asms then []
          else prt_sect (Pretty.keyword1 "assume") [Pretty.keyword2 "and"] prt_asm asms) @
        prt_sect (Pretty.str "subcases:") [] (Pretty.str o fst) cs))
  end;

in

fun pretty_cases ctxt =
  let
    fun mk_case (_, (_, false)) = NONE
      | mk_case (name, (c as Rule_Cases.Case {fixes, ...}, true)) =
          SOME (name, (fixes, case_result c ctxt));
    val cases = dest_cases ctxt |> map_filter mk_case;
  in
    if null cases then []
    else [Pretty.big_list "cases:" (map pretty_case cases)]
  end;

end;


(* core context *)

val debug =
  Config.bool (Config.declare ("Proof_Context.debug", @{here}) (K (Config.Bool false)));

val verbose =
  Config.bool (Config.declare ("Proof_Context.verbose", @{here}) (K (Config.Bool false)));

fun pretty_ctxt ctxt =
  if not (Config.get ctxt debug) then []
  else
    let
      val prt_term = Syntax.pretty_term ctxt;

      (*structures*)
      val {structs, ...} = Local_Syntax.idents_of (syntax_of ctxt);
      val prt_structs =
        if null structs then []
        else [Pretty.block (Pretty.str "structures:" :: Pretty.brk 1 ::
          Pretty.commas (map Pretty.str structs))];

      (*fixes*)
      fun prt_fix (x, x') =
        if x = x' then Pretty.str x
        else Pretty.block [Pretty.str x, Pretty.str " =", Pretty.brk 1, prt_term (Syntax.free x')];
      val fixes =
        filter_out ((Name.is_internal orf member (op =) structs) o #1)
          (Variable.dest_fixes ctxt);
      val prt_fixes =
        if null fixes then []
        else [Pretty.block (Pretty.str "fixed variables:" :: Pretty.brk 1 ::
          Pretty.commas (map prt_fix fixes))];

      (*prems*)
      val prt_prems =
        (case Assumption.all_prems_of ctxt of
          [] => []
        | prems => [Pretty.big_list "prems:" [pretty_fact ctxt ("", prems)]]);
    in prt_structs @ prt_fixes @ prt_prems end;


(* main context *)

fun pretty_context ctxt =
  let
    val verbose = Config.get ctxt verbose;
    fun verb f x = if verbose then f (x ()) else [];

    val prt_term = Syntax.pretty_term ctxt;
    val prt_typ = Syntax.pretty_typ ctxt;
    val prt_sort = Syntax.pretty_sort ctxt;

    (*theory*)
    val pretty_thy = Pretty.block
      [Pretty.str "theory:", Pretty.brk 1, Context.pretty_thy (theory_of ctxt)];

    (*defaults*)
    fun prt_atom prt prtT (x, X) = Pretty.block
      [prt x, Pretty.str " ::", Pretty.brk 1, prtT X];

    fun prt_var (x, ~1) = prt_term (Syntax.free x)
      | prt_var xi = prt_term (Syntax.var xi);

    fun prt_varT (x, ~1) = prt_typ (TFree (x, []))
      | prt_varT xi = prt_typ (TVar (xi, []));

    val prt_defT = prt_atom prt_var prt_typ;
    val prt_defS = prt_atom prt_varT prt_sort;

    val (types, sorts) = Variable.constraints_of ctxt;
  in
    verb single (K pretty_thy) @
    pretty_ctxt ctxt @
    verb (pretty_abbrevs false) (K ctxt) @
    verb pretty_term_bindings (K ctxt) @
    verb (pretty_local_facts ctxt) (K true) @
    verb pretty_cases (K ctxt) @
    verb single (fn () => Pretty.big_list "type constraints:" (map prt_defT (Vartab.dest types))) @
    verb single (fn () => Pretty.big_list "default sorts:" (map prt_defS (Vartab.dest sorts)))
  end;

end;

val show_abbrevs = Proof_Context.show_abbrevs;

