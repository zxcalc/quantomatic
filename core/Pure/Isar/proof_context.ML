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
  val mode_pattern: mode
  val mode_schematic: mode
  val mode_abbrev: mode
  val set_mode: mode -> Proof.context -> Proof.context
  val get_mode: Proof.context -> mode
  val restore_mode: Proof.context -> Proof.context -> Proof.context
  val abbrev_mode: Proof.context -> bool
  val syn_of: Proof.context -> Syntax.syntax
  val tsig_of: Proof.context -> Type.tsig
  val set_defsort: sort -> Proof.context -> Proof.context
  val default_sort: Proof.context -> indexname -> sort
  val arity_sorts: Proof.context -> string -> sort -> sort list
  val consts_of: Proof.context -> Consts.T
  val set_syntax_mode: Syntax.mode -> Proof.context -> Proof.context
  val restore_syntax_mode: Proof.context -> Proof.context -> Proof.context
  val map_naming: (Name_Space.naming -> Name_Space.naming) -> Proof.context -> Proof.context
  val naming_of: Proof.context -> Name_Space.naming
  val restore_naming: Proof.context -> Proof.context -> Proof.context
  val full_name: Proof.context -> binding -> string
  val get_scope: Proof.context -> Binding.scope option
  val new_scope: Proof.context -> Binding.scope * Proof.context
  val private_scope: Binding.scope -> Proof.context -> Proof.context
  val private: Position.T -> Proof.context -> Proof.context
  val qualified_scope: Binding.scope -> Proof.context -> Proof.context
  val qualified: Position.T -> Proof.context -> Proof.context
  val concealed: Proof.context -> Proof.context
  val class_space: Proof.context -> Name_Space.T
  val type_space: Proof.context -> Name_Space.T
  val const_space: Proof.context -> Name_Space.T
  val defs_context: Proof.context -> Defs.context
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
  val transfer_facts: theory -> Proof.context -> Proof.context
  val background_theory: (theory -> theory) -> Proof.context -> Proof.context
  val background_theory_result: (theory -> 'a * theory) -> Proof.context -> 'a * Proof.context
  val facts_of: Proof.context -> Facts.T
  val facts_of_fact: Proof.context -> string -> Facts.T
  val markup_extern_fact: Proof.context -> string -> Markup.T list * xstring
  val print_name: Proof.context -> string -> string
  val pretty_name: Proof.context -> string -> Pretty.T
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
  val inferred_param: string -> Proof.context -> (string * typ) * Proof.context
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
  val auto_bind_goal: term list -> Proof.context -> Proof.context
  val auto_bind_facts: term list -> Proof.context -> Proof.context
  val match_bind: bool -> (term list * term) list -> Proof.context ->
    term list * Proof.context
  val match_bind_cmd: bool -> (string list * string) list -> Proof.context ->
    term list * Proof.context
  val cert_propp: Proof.context -> (term * term list) list list ->
    (term list list * (indexname * term) list)
  val read_propp: Proof.context -> (string * string list) list list ->
    (term list list * (indexname * term) list)
  val fact_tac: Proof.context -> thm list -> int -> tactic
  val some_fact_tac: Proof.context -> int -> tactic
  val lookup_fact: Proof.context -> string -> {dynamic: bool, thms: thm list} option
  val dynamic_facts_dummy: bool Config.T
  val get_fact_generic: Context.generic -> Facts.ref -> string option * thm list
  val get_fact: Proof.context -> Facts.ref -> thm list
  val get_fact_single: Proof.context -> Facts.ref -> thm
  val get_thms: Proof.context -> xstring -> thm list
  val get_thm: Proof.context -> xstring -> thm
  val set_stmt: bool -> Proof.context -> Proof.context
  val restore_stmt: Proof.context -> Proof.context -> Proof.context
  val add_thms_dynamic: binding * (Context.generic -> thm list) ->
    Proof.context -> string * Proof.context
  val note_thmss: string -> (Thm.binding * (thm list * attribute list) list) list ->
    Proof.context -> (string * thm list) list * Proof.context
  val put_thms: bool -> string * thm list option -> Proof.context -> Proof.context
  val read_var: binding * string option * mixfix -> Proof.context ->
    (binding * typ option * mixfix) * Proof.context
  val cert_var: binding * typ option * mixfix -> Proof.context ->
    (binding * typ option * mixfix) * Proof.context
  val add_fixes: (binding * typ option * mixfix) list -> Proof.context ->
    string list * Proof.context
  val add_fixes_cmd: (binding * string option * mixfix) list -> Proof.context ->
    string list * Proof.context
  val add_assms: Assumption.export ->
    (Thm.binding * (term * term list) list) list ->
    Proof.context -> (string * thm list) list * Proof.context
  val add_assms_cmd: Assumption.export ->
    (Thm.binding * (string * string list) list) list ->
    Proof.context -> (string * thm list) list * Proof.context
  val dest_cases: Proof.context option -> Proof.context -> (string * (Rule_Cases.T * {legacy: bool})) list
  val update_cases: (string * Rule_Cases.T option) list -> Proof.context -> Proof.context
  val update_cases_legacy: (string * Rule_Cases.T option) list -> Proof.context -> Proof.context
  val apply_case: Rule_Cases.T -> Proof.context -> (string * term list) list * Proof.context
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
  val fact_alias: binding -> string -> Proof.context -> Proof.context
  val add_const_constraint: string * typ option -> Proof.context -> Proof.context
  val add_abbrev: string -> binding * term -> Proof.context -> (term * term) * Proof.context
  val revert_abbrev: string -> string -> Proof.context -> Proof.context
  val generic_add_abbrev: string -> binding * term -> Context.generic ->
    (term * term) * Context.generic
  val generic_revert_abbrev: string -> string -> Context.generic -> Context.generic
  val cert_stmt:
    (binding * typ option * mixfix) list -> (term * term list) list list -> Proof.context ->
    (((binding * typ option * mixfix) * (string * term)) list * term list list *
      (indexname * term) list * (indexname * term) list) * Proof.context
  val read_stmt:
    (binding * string option * mixfix) list -> (string * string list) list list -> Proof.context ->
    (((binding * typ option * mixfix) * (string * term)) list * term list list *
      (indexname * term) list * (indexname * term) list) * Proof.context
  val cert_statement: (binding * typ option * mixfix) list ->
    (term * term list) list list -> (term * term list) list list -> Proof.context ->
    ((string * term) list * term list list * term list list * (indexname * term option) list) *
      Proof.context
  val read_statement: (binding * string option * mixfix) list ->
    (string * string list) list list -> (string * string list) list list -> Proof.context ->
    ((string * term) list * term list list * term list list * (indexname * term option) list) *
      Proof.context
  val print_syntax: Proof.context -> unit
  val print_abbrevs: bool -> Proof.context -> unit
  val pretty_term_bindings: Proof.context -> Pretty.T list
  val pretty_local_facts: bool -> Proof.context -> Pretty.T list
  val print_local_facts: bool -> Proof.context -> unit
  val pretty_cases: Proof.context -> Pretty.T list
  val print_cases_proof: Proof.context -> Proof.context -> string
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
   {pattern: bool,             (*pattern binding schematic variables*)
    schematic: bool,           (*term referencing loose schematic variables*)
    abbrev: bool};             (*abbrev mode -- no normalization*)

fun make_mode (pattern, schematic, abbrev) =
  Mode {pattern = pattern, schematic = schematic, abbrev = abbrev};

val mode_default   = make_mode (false, false, false);
val mode_pattern   = make_mode (true, false, false);
val mode_schematic = make_mode (false, true, false);
val mode_abbrev    = make_mode (false, false, true);


(** Isar proof context information **)

type cases = (Rule_Cases.T * {legacy: bool}) Name_Space.table;
val empty_cases: cases = Name_Space.empty_table Markup.caseN;

datatype data =
  Data of
   {mode: mode,                  (*inner syntax mode*)
    syntax: Local_Syntax.T,      (*local syntax*)
    tsig: Type.tsig * Type.tsig, (*local/global type signature -- local name space / defsort only*)
    consts: Consts.T * Consts.T, (*local/global consts -- local name space / abbrevs only*)
    facts: Facts.T,              (*local facts, based on initial global facts*)
    cases: cases};               (*named case contexts: case, legacy, running index*)

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

fun map_data_result f ctxt =
  let
    val Data {mode, syntax, tsig, consts, facts, cases} = Data.get ctxt;
    val (res, data') = f (mode, syntax, tsig, consts, facts, cases) ||> make_data;
  in (res, Data.put data' ctxt) end;

fun map_data f = snd o map_data_result (pair () o f);

fun set_mode mode = map_data (fn (_, syntax, tsig, consts, facts, cases) =>
  (mode, syntax, tsig, consts, facts, cases));

fun map_syntax f =
  map_data (fn (mode, syntax, tsig, consts, facts, cases) =>
    (mode, f syntax, tsig, consts, facts, cases));

fun map_syntax_idents f ctxt =
  let val (opt_idents', syntax') = f (#syntax (rep_data ctxt)) in
    ctxt
    |> map_syntax (K syntax')
    |> (case opt_idents' of NONE => I | SOME idents' => Syntax_Trans.put_idents idents')
  end;

fun map_tsig f =
  map_data (fn (mode, syntax, tsig, consts, facts, cases) =>
    (mode, syntax, f tsig, consts, facts, cases));

fun map_consts f =
  map_data (fn (mode, syntax, tsig, consts, facts, cases) =>
    (mode, syntax, tsig, f consts, facts, cases));

fun map_facts_result f =
  map_data_result (fn (mode, syntax, tsig, consts, facts, cases) =>
    let val (res, facts') = f facts
    in (res, (mode, syntax, tsig, consts, facts', cases)) end);

fun map_facts f = snd o map_facts_result (pair () o f);

fun map_cases f =
  map_data (fn (mode, syntax, tsig, consts, facts, cases) =>
    (mode, syntax, tsig, consts, facts, f cases));

val get_mode = #mode o rep_data;
val restore_mode = set_mode o get_mode;
val abbrev_mode = get_mode #> (fn Mode {abbrev, ...} => abbrev);

val syntax_of = #syntax o rep_data;
val syn_of = Local_Syntax.syn_of o syntax_of;
val set_syntax_mode = map_syntax o Local_Syntax.set_mode;
val restore_syntax_mode = map_syntax o Local_Syntax.restore_mode o syntax_of;

val tsig_of = #1 o #tsig o rep_data;
val set_defsort = map_tsig o apfst o Type.set_defsort;
fun default_sort ctxt = the_default (Type.defaultS (tsig_of ctxt)) o Variable.def_sort ctxt;
fun arity_sorts ctxt = Type.arity_sorts (Context.Proof ctxt) (tsig_of ctxt);

val consts_of = #1 o #consts o rep_data;
val cases_of = #cases o rep_data;


(* naming *)

val naming_of = Name_Space.naming_of o Context.Proof;
val map_naming = Context.proof_map o Name_Space.map_naming;
val restore_naming = map_naming o K o naming_of;

val full_name = Name_Space.full_name o naming_of;

val get_scope = Name_Space.get_scope o naming_of;

fun new_scope ctxt =
  let
    val (scope, naming') = Name_Space.new_scope (naming_of ctxt);
    val ctxt' = map_naming (K naming') ctxt;
  in (scope, ctxt') end;

val private_scope = map_naming o Name_Space.private_scope;
val private = map_naming o Name_Space.private;
val qualified_scope = map_naming o Name_Space.qualified_scope;
val qualified = map_naming o Name_Space.qualified;

val concealed = map_naming Name_Space.concealed;


(* name spaces *)

val class_space = Type.class_space o tsig_of;
val type_space = Type.type_space o tsig_of;
val const_space = Consts.space_of o consts_of;

fun defs_context ctxt = (ctxt, (const_space ctxt, type_space ctxt));

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
      else (Type.merge_tsig (Context.Proof ctxt) (local_tsig, thy_tsig), thy_tsig)  (*historic merge order*)
    end) |>
  map_consts (fn consts as (local_consts, global_consts) =>
    let val thy_consts = Sign.consts_of thy in
      if Consts.eq_consts (thy_consts, global_consts) then consts
      else (Consts.merge (local_consts, thy_consts), thy_consts)  (*historic merge order*)
    end);

fun transfer thy = Context.raw_transfer thy #> transfer_syntax thy;

fun transfer_facts thy =
  map_facts (fn local_facts => Facts.merge (Global_Theory.facts_of thy, local_facts));

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
  let
    val facts = facts_of_fact ctxt name;
    val (markup, xname) = Facts.markup_extern ctxt facts name;
    val markups =
      if Facts.is_dynamic facts name then [markup, Markup.dynamic_fact name]
      else [markup];
  in (markups, xname) end;



(** pretty printing **)

val print_name = Token.print_name o Thy_Header.get_keywords';
val pretty_name = Pretty.str oo print_name;

fun pretty_term_abbrev ctxt = Syntax.pretty_term (set_mode mode_abbrev ctxt);

fun pretty_fact_name ctxt a =
  Pretty.block [Pretty.marks_str (markup_extern_fact ctxt a), Pretty.str ":"];

fun pretty_fact ctxt =
  let
    val pretty_thm = Thm.pretty_thm ctxt;
    val pretty_thms = map (Thm.pretty_thm_item ctxt);
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
    val source = Syntax.read_input text;
    val (c, reports) = check_class ctxt (Input.source_content source, Input.pos_of source);
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
  let val p = (x, infer_type ctxt (x, dummyT))
  in (p, ctxt |> Variable.declare_term (Free p)) end;

fun inferred_fixes ctxt =
  fold_map inferred_param (map #2 (Variable.dest_fixes ctxt)) ctxt;


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
    val source = Syntax.read_input text;
    val (T, reports) =
      check_type_name ctxt flags (Input.source_content source, Input.pos_of source);
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
    val source = Syntax.read_input text;
    val (t, reports) = check_const ctxt flags (Input.source_content source, [Input.pos_of source]);
    val _ = Position.reports reports;
  in t end;


(* type arities *)

local

fun prep_arity prep_tycon prep_sort ctxt (t, Ss, S) =
  let val arity = (prep_tycon ctxt t, map (prep_sort ctxt) Ss, prep_sort ctxt S)
  in Type.add_arity (Context.Proof ctxt) arity (tsig_of ctxt); arity end;

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

fun certify_consts ctxt =
  Consts.certify (Context.Proof ctxt) (tsig_of ctxt)
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
    Type_Infer.fixate ctxt pattern #>
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
      #1 (Sign.certify' prop (Context.Proof ctxt) false (consts_of ctxt) (theory_of ctxt) t')
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
  (case Seq.pull (Unify.matchers (Context.Proof ctxt) (map (rpair t) pats)) of
    NONE => error "Pattern match failed!"
  | SOME (env, _) => Vartab.fold (fn (v, (_, t)) => cons (v, t)) (Envir.term_env env) []);


(* auto_bind *)

fun auto_bind f props ctxt = fold Variable.maybe_bind_term (f ctxt props) ctxt;

val auto_bind_goal = auto_bind Auto_Bind.goal;
val auto_bind_facts = auto_bind Auto_Bind.facts;


(* bind terms (non-schematic) *)

fun cert_maybe_bind_term (xi, t) ctxt =
  ctxt
  |> Variable.maybe_bind_term (xi, Option.map (cert_term (set_mode mode_default ctxt)) t);

val cert_bind_term = cert_maybe_bind_term o apsnd SOME;


(* match_bind *)

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
    val ctxt'' =
      tap (Variable.warn_extra_tfrees ctxt)
       (if gen then
          ctxt (*sic!*)
          |> fold Variable.declare_term (map #2 binds')
          |> fold cert_bind_term binds'
        else ctxt' |> fold cert_bind_term binds');
  in (ts, ctxt'') end;

in

fun read_terms ctxt T =
  map (Syntax.parse_term ctxt #> Type.constraint T) #> Syntax.check_terms ctxt;

val match_bind = gen_bind (fn ctxt => fn _ => map (cert_term ctxt));
val match_bind_cmd = gen_bind read_terms;

end;


(* propositions with patterns *)

local

fun prep_propp prep_props ctxt raw_args =
  let
    val props = prep_props ctxt (maps (map fst) raw_args);
    val props_ctxt = fold Variable.declare_term props ctxt;
    val patss = maps (map (prep_props (set_mode mode_pattern props_ctxt) o snd)) raw_args;

    val propps = unflat raw_args (props ~~ patss);
    val binds = (maps o maps) (simult_matches props_ctxt) propps;
  in (map (map fst) propps, binds) end;

in

val cert_propp = prep_propp (map o cert_prop);
val read_propp = prep_propp Syntax.read_props;

end;



(** theorems **)

(* fact_tac *)

local

fun comp_hhf_tac ctxt th i st =
  PRIMSEQ (Thm.bicompose (SOME ctxt) {flatten = true, match = false, incremented = true}
    (false, Drule.lift_all ctxt (Thm.cprem_of st i) th, 0) i) st;

fun comp_incr_tac _ [] _ = no_tac
  | comp_incr_tac ctxt (th :: ths) i =
      (fn st => comp_hhf_tac ctxt (Drule.incr_indexes st th) i st) APPEND
      comp_incr_tac ctxt ths i;

val vacuous_facts = [Drule.termI];

in

fun potential_facts ctxt prop =
  let
    val body = Term.strip_all_body prop;
    val vacuous =
      filter (fn th => Term.could_unify (body, Thm.concl_of th)) vacuous_facts
      |> map (rpair Position.none);
  in Facts.could_unify (facts_of ctxt) body @ vacuous end;

fun fact_tac ctxt facts = Goal.norm_hhf_tac ctxt THEN' comp_incr_tac ctxt facts;

fun some_fact_tac ctxt =
  SUBGOAL (fn (goal, i) => fact_tac ctxt (map #1 (potential_facts ctxt goal)) i);

end;


(* lookup facts *)

fun lookup_fact ctxt name =
  let
    val context = Context.Proof ctxt;
    val thy = Proof_Context.theory_of ctxt;
  in
    (case Facts.lookup context (facts_of ctxt) name of
      NONE => Facts.lookup context (Global_Theory.facts_of thy) name
    | some => some)
  end;


(* retrieve facts *)

val dynamic_facts_dummy =
  Config.bool (Config.declare ("dynamic_facts_dummy_", @{here}) (fn _ => Config.Bool false));

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
        val pos = Syntax.read_input_pos s;
        val prop =
          Syntax.read_prop (ctxt |> set_mode mode_default |> allow_dummies) s
          |> singleton (Variable.polymorphic ctxt);
        fun err ps msg =
          error (msg ^ Position.here_list (pos :: ps) ^ ":\n" ^ Syntax.string_of_term ctxt prop);

        val (prop', _) = Term.replace_dummy_patterns prop (Variable.maxidx_of ctxt + 1);
        fun prove th = Goal.prove ctxt [] [] prop' (K (ALLGOALS (fact_tac ctxt [th])));
        val results = map_filter (try (apfst prove)) (potential_facts ctxt prop');
        val (thm, thm_pos) =
          (case distinct (eq_fst Thm.eq_thm_prop) results of
            [res] => res
          | [] => err [] "Failed to retrieve literal fact"
          | dups => err (distinct (op =) (map #2 dups)) "Ambiguous specification of literal fact");

        val markup =
          Markup.entity Markup.literal_factN ""
          |> Markup.properties (Position.def_properties_of thm_pos);
        val _ = Context_Position.report_generic context pos markup;
      in pick true ("", thm_pos) [thm] end
  | retrieve pick context (Facts.Named ((xname, pos), sel)) =
      let
        val thy = Context.theory_of context;
        fun immediate thms = {name = xname, dynamic = false, thms = map (Thm.transfer thy) thms};
        val {name, dynamic, thms} =
          (case xname of
            "" => immediate [Drule.dummy_thm]
          | "_" => immediate [Drule.asm_rl]
          | "nothing" => immediate []
          | _ => retrieve_generic context (xname, pos));
        val thms' =
          if dynamic andalso Config.get_generic context dynamic_facts_dummy
          then [Drule.free_dummy_thm]
          else Facts.select (Facts.Named ((name, pos), sel)) thms;
      in pick (dynamic andalso is_none sel) (name, pos) thms' end;

in

val get_fact_generic =
  retrieve (fn dynamic => fn (name, _) => fn thms =>
    (if dynamic then SOME name else NONE, thms));

val get_fact = retrieve (K (K I)) o Context.Proof;
val get_fact_single = retrieve (K Facts.the_single) o Context.Proof;

fun get_thms ctxt = get_fact ctxt o Facts.named;
fun get_thm ctxt = get_fact_single ctxt o Facts.named;

end;


(* inner statement mode *)

val inner_stmt =
  Config.bool (Config.declare ("inner_stmt", @{here}) (K (Config.Bool false)));

fun is_stmt ctxt = Config.get ctxt inner_stmt;
val set_stmt = Config.put inner_stmt;
val restore_stmt = set_stmt o is_stmt;


(* facts *)

fun add_thms_dynamic arg ctxt =
  ctxt |> map_facts_result (Facts.add_dynamic (Context.Proof ctxt) arg);

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
    val index = is_stmt ctxt;
  in ((name, thms), ctxt' |> update_thms {strict = false, index = index} (b, SOME thms)) end);

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
  let val T = (case opt_T of SOME T => T | NONE => Mixfix.default_constraint mx)
  in (T, ctxt |> Variable.declare_constraints (Free (x, T))) end;

fun add_syntax vars ctxt =
  map_syntax_idents (Local_Syntax.add_syntax ctxt (map (pair Local_Syntax.Fixed) vars)) ctxt;

fun check_var internal b =
  let
    val x = Variable.check_name b;
    val check = if internal then Name.reject_skolem else Name.reject_internal;
    val _ =
      if can check (x, []) andalso Symbol_Pos.is_identifier x then ()
      else error ("Bad name: " ^ Binding.print b);
  in x end;

local

fun check_mixfix ctxt (b, T, mx) =
  let
    val ([x], ctxt') = Variable.add_fixes_binding [Binding.reset_pos b] ctxt;
    val mx' = Mixfix.reset_pos mx;
    val _ = add_syntax [(x, T, if Context_Position.is_visible ctxt then mx else mx')] ctxt';
  in mx' end;

fun prep_var prep_typ internal (b, raw_T, mx) ctxt =
  let
    val x = check_var internal b;
    fun cond_tvars T =
      if internal then T
      else Type.no_tvars T handle TYPE (msg, _, _) => error msg;
    val opt_T = Option.map (cond_tvars o prep_typ ctxt) raw_T;
    val (T, ctxt') = ctxt |> declare_var (x, opt_T, mx);
    val mx' = if Mixfix.is_empty mx then mx else check_mixfix ctxt' (b, T, mx);
  in ((b, SOME T, mx'), ctxt') end;

in

val read_var = prep_var Syntax.read_typ false;
val cert_var = prep_var cert_typ true;

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
  ctxt |> map_syntax_idents
    (Local_Syntax.update_modesyntax ctxt add mode (map_filter (syntax ctxt) args));

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
fun fact_alias b c ctxt = map_facts (Facts.alias (naming_of ctxt) b c) ctxt;


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

local

fun gen_fixes prep_var raw_vars ctxt =
  let
    val (vars, _) = fold_map prep_var raw_vars ctxt;
    val (xs, ctxt') = Variable.add_fixes_binding (map #1 vars) ctxt;
    val _ =
      Context_Position.reports ctxt'
        (flat (map2 (fn x => fn pos =>
          [(pos, Variable.markup ctxt' x), (pos, Variable.markup_entity_def ctxt' x)])
            xs (map (Binding.pos_of o #1) vars)));
    val vars' = map2 (fn x => fn (_, opt_T, mx) => (x, opt_T, mx)) xs vars;
    val (Ts, ctxt'') = fold_map declare_var vars' ctxt';
    val vars'' = map2 (fn T => fn (x, _, mx) => (x, T, mx)) Ts vars';
  in (xs, add_syntax vars'' ctxt'') end;

in

val add_fixes = gen_fixes cert_var;
val add_fixes_cmd = gen_fixes read_var;

end;



(** assumptions **)

local

fun gen_assms prep_propp exp args ctxt =
  let
    val (propss, binds) = prep_propp ctxt (map snd args);
    val props = flat propss;
  in
    ctxt
    |> fold Variable.declare_term props
    |> tap (Variable.warn_extra_tfrees ctxt)
    |> fold_burrow (Assumption.add_assms exp o map (Thm.cterm_of ctxt)) propss
    |-> (fn premss =>
      auto_bind_facts props
      #> fold Variable.bind_term binds
      #> note_thmss "" (map fst args ~~ map (map (fn th => ([th], []))) premss))
  end;

in

val add_assms = gen_assms cert_propp;
val add_assms_cmd = gen_assms read_propp;

end;



(** cases **)

fun dest_cases prev_ctxt ctxt =
  let
    val serial_of = #serial oo (Name_Space.the_entry o Name_Space.space_of_table);
    val ignored =
      (case prev_ctxt of
        NONE => Inttab.empty
      | SOME ctxt0 =>
          let val cases0 = cases_of ctxt0 in
            Name_Space.fold_table (fn (a, _) => Inttab.insert_set (serial_of cases0 a))
              cases0 Inttab.empty
          end);
     val cases = cases_of ctxt;
  in
    Name_Space.fold_table (fn (a, c) =>
      let val i = serial_of cases a
      in not (Inttab.defined ignored i) ? cons (i, (a, c)) end) cases []
    |> sort (int_ord o apply2 #1) |> map #2
  end;

local

fun drop_schematic (b as (xi, SOME t)) = if Term.exists_subterm is_Var t then (xi, NONE) else b
  | drop_schematic b = b;

fun update_case _ _ ("", _) cases = cases
  | update_case _ _ (name, NONE) cases = Name_Space.del_table name cases
  | update_case context legacy (name, SOME c) cases =
      let
        val binding = Binding.name name |> legacy ? Binding.concealed;
        val (_, cases') = Name_Space.define context false (binding, (c, {legacy = legacy})) cases;
      in cases' end;

fun update_cases' legacy args ctxt =
  let val context = Context.Proof ctxt |> Name_Space.map_naming (K Name_Space.global_naming);
  in map_cases (fold (update_case context legacy) args) ctxt end;

fun fix (b, T) ctxt =
  let val ([x], ctxt') = add_fixes [(b, SOME T, NoSyn)] ctxt
  in (Free (x, T), ctxt') end;

in

val update_cases = update_cases' false;
val update_cases_legacy = update_cases' true;

fun case_result c ctxt =
  let
    val Rule_Cases.Case {fixes, ...} = c;
    val (ts, ctxt') = ctxt |> fold_map fix fixes;
    val Rule_Cases.Case {assumes, binds, cases, ...} = Rule_Cases.apply ts c;
  in
    ctxt'
    |> fold (cert_maybe_bind_term o drop_schematic) binds
    |> update_cases (map (apsnd SOME) cases)
    |> pair (assumes, (binds, cases))
  end;

val apply_case = apfst fst oo case_result;

fun check_case ctxt internal (name, pos) param_specs =
  let
    val (_, (Rule_Cases.Case {fixes, assumes, binds, cases}, {legacy})) =
      Name_Space.check (Context.Proof ctxt) (cases_of ctxt) (name, pos);
    val _ =
      if legacy then
        legacy_feature ("Bad case " ^ quote name ^ Position.here pos ^
          " -- use proof method \"goal_cases\" instead")
      else ();

    val _ = List.app (fn NONE => () | SOME b => ignore (check_var internal b)) param_specs;
    fun replace (opt_x :: xs) ((y, T) :: ys) = (the_default y opt_x, T) :: replace xs ys
      | replace [] ys = ys
      | replace (_ :: _) [] =
          error ("Too many parameters for case " ^ quote name ^ Position.here pos);
    val fixes' = replace param_specs fixes;
    val binds' = map drop_schematic binds;
  in
    if null (fold (Term.add_tvarsT o snd) fixes []) andalso
      null (fold (fold Term.add_vars o snd) assumes []) then
        Rule_Cases.Case {fixes = fixes', assumes = assumes, binds = binds', cases = cases}
    else error ("Illegal schematic variable(s) in case " ^ quote name ^ Position.here pos)
  end;

end;


(* structured statements *)

local

fun export_binds ctxt' ctxt params binds =
  let
    val rhss =
      map (the_list o Option.map (Logic.close_term params) o snd) binds
      |> burrow (Variable.export_terms ctxt' ctxt)
      |> map (try the_single);
  in map fst binds ~~ rhss end;

fun prep_stmt prep_var prep_propp raw_vars raw_propps ctxt =
  let
    val (vars, vars_ctxt) = fold_map prep_var raw_vars ctxt;
    val xs = map (Variable.check_name o #1) vars;
    val (xs', fixes_ctxt) = add_fixes vars vars_ctxt;

    val (propss, binds) = prep_propp fixes_ctxt raw_propps;
    val (ps, params_ctxt) = fixes_ctxt
      |> (fold o fold) Variable.declare_term propss
      |> fold_map inferred_param xs';
    val params = xs ~~ map Free ps;

    val vars' = map2 (fn (b, _, mx) => fn (_, T) => (b, SOME T, mx)) vars ps;
    val binds' = binds
      |> map (apsnd SOME)
      |> export_binds params_ctxt ctxt params
      |> map (apsnd the);

    val _ = Variable.warn_extra_tfrees fixes_ctxt params_ctxt;
  in ((vars' ~~ params, propss, binds, binds'), params_ctxt) end;

fun prep_statement prep_var prep_propp raw_fixes raw_assumes raw_shows ctxt =
  let
    val ((fixes, (assumes, shows), binds), ctxt') = ctxt
      |> prep_stmt prep_var prep_propp raw_fixes (raw_assumes @ raw_shows)
      |-> (fn (vars, propss, binds, _) =>
        fold Variable.bind_term binds #>
        pair (map #2 vars, chop (length raw_assumes) propss, binds));
    val binds' =
      (Auto_Bind.facts ctxt' (flat shows) @
        (case try List.last (flat shows) of
          NONE => []
        | SOME prop => map (apsnd (SOME o Auto_Bind.abs_params prop)) binds))
      |> export_binds ctxt' ctxt fixes;
  in ((fixes, assumes, shows, binds'), ctxt') end;

in

val cert_stmt = prep_stmt cert_var cert_propp;
val read_stmt = prep_stmt read_var read_propp;
val cert_statement = prep_statement cert_var cert_propp;
val read_statement = prep_statement read_var read_propp;

end;



(** print context information **)

(* local syntax *)

val print_syntax = Syntax.print_syntax o syn_of;


(* abbreviations *)

fun pretty_abbrevs verbose show_globals ctxt =
  let
    val space = const_space ctxt;
    val (constants, global_constants) =
      apply2 (#constants o Consts.dest) (#consts (rep_data ctxt));
    val globals = Symtab.make global_constants;
    fun add_abbr (_, (_, NONE)) = I
      | add_abbr (c, (T, SOME t)) =
          if not show_globals andalso Symtab.defined globals c then I
          else cons (c, Logic.mk_equals (Const (c, T), t));
    val abbrevs = Name_Space.markup_entries verbose ctxt space (fold add_abbr constants []);
  in
    if null abbrevs then []
    else [Pretty.big_list "abbreviations:" (map (pretty_term_abbrev ctxt o #2) abbrevs)]
  end;

fun print_abbrevs verbose = Pretty.writeln_chunks o pretty_abbrevs verbose true;


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

fun pretty_local_facts verbose ctxt =
  let
    val facts = facts_of ctxt;
    val props = map #1 (Facts.props facts);
    val local_facts =
      (if null props then [] else [("<unnamed>", props)]) @
      Facts.dest_static verbose [Global_Theory.facts_of (theory_of ctxt)] facts;
  in
    if null local_facts then []
    else
      [Pretty.big_list "local facts:"
        (map #1 (sort_by (#1 o #2) (map (`(pretty_fact ctxt)) local_facts)))]
  end;

fun print_local_facts verbose ctxt =
  Pretty.writeln_chunks (pretty_local_facts verbose ctxt);


(* named local contexts *)

local

fun pretty_case (name, (fixes, ((asms, (lets, cs)), ctxt))) =
  let
    val prt_name = pretty_name ctxt;
    val prt_term = Syntax.pretty_term ctxt;

    fun prt_let (xi, t) = Pretty.block
      [Pretty.quote (prt_term (Var (xi, Term.fastype_of t))), Pretty.str " =", Pretty.brk 1,
        Pretty.quote (prt_term t)];

    fun prt_asm (a, ts) =
      Pretty.block (Pretty.breaks
        ((if a = "" then [] else [prt_name a, Pretty.str ":"]) @
          map (Pretty.quote o prt_term) ts));

    fun prt_sect _ _ _ [] = []
      | prt_sect head sep prt xs =
          [Pretty.block (Pretty.breaks (head ::
            flat (separate sep (map (single o prt) xs))))];
  in
    Pretty.block (Pretty.fbreaks
      (prt_name name :: Pretty.str ":" ::
        prt_sect (Pretty.keyword1 "fix") [] (prt_name o Binding.name_of o fst) fixes @
        prt_sect (Pretty.keyword1 "let") [Pretty.keyword2 "and"] prt_let
          (map_filter (fn (xi, SOME t) => SOME (xi, t) | _ => NONE) lets) @
        (if forall (null o #2) asms then []
          else prt_sect (Pretty.keyword1 "assume") [Pretty.keyword2 "and"] prt_asm asms) @
        prt_sect (Pretty.str "subcases:") [] (prt_name o fst) cs))
  end;

in

fun pretty_cases ctxt =
  let
    fun mk_case (_, (_, {legacy = true})) = NONE
      | mk_case (name, (c as Rule_Cases.Case {fixes, ...}, {legacy = false})) =
          SOME (name, (fixes, case_result c ctxt));
    val cases = dest_cases NONE ctxt |> map_filter mk_case;
  in
    if null cases then []
    else [Pretty.big_list "cases:" (map pretty_case cases)]
  end;

end;

fun print_cases_proof ctxt0 ctxt =
  let
    val print_name = Token.print_name (Thy_Header.get_keywords' ctxt);

    fun trim_name x = if Name.is_internal x then Name.clean x else "_";
    val trim_names = map trim_name #> take_suffix (equal "_") #> #1;

    fun print_case name xs =
      (case trim_names xs of
        [] => print_name name
      | xs' => enclose "(" ")" (space_implode " " (map print_name (name :: xs'))));

    fun is_case x t =
      x = Rule_Cases.case_conclN andalso not (Term.exists_subterm Term.is_Var t);

    fun print_proof (name, (Rule_Cases.Case {fixes, binds, ...}, {legacy})) =
      if legacy then NONE
      else
        let
          val concl =
            if exists (fn ((x, _), SOME t) => is_case x t | _ => false) binds
            then Rule_Cases.case_conclN else Auto_Bind.thesisN;
        in
          SOME (cat_lines
            ["  case " ^ print_case name (map (Binding.name_of o #1) fixes),
             "  then show ?" ^ concl ^ " sorry"])
        end;
  in
    (case map_filter print_proof (dest_cases (SOME ctxt0) ctxt) of
      [] => ""
    | proofs =>
        "Proof outline with cases:\n" ^
        Active.sendback_markup_command (space_implode "\nnext\n" proofs ^ "\nqed"))
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
      val {structs, ...} = Syntax_Trans.get_idents ctxt;
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

      (*assumptions*)
      val prt_assms =
        (case Assumption.all_prems_of ctxt of
          [] => []
        | prems => [Pretty.big_list "assumptions:" [pretty_fact ctxt ("", prems)]]);
    in prt_structs @ prt_fixes @ prt_assms end;


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
    verb (pretty_abbrevs true false) (K ctxt) @
    verb pretty_term_bindings (K ctxt) @
    verb (pretty_local_facts true) (K ctxt) @
    verb pretty_cases (K ctxt) @
    verb single (fn () => Pretty.big_list "type constraints:" (map prt_defT (Vartab.dest types))) @
    verb single (fn () => Pretty.big_list "default sorts:" (map prt_defS (Vartab.dest sorts)))
  end;

end;

val show_abbrevs = Proof_Context.show_abbrevs;
