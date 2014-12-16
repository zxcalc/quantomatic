(*  Title:      Pure/ML/ml_antiquote.ML
    Author:     Makarius

Common ML antiquotations.
*)

signature ML_ANTIQUOTE =
sig
  val macro: string -> Proof.context context_parser -> unit
  val variant: string -> Proof.context -> string * Proof.context
  val inline: string -> string context_parser -> unit
  val declaration: string -> string -> string context_parser -> unit
  val value: string -> string context_parser -> unit
end;

structure ML_Antiquote: ML_ANTIQUOTE =
struct

(** generic tools **)

(* ML names *)

structure Names = Proof_Data
(
  type T = Name.context;
  fun init _ = ML_Syntax.reserved;
);

fun variant a ctxt =
  let
    val names = Names.get ctxt;
    val (b, names') = Name.variant a names;
    val ctxt' = Names.put names' ctxt;
  in (b, ctxt') end;


(* specific antiquotations *)

fun macro name scan = ML_Context.add_antiq name
  (fn _ => scan :|-- (fn ctxt => Scan.depend (fn _ => Scan.succeed
    (Context.Proof ctxt, fn background => (K ("", ""), background)))));

fun inline name scan = ML_Context.add_antiq name
  (fn _ => scan >> (fn s => fn background => (K ("", s), background)));

fun declaration kind name scan = ML_Context.add_antiq name
  (fn _ => scan >> (fn s => fn background =>
    let
      val (a, background') = variant (translate_string (fn "." => "_" | c => c) name) background;
      val env = kind ^ " " ^ a ^ " = " ^ s ^ ";\n";
      val body = "Isabelle." ^ a;
    in (K (env, body), background') end));

val value = declaration "val";



(** misc antiquotations **)

val _ = inline "assert"
  (Scan.succeed "(fn b => if b then () else raise General.Fail \"Assertion failed\")");

val _ = inline "make_string" (Scan.succeed ml_make_string);

val _ = value "binding"
  (Scan.lift (Parse.position Args.name)
    >> (fn name => ML_Syntax.atomic (ML_Syntax.make_binding name)));

val _ = value "theory"
  (Scan.lift Args.name >> (fn name =>
    "Context.get_theory (ML_Context.the_global_context ()) " ^ ML_Syntax.print_string name)
  || Scan.succeed "ML_Context.the_global_context ()");

val _ = value "context" (Scan.succeed "ML_Context.the_local_context ()");

val _ = inline "typ" (Args.typ >> (ML_Syntax.atomic o ML_Syntax.print_typ));
val _ = inline "term" (Args.term >> (ML_Syntax.atomic o ML_Syntax.print_term));
val _ = inline "prop" (Args.prop >> (ML_Syntax.atomic o ML_Syntax.print_term));

val _ = macro "let" (Args.context --
  Scan.lift
    (Parse.and_list1 (Parse.and_list1 Args.name_source -- (Args.$$$ "=" |-- Args.name_source)))
    >> (fn (ctxt, args) => #2 (Proof_Context.match_bind true args ctxt)));

val _ = macro "note" (Args.context :|-- (fn ctxt =>
  Parse.and_list1' (Scan.lift (Args.opt_thm_name I "=") -- Attrib.thms >> (fn ((a, srcs), ths) =>
    ((a, map (Attrib.attribute (Proof_Context.theory_of ctxt)) srcs), [(ths, [])])))
  >> (fn args => #2 (Proof_Context.note_thmss "" args ctxt))));

val _ = value "ctyp" (Args.typ >> (fn T =>
  "Thm.ctyp_of (ML_Context.the_global_context ()) " ^ ML_Syntax.atomic (ML_Syntax.print_typ T)));

val _ = value "cterm" (Args.term >> (fn t =>
  "Thm.cterm_of (ML_Context.the_global_context ()) " ^ ML_Syntax.atomic (ML_Syntax.print_term t)));

val _ = value "cprop" (Args.prop >> (fn t =>
  "Thm.cterm_of (ML_Context.the_global_context ()) " ^ ML_Syntax.atomic (ML_Syntax.print_term t)));

val _ = value "cpat"
  (Args.context -- Scan.lift Args.name_source >> uncurry Proof_Context.read_term_pattern >> (fn t =>
    "Thm.cterm_of (ML_Context.the_global_context ()) " ^ ML_Syntax.atomic (ML_Syntax.print_term t)));


(* type classes *)

fun class syn = Args.context -- Scan.lift Args.name_source >> (fn (ctxt, s) =>
  Proof_Context.read_class ctxt s
  |> syn ? Lexicon.mark_class
  |> ML_Syntax.print_string);

val _ = inline "class" (class false);
val _ = inline "class_syntax" (class true);

val _ = inline "sort" (Args.context -- Scan.lift Args.name_source >> (fn (ctxt, s) =>
  ML_Syntax.atomic (ML_Syntax.print_sort (Syntax.read_sort ctxt s))));


(* type constructors *)

fun type_name kind check = Args.context -- Scan.lift (Parse.position Args.name_source)
  >> (fn (ctxt, (s, pos)) =>
    let
      val Type (c, _) = Proof_Context.read_type_name_proper ctxt false s;
      val decl = Type.the_decl (Proof_Context.tsig_of ctxt) (c, pos);
      val res =
        (case try check (c, decl) of
          SOME res => res
        | NONE => error ("Not a " ^ kind ^ ": " ^ quote c ^ Position.str_of pos));
    in ML_Syntax.print_string res end);

val _ = inline "type_name" (type_name "logical type" (fn (c, Type.LogicalType _) => c));
val _ = inline "type_abbrev" (type_name "type abbreviation" (fn (c, Type.Abbreviation _) => c));
val _ = inline "nonterminal" (type_name "nonterminal" (fn (c, Type.Nonterminal) => c));
val _ = inline "type_syntax" (type_name "type" (fn (c, _) => Lexicon.mark_type c));


(* constants *)

fun const_name check = Args.context -- Scan.lift (Parse.position Args.name_source)
  >> (fn (ctxt, (s, pos)) =>
    let
      val Const (c, _) = Proof_Context.read_const_proper ctxt false s;
      val res = check (Proof_Context.consts_of ctxt, c)
        handle TYPE (msg, _, _) => error (msg ^ Position.str_of pos);
    in ML_Syntax.print_string res end);

val _ = inline "const_name" (const_name (fn (consts, c) => (Consts.the_type consts c; c)));
val _ = inline "const_abbrev" (const_name (fn (consts, c) => (Consts.the_abbreviation consts c; c)));
val _ = inline "const_syntax" (const_name (fn (_, c) => Lexicon.mark_const c));


val _ = inline "syntax_const"
  (Args.context -- Scan.lift (Parse.position Args.name) >> (fn (ctxt, (c, pos)) =>
    if is_some (Syntax.lookup_const (Proof_Context.syn_of ctxt) c)
    then ML_Syntax.print_string c
    else error ("Unknown syntax const: " ^ quote c ^ Position.str_of pos)));

val _ = inline "const"
  (Args.context -- Scan.lift Args.name_source -- Scan.optional
      (Scan.lift (Args.$$$ "(") |-- Parse.enum1' "," Args.typ --| Scan.lift (Args.$$$ ")")) []
    >> (fn ((ctxt, raw_c), Ts) =>
      let
        val Const (c, _) = Proof_Context.read_const_proper ctxt true raw_c;
        val const = Const (c, Consts.instance (Proof_Context.consts_of ctxt) (c, Ts));
      in ML_Syntax.atomic (ML_Syntax.print_term const) end));

end;

