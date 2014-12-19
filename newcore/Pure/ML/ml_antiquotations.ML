(*  Title:      Pure/ML/ml_antiquotations.ML
    Author:     Makarius

Miscellaneous ML antiquotations.
*)

structure ML_Antiquotations: sig end =
struct

val _ = Theory.setup
 (ML_Antiquotation.value @{binding system_option}
    (Args.context -- Scan.lift (Parse.position Args.name) >> (fn (ctxt, (name, pos)) =>
      (Context_Position.report ctxt pos (Options.default_markup (name, pos));
        ML_Syntax.print_string name))) #>

  ML_Antiquotation.value @{binding theory}
    (Args.context -- Scan.lift (Parse.position Args.name) >> (fn (ctxt, (name, pos)) =>
      (Context_Position.report ctxt pos
        (Theory.get_markup (Context.get_theory (Proof_Context.theory_of ctxt) name));
       "Context.get_theory (Proof_Context.theory_of ML_context) " ^ ML_Syntax.print_string name))
    || Scan.succeed "Proof_Context.theory_of ML_context") #>

  ML_Antiquotation.value @{binding theory_context}
    (Args.context -- Scan.lift (Parse.position Args.name) >> (fn (ctxt, (name, pos)) =>
      (Context_Position.report ctxt pos
        (Theory.get_markup (Context.get_theory (Proof_Context.theory_of ctxt) name));
       "Proof_Context.get_global (Proof_Context.theory_of ML_context) " ^
        ML_Syntax.print_string name))) #>

  ML_Antiquotation.inline @{binding context} (Scan.succeed "Isabelle.ML_context") #>

  ML_Antiquotation.inline @{binding typ} (Args.typ >> (ML_Syntax.atomic o ML_Syntax.print_typ)) #>
  ML_Antiquotation.inline @{binding term} (Args.term >> (ML_Syntax.atomic o ML_Syntax.print_term)) #>
  ML_Antiquotation.inline @{binding prop} (Args.prop >> (ML_Syntax.atomic o ML_Syntax.print_term)) #>

  ML_Antiquotation.value @{binding ctyp} (Args.typ >> (fn T =>
    "Thm.ctyp_of (Proof_Context.theory_of ML_context) " ^
      ML_Syntax.atomic (ML_Syntax.print_typ T))) #>

  ML_Antiquotation.value @{binding cterm} (Args.term >> (fn t =>
    "Thm.cterm_of (Proof_Context.theory_of ML_context) " ^
     ML_Syntax.atomic (ML_Syntax.print_term t))) #>

  ML_Antiquotation.value @{binding cprop} (Args.prop >> (fn t =>
    "Thm.cterm_of (Proof_Context.theory_of ML_context) " ^
     ML_Syntax.atomic (ML_Syntax.print_term t))) #>

  ML_Antiquotation.value @{binding cpat}
    (Args.context --
      Scan.lift Args.name_inner_syntax >> uncurry Proof_Context.read_term_pattern >> (fn t =>
        "Thm.cterm_of (Proof_Context.theory_of ML_context) " ^
          ML_Syntax.atomic (ML_Syntax.print_term t))));


(* ML support *)

val _ = Theory.setup
 (ML_Antiquotation.inline @{binding assert}
    (Scan.succeed "(fn b => if b then () else raise General.Fail \"Assertion failed\")") #>

  ML_Antiquotation.inline @{binding make_string} (Scan.succeed ml_make_string) #>

  ML_Antiquotation.declaration @{binding print}
    (Scan.lift (Scan.optional Args.name "Output.writeln"))
      (fn src => fn output => fn ctxt =>
        let
          val (_, pos) = Args.name_of_src src;
          val (a, ctxt') = ML_Antiquotation.variant "output" ctxt;
          val env =
            "val " ^ a ^ ": string -> unit =\n\
            \  (" ^ output ^ ") o (fn s => s ^ Position.here (" ^
            ML_Syntax.print_position pos ^ "));\n";
          val body =
            "(fn x => (Isabelle." ^ a ^ " (" ^ ml_make_string ^ " x); x))";
        in (K (env, body), ctxt') end));


(* type classes *)

fun class syn = Args.context -- Scan.lift Args.name_inner_syntax >> (fn (ctxt, s) =>
  Proof_Context.read_class ctxt s
  |> syn ? Lexicon.mark_class
  |> ML_Syntax.print_string);

val _ = Theory.setup
 (ML_Antiquotation.inline @{binding class} (class false) #>
  ML_Antiquotation.inline @{binding class_syntax} (class true) #>

  ML_Antiquotation.inline @{binding sort}
    (Args.context -- Scan.lift Args.name_inner_syntax >> (fn (ctxt, s) =>
      ML_Syntax.atomic (ML_Syntax.print_sort (Syntax.read_sort ctxt s)))));


(* type constructors *)

fun type_name kind check = Args.context -- Scan.lift (Parse.position Args.name_inner_syntax)
  >> (fn (ctxt, (s, pos)) =>
    let
      val Type (c, _) = Proof_Context.read_type_name {proper = true, strict = false} ctxt s;
      val decl = Type.the_decl (Proof_Context.tsig_of ctxt) (c, pos);
      val res =
        (case try check (c, decl) of
          SOME res => res
        | NONE => error ("Not a " ^ kind ^ ": " ^ quote c ^ Position.here pos));
    in ML_Syntax.print_string res end);

val _ = Theory.setup
 (ML_Antiquotation.inline @{binding type_name}
    (type_name "logical type" (fn (c, Type.LogicalType _) => c)) #>
  ML_Antiquotation.inline @{binding type_abbrev}
    (type_name "type abbreviation" (fn (c, Type.Abbreviation _) => c)) #>
  ML_Antiquotation.inline @{binding nonterminal}
    (type_name "nonterminal" (fn (c, Type.Nonterminal) => c)) #>
  ML_Antiquotation.inline @{binding type_syntax}
    (type_name "type" (fn (c, _) => Lexicon.mark_type c)));


(* constants *)

fun const_name check = Args.context -- Scan.lift (Parse.position Args.name_inner_syntax)
  >> (fn (ctxt, (s, pos)) =>
    let
      val Const (c, _) = Proof_Context.read_const {proper = true, strict = false} ctxt s;
      val res = check (Proof_Context.consts_of ctxt, c)
        handle TYPE (msg, _, _) => error (msg ^ Position.here pos);
    in ML_Syntax.print_string res end);

val _ = Theory.setup
 (ML_Antiquotation.inline @{binding const_name}
    (const_name (fn (consts, c) => (Consts.the_const consts c; c))) #>
  ML_Antiquotation.inline @{binding const_abbrev}
    (const_name (fn (consts, c) => (Consts.the_abbreviation consts c; c))) #>
  ML_Antiquotation.inline @{binding const_syntax}
    (const_name (fn (_, c) => Lexicon.mark_const c)) #>

  ML_Antiquotation.inline @{binding syntax_const}
    (Args.context -- Scan.lift (Parse.position Args.name) >> (fn (ctxt, (c, pos)) =>
      if is_some (Syntax.lookup_const (Proof_Context.syn_of ctxt) c)
      then ML_Syntax.print_string c
      else error ("Unknown syntax const: " ^ quote c ^ Position.here pos))) #>

  ML_Antiquotation.inline @{binding const}
    (Args.context -- Scan.lift (Parse.position Args.name_inner_syntax) -- Scan.optional
        (Scan.lift (Args.$$$ "(") |-- Parse.enum1' "," Args.typ --| Scan.lift (Args.$$$ ")")) []
      >> (fn ((ctxt, (raw_c, pos)), Ts) =>
        let
          val Const (c, _) =
            Proof_Context.read_const {proper = true, strict = true} ctxt raw_c;
          val consts = Proof_Context.consts_of ctxt;
          val n = length (Consts.typargs consts (c, Consts.type_scheme consts c));
          val _ = length Ts <> n andalso
            error ("Constant requires " ^ string_of_int n ^ " type argument(s): " ^
              quote c ^ enclose "(" ")" (commas (replicate n "_")) ^ Position.here pos);
          val const = Const (c, Consts.instance consts (c, Ts));
        in ML_Syntax.atomic (ML_Syntax.print_term const) end)));


(* outer syntax *)

fun with_keyword f =
  Args.theory -- Scan.lift (Parse.position Parse.string) >> (fn (thy, (name, pos)) =>
    (f ((name, Thy_Header.the_keyword thy name), pos)
      handle ERROR msg => error (msg ^ Position.here pos)));

val _ = Theory.setup
 (ML_Antiquotation.value @{binding keyword}
    (with_keyword
      (fn ((name, NONE), _) => "Parse.$$$ " ^ ML_Syntax.print_string name
        | ((name, SOME _), pos) =>
            error ("Expected minor keyword " ^ quote name ^ Position.here pos))) #>
  ML_Antiquotation.value @{binding command_spec}
    (with_keyword
      (fn ((name, SOME kind), pos) =>
          "Keyword.command_spec " ^ ML_Syntax.atomic
            ((ML_Syntax.print_pair
              (ML_Syntax.print_pair ML_Syntax.print_string
                (ML_Syntax.print_pair
                  (ML_Syntax.print_pair ML_Syntax.print_string
                    (ML_Syntax.print_list ML_Syntax.print_string))
                  (ML_Syntax.print_list ML_Syntax.print_string)))
              ML_Syntax.print_position) ((name, kind), pos))
        | ((name, NONE), pos) =>
            error ("Expected command keyword " ^ quote name ^ Position.here pos))));

end;

