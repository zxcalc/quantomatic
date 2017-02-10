(*  Title:      Pure/Isar/isar_syn.ML
    Author:     Makarius

Outer syntax for Isabelle/Pure.
*)

structure Isar_Syn: sig end =
struct

(** markup commands **)

val _ =
  Outer_Syntax.markup_command Thy_Output.Markup
    @{command_spec "header"} "theory header"
    (Parse.document_source >> Isar_Cmd.header_markup);

val _ =
  Outer_Syntax.markup_command Thy_Output.Markup
    @{command_spec "chapter"} "chapter heading"
    (Parse.opt_target -- Parse.document_source >> Isar_Cmd.local_theory_markup);

val _ =
  Outer_Syntax.markup_command Thy_Output.Markup
    @{command_spec "section"} "section heading"
    (Parse.opt_target -- Parse.document_source >> Isar_Cmd.local_theory_markup);

val _ =
  Outer_Syntax.markup_command Thy_Output.Markup
    @{command_spec "subsection"} "subsection heading"
    (Parse.opt_target -- Parse.document_source >> Isar_Cmd.local_theory_markup);

val _ =
  Outer_Syntax.markup_command Thy_Output.Markup
    @{command_spec "subsubsection"} "subsubsection heading"
    (Parse.opt_target -- Parse.document_source >> Isar_Cmd.local_theory_markup);

val _ =
  Outer_Syntax.markup_command Thy_Output.MarkupEnv
    @{command_spec "text"} "formal comment (theory)"
    (Parse.opt_target -- Parse.document_source >> Isar_Cmd.local_theory_markup);

val _ =
  Outer_Syntax.markup_command Thy_Output.Verbatim
    @{command_spec "text_raw"} "raw document preparation text"
    (Parse.opt_target -- Parse.document_source >> Isar_Cmd.local_theory_markup);

val _ =
  Outer_Syntax.markup_command Thy_Output.Markup
    @{command_spec "sect"} "formal comment (proof)"
    (Parse.document_source >> Isar_Cmd.proof_markup);

val _ =
  Outer_Syntax.markup_command Thy_Output.Markup
    @{command_spec "subsect"} "formal comment (proof)"
    (Parse.document_source >> Isar_Cmd.proof_markup);

val _ =
  Outer_Syntax.markup_command Thy_Output.Markup
    @{command_spec "subsubsect"} "formal comment (proof)"
    (Parse.document_source >> Isar_Cmd.proof_markup);

val _ =
  Outer_Syntax.markup_command Thy_Output.MarkupEnv
    @{command_spec "txt"} "formal comment (proof)"
    (Parse.document_source >> Isar_Cmd.proof_markup);

val _ =
  Outer_Syntax.markup_command Thy_Output.Verbatim
    @{command_spec "txt_raw"} "raw document preparation text (proof)"
    (Parse.document_source >> Isar_Cmd.proof_markup);



(** theory commands **)

(* sorts *)

val _ =
  Outer_Syntax.local_theory @{command_spec "default_sort"}
    "declare default sort for explicit type variables"
    (Parse.sort >> (fn s => fn lthy => Local_Theory.set_defsort (Syntax.read_sort lthy s) lthy));


(* types *)

val _ =
  Outer_Syntax.local_theory @{command_spec "typedecl"} "type declaration"
    (Parse.type_args -- Parse.binding -- Parse.opt_mixfix
      >> (fn ((args, a), mx) => Typedecl.typedecl (a, map (rpair dummyS) args, mx) #> snd));

val _ =
  Outer_Syntax.local_theory @{command_spec "type_synonym"} "declare type abbreviation"
    (Parse.type_args -- Parse.binding --
      (@{keyword "="} |-- Parse.!!! (Parse.typ -- Parse.opt_mixfix'))
      >> (fn ((args, a), (rhs, mx)) => snd o Typedecl.abbrev_cmd (a, args, mx) rhs));

val _ =
  Outer_Syntax.command @{command_spec "nonterminal"}
    "declare syntactic type constructors (grammar nonterminal symbols)"
    (Parse.and_list1 Parse.binding >> (Toplevel.theory o Sign.add_nonterminals_global));


(* consts and syntax *)

val _ =
  Outer_Syntax.command @{command_spec "judgment"} "declare object-logic judgment"
    (Parse.const_binding >> (Toplevel.theory o Object_Logic.add_judgment_cmd));

val _ =
  Outer_Syntax.command @{command_spec "consts"} "declare constants"
    (Scan.repeat1 Parse.const_binding >> (Toplevel.theory o Sign.add_consts_cmd));

val mode_spec =
  (@{keyword "output"} >> K ("", false)) ||
    Parse.name -- Scan.optional (@{keyword "output"} >> K false) true;

val opt_mode =
  Scan.optional (@{keyword "("} |-- Parse.!!! (mode_spec --| @{keyword ")"})) Syntax.mode_default;

val _ =
  Outer_Syntax.command @{command_spec "syntax"} "add raw syntax clauses"
    (opt_mode -- Scan.repeat1 Parse.const_decl
      >> (Toplevel.theory o uncurry Sign.add_syntax_cmd));

val _ =
  Outer_Syntax.command @{command_spec "no_syntax"} "delete raw syntax clauses"
    (opt_mode -- Scan.repeat1 Parse.const_decl
      >> (Toplevel.theory o uncurry Sign.del_syntax_cmd));


(* translations *)

val trans_pat =
  Scan.optional
    (@{keyword "("} |-- Parse.!!! (Parse.inner_syntax Parse.xname --| @{keyword ")"})) "logic"
    -- Parse.inner_syntax Parse.string;

fun trans_arrow toks =
  ((@{keyword "\<rightharpoonup>"} || @{keyword "=>"}) >> K Syntax.Parse_Rule ||
    (@{keyword "\<leftharpoondown>"} || @{keyword "<="}) >> K Syntax.Print_Rule ||
    (@{keyword "\<rightleftharpoons>"} || @{keyword "=="}) >> K Syntax.Parse_Print_Rule) toks;

val trans_line =
  trans_pat -- Parse.!!! (trans_arrow -- trans_pat)
    >> (fn (left, (arr, right)) => arr (left, right));

val _ =
  Outer_Syntax.command @{command_spec "translations"} "add syntax translation rules"
    (Scan.repeat1 trans_line >> (Toplevel.theory o Isar_Cmd.translations));

val _ =
  Outer_Syntax.command @{command_spec "no_translations"} "delete syntax translation rules"
    (Scan.repeat1 trans_line >> (Toplevel.theory o Isar_Cmd.no_translations));


(* axioms and definitions *)

val opt_unchecked_overloaded =
  Scan.optional (@{keyword "("} |-- Parse.!!!
    (((@{keyword "unchecked"} >> K true) --
        Scan.optional (@{keyword "overloaded"} >> K true) false ||
      @{keyword "overloaded"} >> K (false, true)) --| @{keyword ")"})) (false, false);

val _ =
  Outer_Syntax.command @{command_spec "defs"} "define constants"
    (opt_unchecked_overloaded --
      Scan.repeat1 (Parse_Spec.thm_name ":" -- Parse.prop >> (fn ((x, y), z) => ((x, z), y)))
      >> (Toplevel.theory o Isar_Cmd.add_defs));


(* constant definitions and abbreviations *)

val _ =
  Outer_Syntax.local_theory' @{command_spec "definition"} "constant definition"
    (Parse_Spec.constdef >> (fn args => #2 oo Specification.definition_cmd args));

val _ =
  Outer_Syntax.local_theory' @{command_spec "abbreviation"} "constant abbreviation"
    (opt_mode -- (Scan.option Parse_Spec.constdecl -- Parse.prop)
      >> (fn (mode, args) => Specification.abbreviation_cmd mode args));

val _ =
  Outer_Syntax.local_theory @{command_spec "type_notation"}
    "add concrete syntax for type constructors"
    (opt_mode -- Parse.and_list1 (Parse.type_const -- Parse.mixfix)
      >> (fn (mode, args) => Specification.type_notation_cmd true mode args));

val _ =
  Outer_Syntax.local_theory @{command_spec "no_type_notation"}
    "delete concrete syntax for type constructors"
    (opt_mode -- Parse.and_list1 (Parse.type_const -- Parse.mixfix)
      >> (fn (mode, args) => Specification.type_notation_cmd false mode args));

val _ =
  Outer_Syntax.local_theory @{command_spec "notation"}
    "add concrete syntax for constants / fixed variables"
    (opt_mode -- Parse.and_list1 (Parse.const -- Parse.mixfix)
      >> (fn (mode, args) => Specification.notation_cmd true mode args));

val _ =
  Outer_Syntax.local_theory @{command_spec "no_notation"}
    "delete concrete syntax for constants / fixed variables"
    (opt_mode -- Parse.and_list1 (Parse.const -- Parse.mixfix)
      >> (fn (mode, args) => Specification.notation_cmd false mode args));


(* constant specifications *)

val _ =
  Outer_Syntax.command @{command_spec "axiomatization"} "axiomatic constant specification"
    (Scan.optional Parse.fixes [] --
      Scan.optional (Parse.where_ |-- Parse.!!! (Parse.and_list1 Parse_Spec.specs)) []
      >> (fn (x, y) => Toplevel.theory (#2 o Specification.axiomatization_cmd x y)));


(* theorems *)

fun theorems kind =
  Parse_Spec.name_facts -- Parse.for_fixes
    >> (fn (facts, fixes) => #2 oo Specification.theorems_cmd kind facts fixes);

val _ =
  Outer_Syntax.local_theory' @{command_spec "theorems"} "define theorems"
    (theorems Thm.theoremK);

val _ =
  Outer_Syntax.local_theory' @{command_spec "lemmas"} "define lemmas" (theorems Thm.lemmaK);

val _ =
  Outer_Syntax.local_theory' @{command_spec "declare"} "declare theorems"
    (Parse.and_list1 Parse_Spec.xthms1 -- Parse.for_fixes
      >> (fn (facts, fixes) =>
          #2 oo Specification.theorems_cmd "" [(Attrib.empty_binding, flat facts)] fixes));


(* hide names *)

local

fun hide_names command_spec what hide parse prep =
  Outer_Syntax.command command_spec ("hide " ^ what ^ " from name space")
    ((Parse.opt_keyword "open" >> not) -- Scan.repeat1 parse >> (fn (fully, args) =>
      (Toplevel.theory (fn thy =>
        let val ctxt = Proof_Context.init_global thy
        in fold (hide fully o prep ctxt) args thy end))));

in

val _ =
  hide_names @{command_spec "hide_class"} "classes" Sign.hide_class Parse.class
    Proof_Context.read_class;

val _ =
  hide_names @{command_spec "hide_type"} "types" Sign.hide_type Parse.type_const
    ((#1 o dest_Type) oo Proof_Context.read_type_name {proper = true, strict = false});

val _ =
  hide_names @{command_spec "hide_const"} "constants" Sign.hide_const Parse.const
    ((#1 o dest_Const) oo Proof_Context.read_const {proper = true, strict = false});

val _ =
  hide_names @{command_spec "hide_fact"} "facts" Global_Theory.hide_fact
    (Parse.position Parse.xname) (Global_Theory.check_fact o Proof_Context.theory_of);

end;


(* use ML text *)

val _ =
  Outer_Syntax.command @{command_spec "SML_file"} "read and evaluate Standard ML file"
    (Resources.provide_parse_files "SML_file" >> (fn files => Toplevel.theory (fn thy =>
        let
          val ([{lines, pos, ...}], thy') = files thy;
          val source = {delimited = true, text = cat_lines lines, pos = pos};
          val flags = {SML = true, exchange = false, redirect = true, verbose = true};
        in
          thy' |> Context.theory_map
            (ML_Context.exec (fn () => ML_Context.eval_source flags source))
        end)));

val _ =
  Outer_Syntax.command @{command_spec "SML_export"} "evaluate SML within Isabelle/ML environment"
    (Parse.ML_source >> (fn source =>
      let val flags = {SML = true, exchange = true, redirect = false, verbose = true} in
        Toplevel.theory
          (Context.theory_map (ML_Context.exec (fn () => ML_Context.eval_source flags source)))
      end));

val _ =
  Outer_Syntax.command @{command_spec "SML_import"} "evaluate Isabelle/ML within SML environment"
    (Parse.ML_source >> (fn source =>
      let val flags = {SML = false, exchange = true, redirect = false, verbose = true} in
        Toplevel.generic_theory
          (ML_Context.exec (fn () => ML_Context.eval_source flags source) #>
            Local_Theory.propagate_ml_env)
      end));

val _ =
  Outer_Syntax.command @{command_spec "ML"} "ML text within theory or local theory"
    (Parse.ML_source >> (fn source =>
      Toplevel.generic_theory
        (ML_Context.exec (fn () =>
            ML_Context.eval_source (ML_Compiler.verbose true ML_Compiler.flags) source) #>
          Local_Theory.propagate_ml_env)));

val _ =
  Outer_Syntax.command @{command_spec "ML_prf"} "ML text within proof"
    (Parse.ML_source >> (fn source =>
      Toplevel.proof (Proof.map_context (Context.proof_map
        (ML_Context.exec (fn () =>
            ML_Context.eval_source (ML_Compiler.verbose true ML_Compiler.flags) source))) #>
          Proof.propagate_ml_env)));

val _ =
  Outer_Syntax.command @{command_spec "ML_val"} "diagnostic ML text"
    (Parse.ML_source >> Isar_Cmd.ml_diag true);

val _ =
  Outer_Syntax.command @{command_spec "ML_command"} "diagnostic ML text (silent)"
    (Parse.ML_source >> Isar_Cmd.ml_diag false);

val _ =
  Outer_Syntax.command @{command_spec "setup"} "ML theory setup"
    (Parse.ML_source >> (Toplevel.theory o Isar_Cmd.global_setup));

val _ =
  Outer_Syntax.local_theory @{command_spec "local_setup"} "ML local theory setup"
    (Parse.ML_source >> Isar_Cmd.local_setup);

val _ =
  Outer_Syntax.command @{command_spec "attribute_setup"} "define attribute in ML"
    (Parse.position Parse.name --
        Parse.!!! (@{keyword "="} |-- Parse.ML_source -- Scan.optional Parse.text "")
      >> (fn (name, (txt, cmt)) => Toplevel.theory (Attrib.attribute_setup name txt cmt)));

val _ =
  Outer_Syntax.command @{command_spec "method_setup"} "define proof method in ML"
    (Parse.position Parse.name --
        Parse.!!! (@{keyword "="} |-- Parse.ML_source -- Scan.optional Parse.text "")
      >> (fn (name, (txt, cmt)) => Toplevel.theory (Method.method_setup name txt cmt)));

val _ =
  Outer_Syntax.local_theory @{command_spec "declaration"} "generic ML declaration"
    (Parse.opt_keyword "pervasive" -- Parse.ML_source
      >> (fn (pervasive, txt) => Isar_Cmd.declaration {syntax = false, pervasive = pervasive} txt));

val _ =
  Outer_Syntax.local_theory @{command_spec "syntax_declaration"} "generic ML syntax declaration"
    (Parse.opt_keyword "pervasive" -- Parse.ML_source
      >> (fn (pervasive, txt) => Isar_Cmd.declaration {syntax = true, pervasive = pervasive} txt));

val _ =
  Outer_Syntax.local_theory @{command_spec "simproc_setup"} "define simproc in ML"
    (Parse.position Parse.name --
      (@{keyword "("} |-- Parse.enum1 "|" Parse.term --| @{keyword ")"} --| @{keyword "="}) --
      Parse.ML_source -- Scan.optional (@{keyword "identifier"} |-- Scan.repeat1 Parse.xname) []
    >> (fn (((a, b), c), d) => Isar_Cmd.simproc_setup a b c d));


(* translation functions *)

val _ =
  Outer_Syntax.command @{command_spec "parse_ast_translation"}
    "install parse ast translation functions"
    (Parse.ML_source >> (Toplevel.theory o Isar_Cmd.parse_ast_translation));

val _ =
  Outer_Syntax.command @{command_spec "parse_translation"}
    "install parse translation functions"
    (Parse.ML_source >> (Toplevel.theory o Isar_Cmd.parse_translation));

val _ =
  Outer_Syntax.command @{command_spec "print_translation"}
    "install print translation functions"
    (Parse.ML_source >> (Toplevel.theory o Isar_Cmd.print_translation));

val _ =
  Outer_Syntax.command @{command_spec "typed_print_translation"}
    "install typed print translation functions"
    (Parse.ML_source >> (Toplevel.theory o Isar_Cmd.typed_print_translation));

val _ =
  Outer_Syntax.command @{command_spec "print_ast_translation"}
    "install print ast translation functions"
    (Parse.ML_source >> (Toplevel.theory o Isar_Cmd.print_ast_translation));


(* oracles *)

val _ =
  Outer_Syntax.command @{command_spec "oracle"} "declare oracle"
    (Parse.position Parse.name -- (@{keyword "="} |-- Parse.ML_source) >>
      (fn (x, y) => Toplevel.theory (Isar_Cmd.oracle x y)));


(* bundled declarations *)

val _ =
  Outer_Syntax.local_theory @{command_spec "bundle"} "define bundle of declarations"
    ((Parse.binding --| @{keyword "="}) -- Parse_Spec.xthms1 -- Parse.for_fixes
      >> (uncurry Bundle.bundle_cmd));

val _ =
  Outer_Syntax.command @{command_spec "include"}
    "include declarations from bundle in proof body"
    (Scan.repeat1 (Parse.position Parse.xname) >> (Toplevel.proof o Bundle.include_cmd));

val _ =
  Outer_Syntax.command @{command_spec "including"}
    "include declarations from bundle in goal refinement"
    (Scan.repeat1 (Parse.position Parse.xname) >> (Toplevel.proof o Bundle.including_cmd));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_bundles"}
    "print bundles of declarations"
    (Scan.succeed (Toplevel.unknown_context o
      Toplevel.keep (Bundle.print_bundles o Toplevel.context_of)));


(* local theories *)

val _ =
  Outer_Syntax.command @{command_spec "context"} "begin local theory context"
    ((Parse.position Parse.xname >> (fn name =>
        Toplevel.begin_local_theory true (Named_Target.begin name)) ||
      Scan.optional Parse_Spec.includes [] -- Scan.repeat Parse_Spec.context_element
        >> (fn (incls, elems) => Toplevel.open_target (Bundle.context_cmd incls elems)))
      --| Parse.begin);


(* locales *)

val locale_val =
  Parse_Spec.locale_expression false --
    Scan.optional (@{keyword "+"} |-- Parse.!!! (Scan.repeat1 Parse_Spec.context_element)) [] ||
  Scan.repeat1 Parse_Spec.context_element >> pair ([], []);

val _ =
  Outer_Syntax.command @{command_spec "locale"} "define named proof context"
    (Parse.binding --
      Scan.optional (@{keyword "="} |-- Parse.!!! locale_val) (([], []), []) -- Parse.opt_begin
      >> (fn ((name, (expr, elems)), begin) =>
          Toplevel.begin_local_theory begin
            (Expression.add_locale_cmd name Binding.empty expr elems #> snd)));

fun interpretation_args mandatory =
  Parse.!!! (Parse_Spec.locale_expression mandatory) --
    Scan.optional
      (Parse.where_ |-- Parse.and_list1 (Parse_Spec.opt_thm_name ":" -- Parse.prop)) [];

val _ =
  Outer_Syntax.command @{command_spec "sublocale"}
    "prove sublocale relation between a locale and a locale expression"
    ((Parse.position Parse.xname --| (@{keyword "\<subseteq>"} || @{keyword "<"}) --
      interpretation_args false >> (fn (loc, (expr, equations)) =>
        Toplevel.theory_to_proof (Expression.sublocale_global_cmd loc expr equations)))
    || interpretation_args false >> (fn (expr, equations) =>
        Toplevel.local_theory_to_proof NONE (Expression.sublocale_cmd expr equations)));

val _ =
  Outer_Syntax.command @{command_spec "interpretation"}
    "prove interpretation of locale expression in local theory"
    (interpretation_args true >> (fn (expr, equations) =>
      Toplevel.local_theory_to_proof NONE (Expression.interpretation_cmd expr equations)));

val _ =
  Outer_Syntax.command @{command_spec "interpret"}
    "prove interpretation of locale expression in proof context"
    (interpretation_args true >> (fn (expr, equations) =>
      Toplevel.proof' (Expression.interpret_cmd expr equations)));


(* classes *)

val class_val =
  Parse_Spec.class_expression --
    Scan.optional (@{keyword "+"} |-- Parse.!!! (Scan.repeat1 Parse_Spec.context_element)) [] ||
  Scan.repeat1 Parse_Spec.context_element >> pair [];

val _ =
  Outer_Syntax.command @{command_spec "class"} "define type class"
   (Parse.binding -- Scan.optional (@{keyword "="} |-- class_val) ([], []) -- Parse.opt_begin
    >> (fn ((name, (supclasses, elems)), begin) =>
        Toplevel.begin_local_theory begin
          (Class_Declaration.class_cmd name supclasses elems #> snd)));

val _ =
  Outer_Syntax.local_theory_to_proof @{command_spec "subclass"} "prove a subclass relation"
    (Parse.class >> Class_Declaration.subclass_cmd);

val _ =
  Outer_Syntax.command @{command_spec "instantiation"} "instantiate and prove type arity"
   (Parse.multi_arity --| Parse.begin
     >> (fn arities => Toplevel.begin_local_theory true (Class.instantiation_cmd arities)));

val _ =
  Outer_Syntax.command @{command_spec "instance"} "prove type arity or subclass relation"
  ((Parse.class --
    ((@{keyword "\<subseteq>"} || @{keyword "<"}) |-- Parse.!!! Parse.class) >> Class.classrel_cmd ||
    Parse.multi_arity >> Class.instance_arity_cmd) >> Toplevel.theory_to_proof ||
    Scan.succeed (Toplevel.local_theory_to_proof NONE (Class.instantiation_instance I)));


(* arbitrary overloading *)

val _ =
  Outer_Syntax.command @{command_spec "overloading"} "overloaded definitions"
   (Scan.repeat1 (Parse.name --| (@{keyword "\<equiv>"} || @{keyword "=="}) -- Parse.term --
      Scan.optional (@{keyword "("} |-- (@{keyword "unchecked"} >> K false) --| @{keyword ")"}) true
      >> Parse.triple1) --| Parse.begin
   >> (fn operations => Toplevel.begin_local_theory true (Overloading.overloading_cmd operations)));


(* code generation *)

val _ =
  Outer_Syntax.command @{command_spec "code_datatype"}
    "define set of code datatype constructors"
    (Scan.repeat1 Parse.term >> (Toplevel.theory o Code.add_datatype_cmd));



(** proof commands **)

(* statements *)

fun theorem spec schematic kind =
  Outer_Syntax.local_theory_to_proof' spec
    ("state " ^ (if schematic then "schematic " ^ kind else kind))
    (Scan.optional (Parse_Spec.opt_thm_name ":" --|
      Scan.ahead (Parse_Spec.includes >> K "" ||
        Parse_Spec.locale_keyword || Parse_Spec.statement_keyword)) Attrib.empty_binding --
      Scan.optional Parse_Spec.includes [] --
      Parse_Spec.general_statement >> (fn ((a, includes), (elems, concl)) =>
        ((if schematic then Specification.schematic_theorem_cmd else Specification.theorem_cmd)
          kind NONE (K I) a includes elems concl)));

val _ = theorem @{command_spec "theorem"} false Thm.theoremK;
val _ = theorem @{command_spec "lemma"} false Thm.lemmaK;
val _ = theorem @{command_spec "corollary"} false Thm.corollaryK;
val _ = theorem @{command_spec "schematic_theorem"} true Thm.theoremK;
val _ = theorem @{command_spec "schematic_lemma"} true Thm.lemmaK;
val _ = theorem @{command_spec "schematic_corollary"} true Thm.corollaryK;

val _ =
  Outer_Syntax.local_theory_to_proof @{command_spec "notepad"} "begin proof context"
    (Parse.begin >> K Proof.begin_notepad);

val _ =
  Outer_Syntax.command @{command_spec "have"} "state local goal"
    (Parse_Spec.statement >> (Toplevel.proof' o Isar_Cmd.have));

val _ =
  Outer_Syntax.command @{command_spec "hence"} "old-style alias of \"then have\""
    (Parse_Spec.statement >> (Toplevel.proof' o Isar_Cmd.hence));

val _ =
  Outer_Syntax.command @{command_spec "show"}
    "state local goal, solving current obligation"
    (Parse_Spec.statement >> (Toplevel.proof' o Isar_Cmd.show));

val _ =
  Outer_Syntax.command @{command_spec "thus"} "old-style alias of  \"then show\""
    (Parse_Spec.statement >> (Toplevel.proof' o Isar_Cmd.thus));


(* facts *)

val facts = Parse.and_list1 Parse_Spec.xthms1;

val _ =
  Outer_Syntax.command @{command_spec "then"} "forward chaining"
    (Scan.succeed (Toplevel.proof Proof.chain));

val _ =
  Outer_Syntax.command @{command_spec "from"} "forward chaining from given facts"
    (facts >> (Toplevel.proof o Proof.from_thmss_cmd));

val _ =
  Outer_Syntax.command @{command_spec "with"} "forward chaining from given and current facts"
    (facts >> (Toplevel.proof o Proof.with_thmss_cmd));

val _ =
  Outer_Syntax.command @{command_spec "note"} "define facts"
    (Parse_Spec.name_facts >> (Toplevel.proof o Proof.note_thmss_cmd));

val _ =
  Outer_Syntax.command @{command_spec "using"} "augment goal facts"
    (facts >> (Toplevel.proof o Proof.using_cmd));

val _ =
  Outer_Syntax.command @{command_spec "unfolding"} "unfold definitions in goal and facts"
    (facts >> (Toplevel.proof o Proof.unfolding_cmd));


(* proof context *)

val _ =
  Outer_Syntax.command @{command_spec "fix"} "fix local variables (Skolem constants)"
    (Parse.fixes >> (Toplevel.proof o Proof.fix_cmd));

val _ =
  Outer_Syntax.command @{command_spec "assume"} "assume propositions"
    (Parse_Spec.statement >> (Toplevel.proof o Proof.assume_cmd));

val _ =
  Outer_Syntax.command @{command_spec "presume"} "assume propositions, to be established later"
    (Parse_Spec.statement >> (Toplevel.proof o Proof.presume_cmd));

val _ =
  Outer_Syntax.command @{command_spec "def"} "local definition (non-polymorphic)"
    (Parse.and_list1
      (Parse_Spec.opt_thm_name ":" --
        ((Parse.binding -- Parse.opt_mixfix) --
          ((@{keyword "\<equiv>"} || @{keyword "=="}) |-- Parse.!!! Parse.termp)))
    >> (Toplevel.proof o Proof.def_cmd));

val _ =
  Outer_Syntax.command @{command_spec "obtain"} "generalized elimination"
    (Parse.parname -- Scan.optional (Parse.fixes --| Parse.where_) [] -- Parse_Spec.statement
      >> (fn ((x, y), z) => Toplevel.proof' (Obtain.obtain_cmd x y z)));

val _ =
  Outer_Syntax.command @{command_spec "guess"} "wild guessing (unstructured)"
    (Scan.optional Parse.fixes [] >> (Toplevel.proof' o Obtain.guess_cmd));

val _ =
  Outer_Syntax.command @{command_spec "let"} "bind text variables"
    (Parse.and_list1 (Parse.and_list1 Parse.term -- (@{keyword "="} |-- Parse.term))
      >> (Toplevel.proof o Proof.let_bind_cmd));

val _ =
  Outer_Syntax.command @{command_spec "write"} "add concrete syntax for constants / fixed variables"
    (opt_mode -- Parse.and_list1 (Parse.const -- Parse.mixfix)
    >> (fn (mode, args) => Toplevel.proof (Proof.write_cmd mode args)));

val _ =
  Outer_Syntax.command @{command_spec "case"} "invoke local context"
    ((@{keyword "("} |--
      Parse.!!! (Parse.position Parse.xname -- Scan.repeat (Parse.maybe Parse.binding)
        --| @{keyword ")"}) ||
      Parse.position Parse.xname >> rpair []) -- Parse_Spec.opt_attribs >> (fn ((c, xs), atts) =>
        Toplevel.proof (Proof.invoke_case_cmd (c, xs, atts))));


(* proof structure *)

val _ =
  Outer_Syntax.command @{command_spec "{"} "begin explicit proof block"
    (Scan.succeed (Toplevel.proof Proof.begin_block));

val _ =
  Outer_Syntax.command @{command_spec "}"} "end explicit proof block"
    (Scan.succeed (Toplevel.proof Proof.end_block));

val _ =
  Outer_Syntax.command @{command_spec "next"} "enter next proof block"
    (Scan.succeed (Toplevel.proof Proof.next_block));


(* end proof *)

val _ =
  Outer_Syntax.command @{command_spec "qed"} "conclude proof"
    (Scan.option Method.parse >> (fn m =>
     (Option.map Method.report m;
      Isar_Cmd.qed m)));

val _ =
  Outer_Syntax.command @{command_spec "by"} "terminal backward proof"
    (Method.parse -- Scan.option Method.parse >> (fn (m1, m2) =>
     (Method.report m1;
      Option.map Method.report m2;
      Isar_Cmd.terminal_proof (m1, m2))));

val _ =
  Outer_Syntax.command @{command_spec ".."} "default proof"
    (Scan.succeed Isar_Cmd.default_proof);

val _ =
  Outer_Syntax.command @{command_spec "."} "immediate proof"
    (Scan.succeed Isar_Cmd.immediate_proof);

val _ =
  Outer_Syntax.command @{command_spec "done"} "done proof"
    (Scan.succeed Isar_Cmd.done_proof);

val _ =
  Outer_Syntax.command @{command_spec "sorry"} "skip proof (quick-and-dirty mode only!)"
    (Scan.succeed Isar_Cmd.skip_proof);

val _ =
  Outer_Syntax.command @{command_spec "oops"} "forget proof"
    (Scan.succeed Toplevel.forget_proof);


(* proof steps *)

val _ =
  Outer_Syntax.command @{command_spec "defer"} "shuffle internal proof state"
    (Scan.optional Parse.nat 1 >> (Toplevel.proof o Proof.defer));

val _ =
  Outer_Syntax.command @{command_spec "prefer"} "shuffle internal proof state"
    (Parse.nat >> (Toplevel.proof o Proof.prefer));

val _ =
  Outer_Syntax.command @{command_spec "apply"} "initial refinement step (unstructured)"
    (Method.parse >> (fn m => (Method.report m; Toplevel.proofs (Proof.apply_results m))));

val _ =
  Outer_Syntax.command @{command_spec "apply_end"} "terminal refinement step (unstructured)"
    (Method.parse >> (fn m => (Method.report m; Toplevel.proofs (Proof.apply_end_results m))));

val _ =
  Outer_Syntax.command @{command_spec "proof"} "backward proof step"
    (Scan.option Method.parse >> (fn m =>
      (Option.map Method.report m;
        Toplevel.actual_proof (Proof_Node.applys (Proof.proof_results m)) o
        Toplevel.skip_proof (fn i => i + 1))));


(* proof navigation *)

fun report_back () =
  Output.report [Markup.markup Markup.bad "Explicit backtracking"];

val _ =
  Outer_Syntax.command @{command_spec "back"} "explicit backtracking of proof command"
    (Scan.succeed
     (Toplevel.actual_proof (fn prf => (report_back (); Proof_Node.back prf)) o
      Toplevel.skip_proof (fn h => (report_back (); h))));



(** nested commands **)

val props_text =
  Scan.optional Parse.properties [] -- Parse.position Parse.string
  >> (fn (props, (str, pos)) =>
      (Position.of_properties (Position.default_properties pos props), str));

val _ =
  Outer_Syntax.improper_command @{command_spec "Isabelle.command"} "evaluate nested Isabelle command"
    (props_text :|-- (fn (pos, str) =>
      (case Outer_Syntax.parse pos str of
        [tr] => Scan.succeed (K tr)
      | _ => Scan.fail_with (K (fn () => "exactly one command expected")))
      handle ERROR msg => Scan.fail_with (K (fn () => msg))));



(** diagnostic commands (for interactive mode only) **)

val opt_modes =
  Scan.optional (@{keyword "("} |-- Parse.!!! (Scan.repeat1 Parse.xname --| @{keyword ")"})) [];

val opt_bang = Scan.optional (@{keyword "!"} >> K true) false;

val _ = (*Proof General legacy*)
  Outer_Syntax.improper_command @{command_spec "pretty_setmargin"}
    "change default margin for pretty printing"
    (Parse.nat >> (fn n => Toplevel.imperative (fn () => Pretty.margin_default := n)));

val _ =
  Outer_Syntax.improper_command @{command_spec "help"}
    "retrieve outer syntax commands according to name patterns"
    (Scan.repeat Parse.name >>
      (fn pats => Toplevel.imperative (fn () => Outer_Syntax.help_outer_syntax pats)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_commands"} "print outer syntax commands"
    (Scan.succeed (Toplevel.imperative Outer_Syntax.print_outer_syntax));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_options"} "print configuration options"
    (Scan.succeed (Toplevel.unknown_context o
      Toplevel.keep (Attrib.print_options o Toplevel.context_of)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_context"}
    "print context of local theory target"
    (Scan.succeed (Toplevel.keep (Pretty.writeln_chunks o Toplevel.pretty_context)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_theory"}
    "print logical theory contents (verbose!)"
    (opt_bang >> (fn b => Toplevel.unknown_theory o
      Toplevel.keep (Pretty.writeln o Proof_Display.pretty_full_theory b o Toplevel.theory_of)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_syntax"}
    "print inner syntax of context (verbose!)"
    (Scan.succeed (Toplevel.unknown_context o
      Toplevel.keep (Proof_Context.print_syntax o Toplevel.context_of)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_defn_rules"}
    "print definitional rewrite rules of context"
    (Scan.succeed (Toplevel.unknown_context o
      Toplevel.keep (Local_Defs.print_rules o Toplevel.context_of)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_abbrevs"}
    "print constant abbreviations of context"
    (Scan.succeed (Toplevel.unknown_context o
      Toplevel.keep (Proof_Context.print_abbrevs o Toplevel.context_of)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_theorems"}
    "print theorems of local theory or proof context"
    (opt_bang >> (fn b =>
      Toplevel.unknown_context o
      Toplevel.keep (Pretty.writeln o Pretty.chunks o Isar_Cmd.pretty_theorems b)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_locales"}
    "print locales of this theory"
    (Scan.succeed (Toplevel.unknown_theory o
      Toplevel.keep (Locale.print_locales o Toplevel.theory_of)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_classes"}
    "print classes of this theory"
    (Scan.succeed (Toplevel.unknown_theory o
      Toplevel.keep (Class.print_classes o Toplevel.context_of)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_locale"}
    "print locale of this theory"
    (opt_bang -- Parse.position Parse.xname >> (fn (b, name) =>
      Toplevel.unknown_theory o
      Toplevel.keep (fn state => Locale.print_locale (Toplevel.theory_of state) b name)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_interps"}
    "print interpretations of locale for this theory or proof context"
    (Parse.position Parse.xname >> (fn name =>
      Toplevel.unknown_context o
      Toplevel.keep (fn state => Locale.print_registrations (Toplevel.context_of state) name)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_dependencies"}
    "print dependencies of locale expression"
    (opt_bang -- Parse_Spec.locale_expression true >> (fn (b, expr) =>
      Toplevel.unknown_context o
      Toplevel.keep (fn state => Expression.print_dependencies (Toplevel.context_of state) b expr)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_attributes"}
    "print attributes of this theory"
    (Scan.succeed (Toplevel.unknown_theory o
      Toplevel.keep (Attrib.print_attributes o Toplevel.context_of)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_simpset"}
    "print context of Simplifier"
    (Scan.succeed (Toplevel.unknown_context o
      Toplevel.keep (Pretty.writeln o Simplifier.pretty_simpset o Toplevel.context_of)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_rules"} "print intro/elim rules"
    (Scan.succeed (Toplevel.unknown_context o
      Toplevel.keep (Context_Rules.print_rules o Toplevel.context_of)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_methods"} "print methods of this theory"
    (Scan.succeed (Toplevel.unknown_theory o
      Toplevel.keep (Method.print_methods o Toplevel.context_of)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_antiquotations"}
    "print document antiquotations"
    (Scan.succeed (Toplevel.unknown_context o
      Toplevel.keep (Thy_Output.print_antiquotations o Toplevel.context_of)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_ML_antiquotations"}
    "print ML antiquotations"
    (Scan.succeed (Toplevel.unknown_context o
      Toplevel.keep (ML_Context.print_antiquotations o Toplevel.context_of)));

val _ =
  Outer_Syntax.improper_command @{command_spec "thy_deps"} "visualize theory dependencies"
    (Scan.succeed Isar_Cmd.thy_deps);

val _ =
  Outer_Syntax.improper_command @{command_spec "locale_deps"} "visualize locale dependencies"
    (Scan.succeed Isar_Cmd.locale_deps);

val _ =
  Outer_Syntax.improper_command @{command_spec "class_deps"} "visualize class dependencies"
    (Scan.succeed Isar_Cmd.class_deps);

val _ =
  Outer_Syntax.improper_command @{command_spec "thm_deps"} "visualize theorem dependencies"
    (Parse_Spec.xthms1 >> Isar_Cmd.thm_deps);

val _ =
  Outer_Syntax.improper_command @{command_spec "print_binds"}
    "print term bindings of proof context -- Proof General legacy"
    (Scan.succeed (Toplevel.unknown_context o
      Toplevel.keep
        (Pretty.writeln_chunks o Proof_Context.pretty_term_bindings o Toplevel.context_of)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_term_bindings"}
    "print term bindings of proof context"
    (Scan.succeed (Toplevel.unknown_context o
      Toplevel.keep
        (Pretty.writeln_chunks o Proof_Context.pretty_term_bindings o Toplevel.context_of)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_facts"} "print facts of proof context"
    (opt_bang >> (fn verbose => Toplevel.unknown_context o
      Toplevel.keep (fn st => Proof_Context.print_local_facts (Toplevel.context_of st) verbose)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_cases"} "print cases of proof context"
    (Scan.succeed (Toplevel.unknown_context o
      Toplevel.keep (Pretty.writeln_chunks o Proof_Context.pretty_cases o Toplevel.context_of)));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_statement"}
    "print theorems as long statements"
    (opt_modes -- Parse_Spec.xthms1 >> Isar_Cmd.print_stmts);

val _ =
  Outer_Syntax.improper_command @{command_spec "thm"} "print theorems"
    (opt_modes -- Parse_Spec.xthms1 >> Isar_Cmd.print_thms);

val _ =
  Outer_Syntax.improper_command @{command_spec "prf"} "print proof terms of theorems"
    (opt_modes -- Scan.option Parse_Spec.xthms1 >> Isar_Cmd.print_prfs false);

val _ =
  Outer_Syntax.improper_command @{command_spec "full_prf"} "print full proof terms of theorems"
    (opt_modes -- Scan.option Parse_Spec.xthms1 >> Isar_Cmd.print_prfs true);

val _ =
  Outer_Syntax.improper_command @{command_spec "prop"} "read and print proposition"
    (opt_modes -- Parse.term >> Isar_Cmd.print_prop);

val _ =
  Outer_Syntax.improper_command @{command_spec "term"} "read and print term"
    (opt_modes -- Parse.term >> Isar_Cmd.print_term);

val _ =
  Outer_Syntax.improper_command @{command_spec "typ"} "read and print type"
    (opt_modes -- (Parse.typ -- Scan.option (@{keyword "::"} |-- Parse.!!! Parse.sort))
      >> Isar_Cmd.print_type);

val _ =
  Outer_Syntax.improper_command @{command_spec "print_codesetup"} "print code generator setup"
    (Scan.succeed (Toplevel.unknown_theory o
      Toplevel.keep (Code.print_codesetup o Toplevel.theory_of)));

val _ =
  Outer_Syntax.improper_command @{command_spec "unused_thms"} "find unused theorems"
    (Scan.option ((Scan.repeat1 (Scan.unless Parse.minus Parse.name) --| Parse.minus) --
       Scan.option (Scan.repeat1 (Scan.unless Parse.minus Parse.name))) >> Isar_Cmd.unused_thms);



(** system commands (for interactive mode only) **)

val _ =
  Outer_Syntax.improper_command @{command_spec "use_thy"} "use theory file"
    (Parse.position Parse.name >>
      (fn name => Toplevel.imperative (fn () => Thy_Info.use_thy name)));

val _ =
  Outer_Syntax.improper_command @{command_spec "remove_thy"} "remove theory from loader database"
    (Parse.name >> (fn name => Toplevel.imperative (fn () => Thy_Info.remove_thy name)));

val _ =
  Outer_Syntax.improper_command @{command_spec "kill_thy"}
    "kill theory -- try to remove from loader database"
    (Parse.name >> (fn name => Toplevel.imperative (fn () => Thy_Info.kill_thy name)));

val _ = (*partial Proof General legacy*)
  Outer_Syntax.improper_command @{command_spec "display_drafts"}
    "display raw source files with symbols"
    (Scan.repeat1 Parse.path >> (fn names =>
      Toplevel.imperative (fn () => ignore (Present.display_drafts (map Path.explode names)))));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_state"}
    "print current proof state (if present)"
    (opt_modes >> (fn modes => Toplevel.keep (Print_Mode.with_modes modes Toplevel.print_state)));

val _ = (*Proof General legacy, e.g. for ProofGeneral-3.7.x*)
  Outer_Syntax.improper_command @{command_spec "pr"} "print current proof state (if present)"
    (opt_modes -- Scan.option Parse.nat >> (fn (modes, limit) =>
      Toplevel.keep (fn state =>
       (if Isabelle_Process.is_active () then error "Illegal TTY command" else ();
        case limit of NONE => () | SOME n => Options.default_put_int @{system_option goals_limit} n;
        Toplevel.quiet := false;
        Print_Mode.with_modes modes Toplevel.print_state state))));

val _ = (*Proof General legacy*)
  Outer_Syntax.improper_command @{command_spec "disable_pr"}
    "disable printing of toplevel state"
    (Scan.succeed (Toplevel.imperative (fn () => Toplevel.quiet := true)));

val _ = (*Proof General legacy*)
  Outer_Syntax.improper_command @{command_spec "enable_pr"}
    "enable printing of toplevel state"
    (Scan.succeed (Toplevel.imperative (fn () => Toplevel.quiet := false)));

val _ =
  Outer_Syntax.improper_command @{command_spec "commit"}
    "commit current session to ML session image"
    (Parse.opt_unit >> K (Toplevel.imperative Secure.commit));

val _ =
  Outer_Syntax.improper_command @{command_spec "quit"} "quit Isabelle process"
    (Parse.opt_unit >> (K (Toplevel.imperative quit)));

val _ =
  Outer_Syntax.improper_command @{command_spec "exit"} "exit Isar loop"
    (Scan.succeed
      (Toplevel.keep (fn state =>
        (Context.set_thread_data (try Toplevel.generic_theory_of state);
          raise Runtime.TERMINATE))));

val _ =
  Outer_Syntax.improper_command @{command_spec "welcome"} "print welcome message"
    (Scan.succeed (Toplevel.imperative (writeln o Session.welcome)));



(** raw Isar read-eval-print loop **)

val _ =
  Outer_Syntax.improper_command @{command_spec "init_toplevel"} "init toplevel point-of-interest"
    (Scan.succeed (Toplevel.imperative Isar.init));

val _ =
  Outer_Syntax.improper_command @{command_spec "linear_undo"} "undo commands"
    (Scan.optional Parse.nat 1 >>
      (fn n => Toplevel.imperative (fn () => Isar.linear_undo n)));

val _ =
  Outer_Syntax.improper_command @{command_spec "undo"} "undo commands (skipping closed proofs)"
    (Scan.optional Parse.nat 1 >>
      (fn n => Toplevel.imperative (fn () => Isar.undo n)));

val _ =
  Outer_Syntax.improper_command @{command_spec "undos_proof"}
    "undo commands (skipping closed proofs)"
    (Scan.optional Parse.nat 1 >> (fn n =>
      Toplevel.keep (fn state =>
        if Toplevel.is_proof state then (Isar.undo n; Isar.print ()) else raise Toplevel.UNDEF)));

val _ =
  Outer_Syntax.improper_command @{command_spec "cannot_undo"}
    "partial undo -- Proof General legacy"
    (Parse.name >>
      (fn "end" => Toplevel.imperative (fn () => Isar.undo 1)
        | txt => Toplevel.imperative (fn () => error ("Cannot undo " ^ quote txt))));

val _ =
  Outer_Syntax.improper_command @{command_spec "kill"}
    "kill partial proof or theory development"
    (Scan.succeed (Toplevel.imperative Isar.kill));



(** extraction of programs from proofs **)

val parse_vars = Scan.optional (Parse.$$$ "(" |-- Parse.list1 Parse.name --| Parse.$$$ ")") [];

val _ =
  Outer_Syntax.command @{command_spec "realizers"}
    "specify realizers for primitive axioms / theorems, together with correctness proof"
    (Scan.repeat1 (Parse.xname -- parse_vars --| Parse.$$$ ":" -- Parse.string -- Parse.string) >>
     (fn xs => Toplevel.theory (fn thy => Extraction.add_realizers
       (map (fn (((a, vs), s1), s2) => (Global_Theory.get_thm thy a, (vs, s1, s2))) xs) thy)));

val _ =
  Outer_Syntax.command @{command_spec "realizability"}
    "add equations characterizing realizability"
    (Scan.repeat1 Parse.string >> (Toplevel.theory o Extraction.add_realizes_eqns));

val _ =
  Outer_Syntax.command @{command_spec "extract_type"}
    "add equations characterizing type of extracted program"
    (Scan.repeat1 Parse.string >> (Toplevel.theory o Extraction.add_typeof_eqns));

val _ =
  Outer_Syntax.command @{command_spec "extract"} "extract terms from proofs"
    (Scan.repeat1 (Parse.xname -- parse_vars) >> (fn xs => Toplevel.theory (fn thy =>
      Extraction.extract (map (apfst (Global_Theory.get_thm thy)) xs) thy)));



(** end **)

val _ =
  Outer_Syntax.command @{command_spec "end"} "end context"
    (Scan.succeed
      (Toplevel.exit o Toplevel.end_local_theory o Toplevel.close_target o
        Toplevel.end_proof (K Proof.end_notepad)));

end;

