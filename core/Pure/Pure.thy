(*  Title:      Pure/Pure.thy
    Author:     Makarius

The Pure theory, with definitions of Isar commands and some lemmas.
*)

theory Pure
keywords
    "!!" "!" "+" "--" ":" ";" "<" "<=" "==" "=>" "?" "[" "\<comment>" "\<equiv>" "\<leftharpoondown>" "\<rightharpoonup>" "\<rightleftharpoons>"
    "\<subseteq>" "]" "attach" "binder" "for" "if" "in" "infix" "infixl" "infixr" "is"
    "open" "output" "overloaded" "pervasive" "premises" "structure" "unchecked" "when"
  and "private" "qualified" :: before_command
  and "assumes" "constrains" "defines" "fixes" "includes" "notes" "rewrites"
    "obtains" "shows" "where" "|" :: quasi_command
  and "text" "txt" :: document_body
  and "text_raw" :: document_raw
  and "default_sort" :: thy_decl
  and "typedecl" "type_synonym" "nonterminal" "judgment"
    "consts" "syntax" "no_syntax" "translations" "no_translations"
    "definition" "abbreviation" "type_notation" "no_type_notation" "notation"
    "no_notation" "axiomatization" "lemmas" "declare"
    "hide_class" "hide_type" "hide_const" "hide_fact" :: thy_decl
  and "ML_file" "ML_file_debug" "ML_file_no_debug" :: thy_load % "ML"
  and "SML_file" "SML_file_debug" "SML_file_no_debug" :: thy_load % "ML"
  and "SML_import" "SML_export" :: thy_decl % "ML"
  and "ML_prf" :: prf_decl % "proof"  (* FIXME % "ML" ?? *)
  and "ML_val" "ML_command" :: diag % "ML"
  and "simproc_setup" :: thy_decl % "ML"
  and "setup" "local_setup" "attribute_setup" "method_setup"
    "declaration" "syntax_declaration"
    "parse_ast_translation" "parse_translation" "print_translation"
    "typed_print_translation" "print_ast_translation" "oracle" :: thy_decl % "ML"
  and "bundle" :: thy_decl_block
  and "unbundle" :: thy_decl
  and "include" "including" :: prf_decl
  and "print_bundles" :: diag
  and "context" "locale" "experiment" :: thy_decl_block
  and "interpret" :: prf_goal % "proof"
  and "interpretation" "global_interpretation" "sublocale" :: thy_goal
  and "class" :: thy_decl_block
  and "subclass" :: thy_goal
  and "instantiation" :: thy_decl_block
  and "instance" :: thy_goal
  and "overloading" :: thy_decl_block
  and "code_datatype" :: thy_decl
  and "theorem" "lemma" "corollary" "proposition" :: thy_goal
  and "schematic_goal" :: thy_goal
  and "notepad" :: thy_decl_block
  and "have" :: prf_goal % "proof"
  and "hence" :: prf_goal % "proof"
  and "show" :: prf_asm_goal % "proof"
  and "thus" :: prf_asm_goal % "proof"
  and "then" "from" "with" :: prf_chain % "proof"
  and "note" :: prf_decl % "proof"
  and "supply" :: prf_script % "proof"
  and "using" "unfolding" :: prf_decl % "proof"
  and "fix" "assume" "presume" "define" "def" :: prf_asm % "proof"
  and "consider" :: prf_goal % "proof"
  and "obtain" :: prf_asm_goal % "proof"
  and "guess" :: prf_script_asm_goal % "proof"
  and "let" "write" :: prf_decl % "proof"
  and "case" :: prf_asm % "proof"
  and "{" :: prf_open % "proof"
  and "}" :: prf_close % "proof"
  and "next" :: next_block % "proof"
  and "qed" :: qed_block % "proof"
  and "by" ".." "." "sorry" "\<proof>" :: "qed" % "proof"
  and "done" :: "qed_script" % "proof"
  and "oops" :: qed_global % "proof"
  and "defer" "prefer" "apply" :: prf_script % "proof"
  and "apply_end" :: prf_script % "proof"
  and "subgoal" :: prf_script_goal % "proof"
  and "proof" :: prf_block % "proof"
  and "also" "moreover" :: prf_decl % "proof"
  and "finally" "ultimately" :: prf_chain % "proof"
  and "back" :: prf_script % "proof"
  and "help" "print_commands" "print_options" "print_context" "print_theory"
    "print_definitions" "print_syntax" "print_abbrevs" "print_defn_rules"
    "print_theorems" "print_locales" "print_classes" "print_locale"
    "print_interps" "print_dependencies" "print_attributes"
    "print_simpset" "print_rules" "print_trans_rules" "print_methods"
    "print_antiquotations" "print_ML_antiquotations" "thy_deps"
    "locale_deps" "class_deps" "thm_deps" "print_term_bindings"
    "print_facts" "print_cases" "print_statement" "thm" "prf" "full_prf"
    "prop" "term" "typ" "print_codesetup" "unused_thms" :: diag
  and "display_drafts" "print_state" :: diag
  and "welcome" :: diag
  and "end" :: thy_end % "theory"
  and "realizers" :: thy_decl
  and "realizability" :: thy_decl
  and "extract_type" "extract" :: thy_decl
  and "find_theorems" "find_consts" :: diag
  and "named_theorems" :: thy_decl
abbrevs
  "===>" = "===>"  (*prevent replacement of very long arrows*)
  "--->" = "\<midarrow>\<rightarrow>"
  "default_sort" = ""
  "simproc_setup" = ""
  "hence" = ""
  "hence" = "then have"
  "thus" = ""
  "thus" = "then show"
  "apply_end" = ""
  "realizers" = ""
  "realizability" = ""
begin

section \<open>Isar commands\<close>

subsection \<open>Embedded ML text\<close>

ML \<open>
local

val semi = Scan.option @{keyword ";"};

val _ =
  Outer_Syntax.command @{command_keyword ML_file} "read and evaluate Isabelle/ML file"
    (Resources.parse_files "ML_file" --| semi >> ML_File.ML NONE);

val _ =
  Outer_Syntax.command @{command_keyword ML_file_debug}
    "read and evaluate Isabelle/ML file (with debugger information)"
    (Resources.parse_files "ML_file_debug" --| semi >> ML_File.ML (SOME true));

val _ =
  Outer_Syntax.command @{command_keyword ML_file_no_debug}
    "read and evaluate Isabelle/ML file (no debugger information)"
    (Resources.parse_files "ML_file_no_debug" --| semi >> ML_File.ML (SOME false));

val _ =
  Outer_Syntax.command @{command_keyword SML_file} "read and evaluate Standard ML file"
    (Resources.parse_files "SML_file" --| semi >> ML_File.SML NONE);

val _ =
  Outer_Syntax.command @{command_keyword SML_file_debug}
    "read and evaluate Standard ML file (with debugger information)"
    (Resources.parse_files "SML_file_debug" --| semi >> ML_File.SML (SOME true));

val _ =
  Outer_Syntax.command @{command_keyword SML_file_no_debug}
    "read and evaluate Standard ML file (no debugger information)"
    (Resources.parse_files "SML_file_no_debug" --| semi >> ML_File.SML (SOME false));

val _ =
  Outer_Syntax.command @{command_keyword SML_export} "evaluate SML within Isabelle/ML environment"
    (Parse.ML_source >> (fn source =>
      let
        val flags: ML_Compiler.flags =
          {SML = true, exchange = true, redirect = false, verbose = true,
            debug = NONE, writeln = writeln, warning = warning};
      in
        Toplevel.theory
          (Context.theory_map (ML_Context.exec (fn () => ML_Context.eval_source flags source)))
      end));

val _ =
  Outer_Syntax.command @{command_keyword SML_import} "evaluate Isabelle/ML within SML environment"
    (Parse.ML_source >> (fn source =>
      let
        val flags: ML_Compiler.flags =
          {SML = false, exchange = true, redirect = false, verbose = true,
            debug = NONE, writeln = writeln, warning = warning};
      in
        Toplevel.generic_theory
          (ML_Context.exec (fn () => ML_Context.eval_source flags source) #>
            Local_Theory.propagate_ml_env)
      end));

val _ =
  Outer_Syntax.command @{command_keyword ML_prf} "ML text within proof"
    (Parse.ML_source >> (fn source =>
      Toplevel.proof (Proof.map_context (Context.proof_map
        (ML_Context.exec (fn () =>
            ML_Context.eval_source (ML_Compiler.verbose true ML_Compiler.flags) source))) #>
          Proof.propagate_ml_env)));

val _ =
  Outer_Syntax.command @{command_keyword ML_val} "diagnostic ML text"
    (Parse.ML_source >> Isar_Cmd.ml_diag true);

val _ =
  Outer_Syntax.command @{command_keyword ML_command} "diagnostic ML text (silent)"
    (Parse.ML_source >> Isar_Cmd.ml_diag false);

val _ =
  Outer_Syntax.command @{command_keyword setup} "ML setup for global theory"
    (Parse.ML_source >> (Toplevel.theory o Isar_Cmd.setup));

val _ =
  Outer_Syntax.local_theory @{command_keyword local_setup} "ML setup for local theory"
    (Parse.ML_source >> Isar_Cmd.local_setup);

val _ =
  Outer_Syntax.command @{command_keyword oracle} "declare oracle"
    (Parse.range Parse.name -- (@{keyword "="} |-- Parse.ML_source) >>
      (fn (x, y) => Toplevel.theory (Isar_Cmd.oracle x y)));

val _ =
  Outer_Syntax.local_theory @{command_keyword attribute_setup} "define attribute in ML"
    (Parse.position Parse.name --
        Parse.!!! (@{keyword "="} |-- Parse.ML_source -- Scan.optional Parse.text "")
      >> (fn (name, (txt, cmt)) => Attrib.attribute_setup name txt cmt));

val _ =
  Outer_Syntax.local_theory @{command_keyword method_setup} "define proof method in ML"
    (Parse.position Parse.name --
        Parse.!!! (@{keyword "="} |-- Parse.ML_source -- Scan.optional Parse.text "")
      >> (fn (name, (txt, cmt)) => Method.method_setup name txt cmt));

val _ =
  Outer_Syntax.local_theory @{command_keyword declaration} "generic ML declaration"
    (Parse.opt_keyword "pervasive" -- Parse.ML_source
      >> (fn (pervasive, txt) => Isar_Cmd.declaration {syntax = false, pervasive = pervasive} txt));

val _ =
  Outer_Syntax.local_theory @{command_keyword syntax_declaration} "generic ML syntax declaration"
    (Parse.opt_keyword "pervasive" -- Parse.ML_source
      >> (fn (pervasive, txt) => Isar_Cmd.declaration {syntax = true, pervasive = pervasive} txt));

val _ =
  Outer_Syntax.local_theory @{command_keyword simproc_setup} "define simproc in ML"
    (Parse.position Parse.name --
      (@{keyword "("} |-- Parse.enum1 "|" Parse.term --| @{keyword ")"} --| @{keyword "="}) --
      Parse.ML_source >> (fn ((a, b), c) => Isar_Cmd.simproc_setup a b c));

in end\<close>


subsection \<open>Theory commands\<close>

subsubsection \<open>Sorts and types\<close>

ML \<open>
local

val _ =
  Outer_Syntax.local_theory @{command_keyword default_sort}
    "declare default sort for explicit type variables"
    (Parse.sort >> (fn s => fn lthy => Local_Theory.set_defsort (Syntax.read_sort lthy s) lthy));

val _ =
  Outer_Syntax.local_theory @{command_keyword typedecl} "type declaration"
    (Parse.type_args -- Parse.binding -- Parse.opt_mixfix
      >> (fn ((args, a), mx) =>
          Typedecl.typedecl {final = true} (a, map (rpair dummyS) args, mx) #> snd));

val _ =
  Outer_Syntax.local_theory @{command_keyword type_synonym} "declare type abbreviation"
    (Parse.type_args -- Parse.binding --
      (@{keyword "="} |-- Parse.!!! (Parse.typ -- Parse.opt_mixfix'))
      >> (fn ((args, a), (rhs, mx)) => snd o Typedecl.abbrev_cmd (a, args, mx) rhs));

in end\<close>


subsubsection \<open>Consts\<close>

ML \<open>
local

val _ =
  Outer_Syntax.command @{command_keyword judgment} "declare object-logic judgment"
    (Parse.const_binding >> (Toplevel.theory o Object_Logic.add_judgment_cmd));

val _ =
  Outer_Syntax.command @{command_keyword consts} "declare constants"
    (Scan.repeat1 Parse.const_binding >> (Toplevel.theory o Sign.add_consts_cmd));

in end\<close>


subsubsection \<open>Syntax and translations\<close>

ML \<open>
local

val _ =
  Outer_Syntax.command @{command_keyword nonterminal}
    "declare syntactic type constructors (grammar nonterminal symbols)"
    (Parse.and_list1 Parse.binding >> (Toplevel.theory o Sign.add_nonterminals_global));

val _ =
  Outer_Syntax.command @{command_keyword syntax} "add raw syntax clauses"
    (Parse.syntax_mode -- Scan.repeat1 Parse.const_decl
      >> (Toplevel.theory o uncurry Sign.add_syntax_cmd));

val _ =
  Outer_Syntax.command @{command_keyword no_syntax} "delete raw syntax clauses"
    (Parse.syntax_mode -- Scan.repeat1 Parse.const_decl
      >> (Toplevel.theory o uncurry Sign.del_syntax_cmd));

val trans_pat =
  Scan.optional
    (@{keyword "("} |-- Parse.!!! (Parse.inner_syntax Parse.name --| @{keyword ")"})) "logic"
    -- Parse.inner_syntax Parse.string;

fun trans_arrow toks =
  ((@{keyword "\<rightharpoonup>"} || @{keyword "=>"}) >> K Syntax.Parse_Rule ||
    (@{keyword "\<leftharpoondown>"} || @{keyword "<="}) >> K Syntax.Print_Rule ||
    (@{keyword "\<rightleftharpoons>"} || @{keyword "=="}) >> K Syntax.Parse_Print_Rule) toks;

val trans_line =
  trans_pat -- Parse.!!! (trans_arrow -- trans_pat)
    >> (fn (left, (arr, right)) => arr (left, right));

val _ =
  Outer_Syntax.command @{command_keyword translations} "add syntax translation rules"
    (Scan.repeat1 trans_line >> (Toplevel.theory o Isar_Cmd.translations));

val _ =
  Outer_Syntax.command @{command_keyword no_translations} "delete syntax translation rules"
    (Scan.repeat1 trans_line >> (Toplevel.theory o Isar_Cmd.no_translations));

in end\<close>


subsubsection \<open>Translation functions\<close>

ML \<open>
local

val _ =
  Outer_Syntax.command @{command_keyword parse_ast_translation}
    "install parse ast translation functions"
    (Parse.ML_source >> (Toplevel.theory o Isar_Cmd.parse_ast_translation));

val _ =
  Outer_Syntax.command @{command_keyword parse_translation}
    "install parse translation functions"
    (Parse.ML_source >> (Toplevel.theory o Isar_Cmd.parse_translation));

val _ =
  Outer_Syntax.command @{command_keyword print_translation}
    "install print translation functions"
    (Parse.ML_source >> (Toplevel.theory o Isar_Cmd.print_translation));

val _ =
  Outer_Syntax.command @{command_keyword typed_print_translation}
    "install typed print translation functions"
    (Parse.ML_source >> (Toplevel.theory o Isar_Cmd.typed_print_translation));

val _ =
  Outer_Syntax.command @{command_keyword print_ast_translation}
    "install print ast translation functions"
    (Parse.ML_source >> (Toplevel.theory o Isar_Cmd.print_ast_translation));

in end\<close>


subsubsection \<open>Specifications\<close>

ML \<open>
local

val _ =
  Outer_Syntax.local_theory' @{command_keyword definition} "constant definition"
    (Scan.option Parse_Spec.constdecl -- (Parse_Spec.opt_thm_name ":" -- Parse.prop) --
      Parse_Spec.if_assumes -- Parse.for_fixes >> (fn (((decl, spec), prems), params) =>
        #2 oo Specification.definition_cmd decl params prems spec));

val _ =
  Outer_Syntax.local_theory' @{command_keyword abbreviation} "constant abbreviation"
    (Parse.syntax_mode -- Scan.option Parse_Spec.constdecl -- Parse.prop -- Parse.for_fixes
      >> (fn (((mode, decl), spec), params) => Specification.abbreviation_cmd mode decl params spec));

val axiomatization =
  Parse.and_list1 (Parse_Spec.thm_name ":" -- Parse.prop) --
  Parse_Spec.if_assumes -- Parse.for_fixes >> (fn ((a, b), c) => (c, b, a));

val _ =
  Outer_Syntax.command @{command_keyword axiomatization} "axiomatic constant specification"
    (Scan.optional Parse.vars [] --
      Scan.optional (Parse.where_ |-- Parse.!!! axiomatization) ([], [], [])
      >> (fn (a, (b, c, d)) => Toplevel.theory (#2 o Specification.axiomatization_cmd a b c d)));

in end\<close>


subsubsection \<open>Notation\<close>

ML \<open>
local

val _ =
  Outer_Syntax.local_theory @{command_keyword type_notation}
    "add concrete syntax for type constructors"
    (Parse.syntax_mode -- Parse.and_list1 (Parse.type_const -- Parse.mixfix)
      >> (fn (mode, args) => Specification.type_notation_cmd true mode args));

val _ =
  Outer_Syntax.local_theory @{command_keyword no_type_notation}
    "delete concrete syntax for type constructors"
    (Parse.syntax_mode -- Parse.and_list1 (Parse.type_const -- Parse.mixfix)
      >> (fn (mode, args) => Specification.type_notation_cmd false mode args));

val _ =
  Outer_Syntax.local_theory @{command_keyword notation}
    "add concrete syntax for constants / fixed variables"
    (Parse.syntax_mode -- Parse.and_list1 (Parse.const -- Parse.mixfix)
      >> (fn (mode, args) => Specification.notation_cmd true mode args));

val _ =
  Outer_Syntax.local_theory @{command_keyword no_notation}
    "delete concrete syntax for constants / fixed variables"
    (Parse.syntax_mode -- Parse.and_list1 (Parse.const -- Parse.mixfix)
      >> (fn (mode, args) => Specification.notation_cmd false mode args));

in end\<close>


subsubsection \<open>Theorems\<close>

ML \<open>
local

val long_keyword =
  Parse_Spec.includes >> K "" ||
  Parse_Spec.long_statement_keyword;

val long_statement =
  Scan.optional (Parse_Spec.opt_thm_name ":" --| Scan.ahead long_keyword) Binding.empty_atts --
  Scan.optional Parse_Spec.includes [] -- Parse_Spec.long_statement
    >> (fn ((binding, includes), (elems, concl)) => (true, binding, includes, elems, concl));

val short_statement =
  Parse_Spec.statement -- Parse_Spec.if_statement -- Parse.for_fixes
    >> (fn ((shows, assumes), fixes) =>
      (false, Binding.empty_atts, [], [Element.Fixes fixes, Element.Assumes assumes],
        Element.Shows shows));

fun theorem spec schematic descr =
  Outer_Syntax.local_theory_to_proof' spec ("state " ^ descr)
    ((long_statement || short_statement) >> (fn (long, binding, includes, elems, concl) =>
      ((if schematic then Specification.schematic_theorem_cmd else Specification.theorem_cmd)
        long Thm.theoremK NONE (K I) binding includes elems concl)));

val _ = theorem @{command_keyword theorem} false "theorem";
val _ = theorem @{command_keyword lemma} false "lemma";
val _ = theorem @{command_keyword corollary} false "corollary";
val _ = theorem @{command_keyword proposition} false "proposition";
val _ = theorem @{command_keyword schematic_goal} true "schematic goal";

in end\<close>

ML \<open>
local

val _ =
  Outer_Syntax.local_theory' @{command_keyword lemmas} "define theorems"
    (Parse_Spec.name_facts -- Parse.for_fixes >>
      (fn (facts, fixes) => #2 oo Specification.theorems_cmd Thm.theoremK facts fixes));

val _ =
  Outer_Syntax.local_theory' @{command_keyword declare} "declare theorems"
    (Parse.and_list1 Parse.thms1 -- Parse.for_fixes
      >> (fn (facts, fixes) =>
          #2 oo Specification.theorems_cmd "" [(Binding.empty_atts, flat facts)] fixes));

val _ =
  Outer_Syntax.local_theory @{command_keyword named_theorems}
    "declare named collection of theorems"
    (Parse.and_list1 (Parse.binding -- Scan.optional Parse.text "") >>
      fold (fn (b, descr) => snd o Named_Theorems.declare b descr));

in end\<close>


subsubsection \<open>Hide names\<close>

ML \<open>
local

fun hide_names command_keyword what hide parse prep =
  Outer_Syntax.command command_keyword ("hide " ^ what ^ " from name space")
    ((Parse.opt_keyword "open" >> not) -- Scan.repeat1 parse >> (fn (fully, args) =>
      (Toplevel.theory (fn thy =>
        let val ctxt = Proof_Context.init_global thy
        in fold (hide fully o prep ctxt) args thy end))));

val _ =
  hide_names @{command_keyword hide_class} "classes" Sign.hide_class Parse.class
    Proof_Context.read_class;

val _ =
  hide_names @{command_keyword hide_type} "types" Sign.hide_type Parse.type_const
    ((#1 o dest_Type) oo Proof_Context.read_type_name {proper = true, strict = false});

val _ =
  hide_names @{command_keyword hide_const} "consts" Sign.hide_const Parse.const
    ((#1 o dest_Const) oo Proof_Context.read_const {proper = true, strict = false});

val _ =
  hide_names @{command_keyword hide_fact} "facts" Global_Theory.hide_fact
    (Parse.position Parse.name) (Global_Theory.check_fact o Proof_Context.theory_of);

in end\<close>


subsection \<open>Bundled declarations\<close>

ML \<open>
local

val _ =
  Outer_Syntax.maybe_begin_local_theory @{command_keyword bundle}
    "define bundle of declarations"
    ((Parse.binding --| @{keyword "="}) -- Parse.thms1 -- Parse.for_fixes
      >> (uncurry Bundle.bundle_cmd))
    (Parse.binding --| Parse.begin >> Bundle.init);

val _ =
  Outer_Syntax.local_theory @{command_keyword unbundle}
    "activate declarations from bundle in local theory"
    (Scan.repeat1 (Parse.position Parse.name) >> Bundle.unbundle_cmd);

val _ =
  Outer_Syntax.command @{command_keyword include}
    "activate declarations from bundle in proof body"
    (Scan.repeat1 (Parse.position Parse.name) >> (Toplevel.proof o Bundle.include_cmd));

val _ =
  Outer_Syntax.command @{command_keyword including}
    "activate declarations from bundle in goal refinement"
    (Scan.repeat1 (Parse.position Parse.name) >> (Toplevel.proof o Bundle.including_cmd));

val _ =
  Outer_Syntax.command @{command_keyword print_bundles}
    "print bundles of declarations"
    (Parse.opt_bang >> (fn b => Toplevel.keep (Bundle.print_bundles b o Toplevel.context_of)));

in end\<close>


subsection \<open>Local theory specifications\<close>

subsubsection \<open>Specification context\<close>

ML \<open>
local

val _ =
  Outer_Syntax.command @{command_keyword context} "begin local theory context"
    ((Parse.position Parse.name >> (fn name =>
        Toplevel.begin_local_theory true (Named_Target.begin name)) ||
      Scan.optional Parse_Spec.includes [] -- Scan.repeat Parse_Spec.context_element
        >> (fn (incls, elems) => Toplevel.open_target (#2 o Bundle.context_cmd incls elems)))
      --| Parse.begin);

val _ =
  Outer_Syntax.command @{command_keyword end} "end context"
    (Scan.succeed
      (Toplevel.exit o Toplevel.end_local_theory o Toplevel.close_target o
        Toplevel.end_proof (K Proof.end_notepad)));

in end\<close>


subsubsection \<open>Locales and interpretation\<close>

ML \<open>
local

val locale_val =
  Parse_Spec.locale_expression --
    Scan.optional (@{keyword "+"} |-- Parse.!!! (Scan.repeat1 Parse_Spec.context_element)) [] ||
  Scan.repeat1 Parse_Spec.context_element >> pair ([], []);

val _ =
  Outer_Syntax.command @{command_keyword locale} "define named specification context"
    (Parse.binding --
      Scan.optional (@{keyword "="} |-- Parse.!!! locale_val) (([], []), []) -- Parse.opt_begin
      >> (fn ((name, (expr, elems)), begin) =>
          Toplevel.begin_local_theory begin
            (Expression.add_locale_cmd name Binding.empty expr elems #> snd)));

val _ =
  Outer_Syntax.command @{command_keyword experiment} "open private specification context"
    (Scan.repeat Parse_Spec.context_element --| Parse.begin
      >> (fn elems =>
          Toplevel.begin_local_theory true (Experiment.experiment_cmd elems #> snd)));

val interpretation_args =
  Parse.!!! Parse_Spec.locale_expression --
    Scan.optional
      (@{keyword "rewrites"} |-- Parse.and_list1 (Parse_Spec.opt_thm_name ":" -- Parse.prop)) [];

val _ =
  Outer_Syntax.command @{command_keyword interpret}
    "prove interpretation of locale expression in proof context"
    (interpretation_args >> (fn (expr, equations) =>
      Toplevel.proof (Interpretation.interpret_cmd expr equations)));

val interpretation_args_with_defs =
  Parse.!!! Parse_Spec.locale_expression --
    (Scan.optional (@{keyword "defines"} |-- Parse.and_list1 (Parse_Spec.opt_thm_name ":"
      -- ((Parse.binding -- Parse.opt_mixfix') --| @{keyword "="} -- Parse.term))) [] --
    Scan.optional
      (@{keyword "rewrites"} |-- Parse.and_list1 (Parse_Spec.opt_thm_name ":" -- Parse.prop)) []);

val _ =
  Outer_Syntax.local_theory_to_proof @{command_keyword global_interpretation}
    "prove interpretation of locale expression into global theory"
    (interpretation_args_with_defs >> (fn (expr, (defs, equations)) =>
      Interpretation.global_interpretation_cmd expr defs equations));

val _ =
  Outer_Syntax.command @{command_keyword sublocale}
    "prove sublocale relation between a locale and a locale expression"
    ((Parse.position Parse.name --| (@{keyword "\<subseteq>"} || @{keyword "<"}) --
      interpretation_args_with_defs >> (fn (loc, (expr, (defs, equations))) =>
        Toplevel.theory_to_proof (Interpretation.global_sublocale_cmd loc expr defs equations)))
    || interpretation_args_with_defs >> (fn (expr, (defs, equations)) =>
        Toplevel.local_theory_to_proof NONE NONE (Interpretation.sublocale_cmd expr defs equations)));

val _ =
  Outer_Syntax.command @{command_keyword interpretation}
    "prove interpretation of locale expression in local theory or into global theory"
    (interpretation_args >> (fn (expr, equations) =>
      Toplevel.local_theory_to_proof NONE NONE
        (Interpretation.isar_interpretation_cmd expr equations)));

in end\<close>


subsubsection \<open>Type classes\<close>

ML \<open>
local

val class_val =
  Parse_Spec.class_expression --
    Scan.optional (@{keyword "+"} |-- Parse.!!! (Scan.repeat1 Parse_Spec.context_element)) [] ||
  Scan.repeat1 Parse_Spec.context_element >> pair [];

val _ =
  Outer_Syntax.command @{command_keyword class} "define type class"
   (Parse.binding -- Scan.optional (@{keyword "="} |-- class_val) ([], []) -- Parse.opt_begin
    >> (fn ((name, (supclasses, elems)), begin) =>
        Toplevel.begin_local_theory begin
          (Class_Declaration.class_cmd name supclasses elems #> snd)));

val _ =
  Outer_Syntax.local_theory_to_proof @{command_keyword subclass} "prove a subclass relation"
    (Parse.class >> Class_Declaration.subclass_cmd);

val _ =
  Outer_Syntax.command @{command_keyword instantiation} "instantiate and prove type arity"
   (Parse.multi_arity --| Parse.begin
     >> (fn arities => Toplevel.begin_local_theory true (Class.instantiation_cmd arities)));

val _ =
  Outer_Syntax.command @{command_keyword instance} "prove type arity or subclass relation"
  ((Parse.class --
    ((@{keyword "\<subseteq>"} || @{keyword "<"}) |-- Parse.!!! Parse.class) >> Class.classrel_cmd ||
    Parse.multi_arity >> Class.instance_arity_cmd) >> Toplevel.theory_to_proof ||
    Scan.succeed (Toplevel.local_theory_to_proof NONE NONE (Class.instantiation_instance I)));

in end\<close>


subsubsection \<open>Arbitrary overloading\<close>

ML \<open>
local

val _ =
  Outer_Syntax.command @{command_keyword overloading} "overloaded definitions"
   (Scan.repeat1 (Parse.name --| (@{keyword "=="} || @{keyword "\<equiv>"}) -- Parse.term --
      Scan.optional (@{keyword "("} |-- (@{keyword "unchecked"} >> K false) --| @{keyword ")"}) true
      >> Scan.triple1) --| Parse.begin
   >> (fn operations => Toplevel.begin_local_theory true (Overloading.overloading_cmd operations)));

in end\<close>


subsection \<open>Proof commands\<close>

ML \<open>
local

val _ =
  Outer_Syntax.local_theory_to_proof @{command_keyword notepad} "begin proof context"
    (Parse.begin >> K Proof.begin_notepad);

in end\<close>


subsubsection \<open>Statements\<close>

ML \<open>
local

val structured_statement =
  Parse_Spec.statement -- Parse_Spec.cond_statement -- Parse.for_fixes
    >> (fn ((shows, (strict, assumes)), fixes) => (strict, fixes, assumes, shows));

val _ =
  Outer_Syntax.command @{command_keyword have} "state local goal"
    (structured_statement >> (fn (a, b, c, d) =>
      Toplevel.proof' (fn int => Proof.have_cmd a NONE (K I) b c d int #> #2)));

val _ =
  Outer_Syntax.command @{command_keyword show} "state local goal, to refine pending subgoals"
    (structured_statement >> (fn (a, b, c, d) =>
      Toplevel.proof' (fn int => Proof.show_cmd a NONE (K I) b c d int #> #2)));

val _ =
  Outer_Syntax.command @{command_keyword hence} "old-style alias of \"then have\""
    (structured_statement >> (fn (a, b, c, d) =>
      Toplevel.proof' (fn int => Proof.chain #> Proof.have_cmd a NONE (K I) b c d int #> #2)));

val _ =
  Outer_Syntax.command @{command_keyword thus} "old-style alias of  \"then show\""
    (structured_statement >> (fn (a, b, c, d) =>
      Toplevel.proof' (fn int => Proof.chain #> Proof.show_cmd a NONE (K I) b c d int #> #2)));

in end\<close>


subsubsection \<open>Local facts\<close>

ML \<open>
local

val facts = Parse.and_list1 Parse.thms1;

val _ =
  Outer_Syntax.command @{command_keyword then} "forward chaining"
    (Scan.succeed (Toplevel.proof Proof.chain));

val _ =
  Outer_Syntax.command @{command_keyword from} "forward chaining from given facts"
    (facts >> (Toplevel.proof o Proof.from_thmss_cmd));

val _ =
  Outer_Syntax.command @{command_keyword with} "forward chaining from given and current facts"
    (facts >> (Toplevel.proof o Proof.with_thmss_cmd));

val _ =
  Outer_Syntax.command @{command_keyword note} "define facts"
    (Parse_Spec.name_facts >> (Toplevel.proof o Proof.note_thmss_cmd));

val _ =
  Outer_Syntax.command @{command_keyword supply} "define facts during goal refinement (unstructured)"
    (Parse_Spec.name_facts >> (Toplevel.proof o Proof.supply_cmd));

val _ =
  Outer_Syntax.command @{command_keyword using} "augment goal facts"
    (facts >> (Toplevel.proof o Proof.using_cmd));

val _ =
  Outer_Syntax.command @{command_keyword unfolding} "unfold definitions in goal and facts"
    (facts >> (Toplevel.proof o Proof.unfolding_cmd));

in end\<close>


subsubsection \<open>Proof context\<close>

ML \<open>
local

val structured_statement =
  Parse_Spec.statement -- Parse_Spec.if_statement' -- Parse.for_fixes
    >> (fn ((shows, assumes), fixes) => (fixes, assumes, shows));

val _ =
  Outer_Syntax.command @{command_keyword fix} "fix local variables (Skolem constants)"
    (Parse.vars >> (Toplevel.proof o Proof.fix_cmd));

val _ =
  Outer_Syntax.command @{command_keyword assume} "assume propositions"
    (structured_statement >> (fn (a, b, c) => Toplevel.proof (Proof.assume_cmd a b c)));

val _ =
  Outer_Syntax.command @{command_keyword presume} "assume propositions, to be established later"
    (structured_statement >> (fn (a, b, c) => Toplevel.proof (Proof.presume_cmd a b c)));

val _ =
  Outer_Syntax.command @{command_keyword define} "local definition (non-polymorphic)"
    ((Parse.vars --| Parse.where_) -- Parse_Spec.statement -- Parse.for_fixes
      >> (fn ((a, b), c) => Toplevel.proof (Proof.define_cmd a c b)));

val _ =
  Outer_Syntax.command @{command_keyword def} "local definition (non-polymorphic)"
    (Parse.and_list1
      (Parse_Spec.opt_thm_name ":" --
        ((Parse.binding -- Parse.opt_mixfix) --
          ((@{keyword "\<equiv>"} || @{keyword "=="}) |-- Parse.!!! Parse.termp)))
    >> (fn args => Toplevel.proof (fn state =>
        (legacy_feature "Old 'def' command -- use 'define' instead"; Proof.def_cmd args state))));

val _ =
  Outer_Syntax.command @{command_keyword consider} "state cases rule"
    (Parse_Spec.obtains >> (Toplevel.proof' o Obtain.consider_cmd));

val _ =
  Outer_Syntax.command @{command_keyword obtain} "generalized elimination"
    (Parse.parbinding -- Scan.optional (Parse.vars --| Parse.where_) [] -- structured_statement
      >> (fn ((a, b), (c, d, e)) => Toplevel.proof' (Obtain.obtain_cmd a b c d e)));

val _ =
  Outer_Syntax.command @{command_keyword guess} "wild guessing (unstructured)"
    (Scan.optional Parse.vars [] >> (Toplevel.proof' o Obtain.guess_cmd));

val _ =
  Outer_Syntax.command @{command_keyword let} "bind text variables"
    (Parse.and_list1 (Parse.and_list1 Parse.term -- (@{keyword "="} |-- Parse.term))
      >> (Toplevel.proof o Proof.let_bind_cmd));

val _ =
  Outer_Syntax.command @{command_keyword write} "add concrete syntax for constants / fixed variables"
    (Parse.syntax_mode -- Parse.and_list1 (Parse.const -- Parse.mixfix)
    >> (fn (mode, args) => Toplevel.proof (Proof.write_cmd mode args)));

val _ =
  Outer_Syntax.command @{command_keyword case} "invoke local context"
    (Parse_Spec.opt_thm_name ":" --
      (@{keyword "("} |--
        Parse.!!! (Parse.position Parse.name -- Scan.repeat (Parse.maybe Parse.binding)
          --| @{keyword ")"}) ||
        Parse.position Parse.name >> rpair []) >> (Toplevel.proof o Proof.case_cmd));

in end\<close>


subsubsection \<open>Proof structure\<close>

ML \<open>
local

val _ =
  Outer_Syntax.command @{command_keyword "{"} "begin explicit proof block"
    (Scan.succeed (Toplevel.proof Proof.begin_block));

val _ =
  Outer_Syntax.command @{command_keyword "}"} "end explicit proof block"
    (Scan.succeed (Toplevel.proof Proof.end_block));

val _ =
  Outer_Syntax.command @{command_keyword next} "enter next proof block"
    (Scan.succeed (Toplevel.proof Proof.next_block));

in end\<close>


subsubsection \<open>End proof\<close>

ML \<open>
local

val _ =
  Outer_Syntax.command @{command_keyword qed} "conclude proof"
    (Scan.option Method.parse >> (fn m =>
     (Option.map Method.report m;
      Isar_Cmd.qed m)));

val _ =
  Outer_Syntax.command @{command_keyword by} "terminal backward proof"
    (Method.parse -- Scan.option Method.parse >> (fn (m1, m2) =>
     (Method.report m1;
      Option.map Method.report m2;
      Isar_Cmd.terminal_proof (m1, m2))));

val _ =
  Outer_Syntax.command @{command_keyword ".."} "default proof"
    (Scan.succeed Isar_Cmd.default_proof);

val _ =
  Outer_Syntax.command @{command_keyword "."} "immediate proof"
    (Scan.succeed Isar_Cmd.immediate_proof);

val _ =
  Outer_Syntax.command @{command_keyword done} "done proof"
    (Scan.succeed Isar_Cmd.done_proof);

val _ =
  Outer_Syntax.command @{command_keyword sorry} "skip proof (quick-and-dirty mode only!)"
    (Scan.succeed Isar_Cmd.skip_proof);

val _ =
  Outer_Syntax.command @{command_keyword \<proof>} "dummy proof (quick-and-dirty mode only!)"
    (Scan.succeed Isar_Cmd.skip_proof);

val _ =
  Outer_Syntax.command @{command_keyword oops} "forget proof"
    (Scan.succeed (Toplevel.forget_proof true));

in end\<close>


subsubsection \<open>Proof steps\<close>

ML \<open>
local

val _ =
  Outer_Syntax.command @{command_keyword defer} "shuffle internal proof state"
    (Scan.optional Parse.nat 1 >> (Toplevel.proof o Proof.defer));

val _ =
  Outer_Syntax.command @{command_keyword prefer} "shuffle internal proof state"
    (Parse.nat >> (Toplevel.proof o Proof.prefer));

val _ =
  Outer_Syntax.command @{command_keyword apply} "initial goal refinement step (unstructured)"
    (Method.parse >> (fn m => (Method.report m; Toplevel.proofs (Proof.apply m))));

val _ =
  Outer_Syntax.command @{command_keyword apply_end} "terminal goal refinement step (unstructured)"
    (Method.parse >> (fn m => (Method.report m; Toplevel.proofs (Proof.apply_end m))));

val _ =
  Outer_Syntax.command @{command_keyword proof} "backward proof step"
    (Scan.option Method.parse >> (fn m =>
      (Option.map Method.report m;
       Toplevel.proof (fn state =>
         let
          val state' = state |> Proof.proof m |> Seq.the_result "";
          val _ =
            Output.information
              (Proof_Context.print_cases_proof (Proof.context_of state) (Proof.context_of state'));
        in state' end))))

in end\<close>


subsubsection \<open>Subgoal focus\<close>

ML \<open>
local

val opt_fact_binding =
  Scan.optional (Parse.binding -- Parse.opt_attribs || Parse.attribs >> pair Binding.empty)
    Binding.empty_atts;

val for_params =
  Scan.optional
    (@{keyword "for"} |--
      Parse.!!! ((Scan.option Parse.dots >> is_some) --
        (Scan.repeat1 (Parse.position (Parse.maybe Parse.name)))))
    (false, []);

val _ =
  Outer_Syntax.command @{command_keyword subgoal}
    "focus on first subgoal within backward refinement"
    (opt_fact_binding -- (Scan.option (@{keyword "premises"} |-- Parse.!!! opt_fact_binding)) --
      for_params >> (fn ((a, b), c) =>
        Toplevel.proofs (Seq.make_results o Seq.single o #2 o Subgoal.subgoal_cmd a b c)));

in end\<close>


subsubsection \<open>Calculation\<close>

ML \<open>
local

val calculation_args =
  Scan.option (@{keyword "("} |-- Parse.!!! ((Parse.thms1 --| @{keyword ")"})));

val _ =
  Outer_Syntax.command @{command_keyword also} "combine calculation and current facts"
    (calculation_args >> (Toplevel.proofs' o Calculation.also_cmd));

val _ =
  Outer_Syntax.command @{command_keyword finally}
    "combine calculation and current facts, exhibit result"
    (calculation_args >> (Toplevel.proofs' o Calculation.finally_cmd));

val _ =
  Outer_Syntax.command @{command_keyword moreover} "augment calculation by current facts"
    (Scan.succeed (Toplevel.proof' Calculation.moreover));

val _ =
  Outer_Syntax.command @{command_keyword ultimately}
    "augment calculation by current facts, exhibit result"
    (Scan.succeed (Toplevel.proof' Calculation.ultimately));

val _ =
  Outer_Syntax.command @{command_keyword print_trans_rules} "print transitivity rules"
    (Scan.succeed (Toplevel.keep (Calculation.print_rules o Toplevel.context_of)));

in end\<close>


subsubsection \<open>Proof navigation\<close>

ML \<open>
local

fun report_back () =
  Output.report [Markup.markup Markup.bad "Explicit backtracking"];

val _ =
  Outer_Syntax.command @{command_keyword back} "explicit backtracking of proof command"
    (Scan.succeed
     (Toplevel.actual_proof (fn prf => (report_back (); Proof_Node.back prf)) o
      Toplevel.skip_proof report_back));

in end\<close>


subsection \<open>Diagnostic commands (for interactive mode only)\<close>

ML \<open>
local

val opt_modes =
  Scan.optional (@{keyword "("} |-- Parse.!!! (Scan.repeat1 Parse.name --| @{keyword ")"})) [];

val _ =
  Outer_Syntax.command @{command_keyword help}
    "retrieve outer syntax commands according to name patterns"
    (Scan.repeat Parse.name >>
      (fn pats => Toplevel.keep (fn st => Outer_Syntax.help (Toplevel.theory_of st) pats)));

val _ =
  Outer_Syntax.command @{command_keyword print_commands} "print outer syntax commands"
    (Scan.succeed (Toplevel.keep (Outer_Syntax.print_commands o Toplevel.theory_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_options} "print configuration options"
    (Parse.opt_bang >> (fn b => Toplevel.keep (Attrib.print_options b o Toplevel.context_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_context}
    "print context of local theory target"
    (Scan.succeed (Toplevel.keep (Pretty.writeln_chunks o Toplevel.pretty_context)));

val _ =
  Outer_Syntax.command @{command_keyword print_theory}
    "print logical theory contents"
    (Parse.opt_bang >> (fn b =>
      Toplevel.keep (Pretty.writeln o Proof_Display.pretty_theory b o Toplevel.context_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_definitions}
    "print dependencies of definitional theory content"
    (Parse.opt_bang >> (fn b =>
      Toplevel.keep (Pretty.writeln o Proof_Display.pretty_definitions b o Toplevel.context_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_syntax}
    "print inner syntax of context"
    (Scan.succeed (Toplevel.keep (Proof_Context.print_syntax o Toplevel.context_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_defn_rules}
    "print definitional rewrite rules of context"
    (Scan.succeed (Toplevel.keep (Local_Defs.print_rules o Toplevel.context_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_abbrevs}
    "print constant abbreviations of context"
    (Parse.opt_bang >> (fn b =>
      Toplevel.keep (Proof_Context.print_abbrevs b o Toplevel.context_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_theorems}
    "print theorems of local theory or proof context"
    (Parse.opt_bang >> (fn b =>
      Toplevel.keep (Pretty.writeln o Pretty.chunks o Isar_Cmd.pretty_theorems b)));

val _ =
  Outer_Syntax.command @{command_keyword print_locales}
    "print locales of this theory"
    (Parse.opt_bang >> (fn b =>
      Toplevel.keep (Locale.print_locales b o Toplevel.theory_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_classes}
    "print classes of this theory"
    (Scan.succeed (Toplevel.keep (Class.print_classes o Toplevel.context_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_locale}
    "print locale of this theory"
    (Parse.opt_bang -- Parse.position Parse.name >> (fn (b, name) =>
      Toplevel.keep (fn state => Locale.print_locale (Toplevel.theory_of state) b name)));

val _ =
  Outer_Syntax.command @{command_keyword print_interps}
    "print interpretations of locale for this theory or proof context"
    (Parse.position Parse.name >> (fn name =>
      Toplevel.keep (fn state => Locale.print_registrations (Toplevel.context_of state) name)));

val _ =
  Outer_Syntax.command @{command_keyword print_dependencies}
    "print dependencies of locale expression"
    (Parse.opt_bang -- Parse_Spec.locale_expression >> (fn (b, expr) =>
      Toplevel.keep (fn state => Expression.print_dependencies (Toplevel.context_of state) b expr)));

val _ =
  Outer_Syntax.command @{command_keyword print_attributes}
    "print attributes of this theory"
    (Parse.opt_bang >> (fn b => Toplevel.keep (Attrib.print_attributes b o Toplevel.context_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_simpset}
    "print context of Simplifier"
    (Parse.opt_bang >> (fn b =>
      Toplevel.keep (Pretty.writeln o Simplifier.pretty_simpset b o Toplevel.context_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_rules} "print intro/elim rules"
    (Scan.succeed (Toplevel.keep (Context_Rules.print_rules o Toplevel.context_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_methods} "print methods of this theory"
    (Parse.opt_bang >> (fn b => Toplevel.keep (Method.print_methods b o Toplevel.context_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_antiquotations}
    "print document antiquotations"
    (Parse.opt_bang >> (fn b =>
      Toplevel.keep (Thy_Output.print_antiquotations b o Toplevel.context_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_ML_antiquotations}
    "print ML antiquotations"
    (Parse.opt_bang >> (fn b =>
      Toplevel.keep (ML_Context.print_antiquotations b o Toplevel.context_of)));

val _ =
  Outer_Syntax.command @{command_keyword locale_deps} "visualize locale dependencies"
    (Scan.succeed
      (Toplevel.keep (Toplevel.theory_of #> (fn thy =>
        Locale.pretty_locale_deps thy
        |> map (fn {name, parents, body} =>
          ((name, Graph_Display.content_node (Locale.extern thy name) [body]), parents))
        |> Graph_Display.display_graph_old))));

val _ =
  Outer_Syntax.command @{command_keyword print_term_bindings}
    "print term bindings of proof context"
    (Scan.succeed
      (Toplevel.keep
        (Pretty.writeln_chunks o Proof_Context.pretty_term_bindings o Toplevel.context_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_facts} "print facts of proof context"
    (Parse.opt_bang >> (fn b =>
      Toplevel.keep (Proof_Context.print_local_facts b o Toplevel.context_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_cases} "print cases of proof context"
    (Scan.succeed
      (Toplevel.keep (Pretty.writeln_chunks o Proof_Context.pretty_cases o Toplevel.context_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_statement}
    "print theorems as long statements"
    (opt_modes -- Parse.thms1 >> Isar_Cmd.print_stmts);

val _ =
  Outer_Syntax.command @{command_keyword thm} "print theorems"
    (opt_modes -- Parse.thms1 >> Isar_Cmd.print_thms);

val _ =
  Outer_Syntax.command @{command_keyword prf} "print proof terms of theorems"
    (opt_modes -- Scan.option Parse.thms1 >> Isar_Cmd.print_prfs false);

val _ =
  Outer_Syntax.command @{command_keyword full_prf} "print full proof terms of theorems"
    (opt_modes -- Scan.option Parse.thms1 >> Isar_Cmd.print_prfs true);

val _ =
  Outer_Syntax.command @{command_keyword prop} "read and print proposition"
    (opt_modes -- Parse.term >> Isar_Cmd.print_prop);

val _ =
  Outer_Syntax.command @{command_keyword term} "read and print term"
    (opt_modes -- Parse.term >> Isar_Cmd.print_term);

val _ =
  Outer_Syntax.command @{command_keyword typ} "read and print type"
    (opt_modes -- (Parse.typ -- Scan.option (@{keyword "::"} |-- Parse.!!! Parse.sort))
      >> Isar_Cmd.print_type);

val _ =
  Outer_Syntax.command @{command_keyword print_codesetup} "print code generator setup"
    (Scan.succeed (Toplevel.keep (Code.print_codesetup o Toplevel.theory_of)));

val _ =
  Outer_Syntax.command @{command_keyword print_state}
    "print current proof state (if present)"
    (opt_modes >> (fn modes =>
      Toplevel.keep (Print_Mode.with_modes modes (Output.state o Toplevel.string_of_state))));

val _ =
  Outer_Syntax.command @{command_keyword welcome} "print welcome message"
    (Scan.succeed (Toplevel.keep (fn _ => writeln (Session.welcome ()))));

val _ =
  Outer_Syntax.command @{command_keyword display_drafts}
    "display raw source files with symbols"
    (Scan.repeat1 Parse.path >> (fn names =>
      Toplevel.keep (fn _ => ignore (Present.display_drafts (map Path.explode names)))));

in end\<close>


subsection \<open>Dependencies\<close>

ML \<open>
local

val theory_bounds =
  Parse.position Parse.theory_name >> single ||
  (@{keyword "("} |-- Parse.enum "|" (Parse.position Parse.theory_name) --| @{keyword ")"});

val _ =
  Outer_Syntax.command @{command_keyword thy_deps} "visualize theory dependencies"
    (Scan.option theory_bounds -- Scan.option theory_bounds >>
      (fn args => Toplevel.keep (fn st => Thy_Deps.thy_deps_cmd (Toplevel.context_of st) args)));


val class_bounds =
  Parse.sort >> single ||
  (@{keyword "("} |-- Parse.enum "|" Parse.sort --| @{keyword ")"});

val _ =
  Outer_Syntax.command @{command_keyword class_deps} "visualize class dependencies"
    (Scan.option class_bounds -- Scan.option class_bounds >> (fn args =>
      Toplevel.keep (fn st => Class_Deps.class_deps_cmd (Toplevel.context_of st) args)));


val _ =
  Outer_Syntax.command @{command_keyword thm_deps} "visualize theorem dependencies"
    (Parse.thms1 >> (fn args =>
      Toplevel.keep (fn st =>
        Thm_Deps.thm_deps (Toplevel.theory_of st)
          (Attrib.eval_thms (Toplevel.context_of st) args))));


val thy_names =
  Scan.repeat1 (Scan.unless Parse.minus (Parse.position Parse.theory_name));

val _ =
  Outer_Syntax.command @{command_keyword unused_thms} "find unused theorems"
    (Scan.option ((thy_names --| Parse.minus) -- Scan.option thy_names) >> (fn opt_range =>
        Toplevel.keep (fn st =>
          let
            val thy = Toplevel.theory_of st;
            val ctxt = Toplevel.context_of st;
            fun pretty_thm (a, th) = Proof_Context.pretty_fact ctxt (a, [th]);
            val check = Theory.check ctxt;
          in
            Thm_Deps.unused_thms
              (case opt_range of
                NONE => (Theory.parents_of thy, [thy])
              | SOME (xs, NONE) => (map check xs, [thy])
              | SOME (xs, SOME ys) => (map check xs, map check ys))
            |> map pretty_thm |> Pretty.writeln_chunks
          end)));

in end\<close>


subsubsection \<open>Find consts and theorems\<close>

ML \<open>
local

val _ =
  Outer_Syntax.command @{command_keyword find_consts}
    "find constants by name / type patterns"
    (Find_Consts.query_parser >> (fn spec =>
      Toplevel.keep (fn st =>
        Pretty.writeln (Find_Consts.pretty_consts (Toplevel.context_of st) spec))));

val options =
  Scan.optional
    (Parse.$$$ "(" |--
      Parse.!!! (Scan.option Parse.nat --
        Scan.optional (Parse.reserved "with_dups" >> K false) true --| Parse.$$$ ")"))
    (NONE, true);

val _ =
  Outer_Syntax.command @{command_keyword find_theorems}
    "find theorems meeting specified criteria"
    (options -- Find_Theorems.query_parser >> (fn ((opt_lim, rem_dups), spec) =>
      Toplevel.keep (fn st =>
        Pretty.writeln
          (Find_Theorems.pretty_theorems (Find_Theorems.proof_state st) opt_lim rem_dups spec))));

in end\<close>


subsection \<open>Code generation\<close>

ML \<open>
local

val _ =
  Outer_Syntax.command @{command_keyword code_datatype}
    "define set of code datatype constructors"
    (Scan.repeat1 Parse.term >> (Toplevel.theory o Code.add_datatype_cmd));

in end\<close>


subsection \<open>Extraction of programs from proofs\<close>

ML \<open>
local

val parse_vars = Scan.optional (Parse.$$$ "(" |-- Parse.list1 Parse.name --| Parse.$$$ ")") [];

val _ =
  Outer_Syntax.command @{command_keyword realizers}
    "specify realizers for primitive axioms / theorems, together with correctness proof"
    (Scan.repeat1 (Parse.name -- parse_vars --| Parse.$$$ ":" -- Parse.string -- Parse.string) >>
     (fn xs => Toplevel.theory (fn thy => Extraction.add_realizers
       (map (fn (((a, vs), s1), s2) => (Global_Theory.get_thm thy a, (vs, s1, s2))) xs) thy)));

val _ =
  Outer_Syntax.command @{command_keyword realizability}
    "add equations characterizing realizability"
    (Scan.repeat1 Parse.string >> (Toplevel.theory o Extraction.add_realizes_eqns));

val _ =
  Outer_Syntax.command @{command_keyword extract_type}
    "add equations characterizing type of extracted program"
    (Scan.repeat1 Parse.string >> (Toplevel.theory o Extraction.add_typeof_eqns));

val _ =
  Outer_Syntax.command @{command_keyword extract} "extract terms from proofs"
    (Scan.repeat1 (Parse.name -- parse_vars) >> (fn xs => Toplevel.theory (fn thy =>
      Extraction.extract (map (apfst (Global_Theory.get_thm thy)) xs) thy)));

in end\<close>


section \<open>Auxiliary lemmas\<close>

subsection \<open>Meta-level connectives in assumptions\<close>

lemma meta_mp:
  assumes "PROP P \<Longrightarrow> PROP Q" and "PROP P"
  shows "PROP Q"
    by (rule \<open>PROP P \<Longrightarrow> PROP Q\<close> [OF \<open>PROP P\<close>])

lemmas meta_impE = meta_mp [elim_format]

lemma meta_spec:
  assumes "\<And>x. PROP P x"
  shows "PROP P x"
    by (rule \<open>\<And>x. PROP P x\<close>)

lemmas meta_allE = meta_spec [elim_format]

lemma swap_params:
  "(\<And>x y. PROP P x y) \<equiv> (\<And>y x. PROP P x y)" ..


subsection \<open>Meta-level conjunction\<close>

lemma all_conjunction:
  "(\<And>x. PROP A x &&& PROP B x) \<equiv> ((\<And>x. PROP A x) &&& (\<And>x. PROP B x))"
proof
  assume conj: "\<And>x. PROP A x &&& PROP B x"
  show "(\<And>x. PROP A x) &&& (\<And>x. PROP B x)"
  proof -
    fix x
    from conj show "PROP A x" by (rule conjunctionD1)
    from conj show "PROP B x" by (rule conjunctionD2)
  qed
next
  assume conj: "(\<And>x. PROP A x) &&& (\<And>x. PROP B x)"
  fix x
  show "PROP A x &&& PROP B x"
  proof -
    show "PROP A x" by (rule conj [THEN conjunctionD1, rule_format])
    show "PROP B x" by (rule conj [THEN conjunctionD2, rule_format])
  qed
qed

lemma imp_conjunction:
  "(PROP A \<Longrightarrow> PROP B &&& PROP C) \<equiv> ((PROP A \<Longrightarrow> PROP B) &&& (PROP A \<Longrightarrow> PROP C))"
proof
  assume conj: "PROP A \<Longrightarrow> PROP B &&& PROP C"
  show "(PROP A \<Longrightarrow> PROP B) &&& (PROP A \<Longrightarrow> PROP C)"
  proof -
    assume "PROP A"
    from conj [OF \<open>PROP A\<close>] show "PROP B" by (rule conjunctionD1)
    from conj [OF \<open>PROP A\<close>] show "PROP C" by (rule conjunctionD2)
  qed
next
  assume conj: "(PROP A \<Longrightarrow> PROP B) &&& (PROP A \<Longrightarrow> PROP C)"
  assume "PROP A"
  show "PROP B &&& PROP C"
  proof -
    from \<open>PROP A\<close> show "PROP B" by (rule conj [THEN conjunctionD1])
    from \<open>PROP A\<close> show "PROP C" by (rule conj [THEN conjunctionD2])
  qed
qed

lemma conjunction_imp:
  "(PROP A &&& PROP B \<Longrightarrow> PROP C) \<equiv> (PROP A \<Longrightarrow> PROP B \<Longrightarrow> PROP C)"
proof
  assume r: "PROP A &&& PROP B \<Longrightarrow> PROP C"
  assume ab: "PROP A" "PROP B"
  show "PROP C"
  proof (rule r)
    from ab show "PROP A &&& PROP B" .
  qed
next
  assume r: "PROP A \<Longrightarrow> PROP B \<Longrightarrow> PROP C"
  assume conj: "PROP A &&& PROP B"
  show "PROP C"
  proof (rule r)
    from conj show "PROP A" by (rule conjunctionD1)
    from conj show "PROP B" by (rule conjunctionD2)
  qed
qed

end
