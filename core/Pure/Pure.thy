(*  Title:      Pure/Pure.thy
    Author:     Makarius

Final stage of bootstrapping Pure, based on implicit background theory.
*)

theory Pure
  keywords
    "!!" "!" "%" "(" ")" "+" "," "--" ":" "::" ";" "<" "<=" "=" "=="
    "=>" "?" "[" "\<equiv>" "\<leftharpoondown>" "\<rightharpoonup>"
    "\<rightleftharpoons>" "\<subseteq>" "]" "and" "assumes"
    "attach" "begin" "binder" "constrains" "defines" "fixes" "for"
    "identifier" "if" "imports" "in" "includes" "infix" "infixl"
    "infixr" "is" "keywords" "notes" "obtains" "open" "output"
    "overloaded" "pervasive" "shows" "structure" "unchecked" "where" "|"
  and "theory" :: thy_begin % "theory"
  and "header" :: diag
  and "chapter" :: thy_heading1
  and "section" :: thy_heading2
  and "subsection" :: thy_heading3
  and "subsubsection" :: thy_heading4
  and "text" "text_raw" :: thy_decl
  and "sect" :: prf_heading2 % "proof"
  and "subsect" :: prf_heading3 % "proof"
  and "subsubsect" :: prf_heading4 % "proof"
  and "txt" "txt_raw" :: prf_decl % "proof"
  and "default_sort" :: thy_decl == ""
  and "typedecl" "type_synonym" "nonterminal" "judgment"
    "consts" "syntax" "no_syntax" "translations" "no_translations" "defs"
    "definition" "abbreviation" "type_notation" "no_type_notation" "notation"
    "no_notation" "axiomatization" "theorems" "lemmas" "declare"
    "hide_class" "hide_type" "hide_const" "hide_fact" :: thy_decl
  and "SML_file" "ML_file" :: thy_load % "ML"
  and "SML_import" "SML_export" :: thy_decl % "ML"
  and "ML" :: thy_decl % "ML"
  and "ML_prf" :: prf_decl % "proof"  (* FIXME % "ML" ?? *)
  and "ML_val" "ML_command" :: diag % "ML"
  and "simproc_setup" :: thy_decl % "ML" == ""
  and "setup" "local_setup" "attribute_setup" "method_setup"
    "declaration" "syntax_declaration"
    "parse_ast_translation" "parse_translation" "print_translation"
    "typed_print_translation" "print_ast_translation" "oracle" :: thy_decl % "ML"
  and "bundle" :: thy_decl
  and "include" "including" :: prf_decl
  and "print_bundles" :: diag
  and "context" "locale" :: thy_decl
  and "sublocale" "interpretation" :: thy_goal
  and "interpret" :: prf_goal % "proof"
  and "class" :: thy_decl
  and "subclass" :: thy_goal
  and "instantiation" :: thy_decl
  and "instance" :: thy_goal
  and "overloading" :: thy_decl
  and "code_datatype" :: thy_decl
  and "theorem" "lemma" "corollary" :: thy_goal
  and "schematic_theorem" "schematic_lemma" "schematic_corollary" :: thy_goal
  and "notepad" :: thy_decl
  and "have" :: prf_goal % "proof"
  and "hence" :: prf_goal % "proof" == "then have"
  and "show" :: prf_asm_goal % "proof"
  and "thus" :: prf_asm_goal % "proof" == "then show"
  and "then" "from" "with" :: prf_chain % "proof"
  and "note" "using" "unfolding" :: prf_decl % "proof"
  and "fix" "assume" "presume" "def" :: prf_asm % "proof"
  and "obtain" :: prf_asm_goal % "proof"
  and "guess" :: prf_asm_goal_script % "proof"
  and "let" "write" :: prf_decl % "proof"
  and "case" :: prf_asm % "proof"
  and "{" :: prf_open % "proof"
  and "}" :: prf_close % "proof"
  and "next" :: prf_block % "proof"
  and "qed" :: qed_block % "proof"
  and "by" ".." "." "sorry" :: "qed" % "proof"
  and "done" :: "qed_script" % "proof"
  and "oops" :: qed_global % "proof"
  and "defer" "prefer" "apply" :: prf_script % "proof"
  and "apply_end" :: prf_script % "proof" == ""
  and "proof" :: prf_block % "proof"
  and "also" "moreover" :: prf_decl % "proof"
  and "finally" "ultimately" :: prf_chain % "proof"
  and "back" :: prf_script % "proof"
  and "Isabelle.command" :: control
  and "help" "print_commands" "print_options" "print_context"
    "print_theory" "print_syntax" "print_abbrevs" "print_defn_rules"
    "print_theorems" "print_locales" "print_classes" "print_locale"
    "print_interps" "print_dependencies" "print_attributes"
    "print_simpset" "print_rules" "print_trans_rules" "print_methods"
    "print_antiquotations" "print_ML_antiquotations" "thy_deps"
    "locale_deps" "class_deps" "thm_deps" "print_binds" "print_term_bindings"
    "print_facts" "print_cases" "print_statement" "thm" "prf" "full_prf"
    "prop" "term" "typ" "print_codesetup" "unused_thms" :: diag
  and "use_thy" "remove_thy" "kill_thy" :: control
  and "display_drafts" "print_state" "pr" :: diag
  and "pretty_setmargin" "disable_pr" "enable_pr" "commit" "quit" "exit" :: control
  and "welcome" :: diag
  and "init_toplevel" "linear_undo" "undo" "undos_proof" "cannot_undo" "kill" :: control
  and "end" :: thy_end % "theory"
  and "realizers" :: thy_decl == ""
  and "realizability" :: thy_decl == ""
  and "extract_type" "extract" :: thy_decl
  and "find_theorems" "find_consts" :: diag
  and "ProofGeneral.process_pgip" "ProofGeneral.pr" "ProofGeneral.undo"
    "ProofGeneral.restart" "ProofGeneral.kill_proof" "ProofGeneral.inform_file_processed"
    "ProofGeneral.inform_file_retracted" :: control
begin

ML_file "ML/ml_antiquotations.ML"
ML_file "ML/ml_thms.ML"
ML_file "Tools/print_operation.ML"
ML_file "Isar/isar_syn.ML"
ML_file "Isar/calculation.ML"
ML_file "Tools/rail.ML"
ML_file "Tools/rule_insts.ML";
ML_file "Tools/find_theorems.ML"
ML_file "Tools/find_consts.ML"
ML_file "Tools/proof_general_pure.ML"
ML_file "Tools/simplifier_trace.ML"


section {* Basic attributes *}

attribute_setup tagged =
  "Scan.lift (Args.name -- Args.name) >> Thm.tag"
  "tagged theorem"

attribute_setup untagged =
  "Scan.lift Args.name >> Thm.untag"
  "untagged theorem"

attribute_setup kind =
  "Scan.lift Args.name >> Thm.kind"
  "theorem kind"

attribute_setup THEN =
  "Scan.lift (Scan.optional (Args.bracks Parse.nat) 1) -- Attrib.thm
    >> (fn (i, B) => Thm.rule_attribute (fn _ => fn A => A RSN (i, B)))"
  "resolution with rule"

attribute_setup OF =
  "Attrib.thms >> (fn Bs => Thm.rule_attribute (fn _ => fn A => A OF Bs))"
  "rule resolved with facts"

attribute_setup rename_abs =
  "Scan.lift (Scan.repeat (Args.maybe Args.name)) >> (fn vs =>
    Thm.rule_attribute (K (Drule.rename_bvars' vs)))"
  "rename bound variables in abstractions"

attribute_setup unfolded =
  "Attrib.thms >> (fn ths =>
    Thm.rule_attribute (fn context => Local_Defs.unfold (Context.proof_of context) ths))"
  "unfolded definitions"

attribute_setup folded =
  "Attrib.thms >> (fn ths =>
    Thm.rule_attribute (fn context => Local_Defs.fold (Context.proof_of context) ths))"
  "folded definitions"

attribute_setup consumes =
  "Scan.lift (Scan.optional Parse.int 1) >> Rule_Cases.consumes"
  "number of consumed facts"

attribute_setup constraints =
  "Scan.lift Parse.nat >> Rule_Cases.constraints"
  "number of equality constraints"

attribute_setup case_names = {*
  Scan.lift (Scan.repeat1 (Args.name --
    Scan.optional (@{keyword "["} |-- Scan.repeat1 (Args.maybe Args.name) --| @{keyword "]"}) []))
  >> (fn cs =>
      Rule_Cases.cases_hyp_names
        (map #1 cs)
        (map (map (the_default Rule_Cases.case_hypsN) o #2) cs))
*} "named rule cases"

attribute_setup case_conclusion =
  "Scan.lift (Args.name -- Scan.repeat Args.name) >> Rule_Cases.case_conclusion"
  "named conclusion of rule cases"

attribute_setup params =
  "Scan.lift (Parse.and_list1 (Scan.repeat Args.name)) >> Rule_Cases.params"
  "named rule parameters"

attribute_setup rule_format = {*
  Scan.lift (Args.mode "no_asm")
    >> (fn true => Object_Logic.rule_format_no_asm | false => Object_Logic.rule_format)
*} "result put into canonical rule format"

attribute_setup elim_format =
  "Scan.succeed (Thm.rule_attribute (K Tactic.make_elim))"
  "destruct rule turned into elimination rule format"

attribute_setup no_vars = {*
  Scan.succeed (Thm.rule_attribute (fn context => fn th =>
    let
      val ctxt = Variable.set_body false (Context.proof_of context);
      val ((_, [th']), _) = Variable.import true [th] ctxt;
    in th' end))
*} "imported schematic variables"

attribute_setup eta_long =
  "Scan.succeed (Thm.rule_attribute (fn _ => Conv.fconv_rule Drule.eta_long_conversion))"
  "put theorem into eta long beta normal form"

attribute_setup atomize =
  "Scan.succeed Object_Logic.declare_atomize"
  "declaration of atomize rule"

attribute_setup rulify =
  "Scan.succeed Object_Logic.declare_rulify"
  "declaration of rulify rule"

attribute_setup rotated =
  "Scan.lift (Scan.optional Parse.int 1
    >> (fn n => Thm.rule_attribute (fn _ => rotate_prems n)))"
  "rotated theorem premises"

attribute_setup defn =
  "Attrib.add_del Local_Defs.defn_add Local_Defs.defn_del"
  "declaration of definitional transformations"

attribute_setup abs_def =
  "Scan.succeed (Thm.rule_attribute (fn context =>
    Local_Defs.meta_rewrite_rule (Context.proof_of context) #> Drule.abs_def))"
  "abstract over free variables of definitional theorem"


section {* Further content for the Pure theory *}

subsection {* Meta-level connectives in assumptions *}

lemma meta_mp:
  assumes "PROP P ==> PROP Q" and "PROP P"
  shows "PROP Q"
    by (rule `PROP P ==> PROP Q` [OF `PROP P`])

lemmas meta_impE = meta_mp [elim_format]

lemma meta_spec:
  assumes "!!x. PROP P x"
  shows "PROP P x"
    by (rule `!!x. PROP P x`)

lemmas meta_allE = meta_spec [elim_format]

lemma swap_params:
  "(!!x y. PROP P x y) == (!!y x. PROP P x y)" ..


subsection {* Meta-level conjunction *}

lemma all_conjunction:
  "(!!x. PROP A x &&& PROP B x) == ((!!x. PROP A x) &&& (!!x. PROP B x))"
proof
  assume conj: "!!x. PROP A x &&& PROP B x"
  show "(!!x. PROP A x) &&& (!!x. PROP B x)"
  proof -
    fix x
    from conj show "PROP A x" by (rule conjunctionD1)
    from conj show "PROP B x" by (rule conjunctionD2)
  qed
next
  assume conj: "(!!x. PROP A x) &&& (!!x. PROP B x)"
  fix x
  show "PROP A x &&& PROP B x"
  proof -
    show "PROP A x" by (rule conj [THEN conjunctionD1, rule_format])
    show "PROP B x" by (rule conj [THEN conjunctionD2, rule_format])
  qed
qed

lemma imp_conjunction:
  "(PROP A ==> PROP B &&& PROP C) == ((PROP A ==> PROP B) &&& (PROP A ==> PROP C))"
proof
  assume conj: "PROP A ==> PROP B &&& PROP C"
  show "(PROP A ==> PROP B) &&& (PROP A ==> PROP C)"
  proof -
    assume "PROP A"
    from conj [OF `PROP A`] show "PROP B" by (rule conjunctionD1)
    from conj [OF `PROP A`] show "PROP C" by (rule conjunctionD2)
  qed
next
  assume conj: "(PROP A ==> PROP B) &&& (PROP A ==> PROP C)"
  assume "PROP A"
  show "PROP B &&& PROP C"
  proof -
    from `PROP A` show "PROP B" by (rule conj [THEN conjunctionD1])
    from `PROP A` show "PROP C" by (rule conj [THEN conjunctionD2])
  qed
qed

lemma conjunction_imp:
  "(PROP A &&& PROP B ==> PROP C) == (PROP A ==> PROP B ==> PROP C)"
proof
  assume r: "PROP A &&& PROP B ==> PROP C"
  assume ab: "PROP A" "PROP B"
  show "PROP C"
  proof (rule r)
    from ab show "PROP A &&& PROP B" .
  qed
next
  assume r: "PROP A ==> PROP B ==> PROP C"
  assume conj: "PROP A &&& PROP B"
  show "PROP C"
  proof (rule r)
    from conj show "PROP A" by (rule conjunctionD1)
    from conj show "PROP B" by (rule conjunctionD2)
  qed
qed

end

