(*  Title:      Pure/pure_thy.ML
    Author:     Markus Wenzel, TU Muenchen

Pure theory syntax and further logical content.
*)

signature PURE_THY =
sig
  val old_appl_syntax: theory -> bool
  val old_appl_syntax_setup: theory -> theory
  val token_markers: string list
end;

structure Pure_Thy: PURE_THY =
struct

val typ = Simple_Syntax.read_typ;
val prop = Simple_Syntax.read_prop;

val tycon = Lexicon.mark_type;
val const = Lexicon.mark_const;

val qualify = Binding.qualify true Context.PureN;


(* application syntax variants *)

val appl_syntax =
 [("_appl", typ "('b => 'a) => args => logic", Mixfix ("(1_/(1'(_')))", [1000, 0], 1000)),
  ("_appl", typ "('b => 'a) => args => aprop", Mixfix ("(1_/(1'(_')))", [1000, 0], 1000))];

val applC_syntax =
 [("",       typ "'a => cargs",                  Delimfix "_"),
  ("_cargs", typ "'a => cargs => cargs",         Mixfix ("_/ _", [1000, 1000], 1000)),
  ("_applC", typ "('b => 'a) => cargs => logic", Mixfix ("(1_/ _)", [1000, 1000], 999)),
  ("_applC", typ "('b => 'a) => cargs => aprop", Mixfix ("(1_/ _)", [1000, 1000], 999))];

structure Old_Appl_Syntax = Theory_Data
(
  type T = bool;
  val empty = false;
  val extend = I;
  fun merge (b1, b2) : T =
    if b1 = b2 then b1
    else error "Cannot merge theories with different application syntax";
);

val old_appl_syntax = Old_Appl_Syntax.get;

val old_appl_syntax_setup =
  Old_Appl_Syntax.put true #>
  Sign.del_syntax Syntax.mode_default applC_syntax #>
  Sign.add_syntax Syntax.mode_default appl_syntax;


(* main content *)

val token_markers =
  ["_tfree", "_tvar", "_free", "_bound", "_loose", "_var", "_numeral", "_inner_string"];

val _ = Theory.setup
  (Sign.map_naming (Name_Space.set_theory_name Context.PureN) #>
   Old_Appl_Syntax.put false #>
   Sign.add_types_global
   [(Binding.make ("fun", @{here}), 2, NoSyn),
    (Binding.make ("prop", @{here}), 0, NoSyn),
    (Binding.make ("itself", @{here}), 1, NoSyn),
    (Binding.make ("dummy", @{here}), 0, NoSyn)]
  #> Sign.add_nonterminals_global
    (map (fn name => Binding.make (name, @{here}))
      (Lexicon.terminals @ ["logic", "type", "types", "sort", "classes",
        "args", "cargs", "pttrn", "pttrns", "idt", "idts", "aprop", "asms",
        "any", "prop'", "num_const", "float_const", "num_position",
        "float_position", "index", "struct", "tid_position",
        "tvar_position", "id_position", "longid_position", "var_position",
        "str_position", "string_position", "cartouche_position", "type_name",
        "class_name"]))
  #> Sign.add_syntax Syntax.mode_default (map (fn x => (x, typ "'a", NoSyn)) token_markers)
  #> Sign.add_syntax Syntax.mode_default
   [("",            typ "prop' => prop",               Delimfix "_"),
    ("",            typ "logic => any",                Delimfix "_"),
    ("",            typ "prop' => any",                Delimfix "_"),
    ("",            typ "logic => logic",              Delimfix "'(_')"),
    ("",            typ "prop' => prop'",              Delimfix "'(_')"),
    ("_constrain",  typ "logic => type => logic",      Mixfix ("_::_", [4, 0], 3)),
    ("_constrain",  typ "prop' => type => prop'",      Mixfix ("_::_", [4, 0], 3)),
    ("_ignore_type", typ "'a",                         NoSyn),
    ("",            typ "tid_position => type",        Delimfix "_"),
    ("",            typ "tvar_position => type",       Delimfix "_"),
    ("",            typ "type_name => type",           Delimfix "_"),
    ("_type_name",  typ "id => type_name",             Delimfix "_"),
    ("_type_name",  typ "longid => type_name",         Delimfix "_"),
    ("_ofsort",     typ "tid_position => sort => type", Mixfix ("_::_", [1000, 0], 1000)),
    ("_ofsort",     typ "tvar_position => sort => type", Mixfix ("_::_", [1000, 0], 1000)),
    ("_dummy_ofsort", typ "sort => type",              Mixfix ("'_()::_", [0], 1000)),
    ("",            typ "class_name => sort",          Delimfix "_"),
    ("_class_name", typ "id => class_name",            Delimfix "_"),
    ("_class_name", typ "longid => class_name",        Delimfix "_"),
    ("_topsort",    typ "sort",                        Delimfix "{}"),
    ("_sort",       typ "classes => sort",             Delimfix "{_}"),
    ("",            typ "class_name => classes",       Delimfix "_"),
    ("_classes",    typ "class_name => classes => classes", Delimfix "_,_"),
    ("_tapp",       typ "type => type_name => type",   Mixfix ("_ _", [1000, 0], 1000)),
    ("_tappl",      typ "type => types => type_name => type", Delimfix "((1'(_,/ _')) _)"),
    ("",            typ "type => types",               Delimfix "_"),
    ("_types",      typ "type => types => types",      Delimfix "_,/ _"),
    ("\\<^type>fun", typ "type => type => type",       Mixfix ("(_/ => _)", [1, 0], 0)),
    ("_bracket",    typ "types => type => type",       Mixfix ("([_]/ => _)", [0, 0], 0)),
    ("",            typ "type => type",                Delimfix "'(_')"),
    ("\\<^type>dummy", typ "type",                     Delimfix "'_"),
    ("_type_prop",  typ "'a",                          NoSyn),
    ("_lambda",     typ "pttrns => 'a => logic",       Mixfix ("(3%_./ _)", [0, 3], 3)),
    ("_abs",        typ "'a",                          NoSyn),
    ("",            typ "'a => args",                  Delimfix "_"),
    ("_args",       typ "'a => args => args",          Delimfix "_,/ _"),
    ("",            typ "id_position => idt",          Delimfix "_"),
    ("_idtdummy",   typ "idt",                         Delimfix "'_"),
    ("_idtyp",      typ "id_position => type => idt",  Mixfix ("_::_", [], 0)),
    ("_idtypdummy", typ "type => idt",                 Mixfix ("'_()::_", [], 0)),
    ("",            typ "idt => idt",                  Delimfix "'(_')"),
    ("",            typ "idt => idts",                 Delimfix "_"),
    ("_idts",       typ "idt => idts => idts",         Mixfix ("_/ _", [1, 0], 0)),
    ("",            typ "idt => pttrn",                Delimfix "_"),
    ("",            typ "pttrn => pttrns",             Delimfix "_"),
    ("_pttrns",     typ "pttrn => pttrns => pttrns",   Mixfix ("_/ _", [1, 0], 0)),
    ("",            typ "aprop => aprop",              Delimfix "'(_')"),
    ("",            typ "id_position => aprop",        Delimfix "_"),
    ("",            typ "longid_position => aprop",    Delimfix "_"),
    ("",            typ "var_position => aprop",       Delimfix "_"),
    ("_DDDOT",      typ "aprop",                       Delimfix "..."),
    ("_aprop",      typ "aprop => prop",               Delimfix "PROP _"),
    ("_asm",        typ "prop => asms",                Delimfix "_"),
    ("_asms",       typ "prop => asms => asms",        Delimfix "_;/ _"),
    ("_bigimpl",    typ "asms => prop => prop",        Mixfix ("((3[| _ |])/ ==> _)", [0, 1], 1)),
    ("_ofclass",    typ "type => logic => prop",       Delimfix "(1OFCLASS/(1'(_,/ _')))"),
    ("_mk_ofclass", typ "dummy",                       NoSyn),
    ("_TYPE",       typ "type => logic",               Delimfix "(1TYPE/(1'(_')))"),
    ("",            typ "id_position => logic",        Delimfix "_"),
    ("",            typ "longid_position => logic",    Delimfix "_"),
    ("",            typ "var_position => logic",       Delimfix "_"),
    ("_DDDOT",      typ "logic",                       Delimfix "..."),
    ("_strip_positions", typ "'a", NoSyn),
    ("_position",   typ "num_token => num_position",   Delimfix "_"),
    ("_position",   typ "float_token => float_position", Delimfix "_"),
    ("_constify",   typ "num_position => num_const",   Delimfix "_"),
    ("_constify",   typ "float_position => float_const", Delimfix "_"),
    ("_index",      typ "logic => index",              Delimfix "(00\\<^bsub>_\\<^esub>)"),
    ("_indexdefault", typ "index",                     Delimfix ""),
    ("_indexvar",   typ "index",                       Delimfix "'\\<index>"),
    ("_struct",     typ "index => logic",              Mixfix ("\\<struct>_", [1000], 1000)),
    ("_update_name", typ "idt",                        NoSyn),
    ("_constrainAbs", typ "'a",                        NoSyn),
    ("_position_sort", typ "tid => tid_position",      Delimfix "_"),
    ("_position_sort", typ "tvar => tvar_position",    Delimfix "_"),
    ("_position",   typ "id => id_position",           Delimfix "_"),
    ("_position",   typ "longid => longid_position",   Delimfix "_"),
    ("_position",   typ "var => var_position",         Delimfix "_"),
    ("_position",   typ "str_token => str_position",   Delimfix "_"),
    ("_position",   typ "string_token => string_position", Delimfix "_"),
    ("_position",   typ "cartouche => cartouche_position", Delimfix "_"),
    ("_type_constraint_", typ "'a",                    NoSyn),
    ("_context_const", typ "id_position => logic",     Delimfix "CONST _"),
    ("_context_const", typ "id_position => aprop",     Delimfix "CONST _"),
    ("_context_const", typ "longid_position => logic", Delimfix "CONST _"),
    ("_context_const", typ "longid_position => aprop", Delimfix "CONST _"),
    ("_context_xconst", typ "id_position => logic",    Delimfix "XCONST _"),
    ("_context_xconst", typ "id_position => aprop",    Delimfix "XCONST _"),
    ("_context_xconst", typ "longid_position => logic", Delimfix "XCONST _"),
    ("_context_xconst", typ "longid_position => aprop", Delimfix "XCONST _"),
    (const "Pure.imp",  typ "prop => prop => prop",     Delimfix "op ==>"),
    (const "Pure.dummy_pattern", typ "aprop",          Delimfix "'_"),
    ("_sort_constraint", typ "type => prop",           Delimfix "(1SORT'_CONSTRAINT/(1'(_')))"),
    (const "Pure.term", typ "logic => prop",           Delimfix "TERM _"),
    (const "Pure.conjunction", typ "prop => prop => prop", Infixr ("&&&", 2))]
  #> Sign.add_syntax Syntax.mode_default applC_syntax
  #> Sign.add_syntax (Symbol.xsymbolsN, true)
   [(tycon "fun",         typ "type => type => type",   Mixfix ("(_/ \\<Rightarrow> _)", [1, 0], 0)),
    ("_bracket",          typ "types => type => type",  Mixfix ("([_]/ \\<Rightarrow> _)", [0, 0], 0)),
    ("_ofsort",           typ "tid_position => sort => type", Mixfix ("_\\<Colon>_", [1000, 0], 1000)),
    ("_constrain",        typ "logic => type => logic", Mixfix ("_\\<Colon>_", [4, 0], 3)),
    ("_constrain",        typ "prop' => type => prop'", Mixfix ("_\\<Colon>_", [4, 0], 3)),
    ("_idtyp",            typ "id_position => type => idt", Mixfix ("_\\<Colon>_", [], 0)),
    ("_idtypdummy",       typ "type => idt",            Mixfix ("'_()\\<Colon>_", [], 0)),
    ("_lambda",           typ "pttrns => 'a => logic",  Mixfix ("(3\\<lambda>_./ _)", [0, 3], 3)),
    (const "Pure.eq",     typ "'a => 'a => prop",       Infix ("\\<equiv>", 2)),
    (const "Pure.all_binder", typ "idts => prop => prop", Mixfix ("(3\\<And>_./ _)", [0, 0], 0)),
    (const "Pure.imp",    typ "prop => prop => prop",   Infixr ("\\<Longrightarrow>", 1)),
    ("_DDDOT",            typ "aprop",                  Delimfix "\\<dots>"),
    ("_bigimpl",          typ "asms => prop => prop",   Mixfix ("((1\\<lbrakk>_\\<rbrakk>)/ \\<Longrightarrow> _)", [0, 1], 1)),
    ("_DDDOT",            typ "logic",                  Delimfix "\\<dots>")]
  #> Sign.add_syntax ("", false)
   [(const "Pure.prop", typ "prop => prop", Mixfix ("_", [0], 0))]
  #> Sign.add_syntax ("HTML", false)
   [("_lambda", typ "pttrns => 'a => logic", Mixfix ("(3\\<lambda>_./ _)", [0, 3], 3))]
  #> Sign.add_consts
   [(qualify (Binding.make ("eq", @{here})), typ "'a => 'a => prop", Infix ("==", 2)),
    (qualify (Binding.make ("imp", @{here})), typ "prop => prop => prop", Mixfix ("(_/ ==> _)", [2, 1], 1)),
    (qualify (Binding.make ("all", @{here})), typ "('a => prop) => prop", Binder ("!!", 0, 0)),
    (qualify (Binding.make ("prop", @{here})), typ "prop => prop", NoSyn),
    (qualify (Binding.make ("type", @{here})), typ "'a itself", NoSyn),
    (qualify (Binding.make ("dummy_pattern", @{here})), typ "'a", Delimfix "'_")]
  #> Theory.add_deps_global "Pure.eq" ("Pure.eq", typ "'a => 'a => prop") []
  #> Theory.add_deps_global "Pure.imp" ("Pure.imp", typ "prop => prop => prop") []
  #> Theory.add_deps_global "Pure.all" ("Pure.all", typ "('a => prop) => prop") []
  #> Theory.add_deps_global "Pure.type" ("Pure.type", typ "'a itself") []
  #> Theory.add_deps_global "Pure.dummy_pattern" ("Pure.dummy_pattern", typ "'a") []
  #> Sign.parse_ast_translation Syntax_Trans.pure_parse_ast_translation
  #> Sign.parse_translation Syntax_Trans.pure_parse_translation
  #> Sign.print_ast_translation Syntax_Trans.pure_print_ast_translation
  #> Sign.add_consts
   [(qualify (Binding.make ("term", @{here})), typ "'a => prop", NoSyn),
    (qualify (Binding.make ("sort_constraint", @{here})), typ "'a itself => prop", NoSyn),
    (qualify (Binding.make ("conjunction", @{here})), typ "prop => prop => prop", NoSyn)]
  #> Sign.local_path
  #> (Global_Theory.add_defs false o map Thm.no_attributes)
   [(Binding.make ("prop_def", @{here}),
      prop "(CONST Pure.prop :: prop => prop) (A::prop) == A::prop"),
    (Binding.make ("term_def", @{here}),
      prop "(CONST Pure.term :: 'a => prop) (x::'a) == (!!A::prop. A ==> A)"),
    (Binding.make ("sort_constraint_def", @{here}),
      prop "(CONST Pure.sort_constraint :: 'a itself => prop) (CONST Pure.type :: 'a itself) ==\
      \ (CONST Pure.term :: 'a itself => prop) (CONST Pure.type :: 'a itself)"),
    (Binding.make ("conjunction_def", @{here}),
      prop "(A &&& B) == (!!C::prop. (A ==> B ==> C) ==> C)")] #> snd
  #> Global_Theory.add_thmss [((Binding.make ("nothing", @{here}), []), [])] #> snd
  #> fold (fn (a, prop) =>
      snd o Thm.add_axiom_global (Binding.make (a, @{here}), prop)) Proofterm.equality_axms);

end;
