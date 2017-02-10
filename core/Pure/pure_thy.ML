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

fun mixfix (sy, ps, p) = Mixfix (Input.string sy, ps, p, Position.no_range);
fun infix_ (sy, p) = Infix (Input.string sy, p, Position.no_range);
fun infixr_ (sy, p) = Infixr (Input.string sy, p, Position.no_range);
fun binder (sy, p, q) = Binder (Input.string sy, p, q, Position.no_range);


(* application syntax variants *)

val appl_syntax =
 [("_appl", typ "('b => 'a) => args => logic", mixfix ("(1_/(1'(_')))", [1000, 0], 1000)),
  ("_appl", typ "('b => 'a) => args => aprop", mixfix ("(1_/(1'(_')))", [1000, 0], 1000))];

val applC_syntax =
 [("",       typ "'a => cargs",                  Mixfix.mixfix "_"),
  ("_cargs", typ "'a => cargs => cargs",         mixfix ("_/ _", [1000, 1000], 1000)),
  ("_applC", typ "('b => 'a) => cargs => logic", mixfix ("(1_/ _)", [1000, 1000], 999)),
  ("_applC", typ "('b => 'a) => cargs => aprop", mixfix ("(1_/ _)", [1000, 1000], 999))];

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
  #> Theory.add_deps_global "fun" ((Defs.Type, "fun"), [typ "'a", typ "'b"]) []
  #> Theory.add_deps_global "prop" ((Defs.Type, "prop"), []) []
  #> Theory.add_deps_global "itself" ((Defs.Type, "itself"), [typ "'a"]) []
  #> Theory.add_deps_global "dummy" ((Defs.Type, "dummy"), []) []
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
   [("",            typ "prop' => prop",               Mixfix.mixfix "_"),
    ("",            typ "logic => any",                Mixfix.mixfix "_"),
    ("",            typ "prop' => any",                Mixfix.mixfix "_"),
    ("",            typ "logic => logic",              Mixfix.mixfix "'(_')"),
    ("",            typ "prop' => prop'",              Mixfix.mixfix "'(_')"),
    ("_constrain",  typ "logic => type => logic",      mixfix ("_::_", [4, 0], 3)),
    ("_constrain",  typ "prop' => type => prop'",      mixfix ("_::_", [4, 0], 3)),
    ("_ignore_type", typ "'a",                         NoSyn),
    ("",            typ "tid_position => type",        Mixfix.mixfix "_"),
    ("",            typ "tvar_position => type",       Mixfix.mixfix "_"),
    ("",            typ "type_name => type",           Mixfix.mixfix "_"),
    ("_type_name",  typ "id => type_name",             Mixfix.mixfix "_"),
    ("_type_name",  typ "longid => type_name",         Mixfix.mixfix "_"),
    ("_ofsort",     typ "tid_position => sort => type", mixfix ("_::_", [1000, 0], 1000)),
    ("_ofsort",     typ "tvar_position => sort => type", mixfix ("_::_", [1000, 0], 1000)),
    ("_dummy_ofsort", typ "sort => type",              mixfix ("'_()::_", [0], 1000)),
    ("",            typ "class_name => sort",          Mixfix.mixfix "_"),
    ("_class_name", typ "id => class_name",            Mixfix.mixfix "_"),
    ("_class_name", typ "longid => class_name",        Mixfix.mixfix "_"),
    ("_topsort",    typ "sort",                        Mixfix.mixfix "{}"),
    ("_sort",       typ "classes => sort",             Mixfix.mixfix "{_}"),
    ("",            typ "class_name => classes",       Mixfix.mixfix "_"),
    ("_classes",    typ "class_name => classes => classes", Mixfix.mixfix "_,_"),
    ("_tapp",       typ "type => type_name => type",   mixfix ("_ _", [1000, 0], 1000)),
    ("_tappl",      typ "type => types => type_name => type", Mixfix.mixfix "((1'(_,/ _')) _)"),
    ("",            typ "type => types",               Mixfix.mixfix "_"),
    ("_types",      typ "type => types => types",      Mixfix.mixfix "_,/ _"),
    ("\<^type>fun", typ "type => type => type",        mixfix ("(_/ \<Rightarrow> _)", [1, 0], 0)),
    ("_bracket",    typ "types => type => type",       mixfix ("([_]/ \<Rightarrow> _)", [0, 0], 0)),
    ("",            typ "type => type",                Mixfix.mixfix "'(_')"),
    ("\<^type>dummy", typ "type",                      Mixfix.mixfix "'_"),
    ("_type_prop",  typ "'a",                          NoSyn),
    ("_lambda",     typ "pttrns => 'a => logic",       mixfix ("(3\<lambda>_./ _)", [0, 3], 3)),
    ("_abs",        typ "'a",                          NoSyn),
    ("",            typ "'a => args",                  Mixfix.mixfix "_"),
    ("_args",       typ "'a => args => args",          Mixfix.mixfix "_,/ _"),
    ("",            typ "id_position => idt",          Mixfix.mixfix "_"),
    ("_idtdummy",   typ "idt",                         Mixfix.mixfix "'_"),
    ("_idtyp",      typ "id_position => type => idt",  mixfix ("_::_", [], 0)),
    ("_idtypdummy", typ "type => idt",                 mixfix ("'_()::_", [], 0)),
    ("",            typ "idt => idt",                  Mixfix.mixfix "'(_')"),
    ("",            typ "idt => idts",                 Mixfix.mixfix "_"),
    ("_idts",       typ "idt => idts => idts",         mixfix ("_/ _", [1, 0], 0)),
    ("",            typ "idt => pttrn",                Mixfix.mixfix "_"),
    ("",            typ "pttrn => pttrns",             Mixfix.mixfix "_"),
    ("_pttrns",     typ "pttrn => pttrns => pttrns",   mixfix ("_/ _", [1, 0], 0)),
    ("",            typ "aprop => aprop",              Mixfix.mixfix "'(_')"),
    ("",            typ "id_position => aprop",        Mixfix.mixfix "_"),
    ("",            typ "longid_position => aprop",    Mixfix.mixfix "_"),
    ("",            typ "var_position => aprop",       Mixfix.mixfix "_"),
    ("_DDDOT",      typ "aprop",                       Mixfix.mixfix "\<dots>"),
    ("_aprop",      typ "aprop => prop",               Mixfix.mixfix "PROP _"),
    ("_asm",        typ "prop => asms",                Mixfix.mixfix "_"),
    ("_asms",       typ "prop => asms => asms",        Mixfix.mixfix "_;/ _"),
    ("_bigimpl",    typ "asms => prop => prop",        mixfix ("((1\<lbrakk>_\<rbrakk>)/ \<Longrightarrow> _)", [0, 1], 1)),
    ("_ofclass",    typ "type => logic => prop",       Mixfix.mixfix "(1OFCLASS/(1'(_,/ _')))"),
    ("_mk_ofclass", typ "dummy",                       NoSyn),
    ("_TYPE",       typ "type => logic",               Mixfix.mixfix "(1TYPE/(1'(_')))"),
    ("",            typ "id_position => logic",        Mixfix.mixfix "_"),
    ("",            typ "longid_position => logic",    Mixfix.mixfix "_"),
    ("",            typ "var_position => logic",       Mixfix.mixfix "_"),
    ("_DDDOT",      typ "logic",                       Mixfix.mixfix "\<dots>"),
    ("_strip_positions", typ "'a", NoSyn),
    ("_position",   typ "num_token => num_position",   Mixfix.mixfix "_"),
    ("_position",   typ "float_token => float_position", Mixfix.mixfix "_"),
    ("_constify",   typ "num_position => num_const",   Mixfix.mixfix "_"),
    ("_constify",   typ "float_position => float_const", Mixfix.mixfix "_"),
    ("_index",      typ "logic => index",              Mixfix.mixfix "(\<open>unbreakable\<close>\<^bsub>_\<^esub>)"),
    ("_indexdefault", typ "index",                     Mixfix.mixfix ""),
    ("_indexvar",   typ "index",                       Mixfix.mixfix "'\<index>"),
    ("_struct",     typ "index => logic",              NoSyn),
    ("_update_name", typ "idt",                        NoSyn),
    ("_constrainAbs", typ "'a",                        NoSyn),
    ("_position_sort", typ "tid => tid_position",      Mixfix.mixfix "_"),
    ("_position_sort", typ "tvar => tvar_position",    Mixfix.mixfix "_"),
    ("_position",   typ "id => id_position",           Mixfix.mixfix "_"),
    ("_position",   typ "longid => longid_position",   Mixfix.mixfix "_"),
    ("_position",   typ "var => var_position",         Mixfix.mixfix "_"),
    ("_position",   typ "str_token => str_position",   Mixfix.mixfix "_"),
    ("_position",   typ "string_token => string_position", Mixfix.mixfix "_"),
    ("_position",   typ "cartouche => cartouche_position", Mixfix.mixfix "_"),
    ("_type_constraint_", typ "'a",                    NoSyn),
    ("_context_const", typ "id_position => logic",     Mixfix.mixfix "CONST _"),
    ("_context_const", typ "id_position => aprop",     Mixfix.mixfix "CONST _"),
    ("_context_const", typ "longid_position => logic", Mixfix.mixfix "CONST _"),
    ("_context_const", typ "longid_position => aprop", Mixfix.mixfix "CONST _"),
    ("_context_xconst", typ "id_position => logic",    Mixfix.mixfix "XCONST _"),
    ("_context_xconst", typ "id_position => aprop",    Mixfix.mixfix "XCONST _"),
    ("_context_xconst", typ "longid_position => logic", Mixfix.mixfix "XCONST _"),
    ("_context_xconst", typ "longid_position => aprop", Mixfix.mixfix "XCONST _"),
    (const "Pure.dummy_pattern", typ "aprop",          Mixfix.mixfix "'_"),
    ("_sort_constraint", typ "type => prop",           Mixfix.mixfix "(1SORT'_CONSTRAINT/(1'(_')))"),
    (const "Pure.term", typ "logic => prop",           Mixfix.mixfix "TERM _"),
    (const "Pure.conjunction", typ "prop => prop => prop", infixr_ ("&&&", 2))]
  #> Sign.add_syntax Syntax.mode_default applC_syntax
  #> Sign.add_syntax (Print_Mode.ASCII, true)
   [(tycon "fun",         typ "type => type => type",   mixfix ("(_/ => _)", [1, 0], 0)),
    ("_bracket",          typ "types => type => type",  mixfix ("([_]/ => _)", [0, 0], 0)),
    ("_lambda",           typ "pttrns => 'a => logic",  mixfix ("(3%_./ _)", [0, 3], 3)),
    (const "Pure.eq",     typ "'a => 'a => prop",       infix_ ("==", 2)),
    (const "Pure.all_binder", typ "idts => prop => prop", mixfix ("(3!!_./ _)", [0, 0], 0)),
    (const "Pure.imp",    typ "prop => prop => prop",   infixr_ ("==>", 1)),
    ("_DDDOT",            typ "aprop",                  Mixfix.mixfix "..."),
    ("_bigimpl",          typ "asms => prop => prop",   mixfix ("((3[| _ |])/ ==> _)", [0, 1], 1)),
    ("_DDDOT",            typ "logic",                  Mixfix.mixfix "...")]
  #> Sign.add_syntax ("", false)
   [(const "Pure.prop", typ "prop => prop", mixfix ("_", [0], 0))]
  #> Sign.add_consts
   [(qualify (Binding.make ("eq", @{here})), typ "'a => 'a => prop", infix_ ("\<equiv>", 2)),
    (qualify (Binding.make ("imp", @{here})), typ "prop => prop => prop", infixr_ ("\<Longrightarrow>", 1)),
    (qualify (Binding.make ("all", @{here})), typ "('a => prop) => prop", binder ("\<And>", 0, 0)),
    (qualify (Binding.make ("prop", @{here})), typ "prop => prop", NoSyn),
    (qualify (Binding.make ("type", @{here})), typ "'a itself", NoSyn),
    (qualify (Binding.make ("dummy_pattern", @{here})), typ "'a", Mixfix.mixfix "'_")]
  #> Theory.add_deps_global "Pure.eq" ((Defs.Const, "Pure.eq"), [typ "'a"]) []
  #> Theory.add_deps_global "Pure.imp" ((Defs.Const, "Pure.imp"), []) []
  #> Theory.add_deps_global "Pure.all" ((Defs.Const, "Pure.all"), [typ "'a"]) []
  #> Theory.add_deps_global "Pure.type" ((Defs.Const, "Pure.type"), [typ "'a"]) []
  #> Theory.add_deps_global "Pure.dummy_pattern" ((Defs.Const, "Pure.dummy_pattern"), [typ "'a"]) []
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
  #> fold (fn (a, prop) =>
      snd o Thm.add_axiom_global (Binding.make (a, @{here}), prop)) Proofterm.equality_axms);

end;
