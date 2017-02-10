(*  Title:      Pure/Isar/attrib.ML
    Author:     Markus Wenzel, TU Muenchen

Symbolic representation of attributes -- with name and syntax.
*)

signature ATTRIB =
sig
  val print_attributes: bool -> Proof.context -> unit
  val define_global: binding -> (Token.src -> attribute) -> string -> theory -> string * theory
  val define: binding -> (Token.src -> attribute) -> string -> local_theory -> string * local_theory
  val check_name_generic: Context.generic -> xstring * Position.T -> string
  val check_name: Proof.context -> xstring * Position.T -> string
  val check_src: Proof.context -> Token.src -> Token.src
  val attribs: Token.src list context_parser
  val opt_attribs: Token.src list context_parser
  val pretty_attribs: Proof.context -> Token.src list -> Pretty.T list
  val pretty_binding: Proof.context -> Attrib.binding -> string -> Pretty.T list
  val attribute: Proof.context -> Token.src -> attribute
  val attribute_global: theory -> Token.src -> attribute
  val attribute_cmd: Proof.context -> Token.src -> attribute
  val attribute_cmd_global: theory -> Token.src -> attribute
  val map_specs: ('a list -> 'att list) ->
    (('c * 'a list) * 'b) list -> (('c * 'att list) * 'b) list
  val map_facts: ('a list -> 'att list) ->
    (('c * 'a list) * ('d * 'a list) list) list ->
    (('c * 'att list) * ('d * 'att list) list) list
  val map_facts_refs: ('a list -> 'att list) -> ('b -> 'fact) ->
    (('c * 'a list) * ('b * 'a list) list) list ->
    (('c * 'att list) * ('fact * 'att list) list) list
  type thms = (thm list * Token.src list) list
  val global_notes: string -> (Attrib.binding * thms) list ->
    theory -> (string * thm list) list * theory
  val local_notes: string -> (Attrib.binding * thms) list ->
    Proof.context -> (string * thm list) list * Proof.context
  val generic_notes: string -> (Attrib.binding * thms) list ->
    Context.generic -> (string * thm list) list * Context.generic
  val eval_thms: Proof.context -> (Facts.ref * Token.src list) list -> thm list
  val attribute_syntax: attribute context_parser -> Token.src -> attribute
  val setup: binding -> attribute context_parser -> string -> theory -> theory
  val local_setup: binding -> attribute context_parser -> string ->
    local_theory -> local_theory
  val attribute_setup: bstring * Position.T -> Input.source -> string ->
    local_theory -> local_theory
  val internal: (morphism -> attribute) -> Token.src
  val internal_declaration: declaration -> thms
  val add_del: attribute -> attribute -> attribute context_parser
  val thm: thm context_parser
  val thms: thm list context_parser
  val multi_thm: thm list context_parser
  val transform_facts: morphism -> (Attrib.binding * thms) list -> (Attrib.binding * thms) list
  val partial_evaluation: Proof.context ->
    (Attrib.binding * thms) list -> (Attrib.binding * thms) list
  val print_options: bool -> Proof.context -> unit
  val config_bool: binding -> (Context.generic -> bool) -> bool Config.T * (theory -> theory)
  val config_int: binding -> (Context.generic -> int) -> int Config.T * (theory -> theory)
  val config_real: binding -> (Context.generic -> real) -> real Config.T * (theory -> theory)
  val config_string: binding -> (Context.generic -> string) -> string Config.T * (theory -> theory)
  val setup_config_bool: binding -> (Context.generic -> bool) -> bool Config.T
  val setup_config_int: binding -> (Context.generic -> int) -> int Config.T
  val setup_config_real: binding -> (Context.generic -> real) -> real Config.T
  val setup_config_string: binding -> (Context.generic -> string) -> string Config.T
  val option_bool: string * Position.T -> bool Config.T * (theory -> theory)
  val option_int: string * Position.T -> int Config.T * (theory -> theory)
  val option_real: string * Position.T -> real Config.T * (theory -> theory)
  val option_string: string * Position.T -> string Config.T * (theory -> theory)
  val setup_option_bool: string * Position.T -> bool Config.T
  val setup_option_int: string * Position.T -> int Config.T
  val setup_option_real: string * Position.T -> real Config.T
  val setup_option_string: string * Position.T -> string Config.T
  val consumes: int -> Token.src
  val constraints: int -> Token.src
  val cases_open: Token.src
  val case_names: string list -> Token.src
  val case_conclusion: string * string list -> Token.src
end;

structure Attrib: sig type binding = Attrib.binding include ATTRIB end =
struct

type binding = Attrib.binding;



(** named attributes **)

(* theory data *)

structure Attributes = Generic_Data
(
  type T = ((Token.src -> attribute) * string) Name_Space.table;
  val empty : T = Name_Space.empty_table "attribute";
  val extend = I;
  fun merge data : T = Name_Space.merge_tables data;
);

val ops_attributes = {get_data = Attributes.get, put_data = Attributes.put};

val get_attributes = Attributes.get o Context.Proof;

fun print_attributes verbose ctxt =
  let
    val attribs = get_attributes ctxt;
    fun prt_attr (name, (_, "")) = Pretty.mark_str name
      | prt_attr (name, (_, comment)) =
          Pretty.block
            (Pretty.mark_str name :: Pretty.str ":" :: Pretty.brk 2 :: Pretty.text comment);
  in
    [Pretty.big_list "attributes:" (map prt_attr (Name_Space.markup_table verbose ctxt attribs))]
    |> Pretty.writeln_chunks
  end;

val attribute_space = Name_Space.space_of_table o get_attributes;


(* define *)

fun define_global binding att comment =
  Entity.define_global ops_attributes binding (att, comment);

fun define binding att comment =
  Entity.define ops_attributes binding (att, comment);


(* check *)

fun check_name_generic context = #1 o Name_Space.check context (Attributes.get context);
val check_name = check_name_generic o Context.Proof;

fun check_src ctxt src =
  let
    val _ =
      if Token.checked_src src then ()
      else Context_Position.report ctxt (#1 (Token.range_of src)) Markup.language_attribute;
  in #1 (Token.check_src ctxt get_attributes src) end;

val attribs =
  Args.context -- Scan.lift Parse.attribs
    >> (fn (ctxt, srcs) => map (check_src ctxt) srcs);

val opt_attribs = Scan.optional attribs [];


(* pretty printing *)

fun pretty_attribs _ [] = []
  | pretty_attribs ctxt srcs = [Pretty.enum "," "[" "]" (map (Token.pretty_src ctxt) srcs)];

fun pretty_binding ctxt (b, atts) sep =
  (case (Binding.is_empty b, null atts) of
    (true, true) => []
  | (false, true) => [Pretty.block [Binding.pretty b, Pretty.str sep]]
  | (true, false) => [Pretty.block (pretty_attribs ctxt atts @ [Pretty.str sep])]
  | (false, false) =>
      [Pretty.block
        (Binding.pretty b :: Pretty.brk 1 :: pretty_attribs ctxt atts @ [Pretty.str sep])]);


(* get attributes *)

fun attribute_generic context =
  let val table = Attributes.get context
  in fn src => #1 (Name_Space.get table (#1 (Token.name_of_src src))) src end;

val attribute = attribute_generic o Context.Proof;
val attribute_global = attribute_generic o Context.Theory;

fun attribute_cmd ctxt = attribute ctxt o check_src ctxt;
fun attribute_cmd_global thy = attribute_global thy o check_src (Proof_Context.init_global thy);


(* attributed declarations *)

fun map_specs f = map (apfst (apsnd f));

fun map_facts f = map (apfst (apsnd f) o apsnd (map (apsnd f)));
fun map_facts_refs f g = map_facts f #> map (apsnd (map (apfst g)));


(* fact expressions *)

type thms = (thm list * Token.src list) list;

fun global_notes kind facts thy = thy |>
  Global_Theory.note_thmss kind (map_facts (map (attribute_global thy)) facts);

fun local_notes kind facts ctxt = ctxt |>
  Proof_Context.note_thmss kind (map_facts (map (attribute ctxt)) facts);

fun generic_notes kind facts context = context |>
  Context.mapping_result (global_notes kind facts) (local_notes kind facts);

fun eval_thms ctxt srcs = ctxt
  |> Proof_Context.note_thmss ""
    (map_facts_refs
      (map (attribute_cmd ctxt)) (Proof_Context.get_fact ctxt) [(Binding.empty_atts, srcs)])
  |> fst |> maps snd;


(* attribute setup *)

fun attribute_syntax scan src (context, th) =
  let val (a, context') = Token.syntax_generic scan src context in a (context', th) end;

fun setup binding scan comment = define_global binding (attribute_syntax scan) comment #> snd;
fun local_setup binding scan comment = define binding (attribute_syntax scan) comment #> snd;

fun attribute_setup name source comment =
  ML_Lex.read_source false source
  |> ML_Context.expression (Input.range_of source) "parser" "Thm.attribute context_parser"
    ("Context.map_proof (Attrib.local_setup " ^ ML_Syntax.atomic (ML_Syntax.make_binding name) ^
      " parser " ^ ML_Syntax.print_string comment ^ ")")
  |> Context.proof_map;


(* internal attribute *)

val _ = Theory.setup
  (setup (Binding.make ("attribute", @{here}))
    (Scan.lift Args.internal_attribute >> Morphism.form)
    "internal attribute");

fun internal_name ctxt name =
  Token.make_src (name, Position.none) [] |> check_src ctxt |> hd;

val internal_attribute_name =
  internal_name (Context.the_local_context ()) "attribute";

fun internal att =
  internal_attribute_name ::
    [Token.make_string ("<attribute>", Position.none) |> Token.assign (SOME (Token.Attribute att))];

fun internal_declaration decl =
  [([Drule.dummy_thm], [internal (fn phi => Thm.declaration_attribute (K (decl phi)))])];


(* add/del syntax *)

fun add_del add del = Scan.lift (Args.add >> K add || Args.del >> K del || Scan.succeed add);



(** parsing attributed theorems **)

local

val fact_name = Args.internal_fact >> K "<fact>" || Args.name;

fun gen_thm pick = Scan.depend (fn context =>
  let
    val get = Proof_Context.get_fact_generic context;
    val get_fact = get o Facts.Fact;
    fun get_named is_sel pos name =
      let val (a, ths) = get (Facts.Named ((name, pos), NONE))
      in (if is_sel then NONE else a, ths) end;
  in
    Parse.$$$ "[" |-- Scan.pass context attribs --| Parse.$$$ "]" >> (fn srcs =>
      let
        val atts = map (attribute_generic context) srcs;
        val (th', context') = fold (uncurry o Thm.apply_attribute) atts (Drule.dummy_thm, context);
      in (context', pick ("", Position.none) [th']) end)
    ||
    (Scan.ahead Args.alt_name -- Args.named_fact get_fact
      >> (fn (s, fact) => ("", Facts.Fact s, fact)) ||
     Scan.ahead (Parse.position fact_name -- Scan.option Parse.thm_sel) :|--
      (fn ((name, pos), sel) =>
        Args.named_fact (get_named (is_some sel) pos) --| Scan.option Parse.thm_sel
          >> (fn fact => (name, Facts.Named ((name, pos), sel), fact))))
    -- Scan.pass context opt_attribs >> (fn ((name, thmref, fact), srcs) =>
      let
        val ths = Facts.select thmref fact;
        val atts = map (attribute_generic context) srcs;
        val (ths', context') =
          fold_map (curry (fold (uncurry o Thm.apply_attribute) atts)) ths context;
      in (context', pick (name, Facts.pos_of_ref thmref) ths') end)
  end);

in

val thm = gen_thm Facts.the_single;
val multi_thm = gen_thm (K I);
val thms = Scan.repeats multi_thm;

end;


(* transform fact expressions *)

fun transform_facts phi = map (fn ((a, atts), bs) =>
  ((Morphism.binding phi a, (map o map) (Token.transform phi) atts),
    bs |> map (fn (ths, btts) => (Morphism.fact phi ths, (map o map) (Token.transform phi) btts))));



(** partial evaluation -- observing rule/declaration/mixed attributes **)

(*NB: result length may change due to rearrangement of symbolic expression*)

local

fun apply_att src (context, th) =
  let
    val src1 = map Token.init_assignable src;
    val result = attribute_generic context src1 (context, th);
    val src2 = map Token.closure src1;
  in (src2, result) end;

fun err msg src =
  let val (name, pos) = Token.name_of_src src
  in error (msg ^ " " ^ quote name ^ Position.here pos) end;

fun eval src ((th, dyn), (decls, context)) =
  (case (apply_att src (context, th), dyn) of
    ((_, (NONE, SOME th')), NONE) => ((th', NONE), (decls, context))
  | ((_, (NONE, SOME _)), SOME _) => err "Mixed dynamic attribute followed by static rule" src
  | ((src', (SOME context', NONE)), NONE) =>
      let
        val decls' =
          (case decls of
            [] => [(th, [src'])]
          | (th2, srcs2) :: rest =>
              if Thm.eq_thm_strict (th, th2)
              then ((th2, src' :: srcs2) :: rest)
              else (th, [src']) :: (th2, srcs2) :: rest);
      in ((th, NONE), (decls', context')) end
  | ((src', (opt_context', opt_th')), _) =>
      let
        val context' = the_default context opt_context';
        val th' = the_default th opt_th';
        val dyn' =
          (case dyn of
            NONE => SOME (th, [src'])
          | SOME (dyn_th, srcs) => SOME (dyn_th, src' :: srcs));
      in ((th', dyn'), (decls, context')) end);

in

fun partial_evaluation ctxt facts =
  (facts, Context.Proof (Context_Position.not_really ctxt)) |->
    fold_map (fn ((b, more_atts), fact) => fn context =>
      let
        val (fact', (decls, context')) =
          (fact, ([], context)) |-> fold_map (fn (ths, atts) => fn res1 =>
            (ths, res1) |-> fold_map (fn th => fn res2 =>
              let
                val ((th', dyn'), res3) = fold eval (atts @ more_atts) ((th, NONE), res2);
                val th_atts' =
                  (case dyn' of
                    NONE => (th', [])
                  | SOME (dyn_th', atts') => (dyn_th', rev atts'));
              in (th_atts', res3) end))
          |>> flat;
        val decls' = rev (map (apsnd rev) decls);
        val facts' =
          if eq_list (eq_fst Thm.eq_thm_strict) (decls', fact') then
            [((b, []), map2 (fn (th, atts1) => fn (_, atts2) => (th, atts1 @ atts2)) decls' fact')]
          else if null decls' then [((b, []), fact')]
          else [(Binding.empty_atts, decls'), ((b, []), fact')];
      in (facts', context') end)
  |> fst |> flat |> map (apsnd (map (apfst single)))
  |> filter_out (fn (b, fact) => Binding.is_empty_atts b andalso forall (null o #2) fact);

end;



(** configuration options **)

(* naming *)

structure Configs = Theory_Data
(
  type T = Config.raw Symtab.table;
  val empty = Symtab.empty;
  val extend = I;
  fun merge data = Symtab.merge (K true) data;
);

fun print_options verbose ctxt =
  let
    fun prt (name, config) =
      let val value = Config.get ctxt config in
        Pretty.block [Pretty.mark_str name, Pretty.str (": " ^ Config.print_type value ^ " ="),
          Pretty.brk 1, Pretty.str (Config.print_value value)]
      end;
    val space = attribute_space ctxt;
    val configs =
      Name_Space.markup_entries verbose ctxt space
        (Symtab.dest (Configs.get (Proof_Context.theory_of ctxt)));
  in Pretty.writeln (Pretty.big_list "configuration options" (map prt configs)) end;


(* concrete syntax *)

local

val equals = Parse.$$$ "=";

fun scan_value (Config.Bool _) =
      equals -- Args.$$$ "false" >> K (Config.Bool false) ||
      equals -- Args.$$$ "true" >> K (Config.Bool true) ||
      Scan.succeed (Config.Bool true)
  | scan_value (Config.Int _) = equals |-- Parse.int >> Config.Int
  | scan_value (Config.Real _) = equals |-- Parse.real >> Config.Real
  | scan_value (Config.String _) = equals |-- Args.name >> Config.String;

fun scan_config thy config =
  let val config_type = Config.get_global thy config
  in scan_value config_type >> (K o Thm.declaration_attribute o K o Config.put_generic config) end;

fun register binding config thy =
  let val name = Sign.full_name thy binding in
    thy
    |> setup binding (Scan.lift (scan_config thy config) >> Morphism.form) "configuration option"
    |> Configs.map (Symtab.update (name, config))
  end;

fun declare make coerce binding default =
  let
    val name = Binding.name_of binding;
    val pos = Binding.pos_of binding;
    val config_value = Config.declare (name, pos) (make o default);
    val config = coerce config_value;
  in (config, register binding config_value) end;

in

fun register_config config =
  register (Binding.make (Config.name_of config, Config.pos_of config)) config;

val config_bool = declare Config.Bool Config.bool;
val config_int = declare Config.Int Config.int;
val config_real = declare Config.Real Config.real;
val config_string = declare Config.String Config.string;

end;


(* implicit setup *)

local

fun setup_config declare_config binding default =
  let
    val (config, setup) = declare_config binding default;
    val _ = Theory.setup setup;
  in config end;

in

val setup_config_bool = setup_config config_bool;
val setup_config_int = setup_config config_int;
val setup_config_string = setup_config config_string;
val setup_config_real = setup_config config_real;

end;


(* system options *)

local

fun declare_option coerce (name, pos) =
  let
    val config = Config.declare_option (name, pos);
  in (coerce config, register_config config) end;

fun setup_option coerce (name, pos) =
  let
    val config = Config.declare_option (name, pos);
    val _ = Theory.setup (register_config config);
  in coerce config end;

in

val option_bool = declare_option Config.bool;
val option_int = declare_option Config.int;
val option_real = declare_option Config.real;
val option_string = declare_option Config.string;

val setup_option_bool = setup_option Config.bool;
val setup_option_int = setup_option Config.int;
val setup_option_real = setup_option Config.real;
val setup_option_string = setup_option Config.string;

end;


(* theory setup *)

val _ = Theory.setup
 (setup @{binding tagged} (Scan.lift (Args.name -- Args.name) >> Thm.tag) "tagged theorem" #>
  setup @{binding untagged} (Scan.lift Args.name >> Thm.untag) "untagged theorem" #>
  setup @{binding kind} (Scan.lift Args.name >> Thm.kind) "theorem kind" #>
  setup @{binding THEN}
    (Scan.lift (Scan.optional (Args.bracks Parse.nat) 1) -- thm
      >> (fn (i, B) => Thm.rule_attribute [B] (fn _ => fn A => A RSN (i, B))))
    "resolution with rule" #>
  setup @{binding OF}
    (thms >> (fn Bs => Thm.rule_attribute Bs (fn _ => fn A => A OF Bs)))
    "rule resolved with facts" #>
  setup @{binding rename_abs}
    (Scan.lift (Scan.repeat (Args.maybe Args.name)) >> (fn vs =>
      Thm.rule_attribute [] (K (Drule.rename_bvars' vs))))
    "rename bound variables in abstractions" #>
  setup @{binding unfolded}
    (thms >> (fn ths =>
      Thm.rule_attribute ths (fn context => Local_Defs.unfold (Context.proof_of context) ths)))
    "unfolded definitions" #>
  setup @{binding folded}
    (thms >> (fn ths =>
      Thm.rule_attribute ths (fn context => Local_Defs.fold (Context.proof_of context) ths)))
    "folded definitions" #>
  setup @{binding consumes}
    (Scan.lift (Scan.optional Parse.int 1) >> Rule_Cases.consumes)
    "number of consumed facts" #>
  setup @{binding constraints}
    (Scan.lift Parse.nat >> Rule_Cases.constraints)
    "number of equality constraints" #>
  setup @{binding cases_open}
    (Scan.succeed Rule_Cases.cases_open)
    "rule with open parameters" #>
  setup @{binding case_names}
    (Scan.lift (Scan.repeat (Args.name --
      Scan.optional (Parse.$$$ "[" |-- Scan.repeat1 (Args.maybe Args.name) --| Parse.$$$ "]") []))
      >> (fn cs =>
        Rule_Cases.cases_hyp_names
          (map #1 cs)
          (map (map (the_default Rule_Cases.case_hypsN) o #2) cs)))
    "named rule cases" #>
  setup @{binding case_conclusion}
    (Scan.lift (Args.name -- Scan.repeat Args.name) >> Rule_Cases.case_conclusion)
    "named conclusion of rule cases" #>
  setup @{binding params}
    (Scan.lift (Parse.and_list1 (Scan.repeat Args.name)) >> Rule_Cases.params)
    "named rule parameters" #>
  setup @{binding rule_format}
    (Scan.lift (Args.mode "no_asm")
      >> (fn true => Object_Logic.rule_format_no_asm | false => Object_Logic.rule_format))
    "result put into canonical rule format" #>
  setup @{binding elim_format}
    (Scan.succeed (Thm.rule_attribute [] (K Tactic.make_elim)))
    "destruct rule turned into elimination rule format" #>
  setup @{binding no_vars}
    (Scan.succeed (Thm.rule_attribute [] (fn context => fn th =>
      let
        val ctxt = Variable.set_body false (Context.proof_of context);
        val ((_, [th']), _) = Variable.import true [th] ctxt;
      in th' end)))
    "imported schematic variables" #>
  setup @{binding atomize}
    (Scan.succeed Object_Logic.declare_atomize) "declaration of atomize rule" #>
  setup @{binding rulify}
    (Scan.succeed Object_Logic.declare_rulify) "declaration of rulify rule" #>
  setup @{binding rotated}
    (Scan.lift (Scan.optional Parse.int 1
      >> (fn n => Thm.rule_attribute [] (fn _ => rotate_prems n)))) "rotated theorem premises" #>
  setup @{binding defn}
    (add_del Local_Defs.defn_add Local_Defs.defn_del)
    "declaration of definitional transformations" #>
  setup @{binding abs_def}
    (Scan.succeed (Thm.rule_attribute [] (Local_Defs.abs_def_rule o Context.proof_of)))
    "abstract over free variables of definitional theorem" #>

  register_config Goal.quick_and_dirty_raw #>
  register_config Ast.trace_raw #>
  register_config Ast.stats_raw #>
  register_config Printer.show_brackets_raw #>
  register_config Printer.show_sorts_raw #>
  register_config Printer.show_types_raw #>
  register_config Printer.show_markup_raw #>
  register_config Printer.show_structs_raw #>
  register_config Printer.show_question_marks_raw #>
  register_config Syntax.ambiguity_warning_raw #>
  register_config Syntax.ambiguity_limit_raw #>
  register_config Syntax_Trans.eta_contract_raw #>
  register_config Name_Space.names_long_raw #>
  register_config Name_Space.names_short_raw #>
  register_config Name_Space.names_unique_raw #>
  register_config ML_Print_Depth.print_depth_raw #>
  register_config ML_Options.source_trace_raw #>
  register_config ML_Options.exception_trace_raw #>
  register_config ML_Options.exception_debugger_raw #>
  register_config ML_Options.debugger_raw #>
  register_config Proof_Context.show_abbrevs_raw #>
  register_config Goal_Display.goals_limit_raw #>
  register_config Goal_Display.show_main_goal_raw #>
  register_config Thm.show_consts_raw #>
  register_config Thm.show_hyps_raw #>
  register_config Thm.show_tags_raw #>
  register_config Pattern.unify_trace_failure_raw #>
  register_config Unify.trace_bound_raw #>
  register_config Unify.search_bound_raw #>
  register_config Unify.trace_simp_raw #>
  register_config Unify.trace_types_raw #>
  register_config Raw_Simplifier.simp_depth_limit_raw #>
  register_config Raw_Simplifier.simp_trace_depth_limit_raw #>
  register_config Raw_Simplifier.simp_debug_raw #>
  register_config Raw_Simplifier.simp_trace_raw #>
  register_config Local_Defs.unfold_abs_def_raw);


(* internal source *)

local

val internal = internal_name (Context.the_local_context ());

val consumes_name = internal "consumes";
val constraints_name = internal "constraints";
val cases_open_name = internal "cases_open";
val case_names_name = internal "case_names";
val case_conclusion_name = internal "case_conclusion";

fun make_string s = Token.make_string (s, Position.none);

in

fun consumes i = consumes_name :: Token.make_int i;
fun constraints i = constraints_name :: Token.make_int i;
val cases_open = [cases_open_name];
fun case_names names = case_names_name :: map make_string names;
fun case_conclusion (name, names) = case_conclusion_name :: map make_string (name :: names);

end;

end;