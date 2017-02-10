(*  Title:      Pure/Isar/method.ML
    Author:     Markus Wenzel, TU Muenchen

Isar proof methods.
*)

infix 1 CONTEXT_THEN_ALL_NEW;

signature BASIC_METHOD =
sig
  type context_state = Proof.context * thm
  type context_tactic = context_state -> context_state Seq.result Seq.seq
  val CONTEXT_CASES: Rule_Cases.cases -> tactic -> context_tactic
  val CONTEXT_SUBGOAL: (term * int -> context_tactic) -> int -> context_tactic
  val CONTEXT_THEN_ALL_NEW: (int -> context_tactic) * (int -> tactic) -> int -> context_tactic
end;

signature METHOD =
sig
  include BASIC_METHOD
  type method = thm list -> context_tactic
  val CONTEXT: Proof.context -> thm Seq.seq -> context_state Seq.result Seq.seq
  val CONTEXT_TACTIC: tactic -> context_tactic
  val NO_CONTEXT_TACTIC: Proof.context -> context_tactic -> tactic
  val CONTEXT_METHOD: (thm list -> context_tactic) -> method
  val METHOD: (thm list -> tactic) -> method
  val fail: method
  val succeed: method
  val insert_tac: Proof.context -> thm list -> int -> tactic
  val insert: thm list -> method
  val SIMPLE_METHOD: tactic -> method
  val SIMPLE_METHOD': (int -> tactic) -> method
  val SIMPLE_METHOD'': ((int -> tactic) -> tactic) -> (int -> tactic) -> method
  val goal_cases_tac: string list -> context_tactic
  val cheating: bool -> method
  val intro: Proof.context -> thm list -> method
  val elim: Proof.context -> thm list -> method
  val unfold: thm list -> Proof.context -> method
  val fold: thm list -> Proof.context -> method
  val atomize: bool -> Proof.context -> method
  val this: Proof.context -> method
  val fact: thm list -> Proof.context -> method
  val assm_tac: Proof.context -> int -> tactic
  val all_assm_tac: Proof.context -> tactic
  val assumption: Proof.context -> method
  val rule_trace: bool Config.T
  val trace: Proof.context -> thm list -> unit
  val rule_tac: Proof.context -> thm list -> thm list -> int -> tactic
  val some_rule_tac: Proof.context -> thm list -> thm list -> int -> tactic
  val intros_tac: Proof.context -> thm list -> thm list -> tactic
  val try_intros_tac: Proof.context -> thm list -> thm list -> tactic
  val rule: Proof.context -> thm list -> method
  val erule: Proof.context -> int -> thm list -> method
  val drule: Proof.context -> int -> thm list -> method
  val frule: Proof.context -> int -> thm list -> method
  val set_tactic: (morphism -> thm list -> tactic) -> Context.generic -> Context.generic
  val clean_facts: thm list -> thm list
  val set_facts: thm list -> Proof.context -> Proof.context
  val get_facts: Proof.context -> thm list
  type combinator_info
  val no_combinator_info: combinator_info
  datatype combinator = Then | Then_All_New | Orelse | Try | Repeat1 | Select_Goals of int
  datatype text =
    Source of Token.src |
    Basic of Proof.context -> method |
    Combinator of combinator_info * combinator * text list
  val map_source: (Token.src -> Token.src) -> text -> text
  val primitive_text: (Proof.context -> thm -> thm) -> text
  val succeed_text: text
  val standard_text: text
  val this_text: text
  val done_text: text
  val sorry_text: bool -> text
  val finish_text: text option * bool -> text
  val print_methods: bool -> Proof.context -> unit
  val check_name: Proof.context -> xstring * Position.T -> string
  val check_src: Proof.context -> Token.src -> Token.src
  val check_text: Proof.context -> text -> text
  val checked_text: text -> bool
  val method_syntax: (Proof.context -> method) context_parser ->
    Token.src -> Proof.context -> method
  val setup: binding -> (Proof.context -> method) context_parser -> string -> theory -> theory
  val local_setup: binding -> (Proof.context -> method) context_parser -> string ->
    local_theory -> local_theory
  val method_setup: bstring * Position.T -> Input.source -> string -> local_theory -> local_theory
  val method: Proof.context -> Token.src -> Proof.context -> method
  val method_closure: Proof.context -> Token.src -> Token.src
  val closure: bool Config.T
  val method_cmd: Proof.context -> Token.src -> Proof.context -> method
  val detect_closure_state: thm -> bool
  val STATIC: (unit -> unit) -> context_tactic
  val RUNTIME: context_tactic -> context_tactic
  val sleep: Time.time -> context_tactic
  val evaluate: text -> Proof.context -> method
  val evaluate_runtime: text -> Proof.context -> method
  type modifier = {init: Proof.context -> Proof.context, attribute: attribute, pos: Position.T}
  val modifier: attribute -> Position.T -> modifier
  val old_section_parser: bool Config.T
  val sections: modifier parser list -> unit context_parser
  type text_range = text * Position.range
  val text: text_range option -> text option
  val position: text_range option -> Position.T
  val reports_of: text_range -> Position.report list
  val report: text_range -> unit
  val parser: int -> text_range parser
  val parse: text_range parser
  val read: Proof.context -> Token.src -> text
  val read_closure: Proof.context -> Token.src -> text * Token.src
  val read_closure_input: Proof.context -> Input.source -> text * Token.src
  val text_closure: text context_parser
end;

structure Method: METHOD =
struct

(** proof methods **)

(* tactics with proof context / cases *)

type context_state = Proof.context * thm;
type context_tactic = context_state -> context_state Seq.result Seq.seq;

fun CONTEXT ctxt : thm Seq.seq -> context_state Seq.result Seq.seq =
  Seq.map (Seq.Result o pair ctxt);

fun CONTEXT_TACTIC tac : context_tactic =
  fn (ctxt, st) => CONTEXT ctxt (tac st);

fun NO_CONTEXT_TACTIC ctxt (tac: context_tactic) st =
  tac (ctxt, st) |> Seq.filter_results |> Seq.map snd;

fun CONTEXT_CASES cases tac : context_tactic =
  fn (ctxt, st) => CONTEXT (Proof_Context.update_cases cases ctxt) (tac st);

fun CONTEXT_SUBGOAL tac i : context_tactic =
  fn (ctxt, st) =>
    (case try Logic.nth_prem (i, Thm.prop_of st) of
      SOME goal => tac (goal, i) (ctxt, st)
    | NONE => Seq.empty);

fun (tac1 CONTEXT_THEN_ALL_NEW tac2) i : context_tactic =
  fn (ctxt, st) =>
    (ctxt, st) |> tac1 i |> Seq.maps_results (fn (ctxt', st') =>
      CONTEXT ctxt' ((Seq.INTERVAL tac2 i (i + Thm.nprems_of st' - Thm.nprems_of st)) st'));


(* type method *)

type method = thm list -> context_tactic;

fun CONTEXT_METHOD tac : method =
  fn facts => CONTEXT_TACTIC (ALLGOALS Goal.conjunction_tac) #> Seq.maps_results (tac facts);

fun METHOD tac : method =
  fn facts => CONTEXT_TACTIC (ALLGOALS Goal.conjunction_tac THEN tac facts);

val fail = METHOD (K no_tac);
val succeed = METHOD (K all_tac);


(* insert facts *)

fun insert_tac _ [] _ = all_tac
  | insert_tac ctxt facts i =
      EVERY (map (fn r => resolve_tac ctxt [Drule.forall_intr_vars r COMP_INCR revcut_rl] i) facts);

fun insert thms =
  CONTEXT_METHOD (fn _ => fn (ctxt, st) => st |> ALLGOALS (insert_tac ctxt thms) |> CONTEXT ctxt);


fun SIMPLE_METHOD tac =
  CONTEXT_METHOD (fn facts => fn (ctxt, st) =>
    st |> (ALLGOALS (insert_tac ctxt facts) THEN tac) |> CONTEXT ctxt);

fun SIMPLE_METHOD'' quant tac =
  CONTEXT_METHOD (fn facts => fn (ctxt, st) =>
    st |> quant (insert_tac ctxt facts THEN' tac) |> CONTEXT ctxt);

val SIMPLE_METHOD' = SIMPLE_METHOD'' HEADGOAL;


(* goals as cases *)

fun goal_cases_tac case_names : context_tactic =
  fn (ctxt, st) =>
    let
      val cases =
        (if null case_names then map string_of_int (1 upto Thm.nprems_of st) else case_names)
        |> map (rpair [] o rpair [])
        |> Rule_Cases.make_common ctxt (Thm.prop_of (Rule_Cases.internalize_params st));
    in CONTEXT_CASES cases all_tac (ctxt, st) end;


(* cheating *)

fun cheating int = CONTEXT_METHOD (fn _ => fn (ctxt, st) =>
  if int orelse Config.get ctxt quick_and_dirty then
    CONTEXT ctxt (ALLGOALS (Skip_Proof.cheat_tac ctxt) st)
  else error "Cheating requires quick_and_dirty mode!");


(* unfold intro/elim rules *)

fun intro ctxt ths = SIMPLE_METHOD' (CHANGED_PROP o REPEAT_ALL_NEW (match_tac ctxt ths));
fun elim ctxt ths = SIMPLE_METHOD' (CHANGED_PROP o REPEAT_ALL_NEW (ematch_tac ctxt ths));


(* unfold/fold definitions *)

fun unfold_meth ths ctxt = SIMPLE_METHOD (CHANGED_PROP (Local_Defs.unfold_tac ctxt ths));
fun fold_meth ths ctxt = SIMPLE_METHOD (CHANGED_PROP (Local_Defs.fold_tac ctxt ths));


(* atomize rule statements *)

fun atomize false ctxt =
      SIMPLE_METHOD' (CHANGED_PROP o Object_Logic.atomize_prems_tac ctxt)
  | atomize true ctxt =
      CONTEXT_TACTIC o K (HEADGOAL (CHANGED_PROP o Object_Logic.full_atomize_tac ctxt));


(* this -- resolve facts directly *)

fun this ctxt = METHOD (EVERY o map (HEADGOAL o resolve_tac ctxt o single));


(* fact -- composition by facts from context *)

fun fact [] ctxt = SIMPLE_METHOD' (Proof_Context.some_fact_tac ctxt)
  | fact rules ctxt = SIMPLE_METHOD' (Proof_Context.fact_tac ctxt rules);


(* assumption *)

local

fun cond_rtac ctxt cond rule = SUBGOAL (fn (prop, i) =>
  if cond (Logic.strip_assums_concl prop)
  then resolve_tac ctxt [rule] i else no_tac);

in

fun assm_tac ctxt =
  assume_tac ctxt APPEND'
  Goal.assume_rule_tac ctxt APPEND'
  cond_rtac ctxt (can Logic.dest_equals) Drule.reflexive_thm APPEND'
  cond_rtac ctxt (can Logic.dest_term) Drule.termI;

fun all_assm_tac ctxt =
  let
    fun tac i st =
      if i > Thm.nprems_of st then all_tac st
      else ((assm_tac ctxt i THEN tac i) ORELSE tac (i + 1)) st;
  in tac 1 end;

fun assumption ctxt = METHOD (HEADGOAL o
  (fn [] => assm_tac ctxt
    | [fact] => solve_tac ctxt [fact]
    | _ => K no_tac));

fun finish immed ctxt =
  METHOD (K ((if immed then all_assm_tac ctxt else all_tac) THEN flexflex_tac ctxt));

end;


(* rule etc. -- single-step refinements *)

val rule_trace = Attrib.setup_config_bool @{binding rule_trace} (fn _ => false);

fun trace ctxt rules =
  if Config.get ctxt rule_trace andalso not (null rules) then
    Pretty.big_list "rules:" (map (Thm.pretty_thm_item ctxt) rules)
    |> Pretty.string_of |> tracing
  else ();

local

fun gen_rule_tac tac ctxt rules facts =
  (fn i => fn st =>
    if null facts then tac ctxt rules i st
    else
      Seq.maps (fn rule => (tac ctxt o single) rule i st)
        (Drule.multi_resolves (SOME ctxt) facts rules))
  THEN_ALL_NEW Goal.norm_hhf_tac ctxt;

fun gen_arule_tac tac ctxt j rules facts =
  EVERY' (gen_rule_tac tac ctxt rules facts :: replicate j (assume_tac ctxt));

fun gen_some_rule_tac tac ctxt arg_rules facts = SUBGOAL (fn (goal, i) =>
  let
    val rules =
      if not (null arg_rules) then arg_rules
      else flat (Context_Rules.find_rules ctxt false facts goal);
  in trace ctxt rules; tac ctxt rules facts i end);

fun meth tac x y = METHOD (HEADGOAL o tac x y);
fun meth' tac x y z = METHOD (HEADGOAL o tac x y z);

in

val rule_tac = gen_rule_tac resolve_tac;
val rule = meth rule_tac;
val some_rule_tac = gen_some_rule_tac rule_tac;
val some_rule = meth some_rule_tac;

val erule = meth' (gen_arule_tac eresolve_tac);
val drule = meth' (gen_arule_tac dresolve_tac);
val frule = meth' (gen_arule_tac forward_tac);

end;


(* intros_tac -- pervasive search spanned by intro rules *)

fun gen_intros_tac goals ctxt intros facts =
  goals (insert_tac ctxt facts THEN'
      REPEAT_ALL_NEW (resolve_tac ctxt intros))
    THEN Tactic.distinct_subgoals_tac;

val intros_tac = gen_intros_tac ALLGOALS;
val try_intros_tac = gen_intros_tac TRYALL;



(** method syntax **)

(* context data *)

structure Data = Generic_Data
(
  type T =
   {methods: ((Token.src -> Proof.context -> method) * string) Name_Space.table,
    ml_tactic: (morphism -> thm list -> tactic) option,
    facts: thm list option};
  val empty : T =
    {methods = Name_Space.empty_table "method", ml_tactic = NONE, facts = NONE};
  val extend = I;
  fun merge
    ({methods = methods1, ml_tactic = ml_tactic1, facts = facts1},
     {methods = methods2, ml_tactic = ml_tactic2, facts = facts2}) : T =
    {methods = Name_Space.merge_tables (methods1, methods2),
     ml_tactic = merge_options (ml_tactic1, ml_tactic2),
     facts = merge_options (facts1, facts2)};
);

fun map_data f = Data.map (fn {methods, ml_tactic, facts} =>
  let val (methods', ml_tactic', facts') = f (methods, ml_tactic, facts)
  in {methods = methods', ml_tactic = ml_tactic', facts = facts'} end);

val get_methods = #methods o Data.get;

val ops_methods =
 {get_data = get_methods,
  put_data = fn methods => map_data (fn (_, ml_tactic, facts) => (methods, ml_tactic, facts))};


(* ML tactic *)

fun set_tactic ml_tactic = map_data (fn (methods, _, facts) => (methods, SOME ml_tactic, facts));

fun the_tactic context =
  (case #ml_tactic (Data.get context) of
    SOME tac => tac
  | NONE => raise Fail "Undefined ML tactic");

val parse_tactic =
  Scan.state :|-- (fn context =>
    Scan.lift (Args.text_declaration (fn source =>
      let
        val context' = context |>
          ML_Context.expression (Input.range_of source)
            "tactic" "Morphism.morphism -> thm list -> tactic"
            "Method.set_tactic tactic"
            (ML_Lex.read "fn morphism: Morphism.morphism => fn facts: thm list =>" @
             ML_Lex.read_source false source);
        val tac = the_tactic context';
      in
        fn phi => set_tactic (fn _ => Context.setmp_generic_context (SOME context) (tac phi))
      end)) >> (fn decl => Morphism.form (the_tactic (Morphism.form decl context))));


(* method facts *)

val clean_facts = filter_out Thm.is_dummy;

fun set_facts facts =
  (Context.proof_map o map_data)
    (fn (methods, ml_tactic, _) => (methods, ml_tactic, SOME (clean_facts facts)));

val get_facts_generic = these o #facts o Data.get;
val get_facts = get_facts_generic o Context.Proof;

val _ =
  Theory.setup
    (Global_Theory.add_thms_dynamic (Binding.make ("method_facts", @{here}), get_facts_generic));


(* method text *)

datatype combinator_info = Combinator_Info of {keywords: Position.T list};
fun combinator_info keywords = Combinator_Info {keywords = keywords};
val no_combinator_info = combinator_info [];

datatype combinator = Then | Then_All_New | Orelse | Try | Repeat1 | Select_Goals of int;

datatype text =
  Source of Token.src |
  Basic of Proof.context -> method |
  Combinator of combinator_info * combinator * text list;

fun map_source f (Source src) = Source (f src)
  | map_source _ (Basic meth) = Basic meth
  | map_source f (Combinator (info, comb, txts)) = Combinator (info, comb, map (map_source f) txts);

fun primitive_text r = Basic (SIMPLE_METHOD o PRIMITIVE o r);
val succeed_text = Basic (K succeed);
val standard_text = Source (Token.make_src ("standard", Position.none) []);
val this_text = Basic this;
val done_text = Basic (K (SIMPLE_METHOD all_tac));
fun sorry_text int = Basic (fn _ => cheating int);

fun finish_text (NONE, immed) = Basic (finish immed)
  | finish_text (SOME txt, immed) =
      Combinator (no_combinator_info, Then, [txt, Basic (finish immed)]);


(* method definitions *)

fun print_methods verbose ctxt =
  let
    val meths = get_methods (Context.Proof ctxt);
    fun prt_meth (name, (_, "")) = Pretty.mark_str name
      | prt_meth (name, (_, comment)) =
          Pretty.block
            (Pretty.mark_str name :: Pretty.str ":" :: Pretty.brk 2 :: Pretty.text comment);
  in
    [Pretty.big_list "methods:" (map prt_meth (Name_Space.markup_table verbose ctxt meths))]
    |> Pretty.writeln_chunks
  end;


(* define *)

fun define_global binding meth comment =
  Entity.define_global ops_methods binding (meth, comment);

fun define binding meth comment =
  Entity.define ops_methods binding (meth, comment);


(* check *)

fun check_name ctxt =
  let val context = Context.Proof ctxt
  in #1 o Name_Space.check context (get_methods context) end;

fun check_src ctxt =
  #1 o Token.check_src ctxt (get_methods o Context.Proof);

fun check_text ctxt (Source src) = Source (check_src ctxt src)
  | check_text _ (Basic m) = Basic m
  | check_text ctxt (Combinator (x, y, body)) = Combinator (x, y, map (check_text ctxt) body);

fun checked_text (Source src) = Token.checked_src src
  | checked_text (Basic _) = true
  | checked_text (Combinator (_, _, body)) = forall checked_text body;


(* method setup *)

fun method_syntax scan src ctxt : method =
  let val (m, ctxt') = Token.syntax scan src ctxt in m ctxt' end;

fun setup binding scan comment = define_global binding (method_syntax scan) comment #> snd;
fun local_setup binding scan comment = define binding (method_syntax scan) comment #> snd;

fun method_setup name source comment =
  ML_Lex.read_source false source
  |> ML_Context.expression (Input.range_of source) "parser"
    "(Proof.context -> Proof.method) context_parser"
    ("Context.map_proof (Method.local_setup " ^ ML_Syntax.atomic (ML_Syntax.make_binding name) ^
      " parser " ^ ML_Syntax.print_string comment ^ ")")
  |> Context.proof_map;


(* prepare methods *)

fun method ctxt =
  let val table = get_methods (Context.Proof ctxt)
  in fn src => #1 (Name_Space.get table (#1 (Token.name_of_src src))) src end;

fun method_closure ctxt src =
  let
    val src' = map Token.init_assignable src;
    val ctxt' = Context_Position.not_really ctxt;
    val _ = Seq.pull (method ctxt' src' ctxt' [] (ctxt', Goal.protect 0 Drule.dummy_thm));
  in map Token.closure src' end;

val closure = Config.bool (Config.declare ("Method.closure", @{here}) (K (Config.Bool true)));

fun method_cmd ctxt =
  check_src ctxt #>
  Config.get ctxt closure ? method_closure ctxt #>
  method ctxt;


(* static vs. runtime state *)

fun detect_closure_state st =
  (case try Logic.dest_term (Thm.concl_of (perhaps (try Goal.conclude) st)) of
    NONE => false
  | SOME t => Term.is_dummy_pattern t);

fun STATIC test : context_tactic =
  fn (ctxt, st) =>
    if detect_closure_state st then (test (); Seq.single (Seq.Result (ctxt, st))) else Seq.empty;

fun RUNTIME (tac: context_tactic) (ctxt, st) =
  if detect_closure_state st then Seq.empty else tac (ctxt, st);

fun sleep t = RUNTIME (fn ctxt_st => (OS.Process.sleep t; Seq.single (Seq.Result ctxt_st)));


(* evaluate method text *)

local

val op THEN = Seq.THEN;

fun BYPASS_CONTEXT (tac: tactic) =
  fn result =>
    (case result of
      Seq.Error _ => Seq.single result
    | Seq.Result (ctxt, st) => tac st |> CONTEXT ctxt);

val preparation = BYPASS_CONTEXT (ALLGOALS Goal.conjunction_tac);

fun RESTRICT_GOAL i n method =
  BYPASS_CONTEXT (PRIMITIVE (Goal.restrict i n)) THEN
  method THEN
  BYPASS_CONTEXT (PRIMITIVE (Goal.unrestrict i));

fun SELECT_GOAL method i = RESTRICT_GOAL i 1 method;

fun (method1 THEN_ALL_NEW method2) i (result : context_state Seq.result) =
  (case result of
    Seq.Error _ => Seq.single result
  | Seq.Result (_, st) =>
      result |> method1 i
      |> Seq.maps (fn result' =>
          (case result' of
            Seq.Error _ => Seq.single result'
          | Seq.Result (_, st') =>
              result' |> Seq.INTERVAL method2 i (i + Thm.nprems_of st' - Thm.nprems_of st))))

fun COMBINATOR1 comb [meth] = comb meth
  | COMBINATOR1 _ _ = raise Fail "Method combinator requires exactly one argument";

fun combinator Then = Seq.EVERY
  | combinator Then_All_New =
      (fn [] => Seq.single
        | methods =>
            preparation THEN (foldl1 (op THEN_ALL_NEW) (map SELECT_GOAL methods) 1))
  | combinator Orelse = Seq.FIRST
  | combinator Try = COMBINATOR1 Seq.TRY
  | combinator Repeat1 = COMBINATOR1 Seq.REPEAT1
  | combinator (Select_Goals n) =
      COMBINATOR1 (fn method => preparation THEN RESTRICT_GOAL 1 n method);

in

fun evaluate text ctxt0 facts =
  let
    val ctxt = set_facts facts ctxt0;
    fun eval0 m = Seq.single #> Seq.maps_results (m facts);
    fun eval (Basic m) = eval0 (m ctxt)
      | eval (Source src) = eval0 (method_cmd ctxt src ctxt)
      | eval (Combinator (_, c, txts)) = combinator c (map eval txts);
  in eval text o Seq.Result end;

end;

fun evaluate_runtime text ctxt =
  let
    val text' =
      text |> (map_source o map o Token.map_facts)
        (fn SOME name =>
              (case Proof_Context.lookup_fact ctxt name of
                SOME {dynamic = true, thms} => K thms
              | _ => I)
          | NONE => I);
    val ctxt' = Config.put closure false ctxt;
  in fn facts => RUNTIME (fn st => evaluate text' ctxt' facts st) end;



(** concrete syntax **)

(* type modifier *)

type modifier =
  {init: Proof.context -> Proof.context, attribute: attribute, pos: Position.T};

fun modifier attribute pos : modifier = {init = I, attribute = attribute, pos = pos};


(* sections *)

val old_section_parser =
  Config.bool (Config.declare ("Method.old_section_parser", @{here}) (K (Config.Bool false)));

local

fun thms ss =
  Scan.repeats (Scan.unless (Scan.lift (Scan.first ss)) Attrib.multi_thm);

fun app {init, attribute, pos = _} ths context =
  fold_map (Thm.apply_attribute attribute) ths (Context.map_proof init context);

fun section ss = Scan.depend (fn context => (Scan.first ss -- Scan.pass context (thms ss)) :|--
  (fn (m, ths) => Scan.succeed (swap (app m ths context))));

in

fun old_sections ss = Scan.repeat (section ss) >> K ();

end;

local

fun sect (modifier : modifier parser) = Scan.depend (fn context =>
  Scan.ahead Parse.not_eof -- Scan.trace modifier -- Scan.repeat (Scan.unless modifier Parse.thm)
    >> (fn ((tok0, ({init, attribute, pos}, modifier_toks)), xthms) =>
      let
        val decl =
          (case Token.get_value tok0 of
            SOME (Token.Declaration decl) => decl
          | _ =>
              let
                val ctxt = Context.proof_of context;
                val prep_att = Attrib.check_src ctxt #> map (Token.assign NONE);
                val thms =
                  map (fn (a, bs) => (Proof_Context.get_fact ctxt a, map prep_att bs)) xthms;
                val facts =
                  Attrib.partial_evaluation ctxt [((Binding.name "dummy", []), thms)]
                  |> map (fn (_, bs) => ((Binding.empty, [Attrib.internal (K attribute)]), bs));

                fun decl phi =
                  Context.mapping I init #>
                  Attrib.generic_notes "" (Attrib.transform_facts phi facts) #> snd;

                val modifier_report =
                  (#1 (Token.range_of modifier_toks),
                    Markup.properties (Position.def_properties_of pos)
                      (Markup.entity Markup.method_modifierN ""));
                val _ =
                  Context_Position.reports ctxt (modifier_report :: Token.reports_of_value tok0);
                val _ = Token.assign (SOME (Token.Declaration decl)) tok0;
              in decl end);
      in (Morphism.form decl context, decl) end));

in

fun sections ss =
  Args.context :|-- (fn ctxt =>
    if Config.get ctxt old_section_parser then old_sections ss
    else Scan.repeat (sect (Scan.first ss)) >> K ());

end;


(* extra rule methods *)

fun xrule_meth meth =
  Scan.lift (Scan.optional (Args.parens Parse.nat) 0) -- Attrib.thms >>
  (fn (n, ths) => fn ctxt => meth ctxt n ths);


(* text range *)

type text_range = text * Position.range;

fun text NONE = NONE
  | text (SOME (txt, _)) = SOME txt;

fun position NONE = Position.none
  | position (SOME (_, (pos, _))) = pos;


(* reports *)

local

fun keyword_positions (Source _) = []
  | keyword_positions (Basic _) = []
  | keyword_positions (Combinator (Combinator_Info {keywords}, _, texts)) =
      keywords @ maps keyword_positions texts;

in

fun reports_of ((text, (pos, _)): text_range) =
  (pos, Markup.language_method) ::
    maps (fn p => map (pair p) (Markup.keyword3 :: Completion.suppress_abbrevs ""))
      (keyword_positions text);

val report = Position.reports o reports_of;

end;


(* parser *)

local

fun is_symid_meth s =
  s <> "|" andalso s <> "?" andalso s <> "+" andalso Token.ident_or_symbolic s;

in

fun parser pri =
  let
    val meth_name = Parse.token Parse.name;

    fun meth5 x =
     (meth_name >> (Source o single) ||
      Scan.ahead Parse.cartouche |-- Parse.not_eof >> (fn tok =>
        Source (Token.make_src ("cartouche", Position.none) [tok])) ||
      Parse.$$$ "(" |-- Parse.!!! (meth0 --| Parse.$$$ ")")) x
    and meth4 x =
     (meth5 -- Parse.position (Parse.$$$ "?")
        >> (fn (m, (_, pos)) => Combinator (combinator_info [pos], Try, [m])) ||
      meth5 -- Parse.position (Parse.$$$ "+")
        >> (fn (m, (_, pos)) => Combinator (combinator_info [pos], Repeat1, [m])) ||
      meth5 -- (Parse.position (Parse.$$$ "[") --
          Scan.optional Parse.nat 1 -- Parse.position (Parse.$$$ "]"))
        >> (fn (m, (((_, pos1), n), (_, pos2))) =>
            Combinator (combinator_info [pos1, pos2], Select_Goals n, [m])) ||
      meth5) x
    and meth3 x =
     (meth_name ::: Parse.args1 is_symid_meth >> Source ||
      meth4) x
    and meth2 x =
      (Parse.enum1_positions "," meth3
        >> (fn ([m], _) => m | (ms, ps) => Combinator (combinator_info ps, Then, ms))) x
    and meth1 x =
      (Parse.enum1_positions ";" meth2
        >> (fn ([m], _) => m | (ms, ps) => Combinator (combinator_info ps, Then_All_New, ms))) x
    and meth0 x =
      (Parse.enum1_positions "|" meth1
        >> (fn ([m], _) => m | (ms, ps) => Combinator (combinator_info ps, Orelse, ms))) x;

    val meth =
      nth [meth0, meth1, meth2, meth3, meth4, meth5] pri
        handle General.Subscript => raise Fail ("Bad method parser priority " ^ string_of_int pri);
  in Scan.trace meth >> (fn (m, toks) => (m, Token.range_of toks)) end;

val parse = parser 4;

end;


(* read method text *)

fun read ctxt src =
  (case Scan.read Token.stopper (Parse.!!! (parser 0 --| Scan.ahead Parse.eof)) src of
    SOME (text, range) =>
      if checked_text text then text
      else (report (text, range); check_text ctxt text)
  | NONE => error ("Failed to parse method" ^ Position.here (#1 (Token.range_of src))));

fun read_closure ctxt src0 =
  let
    val src1 = map Token.init_assignable src0;
    val text = read ctxt src1 |> map_source (method_closure ctxt);
    val src2 = map Token.closure src1;
  in (text, src2) end;

fun read_closure_input ctxt =
  Input.source_explode #>
  Token.read_no_commands (Thy_Header.get_keywords' ctxt) (Scan.one Token.not_eof) #>
  read_closure ctxt;

val text_closure =
  Args.context -- Scan.lift (Parse.token Parse.text) >> (fn (ctxt, tok) =>
    (case Token.get_value tok of
      SOME (Token.Source src) => read ctxt src
    | _ =>
        let
          val (text, src) = read_closure_input ctxt (Token.input_of tok);
          val _ = Token.assign (SOME (Token.Source src)) tok;
        in text end));


(* theory setup *)

val _ = Theory.setup
 (setup @{binding fail} (Scan.succeed (K fail)) "force failure" #>
  setup @{binding succeed} (Scan.succeed (K succeed)) "succeed" #>
  setup @{binding sleep} (Scan.lift Parse.real >> (fn s => fn _ => fn _ => sleep (seconds s)))
    "succeed after delay (in seconds)" #>
  setup @{binding "-"} (Scan.succeed (K (SIMPLE_METHOD all_tac)))
    "insert current facts, nothing else" #>
  setup @{binding goal_cases} (Scan.lift (Scan.repeat Args.name_token) >> (fn names => fn _ =>
    CONTEXT_METHOD (fn _ => fn (ctxt, st) =>
      (case drop (Thm.nprems_of st) names of
        [] => NONE
      | bad =>
          if detect_closure_state st then NONE
          else
            SOME (fn () => ("Excessive case name(s): " ^ commas_quote (map Token.content_of bad) ^
              Position.here (#1 (Token.range_of bad)))))
      |> (fn SOME msg => Seq.single (Seq.Error msg)
           | NONE => goal_cases_tac (map Token.content_of names) (ctxt, st)))))
    "bind cases for goals" #>
  setup @{binding insert} (Attrib.thms >> (K o insert))
    "insert theorems, ignoring facts" #>
  setup @{binding intro} (Attrib.thms >> (fn ths => fn ctxt => intro ctxt ths))
    "repeatedly apply introduction rules" #>
  setup @{binding elim} (Attrib.thms >> (fn ths => fn ctxt => elim ctxt ths))
    "repeatedly apply elimination rules" #>
  setup @{binding unfold} (Attrib.thms >> unfold_meth) "unfold definitions" #>
  setup @{binding fold} (Attrib.thms >> fold_meth) "fold definitions" #>
  setup @{binding atomize} (Scan.lift (Args.mode "full") >> atomize)
    "present local premises as object-level statements" #>
  setup @{binding rule} (Attrib.thms >> (fn ths => fn ctxt => some_rule ctxt ths))
    "apply some intro/elim rule" #>
  setup @{binding erule} (xrule_meth erule) "apply rule in elimination manner (improper)" #>
  setup @{binding drule} (xrule_meth drule) "apply rule in destruct manner (improper)" #>
  setup @{binding frule} (xrule_meth frule) "apply rule in forward manner (improper)" #>
  setup @{binding this} (Scan.succeed this) "apply current facts as rules" #>
  setup @{binding fact} (Attrib.thms >> fact) "composition by facts from context" #>
  setup @{binding assumption} (Scan.succeed assumption)
    "proof by assumption, preferring facts" #>
  setup @{binding rename_tac} (Args.goal_spec -- Scan.lift (Scan.repeat1 Args.name) >>
    (fn (quant, xs) => K (SIMPLE_METHOD'' quant (rename_tac xs))))
    "rename parameters of goal" #>
  setup @{binding rotate_tac} (Args.goal_spec -- Scan.lift (Scan.optional Parse.int 1) >>
    (fn (quant, i) => K (SIMPLE_METHOD'' quant (rotate_tac i))))
      "rotate assumptions of goal" #>
  setup @{binding tactic} (parse_tactic >> (K o METHOD))
    "ML tactic as proof method" #>
  setup @{binding raw_tactic} (parse_tactic >> (fn tac => fn _ => CONTEXT_TACTIC o tac))
    "ML tactic as raw proof method" #>
  setup @{binding use}
    (Attrib.thms -- (Scan.lift (Parse.$$$ "in") |-- text_closure) >>
      (fn (thms, text) => fn ctxt => fn _ => evaluate_runtime text ctxt thms))
    "indicate method facts and context for method expression");


(*final declarations of this structure!*)
val unfold = unfold_meth;
val fold = fold_meth;

end;

structure Basic_Method: BASIC_METHOD = Method;
open Basic_Method;

val CONTEXT_METHOD = Method.CONTEXT_METHOD;
val METHOD = Method.METHOD;
val SIMPLE_METHOD = Method.SIMPLE_METHOD;
val SIMPLE_METHOD' = Method.SIMPLE_METHOD';
val SIMPLE_METHOD'' = Method.SIMPLE_METHOD'';
