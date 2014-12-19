(*  Title:      Pure/Isar/method.ML
    Author:     Markus Wenzel, TU Muenchen

Isar proof methods.
*)

signature METHOD =
sig
  type method
  val apply: (Proof.context -> method) -> Proof.context -> thm list -> cases_tactic
  val RAW_METHOD_CASES: (thm list -> cases_tactic) -> method
  val RAW_METHOD: (thm list -> tactic) -> method
  val METHOD_CASES: (thm list -> cases_tactic) -> method
  val METHOD: (thm list -> tactic) -> method
  val fail: method
  val succeed: method
  val insert_tac: thm list -> int -> tactic
  val insert: thm list -> method
  val insert_facts: method
  val SIMPLE_METHOD: tactic -> method
  val SIMPLE_METHOD': (int -> tactic) -> method
  val SIMPLE_METHOD'': ((int -> tactic) -> tactic) -> (int -> tactic) -> method
  val cheating: Proof.context -> bool -> method
  val intro: thm list -> method
  val elim: thm list -> method
  val unfold: thm list -> Proof.context -> method
  val fold: thm list -> Proof.context -> method
  val atomize: bool -> Proof.context -> method
  val this: method
  val fact: thm list -> Proof.context -> method
  val assm_tac: Proof.context -> int -> tactic
  val all_assm_tac: Proof.context -> tactic
  val assumption: Proof.context -> method
  val rule_trace: bool Config.T
  val trace: Proof.context -> thm list -> unit
  val rule_tac: Proof.context -> thm list -> thm list -> int -> tactic
  val some_rule_tac: Proof.context -> thm list -> thm list -> int -> tactic
  val intros_tac: thm list -> thm list -> tactic
  val try_intros_tac: thm list -> thm list -> tactic
  val rule: Proof.context -> thm list -> method
  val erule: Proof.context -> int -> thm list -> method
  val drule: Proof.context -> int -> thm list -> method
  val frule: Proof.context -> int -> thm list -> method
  val set_tactic: (thm list -> tactic) -> Proof.context -> Proof.context
  val tactic: Symbol_Pos.source -> Proof.context -> method
  val raw_tactic: Symbol_Pos.source -> Proof.context -> method
  type src = Args.src
  type combinator_info
  val no_combinator_info: combinator_info
  datatype text =
    Source of src |
    Basic of Proof.context -> method |
    Then of combinator_info * text list |
    Orelse of combinator_info * text list |
    Try of combinator_info * text |
    Repeat1 of combinator_info * text |
    Select_Goals of combinator_info * int * text
  val primitive_text: (Proof.context -> thm -> thm) -> text
  val succeed_text: text
  val default_text: text
  val this_text: text
  val done_text: text
  val sorry_text: bool -> text
  val finish_text: text option * bool -> text
  val print_methods: Proof.context -> unit
  val check_name: Proof.context -> xstring * Position.T -> string
  val method: Proof.context -> src -> Proof.context -> method
  val method_cmd: Proof.context -> src -> Proof.context -> method
  val setup: binding -> (Proof.context -> method) context_parser -> string -> theory -> theory
  val method_setup: bstring * Position.T -> Symbol_Pos.source -> string -> theory -> theory
  type modifier = (Proof.context -> Proof.context) * attribute
  val section: modifier parser list -> thm list context_parser
  val sections: modifier parser list -> thm list list context_parser
  type text_range = text * Position.range
  val text: text_range option -> text option
  val position: text_range option -> Position.T
  val reports_of: text_range -> Position.report list
  val report: text_range -> unit
  val parse: text_range parser
end;

structure Method: METHOD =
struct

(** proof methods **)

(* datatype method *)

datatype method = Meth of thm list -> cases_tactic;

fun apply meth ctxt = let val Meth m = meth ctxt in m end;

val RAW_METHOD_CASES = Meth;

fun RAW_METHOD tac = RAW_METHOD_CASES (NO_CASES o tac);

fun METHOD_CASES tac = RAW_METHOD_CASES (fn facts =>
  Seq.THEN (ALLGOALS Goal.conjunction_tac, tac facts));

fun METHOD tac = RAW_METHOD (fn facts => ALLGOALS Goal.conjunction_tac THEN tac facts);

val fail = METHOD (K no_tac);
val succeed = METHOD (K all_tac);


(* insert facts *)

local

fun cut_rule_tac rule =
  rtac (Drule.forall_intr_vars rule COMP_INCR revcut_rl);

in

fun insert_tac [] _ = all_tac
  | insert_tac facts i = EVERY (map (fn th => cut_rule_tac th i) facts);

val insert_facts = METHOD (ALLGOALS o insert_tac);
fun insert thms = METHOD (fn _ => ALLGOALS (insert_tac thms));

fun SIMPLE_METHOD tac = METHOD (fn facts => ALLGOALS (insert_tac facts) THEN tac);
fun SIMPLE_METHOD'' quant tac = METHOD (fn facts => quant (insert_tac facts THEN' tac));
val SIMPLE_METHOD' = SIMPLE_METHOD'' HEADGOAL;

end;


(* cheating *)

fun cheating ctxt int = METHOD (fn _ => fn st =>
  if int orelse Config.get ctxt quick_and_dirty then
    ALLGOALS Skip_Proof.cheat_tac st
  else error "Cheating requires quick_and_dirty mode!");


(* unfold intro/elim rules *)

fun intro ths = SIMPLE_METHOD' (CHANGED_PROP o REPEAT_ALL_NEW (match_tac ths));
fun elim ths = SIMPLE_METHOD' (CHANGED_PROP o REPEAT_ALL_NEW (ematch_tac ths));


(* unfold/fold definitions *)

fun unfold_meth ths ctxt = SIMPLE_METHOD (CHANGED_PROP (Local_Defs.unfold_tac ctxt ths));
fun fold_meth ths ctxt = SIMPLE_METHOD (CHANGED_PROP (Local_Defs.fold_tac ctxt ths));


(* atomize rule statements *)

fun atomize false ctxt =
      SIMPLE_METHOD' (CHANGED_PROP o Object_Logic.atomize_prems_tac ctxt)
  | atomize true ctxt =
      RAW_METHOD (K (HEADGOAL (CHANGED_PROP o Object_Logic.full_atomize_tac ctxt)));


(* this -- resolve facts directly *)

val this = METHOD (EVERY o map (HEADGOAL o rtac));


(* fact -- composition by facts from context *)

fun fact [] ctxt = SIMPLE_METHOD' (Proof_Context.some_fact_tac ctxt)
  | fact rules ctxt = SIMPLE_METHOD' (Proof_Context.fact_tac ctxt rules);


(* assumption *)

local

fun cond_rtac cond rule = SUBGOAL (fn (prop, i) =>
  if cond (Logic.strip_assums_concl prop)
  then rtac rule i else no_tac);

in

fun assm_tac ctxt =
  assume_tac APPEND'
  Goal.assume_rule_tac ctxt APPEND'
  cond_rtac (can Logic.dest_equals) Drule.reflexive_thm APPEND'
  cond_rtac (can Logic.dest_term) Drule.termI;

fun all_assm_tac ctxt =
  let
    fun tac i st =
      if i > Thm.nprems_of st then all_tac st
      else ((assm_tac ctxt i THEN tac i) ORELSE tac (i + 1)) st;
  in tac 1 end;

fun assumption ctxt = METHOD (HEADGOAL o
  (fn [] => assm_tac ctxt
    | [fact] => solve_tac [fact]
    | _ => K no_tac));

fun finish immed ctxt =
  METHOD (K ((if immed then all_assm_tac ctxt else all_tac) THEN flexflex_tac));

end;


(* rule etc. -- single-step refinements *)

val rule_trace = Attrib.setup_config_bool @{binding rule_trace} (fn _ => false);

fun trace ctxt rules =
  if Config.get ctxt rule_trace andalso not (null rules) then
    Pretty.big_list "rules:" (map (Display.pretty_thm_item ctxt) rules)
    |> Pretty.string_of |> tracing
  else ();

local

fun gen_rule_tac tac ctxt rules facts =
  (fn i => fn st =>
    if null facts then tac rules i st
    else Seq.maps (fn rule => (tac o single) rule i st) (Drule.multi_resolves facts rules))
  THEN_ALL_NEW Goal.norm_hhf_tac ctxt;

fun gen_arule_tac tac ctxt j rules facts =
  EVERY' (gen_rule_tac tac ctxt rules facts :: replicate j assume_tac);

fun gen_some_rule_tac tac ctxt arg_rules facts = SUBGOAL (fn (goal, i) =>
  let
    val rules =
      if not (null arg_rules) then arg_rules
      else flat (Context_Rules.find_rules false facts goal ctxt)
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

fun gen_intros_tac goals intros facts =
  goals (insert_tac facts THEN'
      REPEAT_ALL_NEW (resolve_tac intros))
    THEN Tactic.distinct_subgoals_tac;

val intros_tac = gen_intros_tac ALLGOALS;
val try_intros_tac = gen_intros_tac TRYALL;


(* ML tactics *)

structure ML_Tactic = Proof_Data
(
  type T = thm list -> tactic;
  fun init _ = undefined;
);

val set_tactic = ML_Tactic.put;

fun ml_tactic source ctxt =
  let
    val ctxt' = ctxt |> Context.proof_map
      (ML_Context.expression (#pos source)
        "fun tactic (facts: thm list) : tactic"
        "Context.map_proof (Method.set_tactic tactic)" (ML_Lex.read_source false source));
  in Context.setmp_thread_data (SOME (Context.Proof ctxt)) (ML_Tactic.get ctxt') end;

fun tactic source ctxt = METHOD (ml_tactic source ctxt);
fun raw_tactic source ctxt = RAW_METHOD (ml_tactic source ctxt);



(** method syntax **)

(* method text *)

type src = Args.src;

datatype combinator_info = Combinator_Info of {keywords: Position.T list};
fun combinator_info keywords = Combinator_Info {keywords = keywords};
val no_combinator_info = combinator_info [];

datatype text =
  Source of src |
  Basic of Proof.context -> method |
  Then of combinator_info * text list |
  Orelse of combinator_info * text list |
  Try of combinator_info * text |
  Repeat1 of combinator_info * text |
  Select_Goals of combinator_info * int * text;

fun primitive_text r = Basic (SIMPLE_METHOD o PRIMITIVE o r);
val succeed_text = Basic (K succeed);
val default_text = Source (Args.src ("default", Position.none) []);
val this_text = Basic (K this);
val done_text = Basic (K (SIMPLE_METHOD all_tac));
fun sorry_text int = Basic (fn ctxt => cheating ctxt int);

fun finish_text (NONE, immed) = Basic (finish immed)
  | finish_text (SOME txt, immed) = Then (no_combinator_info, [txt, Basic (finish immed)]);


(* method definitions *)

structure Methods = Theory_Data
(
  type T = ((src -> Proof.context -> method) * string) Name_Space.table;
  val empty : T = Name_Space.empty_table "method";
  val extend = I;
  fun merge data : T = Name_Space.merge_tables data;
);

val get_methods = Methods.get o Proof_Context.theory_of;

fun print_methods ctxt =
  let
    val meths = get_methods ctxt;
    fun prt_meth (name, (_, "")) = Pretty.mark_str name
      | prt_meth (name, (_, comment)) =
          Pretty.block
            (Pretty.mark_str name :: Pretty.str ":" :: Pretty.brk 2 :: Pretty.text comment);
  in
    [Pretty.big_list "methods:" (map prt_meth (Name_Space.markup_table ctxt meths))]
    |> Pretty.writeln_chunks
  end;

fun add_method name meth comment thy = thy
  |> Methods.map (Name_Space.define (Context.Theory thy) true (name, (meth, comment)) #> snd);


(* check *)

fun check_name ctxt = #1 o Name_Space.check (Context.Proof ctxt) (get_methods ctxt);
fun check_src ctxt src = Args.check_src ctxt (get_methods ctxt) src;


(* get methods *)

fun method ctxt =
  let val table = get_methods ctxt
  in fn src => #1 (Name_Space.get table (#1 (Args.name_of_src src))) src end;

fun method_closure ctxt0 src0 =
  let
    val (src1, meth) = check_src ctxt0 src0;
    val src2 = Args.init_assignable src1;
    val ctxt = Context_Position.not_really ctxt0;
    val _ = Seq.pull (apply (method ctxt src2) ctxt [] (Goal.protect 0 Drule.dummy_thm));
  in Args.closure src2 end;

fun method_cmd ctxt = method ctxt o method_closure ctxt;


(* method setup *)

fun setup name scan =
  add_method name
    (fn src => fn ctxt => let val (m, ctxt') = Args.syntax scan src ctxt in m ctxt' end);

fun method_setup name source cmt =
  Context.theory_map (ML_Context.expression (#pos source)
    "val (name, scan, comment): binding * (Proof.context -> Proof.method) context_parser * string"
    "Context.map_theory (Method.setup name scan comment)"
    (ML_Lex.read Position.none ("(" ^ ML_Syntax.make_binding name ^ ", ") @
      ML_Lex.read_source false source @
      ML_Lex.read Position.none (", " ^ ML_Syntax.print_string cmt ^ ")")));



(** concrete syntax **)

(* sections *)

type modifier = (Proof.context -> Proof.context) * attribute;

local

fun thms ss = Scan.repeat (Scan.unless (Scan.lift (Scan.first ss)) Attrib.multi_thm) >> flat;
fun app (f, att) ths context = fold_map (Thm.apply_attribute att) ths (Context.map_proof f context);

in

fun section ss = Scan.depend (fn context => (Scan.first ss -- Scan.pass context (thms ss)) :|--
  (fn (m, ths) => Scan.succeed (swap (app m ths context))));

fun sections ss = Scan.repeat (section ss);

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
  | keyword_positions (Then (Combinator_Info {keywords}, texts)) =
      keywords @ maps keyword_positions texts
  | keyword_positions (Orelse (Combinator_Info {keywords}, texts)) =
      keywords @ maps keyword_positions texts
  | keyword_positions (Try (Combinator_Info {keywords}, text)) =
      keywords @ keyword_positions text
  | keyword_positions (Repeat1 (Combinator_Info {keywords}, text)) =
      keywords @ keyword_positions text
  | keyword_positions (Select_Goals (Combinator_Info {keywords}, _, text)) =
      keywords @ keyword_positions text;

in

fun reports_of ((text, (pos, _)): text_range) =
  (pos, Markup.language_method) ::
    maps (fn p => map (pair p) (Markup.keyword3 :: Completion.suppress_abbrevs ""))
      (keyword_positions text);

val report = Position.reports o reports_of;

end;


(* outer parser *)

fun is_symid_meth s =
  s <> "|" andalso s <> "?" andalso s <> "+" andalso Token.ident_or_symbolic s;

local

fun meth4 x =
 (Parse.position Parse.xname >> (fn name => Source (Args.src name [])) ||
  Scan.ahead Parse.cartouche |-- Parse.not_eof >> (fn tok =>
    Source (Args.src ("cartouche", Token.pos_of tok) [tok])) ||
  Parse.$$$ "(" |-- Parse.!!! (meth0 --| Parse.$$$ ")")) x
and meth3 x =
 (meth4 -- Parse.position (Parse.$$$ "?")
    >> (fn (m, (_, pos)) => Try (combinator_info [pos], m)) ||
  meth4 -- Parse.position (Parse.$$$ "+")
    >> (fn (m, (_, pos)) => Repeat1 (combinator_info [pos], m)) ||
  meth4 --
    (Parse.position (Parse.$$$ "[") -- Scan.optional Parse.nat 1 -- Parse.position (Parse.$$$ "]"))
    >> (fn (m, (((_, pos1), n), (_, pos2))) =>
        Select_Goals (combinator_info [pos1, pos2], n, m)) ||
  meth4) x
and meth2 x =
 (Parse.position Parse.xname -- Parse.args1 is_symid_meth >> (Source o uncurry Args.src) ||
  meth3) x
and meth1 x =
  (Parse.enum1_positions "," meth2
    >> (fn ([m], _) => m | (ms, ps) => Then (combinator_info ps, ms))) x
and meth0 x =
  (Parse.enum1_positions "|" meth1
    >> (fn ([m], _) => m | (ms, ps) => Orelse (combinator_info ps, ms))) x;

in

val parse =
  Scan.trace meth3 >> (fn (m, toks) => (m, Token.range_of toks));

end;


(* theory setup *)

val _ = Theory.setup
 (setup @{binding fail} (Scan.succeed (K fail)) "force failure" #>
  setup @{binding succeed} (Scan.succeed (K succeed)) "succeed" #>
  setup @{binding "-"} (Scan.succeed (K insert_facts))
    "do nothing (insert current facts only)" #>
  setup @{binding insert} (Attrib.thms >> (K o insert))
    "insert theorems, ignoring facts (improper)" #>
  setup @{binding intro} (Attrib.thms >> (K o intro))
    "repeatedly apply introduction rules" #>
  setup @{binding elim} (Attrib.thms >> (K o elim))
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
  setup @{binding this} (Scan.succeed (K this)) "apply current facts as rules" #>
  setup @{binding fact} (Attrib.thms >> fact) "composition by facts from context" #>
  setup @{binding assumption} (Scan.succeed assumption)
    "proof by assumption, preferring facts" #>
  setup @{binding rename_tac} (Args.goal_spec -- Scan.lift (Scan.repeat1 Args.name) >>
    (fn (quant, xs) => K (SIMPLE_METHOD'' quant (rename_tac xs))))
    "rename parameters of goal" #>
  setup @{binding rotate_tac} (Args.goal_spec -- Scan.lift (Scan.optional Parse.int 1) >>
    (fn (quant, i) => K (SIMPLE_METHOD'' quant (rotate_tac i))))
      "rotate assumptions of goal" #>
  setup @{binding tactic} (Scan.lift Args.text_source_position >> tactic)
    "ML tactic as proof method" #>
  setup @{binding raw_tactic} (Scan.lift Args.text_source_position >> raw_tactic)
    "ML tactic as raw proof method");


(*final declarations of this structure!*)
val unfold = unfold_meth;
val fold = fold_meth;

end;

val RAW_METHOD_CASES = Method.RAW_METHOD_CASES;
val RAW_METHOD = Method.RAW_METHOD;
val METHOD_CASES = Method.METHOD_CASES;
val METHOD = Method.METHOD;
val SIMPLE_METHOD = Method.SIMPLE_METHOD;
val SIMPLE_METHOD' = Method.SIMPLE_METHOD';
val SIMPLE_METHOD'' = Method.SIMPLE_METHOD'';

