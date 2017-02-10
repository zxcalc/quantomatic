(*  Title:      Pure/Isar/proof.ML
    Author:     Markus Wenzel, TU Muenchen

The Isar/VM proof language interpreter: maintains a structured flow of
context elements, goals, refinements, and facts.
*)

signature PROOF =
sig
  type context = Proof.context
  type method = Method.method
  type state
  val init: context -> state
  val level: state -> int
  val assert_bottom: bool -> state -> state
  val context_of: state -> context
  val theory_of: state -> theory
  val map_context: (context -> context) -> state -> state
  val map_context_result : (context -> 'a * context) -> state -> 'a * state
  val map_contexts: (context -> context) -> state -> state
  val propagate_ml_env: state -> state
  val report_improper: state -> unit
  val the_facts: state -> thm list
  val the_fact: state -> thm
  val set_facts: thm list -> state -> state
  val reset_facts: state -> state
  val improper_reset_facts: state -> state
  val assert_forward: state -> state
  val assert_chain: state -> state
  val assert_forward_or_chain: state -> state
  val assert_backward: state -> state
  val assert_no_chain: state -> state
  val enter_forward: state -> state
  val enter_chain: state -> state
  val enter_backward: state -> state
  val has_bottom_goal: state -> bool
  val using_facts: thm list -> state -> state
  val pretty_state: state -> Pretty.T list
  val refine: Method.text -> state -> state Seq.result Seq.seq
  val refine_end: Method.text -> state -> state Seq.result Seq.seq
  val refine_singleton: Method.text -> state -> state
  val refine_insert: thm list -> state -> state
  val refine_primitive: (Proof.context -> thm -> thm) -> state -> state
  val raw_goal: state -> {context: context, facts: thm list, goal: thm}
  val goal: state -> {context: context, facts: thm list, goal: thm}
  val simple_goal: state -> {context: context, goal: thm}
  val let_bind: (term list * term) list -> state -> state
  val let_bind_cmd: (string list * string) list -> state -> state
  val write: Syntax.mode -> (term * mixfix) list -> state -> state
  val write_cmd: Syntax.mode -> (string * mixfix) list -> state -> state
  val fix: (binding * typ option * mixfix) list -> state -> state
  val fix_cmd: (binding * string option * mixfix) list -> state -> state
  val assm: Assumption.export -> (binding * typ option * mixfix) list ->
    (term * term list) list list -> (Thm.binding * (term * term list) list) list ->
    state -> state
  val assm_cmd: Assumption.export -> (binding * string option * mixfix) list ->
    (string * string list) list list -> (Attrib.binding * (string * string list) list) list ->
    state -> state
  val assume: (binding * typ option * mixfix) list ->
    (term * term list) list list -> (Thm.binding * (term * term list) list) list ->
    state -> state
  val assume_cmd: (binding * string option * mixfix) list ->
    (string * string list) list list -> (Attrib.binding * (string * string list) list) list ->
    state -> state
  val presume: (binding * typ option * mixfix) list ->
    (term * term list) list list -> (Thm.binding * (term * term list) list) list ->
    state -> state
  val presume_cmd: (binding * string option * mixfix) list ->
    (string * string list) list list -> (Attrib.binding * (string * string list) list) list ->
    state -> state
  val def: (Thm.binding * ((binding * mixfix) * (term * term list))) list -> state -> state
  val def_cmd: (Attrib.binding * ((binding * mixfix) * (string * string list))) list -> state -> state
  val chain: state -> state
  val chain_facts: thm list -> state -> state
  val note_thmss: (Thm.binding * (thm list * attribute list) list) list -> state -> state
  val note_thmss_cmd: (Attrib.binding * (Facts.ref * Token.src list) list) list -> state -> state
  val from_thmss: ((thm list * attribute list) list) list -> state -> state
  val from_thmss_cmd: ((Facts.ref * Token.src list) list) list -> state -> state
  val with_thmss: ((thm list * attribute list) list) list -> state -> state
  val with_thmss_cmd: ((Facts.ref * Token.src list) list) list -> state -> state
  val supply: (Thm.binding * (thm list * attribute list) list) list -> state -> state
  val supply_cmd: (Attrib.binding * (Facts.ref * Token.src list) list) list -> state -> state
  val using: ((thm list * attribute list) list) list -> state -> state
  val using_cmd: ((Facts.ref * Token.src list) list) list -> state -> state
  val unfolding: ((thm list * attribute list) list) list -> state -> state
  val unfolding_cmd: ((Facts.ref * Token.src list) list) list -> state -> state
  val case_: Thm.binding * ((string * Position.T) * binding option list) -> state -> state
  val case_cmd: Attrib.binding * ((string * Position.T) * binding option list) -> state -> state
  val define: (binding * typ option * mixfix) list ->
    (binding * typ option * mixfix) list ->
    (Thm.binding * (term * term list) list) list -> state -> state
  val define_cmd: (binding * string option * mixfix) list ->
    (binding * string option * mixfix) list ->
    (Attrib.binding * (string * string list) list) list -> state -> state
  val begin_block: state -> state
  val next_block: state -> state
  val end_block: state -> state
  val begin_notepad: context -> state
  val end_notepad: state -> context
  val proof: Method.text_range option -> state -> state Seq.result Seq.seq
  val defer: int -> state -> state
  val prefer: int -> state -> state
  val apply: Method.text_range -> state -> state Seq.result Seq.seq
  val apply_end: Method.text_range -> state -> state Seq.result Seq.seq
  val local_qed: Method.text_range option * bool -> state -> state
  val theorem: Method.text option -> (thm list list -> context -> context) ->
    (term * term list) list list -> context -> state
  val theorem_cmd: Method.text option -> (thm list list -> context -> context) ->
    (string * string list) list list -> context -> state
  val global_qed: Method.text_range option * bool -> state -> context
  val local_terminal_proof: Method.text_range * Method.text_range option -> state -> state
  val local_default_proof: state -> state
  val local_immediate_proof: state -> state
  val local_skip_proof: bool -> state -> state
  val local_done_proof: state -> state
  val global_terminal_proof: Method.text_range * Method.text_range option -> state -> context
  val global_default_proof: state -> context
  val global_immediate_proof: state -> context
  val global_skip_proof: bool -> state -> context
  val global_done_proof: state -> context
  val internal_goal: (context -> (string * string) * (string * thm list) list -> unit) ->
    Proof_Context.mode -> bool -> string -> Method.text option ->
    (context * thm list list -> state -> state) ->
    (binding * typ option * mixfix) list ->
    (Thm.binding * (term * term list) list) list ->
    (Thm.binding * (term * term list) list) list -> state -> thm list * state
  val have: bool -> Method.text option -> (context * thm list list -> state -> state) ->
    (binding * typ option * mixfix) list ->
    (Thm.binding * (term * term list) list) list ->
    (Thm.binding * (term * term list) list) list -> bool -> state -> thm list * state
  val have_cmd: bool -> Method.text option -> (context * thm list list -> state -> state) ->
    (binding * string option * mixfix) list ->
    (Attrib.binding * (string * string list) list) list ->
    (Attrib.binding * (string * string list) list) list -> bool -> state -> thm list * state
  val show: bool -> Method.text option -> (context * thm list list -> state -> state) ->
    (binding * typ option * mixfix) list ->
    (Thm.binding * (term * term list) list) list ->
    (Thm.binding * (term * term list) list) list -> bool -> state -> thm list * state
  val show_cmd: bool -> Method.text option -> (context * thm list list -> state -> state) ->
    (binding * string option * mixfix) list ->
    (Attrib.binding * (string * string list) list) list ->
    (Attrib.binding * (string * string list) list) list -> bool -> state -> thm list * state
  val schematic_goal: state -> bool
  val is_relevant: state -> bool
  val future_proof: (state -> ('a * context) future) -> state -> 'a future * state
  val local_future_terminal_proof: Method.text_range * Method.text_range option -> state -> state
  val global_future_terminal_proof: Method.text_range * Method.text_range option -> state -> context
end;

structure Proof: PROOF =
struct

type context = Proof.context;
type method = Method.method;


(** proof state **)

(* datatype state *)

datatype mode = Forward | Chain | Backward;

datatype state =
  State of node Stack.T
and node =
  Node of
   {context: context,
    facts: (thm list * bool) option,
    mode: mode,
    goal: goal option}
and goal =
  Goal of
   {statement: (string * Position.T) * term list list * term,
      (*goal kind and statement (starting with vars), initial proposition*)
    using: thm list,                      (*goal facts*)
    goal: thm,                            (*subgoals ==> statement*)
    before_qed: Method.text option,
    after_qed:
      (context * thm list list -> state -> state) *
      (context * thm list list -> context -> context)};

val _ =
  ML_system_pp (fn _ => fn _ => fn _: state =>
    Pretty.to_polyml (Pretty.str "<Proof.state>"));

fun make_goal (statement, using, goal, before_qed, after_qed) =
  Goal {statement = statement, using = using, goal = goal,
    before_qed = before_qed, after_qed = after_qed};

fun make_node (context, facts, mode, goal) =
  Node {context = context, facts = facts, mode = mode, goal = goal};

fun map_node f (Node {context, facts, mode, goal}) =
  make_node (f (context, facts, mode, goal));

val init_context =
  Proof_Context.set_stmt true #>
  Proof_Context.map_naming (K Name_Space.local_naming);

fun init ctxt =
  State (Stack.init (make_node (init_context ctxt, NONE, Forward, NONE)));

fun top (State stack) = Stack.top stack |> (fn Node node => node);
fun map_top f (State stack) = State (Stack.map_top (map_node f) stack);
fun map_all f (State stack) = State (Stack.map_all (map_node f) stack);



(** basic proof state operations **)

(* block structure *)

fun open_block (State stack) = State (Stack.push stack);

fun close_block (State stack) = State (Stack.pop stack)
  handle List.Empty => error "Unbalanced block parentheses";

fun level (State stack) = Stack.level stack;

fun assert_bottom b state =
  let val b' = level state <= 2 in
    if b andalso not b' then error "Not at bottom of proof"
    else if not b andalso b' then error "Already at bottom of proof"
    else state
  end;


(* context *)

val context_of = #context o top;
val theory_of = Proof_Context.theory_of o context_of;

fun map_context f =
  map_top (fn (ctxt, facts, mode, goal) => (f ctxt, facts, mode, goal));

fun map_context_result f state =
  f (context_of state) ||> (fn ctxt => map_context (K ctxt) state);

fun map_contexts f = map_all (fn (ctxt, facts, mode, goal) => (f ctxt, facts, mode, goal));

fun propagate_ml_env state = map_contexts
  (Context.proof_map (ML_Env.inherit (Context.Proof (context_of state)))) state;


(* facts *)

fun report_improper state =
  Context_Position.report (context_of state) (Position.thread_data ()) Markup.improper;

val get_facts = #facts o top;

fun the_facts state =
  (case get_facts state of
    SOME (facts, proper) => (if proper then () else report_improper state; facts)
  | NONE => error "No current facts available");

fun the_fact state =
  (case the_facts state of
    [thm] => thm
  | _ => error "Single theorem expected");

fun put_facts index facts =
  map_top (fn (ctxt, _, mode, goal) => (ctxt, facts, mode, goal)) #>
  map_context (Proof_Context.put_thms index (Auto_Bind.thisN, Option.map #1 facts));

fun set_facts thms = put_facts false (SOME (thms, true));
val reset_facts = put_facts false NONE;

fun improper_reset_facts state =
  (case get_facts state of
    SOME (thms, _) => put_facts false (SOME (thms, false)) state
  | NONE => state);

fun these_factss more_facts (named_factss, state) =
  (named_factss, state |> set_facts (maps snd named_factss @ more_facts));

fun export_facts inner outer =
  (case get_facts inner of
    NONE => reset_facts outer
  | SOME (thms, proper) =>
      let val thms' = Proof_Context.export (context_of inner) (context_of outer) thms
      in put_facts true (SOME (thms', proper)) outer end);


(* mode *)

val get_mode = #mode o top;
fun put_mode mode = map_top (fn (ctxt, facts, _, goal) => (ctxt, facts, mode, goal));

val mode_name = (fn Forward => "state" | Chain => "chain" | Backward => "prove");

fun assert_mode pred state =
  let val mode = get_mode state in
    if pred mode then state
    else error ("Illegal application of proof command in " ^ quote (mode_name mode) ^ " mode")
  end;

val assert_forward = assert_mode (fn mode => mode = Forward);
val assert_chain = assert_mode (fn mode => mode = Chain);
val assert_forward_or_chain = assert_mode (fn mode => mode = Forward orelse mode = Chain);
val assert_backward = assert_mode (fn mode => mode = Backward);
val assert_no_chain = assert_mode (fn mode => mode <> Chain);

val enter_forward = put_mode Forward;
val enter_chain = put_mode Chain;
val enter_backward = put_mode Backward;


(* current goal *)

fun current_goal state =
  (case top state of
    {context = ctxt, goal = SOME (Goal goal), ...} => (ctxt, goal)
  | _ => error "No current goal");

fun assert_current_goal g state =
  let val g' = can current_goal state in
    if g andalso not g' then error "No goal in this block"
    else if not g andalso g' then error "Goal present in this block"
    else state
  end;

fun put_goal goal = map_top (fn (ctxt, facts, mode, _) => (ctxt, facts, mode, goal));

val set_goal = put_goal o SOME;
val reset_goal = put_goal NONE;

val before_qed = #before_qed o #2 o current_goal;


(* bottom goal *)

fun has_bottom_goal (State stack) =
  let
    fun bottom [Node {goal = SOME _, ...}, Node {goal = NONE, ...}] = true
      | bottom [Node {goal, ...}] = is_some goal
      | bottom [] = false
      | bottom (_ :: rest) = bottom rest;
  in bottom (op :: (Stack.dest stack)) end;


(* nested goal *)

fun map_goal f (State stack) =
  (case Stack.dest stack of
    (Node {context = ctxt, facts, mode, goal = SOME goal}, node :: nodes) =>
      let
        val Goal {statement, using, goal, before_qed, after_qed} = goal;
        val (ctxt', statement', using', goal', before_qed', after_qed') =
          f (ctxt, statement, using, goal, before_qed, after_qed);
        val goal' = make_goal (statement', using', goal', before_qed', after_qed');
      in State (Stack.make (make_node (ctxt', facts, mode, SOME goal')) (node :: nodes)) end
  | (top_node, node :: nodes) =>
      let
        val State stack' = map_goal f (State (Stack.make node nodes));
        val (node', nodes') = Stack.dest stack';
      in State (Stack.make top_node (node' :: nodes')) end
  | _ => State stack);

fun provide_goal goal =
  map_goal (fn (ctxt, statement, using, _, before_qed, after_qed) =>
    (ctxt, statement, using, goal, before_qed, after_qed));

fun using_facts using =
  map_goal (fn (ctxt, statement, _, goal, before_qed, after_qed) =>
    (ctxt, statement, using, goal, before_qed, after_qed));

fun find_goal state =
  (case try current_goal state of
    SOME ctxt_goal => ctxt_goal
  | NONE => find_goal (close_block state handle ERROR _ => error "No proof goal"));

fun get_goal state =
  let val (ctxt, {using, goal, ...}) = find_goal state
  in (ctxt, (using, goal)) end;



(** pretty_state **)

local

fun pretty_facts _ _ NONE = []
  | pretty_facts ctxt s (SOME ths) = [Proof_Display.pretty_goal_facts ctxt s ths];

fun pretty_sep prts [] = prts
  | pretty_sep [] prts = prts
  | pretty_sep prts1 prts2 = prts1 @ [Pretty.str ""] @ prts2;

in

fun pretty_state state =
  let
    val {context = ctxt, facts, mode, goal = _} = top state;
    val verbose = Config.get ctxt Proof_Context.verbose;

    fun prt_goal (SOME (_, {statement = _, using, goal, before_qed = _, after_qed = _})) =
          pretty_sep
            (pretty_facts ctxt "using"
              (if mode <> Backward orelse null using then NONE else SOME using))
            ([Proof_Display.pretty_goal_header goal] @ Goal_Display.pretty_goals ctxt goal)
      | prt_goal NONE = [];

    val prt_ctxt =
      if verbose orelse mode = Forward then Proof_Context.pretty_context ctxt
      else if mode = Backward then Proof_Context.pretty_ctxt ctxt
      else [];

    val position_markup = Position.markup (Position.thread_data ()) Markup.position;
  in
    [Pretty.block
      [Pretty.mark_str (position_markup, "proof"), Pretty.str (" (" ^ mode_name mode ^ ")")]] @
    (if null prt_ctxt then [] else prt_ctxt @ [Pretty.str ""]) @
    (if verbose orelse mode = Forward then
       pretty_sep (pretty_facts ctxt "" (Option.map #1 facts)) (prt_goal (try find_goal state))
     else if mode = Chain then pretty_facts ctxt "picking" (Option.map #1 facts)
     else prt_goal (try find_goal state))
  end;

end;



(** proof steps **)

(* refine via method *)

local

fun goalN i = "goal" ^ string_of_int i;
fun goals st = map goalN (1 upto Thm.nprems_of st);

fun no_goal_cases st = map (rpair NONE) (goals st);

fun goal_cases ctxt st =
  Rule_Cases.make_common ctxt (Thm.prop_of st) (map (rpair [] o rpair []) (goals st));

fun apply_method text ctxt state =
  find_goal state |> (fn (goal_ctxt, {statement, using, goal, before_qed, after_qed}) =>
    Method.evaluate text ctxt using (goal_ctxt, goal)
    |> Seq.map_result (fn (goal_ctxt', goal') =>
      let
        val goal_ctxt'' = goal_ctxt'
          |> Proof_Context.update_cases_legacy (no_goal_cases goal @ goal_cases ctxt goal');
        val state' = state
          |> map_goal (K (goal_ctxt'', statement, using, goal', before_qed, after_qed));
      in state' end));

in

fun refine text state = apply_method text (context_of state) state;
fun refine_end text state = apply_method text (#1 (find_goal state)) state;

fun refine_singleton text = refine text #> Seq.the_result "";

fun refine_insert ths =
  refine_singleton (Method.Basic (K (Method.insert ths)));

fun refine_primitive r =
  refine_singleton (Method.Basic (fn ctxt => fn _ => Method.CONTEXT_TACTIC (PRIMITIVE (r ctxt))));

end;


(* refine via sub-proof *)

local

fun finish_tac _ 0 = K all_tac
  | finish_tac ctxt n =
      Goal.norm_hhf_tac ctxt THEN'
      SUBGOAL (fn (goal, i) =>
        if can Logic.unprotect (Logic.strip_assums_concl goal) then
          eresolve_tac ctxt [Drule.protectI] i THEN finish_tac ctxt (n - 1) i
        else finish_tac ctxt (n - 1) (i + 1));

fun goal_tac ctxt rule =
  Goal.norm_hhf_tac ctxt THEN'
  resolve_tac ctxt [rule] THEN'
  finish_tac ctxt (Thm.nprems_of rule);

fun FINDGOAL tac st =
  let fun find i n = if i > n then Seq.fail else Seq.APPEND (tac i, find (i + 1) n)
  in find 1 (Thm.nprems_of st) st end;

fun protect_prem i th =
  Thm.bicompose NONE {flatten = false, match = false, incremented = true}
    (false, Drule.incr_indexes th Drule.protectD, 1) i th
  |> Seq.hd;

fun protect_prems th =
  fold_rev protect_prem (1 upto Thm.nprems_of th) th;

in

fun refine_goals print_rule result_ctxt result state =
  let
    val (goal_ctxt, (_, goal)) = get_goal state;
    fun refine rule st =
      (print_rule goal_ctxt rule; FINDGOAL (goal_tac goal_ctxt rule) st);
  in
    result
    |> map (Raw_Simplifier.norm_hhf result_ctxt #> protect_prems)
    |> Proof_Context.goal_export result_ctxt goal_ctxt
    |> (fn rules => Seq.lift provide_goal (EVERY (map refine rules) goal) state)
  end;

end;


(* conclude goal *)

fun conclude_goal ctxt goal propss =
  let
    val thy = Proof_Context.theory_of ctxt;

    val _ =
      Context.subthy_id (Thm.theory_id_of_thm goal, Context.theory_id thy) orelse
        error "Bad background theory of goal state";
    val _ = Thm.no_prems goal orelse error (Proof_Display.string_of_goal ctxt goal);

    fun err_lost () = error ("Lost goal structure:\n" ^ Thm.string_of_thm ctxt goal);

    val th =
      (Goal.conclude (if length (flat propss) > 1 then Thm.close_derivation goal else goal)
        handle THM _ => err_lost ())
      |> Drule.flexflex_unique (SOME ctxt)
      |> Thm.check_shyps ctxt
      |> Thm.check_hyps (Context.Proof ctxt);

    val goal_propss = filter_out null propss;
    val results =
      Conjunction.elim_balanced (length goal_propss) th
      |> map2 Conjunction.elim_balanced (map length goal_propss)
      handle THM _ => err_lost ();
    val _ =
      Unify.matches_list (Context.Proof ctxt) (flat goal_propss) (map Thm.prop_of (flat results))
        orelse error ("Proved a different theorem:\n" ^ Thm.string_of_thm ctxt th);

    fun recover_result ([] :: pss) thss = [] :: recover_result pss thss
      | recover_result (_ :: pss) (ths :: thss) = ths :: recover_result pss thss
      | recover_result [] [] = []
      | recover_result _ _ = err_lost ();
  in recover_result propss results end;

val finished_goal_error = "Failed to finish proof";

fun finished_goal pos state =
  let val (ctxt, (_, goal)) = get_goal state in
    if Thm.no_prems goal then Seq.Result state
    else
      Seq.Error (fn () =>
        finished_goal_error ^ Position.here pos ^ ":\n" ^
          Proof_Display.string_of_goal ctxt goal)
  end;


(* goal views -- corresponding to methods *)

fun raw_goal state =
  let val (ctxt, (using, goal)) = get_goal state
  in {context = ctxt, facts = using, goal = goal} end;

val goal = raw_goal o refine_insert [];

fun simple_goal state =
  let
    val (_, (using, _)) = get_goal state;
    val (ctxt, (_, goal)) = get_goal (refine_insert using state);
  in {context = ctxt, goal = goal} end;

fun method_error kind pos state =
  Seq.single (Proof_Display.method_error kind pos (raw_goal state));



(*** structured proof commands ***)

(** context elements **)

(* let bindings *)

local

fun gen_bind bind args state =
  state
  |> assert_forward
  |> map_context (bind true args #> snd)
  |> reset_facts;

in

val let_bind = gen_bind Proof_Context.match_bind;
val let_bind_cmd = gen_bind Proof_Context.match_bind_cmd;

end;


(* concrete syntax *)

local

fun read_arg (c, mx) ctxt =
  (case Proof_Context.read_const {proper = false, strict = false} ctxt c of
    Free (x, _) =>
      let
        val ctxt' =
          ctxt |> is_none (Variable.default_type ctxt x) ?
            Variable.declare_constraints (Free (x, Mixfix.default_constraint mx));
        val t = Free (#1 (Proof_Context.inferred_param x ctxt'));
      in ((t, mx), ctxt') end
  | t => ((t, mx), ctxt));

fun gen_write prep_arg mode args =
  assert_forward
  #> map_context (fold_map prep_arg args #-> Proof_Context.notation true mode)
  #> reset_facts;

in

val write = gen_write pair;
val write_cmd = gen_write read_arg;

end;


(* fix *)

local

fun gen_fix add_fixes raw_fixes =
  assert_forward
  #> map_context (snd o add_fixes raw_fixes)
  #> reset_facts;

in

val fix = gen_fix Proof_Context.add_fixes;
val fix_cmd = gen_fix Proof_Context.add_fixes_cmd;

end;


(* assume *)

local

fun gen_assume prep_statement prep_att export raw_fixes raw_prems raw_concls state =
  let
    val ctxt = context_of state;

    val bindings = map (apsnd (map (prep_att ctxt)) o fst) raw_concls;
    val ((params, prems_propss, concl_propss, result_binds), _) =
      prep_statement raw_fixes raw_prems (map snd raw_concls) ctxt;
    val propss = (map o map) (Logic.close_prop params (flat prems_propss)) concl_propss;
  in
    state
    |> assert_forward
    |> map_context_result (fn ctxt =>
      ctxt
      |> (fold o fold) Variable.declare_term propss
      |> fold Variable.maybe_bind_term result_binds
      |> fold_burrow (Assumption.add_assms export o map (Thm.cterm_of ctxt)) propss
      |-> (fn premss =>
        Proof_Context.note_thmss "" (bindings ~~ (map o map) (fn th => ([th], [])) premss)))
    |> these_factss [] |> #2
  end;

in

val assm = gen_assume Proof_Context.cert_statement (K I);
val assm_cmd = gen_assume Proof_Context.read_statement Attrib.attribute_cmd;

val assume = assm Assumption.assume_export;
val assume_cmd = assm_cmd Assumption.assume_export;

val presume = assm Assumption.presume_export;
val presume_cmd = assm_cmd Assumption.presume_export;

end;


(* def *)

local

fun gen_def prep_att prep_var prep_binds args state =
  let
    val _ = assert_forward state;
    val (raw_name_atts, (raw_vars, raw_rhss)) = args |> split_list ||> split_list;
    val name_atts = map (apsnd (map (prep_att (context_of state)))) raw_name_atts;
  in
    state
    |> map_context_result (fold_map (fn (x, mx) => prep_var (x, NONE, mx)) raw_vars)
    |>> map (fn (x, _, mx) => (x, mx))
    |-> (fn vars =>
      map_context_result (prep_binds false (map swap raw_rhss))
      #-> (fn rhss =>
        let
          val defs = (vars ~~ (name_atts ~~ rhss)) |> map (fn ((x, mx), ((a, atts), rhs)) =>
            ((x, mx), ((Thm.def_binding_optional x a, atts), rhs)));
        in map_context_result (Local_Defs.define defs) end))
    |-> (set_facts o map (#2 o #2))
  end;

in

val def = gen_def (K I) Proof_Context.cert_var Proof_Context.match_bind;
val def_cmd = gen_def Attrib.attribute_cmd Proof_Context.read_var Proof_Context.match_bind_cmd;

end;



(** facts **)

(* chain *)

val chain =
  assert_forward
  #> (fn state => set_facts (Method.clean_facts (the_facts state)) state)
  #> enter_chain;

fun chain_facts facts =
  set_facts facts
  #> chain;


(* note etc. *)

fun empty_bindings args = map (pair Binding.empty_atts) args;

local

fun gen_thmss more_facts opt_chain opt_result prep_atts prep_fact args state =
  state
  |> assert_forward
  |> map_context_result (fn ctxt => ctxt |> Proof_Context.note_thmss ""
    (Attrib.map_facts_refs (map (prep_atts ctxt)) (prep_fact ctxt) args))
  |> these_factss (more_facts state)
  ||> opt_chain
  |> opt_result;

in

val note_thmss = gen_thmss (K []) I #2 (K I) (K I);
val note_thmss_cmd = gen_thmss (K []) I #2 Attrib.attribute_cmd Proof_Context.get_fact;

val from_thmss = gen_thmss (K []) chain #2 (K I) (K I) o empty_bindings;
val from_thmss_cmd =
  gen_thmss (K []) chain #2 Attrib.attribute_cmd Proof_Context.get_fact o empty_bindings;

val with_thmss = gen_thmss the_facts chain #2 (K I) (K I) o empty_bindings;
val with_thmss_cmd =
  gen_thmss the_facts chain #2 Attrib.attribute_cmd Proof_Context.get_fact o empty_bindings;

val local_results = gen_thmss (K []) I I (K I) (K I) o map (apsnd Thm.simple_fact);

end;


(* facts during goal refinement *)

local

fun gen_supply prep_att prep_fact args state =
  state
  |> assert_backward
  |> map_context (fn ctxt => ctxt |> Proof_Context.note_thmss ""
       (Attrib.map_facts_refs (map (prep_att ctxt)) (prep_fact ctxt) args) |> snd);

in

val supply = gen_supply (K I) (K I);
val supply_cmd = gen_supply Attrib.attribute_cmd Proof_Context.get_fact;

end;


(* using/unfolding *)

local

fun gen_using f g prep_att prep_fact args state =
  state
  |> assert_backward
  |> map_context_result
    (fn ctxt => ctxt |> Proof_Context.note_thmss ""
      (Attrib.map_facts_refs (map (prep_att ctxt)) (prep_fact ctxt) (empty_bindings args)))
  |> (fn (named_facts, state') =>
    state' |> map_goal (fn (goal_ctxt, statement, using, goal, before_qed, after_qed) =>
      let
        val ctxt = context_of state';
        val ths = maps snd named_facts;
      in (goal_ctxt, statement, f ctxt ths using, g ctxt ths goal, before_qed, after_qed) end));

fun append_using _ ths using = using @ filter_out Thm.is_dummy ths;
fun unfold_using ctxt ths = map (Local_Defs.unfold ctxt ths);
val unfold_goals = Local_Defs.unfold_goals;

in

val using = gen_using append_using (K (K I)) (K I) (K I);
val using_cmd = gen_using append_using (K (K I)) Attrib.attribute_cmd Proof_Context.get_fact;
val unfolding = gen_using unfold_using unfold_goals (K I) (K I);
val unfolding_cmd = gen_using unfold_using unfold_goals Attrib.attribute_cmd Proof_Context.get_fact;

end;


(* case *)

local

fun gen_case internal prep_att ((raw_binding, raw_atts), ((name, pos), xs)) state =
  let
    val ctxt = context_of state;

    val binding = if Binding.is_empty raw_binding then Binding.make (name, pos) else raw_binding;
    val atts = map (prep_att ctxt) raw_atts;

    val (asms, state') = state |> map_context_result (fn ctxt =>
      ctxt |> Proof_Context.apply_case (Proof_Context.check_case ctxt internal (name, pos) xs));
    val assumptions =
      asms |> map (fn (a, ts) => ((Binding.qualify_name true binding a, []), map (rpair []) ts));
  in
    state'
    |> assume [] [] assumptions
    |> map_context (fold Variable.unbind_term Auto_Bind.no_facts)
    |> `the_facts |-> (fn thms => note_thmss [((binding, atts), [(thms, [])])])
  end;

in

val case_ = gen_case true (K I);
val case_cmd = gen_case false Attrib.attribute_cmd;

end;


(* define *)

local

fun gen_define prep_stmt prep_att raw_decls raw_fixes raw_defs state =
  let
    val _ = assert_forward state;
    val ctxt = context_of state;

    (*vars*)
    val ((vars, propss, _, binds'), vars_ctxt) =
      prep_stmt (raw_decls @ raw_fixes) (map snd raw_defs) ctxt;
    val (decls, fixes) = chop (length raw_decls) vars;
    val show_term = Syntax.string_of_term vars_ctxt;

    (*defs*)
    fun match_defs (((b, _, mx), (_, Free (x, T))) :: more_decls) ((((y, U), t), _) :: more_defs) =
          if x = y then ((b, mx), (Binding.empty_atts, t)) :: match_defs more_decls more_defs
          else
            error ("Mismatch of declaration " ^ show_term (Free (x, T)) ^ " wrt. definition " ^
              show_term (Free (y, U)))
      | match_defs [] [] = []
      | match_defs more_decls more_defs =
          error ("Mismatch of declarations " ^ commas (map (show_term o #2 o #2) more_decls) ^
            (if null more_decls then "" else " ") ^
            "wrt. definitions " ^ commas (map (show_term o Free o #1 o #1) more_defs));

    val derived_def = Local_Defs.derived_def ctxt (K  []) {conditional = false};
    val defs1 = map (derived_def o Logic.close_prop (map #2 fixes) []) (flat propss);
    val defs2 = match_defs decls defs1;
    val (defs3, defs_ctxt) = Local_Defs.define defs2 ctxt;

    (*notes*)
    val def_thmss =
      map (fn (((_, prove), ((b, _), _)), (_, (_, th))) => (b, prove defs_ctxt th))
        (defs1 ~~ defs2 ~~ defs3)
      |> unflat (map snd raw_defs);
    val notes =
      map2 (fn ((a, raw_atts), _) => fn def_thms =>
        ((Thm.def_binding_optional (Binding.conglomerate (map #1 def_thms)) a,
          map (prep_att defs_ctxt) raw_atts), map (fn (_, th) => ([th], [])) def_thms))
        raw_defs def_thmss;
  in
    state
    |> map_context (K defs_ctxt #> fold Variable.bind_term binds')
    |> note_thmss notes
  end;

in

val define = gen_define Proof_Context.cert_stmt (K I);
val define_cmd = gen_define Proof_Context.read_stmt Attrib.attribute_cmd;

end;



(** proof structure **)

(* blocks *)

val begin_block =
  assert_forward
  #> open_block
  #> reset_goal
  #> open_block;

val next_block =
  assert_forward
  #> close_block
  #> open_block
  #> reset_goal
  #> reset_facts;

fun end_block state =
  state
  |> assert_forward
  |> assert_bottom false
  |> close_block
  |> assert_current_goal false
  |> close_block
  |> export_facts state;


(* global notepad *)

val begin_notepad =
  init
  #> open_block
  #> map_context (Variable.set_body true)
  #> open_block;

val end_notepad =
  assert_forward
  #> assert_bottom true
  #> close_block
  #> assert_current_goal false
  #> close_block
  #> context_of;


(* sub-proofs *)

fun proof opt_text =
  Seq.APPEND
    (assert_backward
      #> refine (the_default Method.standard_text (Method.text opt_text))
      #> Seq.map_result
        (using_facts []
          #> enter_forward
          #> open_block
          #> reset_goal),
     method_error "initial" (Method.position opt_text));

fun end_proof bot (prev_pos, (opt_text, immed)) =
  let
    val (finish_text, terminal_pos, finished_pos) =
      (case opt_text of
        NONE => (Method.finish_text (NONE, immed), Position.none, prev_pos)
      | SOME (text, (pos, end_pos)) => (Method.finish_text (SOME text, immed), pos, end_pos));
  in
    Seq.APPEND (fn state =>
      state
      |> assert_forward
      |> assert_bottom bot
      |> close_block
      |> assert_current_goal true
      |> using_facts []
      |> `before_qed |-> (refine o the_default Method.succeed_text)
      |> Seq.maps_results (refine finish_text),
      method_error "terminal" terminal_pos)
    #> Seq.maps_results (Seq.single o finished_goal finished_pos)
  end;

fun check_result msg sq =
  (case Seq.pull sq of
    NONE => error msg
  | SOME (s, _) => s);


(* unstructured refinement *)

fun defer i =
  assert_no_chain #>
  refine_singleton (Method.Basic (fn _ => METHOD (fn _ => ASSERT_SUBGOAL defer_tac i)));

fun prefer i =
  assert_no_chain #>
  refine_singleton (Method.Basic (fn _ => METHOD (fn _ => ASSERT_SUBGOAL prefer_tac i)));

fun apply (text, (pos, _)) =
  Seq.APPEND (assert_backward #> refine text #> Seq.map_result (using_facts []),
    method_error "" pos);

fun apply_end (text, (pos, _)) =
  Seq.APPEND (assert_forward #> refine_end text, method_error "" pos);



(** goals **)

(* generic goals *)

local

val is_var =
  can (dest_TVar o Logic.dest_type o Logic.dest_term) orf
  can (dest_Var o Logic.dest_term);

fun implicit_vars props =
  let
    val (var_props, _) = take_prefix is_var props;
    val explicit_vars = fold Term.add_vars var_props [];
    val vars = filter_out (member (op =) explicit_vars) (fold Term.add_vars props []);
  in map (Logic.mk_term o Var) vars end;

fun refine_terms n =
  refine_singleton (Method.Basic (fn ctxt => Method.CONTEXT_TACTIC o
    K (HEADGOAL (PRECISE_CONJUNCTS n
      (HEADGOAL (CONJUNCTS (ALLGOALS (resolve_tac ctxt [Drule.termI]))))))));

in

fun generic_goal kind before_qed after_qed goal_ctxt goal_propss result_binds state =
  let
    val chaining = can assert_chain state;
    val pos = Position.thread_data ();

    val goal_props = flat goal_propss;
    val vars = implicit_vars goal_props;
    val propss' = vars :: goal_propss;
    val goal_propss = filter_out null propss';

    val goal =
      Logic.mk_conjunction_balanced (map Logic.mk_conjunction_balanced goal_propss)
      |> Thm.cterm_of goal_ctxt
      |> Thm.weaken_sorts' goal_ctxt;
    val statement = ((kind, pos), propss', Thm.term_of goal);

    val after_qed' = after_qed |>> (fn after_local => fn results =>
      map_context (fold Variable.maybe_bind_term result_binds) #> after_local results);
  in
    state
    |> assert_forward_or_chain
    |> enter_forward
    |> open_block
    |> enter_backward
    |> map_context
      (K goal_ctxt
        #> init_context
        #> Variable.set_body true
        #> Proof_Context.auto_bind_goal goal_props)
    |> set_goal (make_goal (statement, [], Goal.init goal, before_qed, after_qed'))
    |> chaining ? (`the_facts #-> using_facts)
    |> reset_facts
    |> not (null vars) ? refine_terms (length goal_propss)
    |> null goal_props ? refine_singleton (Method.Basic Method.assumption)
  end;

fun generic_qed state =
  let
    val (goal_ctxt, {statement = (_, propss, _), goal, after_qed, ...}) =
      current_goal state;
    val results = tl (conclude_goal goal_ctxt goal propss);
  in state |> close_block |> pair (after_qed, (goal_ctxt, results)) end;

end;


(* local goals *)

fun local_goal print_results prep_statement prep_att strict_asm
    kind before_qed after_qed raw_fixes raw_assumes raw_shows state =
  let
    val ctxt = context_of state;

    val add_assumes =
      Assumption.add_assms
        (if strict_asm then Assumption.assume_export else Assumption.presume_export);

    (*params*)
    val ((params, assumes_propss, shows_propss, result_binds), params_ctxt) = ctxt
      |> prep_statement raw_fixes (map snd raw_assumes) (map snd raw_shows);

    (*prems*)
    val (assumes_bindings, shows_bindings) =
      apply2 (map (apsnd (map (prep_att ctxt)) o fst)) (raw_assumes, raw_shows);
    val (that_fact, goal_ctxt) = params_ctxt
      |> fold_burrow add_assumes ((map o map) (Thm.cterm_of params_ctxt) assumes_propss)
      |> (fn (premss, ctxt') =>
          let
            val prems = Assumption.local_prems_of ctxt' ctxt;
            val ctxt'' =
              ctxt'
              |> not (null assumes_propss) ?
                (snd o Proof_Context.note_thmss ""
                  [((Binding.name Auto_Bind.thatN, []), [(prems, [])])])
              |> (snd o Proof_Context.note_thmss ""
                  (assumes_bindings ~~ map (map (fn th => ([th], []))) premss))
          in (prems, ctxt'') end);

    (*result*)
    val results_bindings = map (apfst Binding.default_pos) shows_bindings;
    fun after_qed' (result_ctxt, results) state' =
      let
        val ctxt' = context_of state';
        val export0 =
          Assumption.export false result_ctxt ctxt' #>
          fold_rev (fn (x, v) => Thm.forall_intr_name (x, Thm.cterm_of params_ctxt v)) params #>
          Raw_Simplifier.norm_hhf_protect ctxt';
        val export = map export0 #> Variable.export result_ctxt ctxt';
      in
        state'
        |> local_results (results_bindings ~~ burrow export results)
        |-> (fn res => tap (fn st => print_results (context_of st) ((kind, ""), res) : unit))
        |> after_qed (result_ctxt, results)
      end;
  in
    state
    |> generic_goal kind before_qed (after_qed', K I) goal_ctxt shows_propss result_binds
    |> pair that_fact
  end;

fun local_qeds arg =
  end_proof false arg
  #> Seq.map_result (generic_qed #-> (fn ((after_qed, _), results) => after_qed results));

fun local_qed arg =
  local_qeds (Position.none, arg) #> Seq.the_result finished_goal_error;


(* global goals *)

fun global_goal prep_propp before_qed after_qed propp ctxt =
  let
    val (propss, binds) =
      prep_propp (Proof_Context.set_mode Proof_Context.mode_schematic ctxt) propp;
    val goal_ctxt = ctxt
      |> (fold o fold) Variable.auto_fixes propss
      |> fold Variable.bind_term binds;
    fun after_qed' (result_ctxt, results) ctxt' = ctxt'
      |> Proof_Context.restore_naming ctxt
      |> after_qed (burrow (Proof_Context.export result_ctxt ctxt') results);
  in
    ctxt
    |> init
    |> generic_goal "" before_qed (K I, after_qed') goal_ctxt propss []
  end;

val theorem = global_goal Proof_Context.cert_propp;
val theorem_cmd = global_goal Proof_Context.read_propp;

fun global_qeds arg =
  end_proof true arg
  #> Seq.map_result (generic_qed #> (fn (((_, after_qed), results), state) =>
    after_qed results (context_of state)));

fun global_qed arg =
  global_qeds (Position.none, arg) #> Seq.the_result finished_goal_error;


(* terminal proof steps *)

local

fun terminal_proof qeds initial terminal state =
  let
    val ctxt = context_of state;
    val check_closure = Method.check_text ctxt #> Method.map_source (Method.method_closure ctxt);
    val initial' = apfst check_closure initial;
    val terminal' = (apfst o Option.map o apfst) check_closure terminal;
  in
    state
    |> proof (SOME initial')
    |> Seq.maps_results (qeds (#2 (#2 initial), terminal'))
    |> Seq.the_result ""
  end;

in

fun local_terminal_proof (text, opt_text) = terminal_proof local_qeds text (opt_text, true);
val local_default_proof = local_terminal_proof ((Method.standard_text, Position.no_range), NONE);
val local_immediate_proof = local_terminal_proof ((Method.this_text, Position.no_range), NONE);
val local_done_proof = terminal_proof local_qeds (Method.done_text, Position.no_range) (NONE, false);

fun global_terminal_proof (text, opt_text) = terminal_proof global_qeds text (opt_text, true);
val global_default_proof = global_terminal_proof ((Method.standard_text, Position.no_range), NONE);
val global_immediate_proof = global_terminal_proof ((Method.this_text, Position.no_range), NONE);
val global_done_proof = terminal_proof global_qeds (Method.done_text, Position.no_range) (NONE, false);

end;


(* skip proofs *)

fun local_skip_proof int state =
  local_terminal_proof ((Method.sorry_text int, Position.no_range), NONE) state before
  Skip_Proof.report (context_of state);

fun global_skip_proof int state =
  global_terminal_proof ((Method.sorry_text int, Position.no_range), NONE) state before
  Skip_Proof.report (context_of state);


(* common goal statements *)

fun internal_goal print_results mode =
  local_goal print_results
    (fn a => fn b => fn c => Proof_Context.cert_statement a b c o Proof_Context.set_mode mode) (K I);

local

fun gen_have prep_statement prep_att strict_asm before_qed after_qed fixes assumes shows int =
  local_goal (Proof_Display.print_results int (Position.thread_data ()))
    prep_statement prep_att strict_asm "have" before_qed after_qed fixes assumes shows;

fun gen_show prep_statement prep_att strict_asm before_qed after_qed fixes assumes shows int state =
  let
    val testing = Unsynchronized.ref false;
    val rule = Unsynchronized.ref (NONE: thm option);
    fun fail_msg ctxt =
      "Local statement fails to refine any pending goal" ::
      (case ! rule of NONE => [] | SOME th => [Proof_Display.string_of_rule ctxt "Failed" th])
      |> cat_lines;

    val pos = Position.thread_data ();
    fun print_results ctxt res =
      if ! testing then ()
      else Proof_Display.print_results int pos ctxt res;
    fun print_rule ctxt th =
      if ! testing then rule := SOME th
      else if int then
        Proof_Display.string_of_rule ctxt "Successful" th
        |> Markup.markup Markup.text_fold
        |> Output.state
      else ();
    val test_proof =
      local_skip_proof true
      |> Unsynchronized.setmp testing true
      |> Exn.interruptible_capture;

    fun after_qed' (result_ctxt, results) state' = state'
      |> refine_goals print_rule result_ctxt (flat results)
      |> check_result "Failed to refine any pending goal"
      |> after_qed (result_ctxt, results);
  in
    state
    |> local_goal print_results prep_statement prep_att strict_asm
      "show" before_qed after_qed' fixes assumes shows
    ||> int ? (fn goal_state =>
      (case test_proof (map_context (Context_Position.set_visible false) goal_state) of
        Exn.Res _ => goal_state
      | Exn.Exn exn => raise Exn.EXCEPTIONS ([exn, ERROR (fail_msg (context_of goal_state))])))
  end;

in

val have = gen_have Proof_Context.cert_statement (K I);
val have_cmd = gen_have Proof_Context.read_statement Attrib.attribute_cmd;
val show = gen_show Proof_Context.cert_statement (K I);
val show_cmd = gen_show Proof_Context.read_statement Attrib.attribute_cmd;

end;



(** future proofs **)

(* relevant proof states *)

fun schematic_goal state =
  let val (_, {statement = (_, _, prop), ...}) = find_goal state
  in Goal.is_schematic prop end;

fun is_relevant state =
  (case try find_goal state of
    NONE => true
  | SOME (_, {statement = (_, _, prop), goal, ...}) =>
      Goal.is_schematic prop orelse not (Logic.protect prop aconv Thm.concl_of goal));


(* full proofs *)

local

structure Result = Proof_Data
(
  type T = thm option;
  fun init _ = NONE;
);

fun the_result ctxt =
  (case Result.get ctxt of
    NONE => error "No result of forked proof"
  | SOME th => th);

val set_result = Result.put o SOME;
val reset_result = Result.put NONE;

in

fun future_proof fork_proof state =
  let
    val _ = assert_backward state;
    val (goal_ctxt, goal_info) = find_goal state;
    val {statement as (kind, _, prop), using, goal, before_qed, after_qed} = goal_info;

    val _ = is_relevant state andalso error "Cannot fork relevant proof";

    val after_qed' =
      (fn (_, [[th]]) => map_context (set_result th),
       fn (_, [[th]]) => set_result th);
    val result_ctxt =
      state
      |> map_context reset_result
      |> map_goal (K (goal_ctxt, (kind, [[], [prop]], prop), using, goal, before_qed, after_qed'))
      |> fork_proof;

    val future_thm = Future.map (the_result o snd) result_ctxt;
    val finished_goal = Goal.protect 0 (Goal.future_result goal_ctxt future_thm prop);
    val state' =
      state
      |> map_goal (K (goal_ctxt, statement, using, finished_goal, NONE, after_qed));
  in (Future.map fst result_ctxt, state') end;

end;


(* terminal proofs *)  (* FIXME avoid toplevel imitation -- include in PIDE/document *)

local

fun future_terminal_proof proof1 proof2 done state =
  if Goal.future_enabled 3 andalso not (is_relevant state) then
    state |> future_proof (fn state' =>
      let
        val pos = Position.thread_data ();
        val props = Markup.command_timing :: (Markup.nameN, "by") :: Position.properties_of pos;
      in
        Execution.fork {name = "Proof.future_terminal_proof", pos = pos, pri = ~1}
          (fn () => ((), Timing.protocol props proof2 state'))
      end) |> snd |> done
  else proof1 state;

in

fun local_future_terminal_proof meths =
  future_terminal_proof
    (local_terminal_proof meths)
    (local_terminal_proof meths #> context_of) local_done_proof;

fun global_future_terminal_proof meths =
  future_terminal_proof
    (global_terminal_proof meths)
    (global_terminal_proof meths) global_done_proof;

end;

end;
