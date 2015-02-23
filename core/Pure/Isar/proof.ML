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
  val bind_terms: (indexname * term option) list -> state -> state
  val put_thms: bool -> string * thm list option -> state -> state
  val the_facts: state -> thm list
  val the_fact: state -> thm
  val set_facts: thm list -> state -> state
  val reset_facts: state -> state
  val assert_forward: state -> state
  val assert_chain: state -> state
  val assert_forward_or_chain: state -> state
  val assert_backward: state -> state
  val assert_no_chain: state -> state
  val enter_forward: state -> state
  val goal_message: (unit -> Pretty.T) -> state -> state
  val pretty_goal_messages: state -> Pretty.T list
  val pretty_state: int -> state -> Pretty.T list
  val refine: Method.text -> state -> state Seq.seq
  val refine_end: Method.text -> state -> state Seq.seq
  val refine_insert: thm list -> state -> state
  val refine_goals: (context -> thm -> unit) -> context -> thm list -> state -> state Seq.seq
  val raw_goal: state -> {context: context, facts: thm list, goal: thm}
  val goal: state -> {context: context, facts: thm list, goal: thm}
  val simple_goal: state -> {context: context, goal: thm}
  val status_markup: state -> Markup.T
  val let_bind: (term list * term) list -> state -> state
  val let_bind_cmd: (string list * string) list -> state -> state
  val write: Syntax.mode -> (term * mixfix) list -> state -> state
  val write_cmd: Syntax.mode -> (string * mixfix) list -> state -> state
  val fix: (binding * typ option * mixfix) list -> state -> state
  val fix_cmd: (binding * string option * mixfix) list -> state -> state
  val assm: Assumption.export ->
    (Thm.binding * (term * term list) list) list -> state -> state
  val assm_cmd: Assumption.export ->
    (Attrib.binding * (string * string list) list) list -> state -> state
  val assume: (Thm.binding * (term * term list) list) list -> state -> state
  val assume_cmd: (Attrib.binding * (string * string list) list) list -> state -> state
  val presume: (Thm.binding * (term * term list) list) list -> state -> state
  val presume_cmd: (Attrib.binding * (string * string list) list) list -> state -> state
  val def: (Thm.binding * ((binding * mixfix) * (term * term list))) list -> state -> state
  val def_cmd: (Attrib.binding * ((binding * mixfix) * (string * string list))) list -> state -> state
  val chain: state -> state
  val chain_facts: thm list -> state -> state
  val note_thmss: (Thm.binding * (thm list * attribute list) list) list -> state -> state
  val note_thmss_cmd: (Attrib.binding * (Facts.ref * Attrib.src list) list) list -> state -> state
  val from_thmss: ((thm list * attribute list) list) list -> state -> state
  val from_thmss_cmd: ((Facts.ref * Attrib.src list) list) list -> state -> state
  val with_thmss: ((thm list * attribute list) list) list -> state -> state
  val with_thmss_cmd: ((Facts.ref * Attrib.src list) list) list -> state -> state
  val using: ((thm list * attribute list) list) list -> state -> state
  val using_cmd: ((Facts.ref * Attrib.src list) list) list -> state -> state
  val unfolding: ((thm list * attribute list) list) list -> state -> state
  val unfolding_cmd: ((Facts.ref * Attrib.src list) list) list -> state -> state
  val invoke_case: (string * Position.T) * binding option list * attribute list ->
    state -> state
  val invoke_case_cmd: (string * Position.T) * binding option list * Attrib.src list ->
    state -> state
  val begin_block: state -> state
  val next_block: state -> state
  val end_block: state -> state
  val begin_notepad: context -> state
  val end_notepad: state -> context
  val proof: Method.text option -> state -> state Seq.seq
  val proof_results: Method.text_range option -> state -> state Seq.result Seq.seq
  val defer: int -> state -> state
  val prefer: int -> state -> state
  val apply: Method.text -> state -> state Seq.seq
  val apply_end: Method.text -> state -> state Seq.seq
  val apply_results: Method.text_range -> state -> state Seq.result Seq.seq
  val apply_end_results: Method.text_range -> state -> state Seq.result Seq.seq
  val local_goal: (context -> ((string * string) * (string * thm list) list) -> unit) ->
    (context -> 'a -> attribute) ->
    ('b list -> context -> (term list list * (context -> context)) * context) ->
    string -> Method.text option -> (thm list list -> state -> state) ->
    ((binding * 'a list) * 'b) list -> state -> state
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
  val have: Method.text option -> (thm list list -> state -> state) ->
    (Thm.binding * (term * term list) list) list -> bool -> state -> state
  val have_cmd: Method.text option -> (thm list list -> state -> state) ->
    (Attrib.binding * (string * string list) list) list -> bool -> state -> state
  val show: Method.text option -> (thm list list -> state -> state) ->
    (Thm.binding * (term * term list) list) list -> bool -> state -> state
  val show_cmd: Method.text option -> (thm list list -> state -> state) ->
    (Attrib.binding * (string * string list) list) list -> bool -> state -> state
  val schematic_goal: state -> bool
  val is_relevant: state -> bool
  val future_proof: (state -> ('a * context) future) -> state -> 'a future * state
  val local_future_terminal_proof: Method.text_range * Method.text_range option -> bool ->
    state -> state
  val global_future_terminal_proof: Method.text_range * Method.text_range option -> bool ->
    state -> context
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
    facts: thm list option,
    mode: mode,
    goal: goal option}
and goal =
  Goal of
   {statement: (string * Position.T) * term list list * term,
      (*goal kind and statement (starting with vars), initial proposition*)
    messages: (unit -> Pretty.T) list,    (*persistent messages (hints etc.)*)
    using: thm list,                      (*goal facts*)
    goal: thm,                            (*subgoals ==> statement*)
    before_qed: Method.text option,
    after_qed:
      (thm list list -> state -> state) *
      (thm list list -> context -> context)};

fun make_goal (statement, messages, using, goal, before_qed, after_qed) =
  Goal {statement = statement, messages = messages, using = using, goal = goal,
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

fun top (State st) = Stack.top st |> (fn Node node => node);
fun map_top f (State st) = State (Stack.map_top (map_node f) st);
fun map_all f (State st) = State (Stack.map_all (map_node f) st);



(** basic proof state operations **)

(* block structure *)

fun open_block (State st) = State (Stack.push st);

fun close_block (State st) = State (Stack.pop st)
  handle List.Empty => error "Unbalanced block parentheses";

fun level (State st) = Stack.level st;

fun assert_bottom b state =
  let val b' = level state <= 2 in
    if b andalso not b' then error "Not at bottom of proof"
    else if not b andalso b' then error "Already at bottom of proof"
    else state
  end;


(* context *)

val context_of = #context o top;
val theory_of = Proof_Context.theory_of o context_of;

fun map_node_context f =
  map_node (fn (ctxt, facts, mode, goal) => (f ctxt, facts, mode, goal));

fun map_context f =
  map_top (fn (ctxt, facts, mode, goal) => (f ctxt, facts, mode, goal));

fun map_context_result f state =
  f (context_of state) ||> (fn ctxt => map_context (K ctxt) state);

fun map_contexts f = map_all (fn (ctxt, facts, mode, goal) => (f ctxt, facts, mode, goal));

fun propagate_ml_env state = map_contexts
  (Context.proof_map (ML_Env.inherit (Context.Proof (context_of state)))) state;

val bind_terms = map_context o Proof_Context.bind_terms;
val put_thms = map_context oo Proof_Context.put_thms;


(* facts *)

val get_facts = #facts o top;

fun the_facts state =
  (case get_facts state of SOME facts => facts
  | NONE => error "No current facts available");

fun the_fact state =
  (case the_facts state of [thm] => thm
  | _ => error "Single theorem expected");

fun put_facts facts =
  map_top (fn (ctxt, _, mode, goal) => (ctxt, facts, mode, goal)) #>
  put_thms true (Auto_Bind.thisN, facts);

val set_facts = put_facts o SOME;
val reset_facts = put_facts NONE;

fun these_factss more_facts (named_factss, state) =
  (named_factss, state |> set_facts (maps snd named_factss @ more_facts));

fun export_facts inner outer =
  (case get_facts inner of
    NONE => reset_facts outer
  | SOME thms =>
      thms
      |> Proof_Context.export (context_of inner) (context_of outer)
      |> (fn ths => set_facts ths outer));


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
    {context, goal = SOME (Goal goal), ...} => (context, goal)
  | _ => error "No current goal");

fun assert_current_goal g state =
  let val g' = can current_goal state in
    if g andalso not g' then error "No goal in this block"
    else if not g andalso g' then error "Goal present in this block"
    else state
  end;

fun put_goal goal = map_top (fn (ctxt, using, mode, _) => (ctxt, using, mode, goal));

val set_goal = put_goal o SOME;
val reset_goal = put_goal NONE;

val before_qed = #before_qed o #2 o current_goal;


(* nested goal *)

fun map_goal f g h (State (Node {context, facts, mode, goal = SOME goal}, node :: nodes)) =
      let
        val Goal {statement, messages, using, goal, before_qed, after_qed} = goal;
        val goal' = make_goal (g (statement, messages, using, goal, before_qed, after_qed));
        val node' = map_node_context h node;
      in State (make_node (f context, facts, mode, SOME goal'), node' :: nodes) end
  | map_goal f g h (State (nd, node :: nodes)) =
      let
        val nd' = map_node_context f nd;
        val State (node', nodes') = map_goal f g h (State (node, nodes));
      in State (nd', node' :: nodes') end
  | map_goal _ _ _ state = state;

fun provide_goal goal = map_goal I (fn (statement, _, using, _, before_qed, after_qed) =>
  (statement, [], using, goal, before_qed, after_qed)) I;

fun goal_message msg = map_goal I (fn (statement, messages, using, goal, before_qed, after_qed) =>
  (statement, msg :: messages, using, goal, before_qed, after_qed)) I;

fun using_facts using = map_goal I (fn (statement, _, _, goal, before_qed, after_qed) =>
  (statement, [], using, goal, before_qed, after_qed)) I;

local
  fun find i state =
    (case try current_goal state of
      SOME (ctxt, goal) => (ctxt, (i, goal))
    | NONE => find (i + 1) (close_block state handle ERROR _ => error "No goal present"));
in val find_goal = find 0 end;

fun get_goal state =
  let val (ctxt, (_, {using, goal, ...})) = find_goal state
  in (ctxt, (using, goal)) end;



(** pretty_state **)

fun pretty_goal_messages state =
  (case try find_goal state of
    SOME (_, (_, {messages, ...})) => map (fn msg => msg ()) (rev messages)
  | NONE => []);

fun pretty_facts _ _ NONE = []
  | pretty_facts ctxt s (SOME ths) = [Proof_Display.pretty_goal_facts ctxt s ths, Pretty.str ""];

fun pretty_state nr state =
  let
    val {context = ctxt, facts, mode, goal = _} = top state;
    val verbose = Config.get ctxt Proof_Context.verbose;

    fun prt_goal (SOME (_, (_,
      {statement = ((_, pos), _, _), messages, using, goal, before_qed = _, after_qed = _}))) =
          pretty_facts ctxt "using"
            (if mode <> Backward orelse null using then NONE else SOME using) @
          [Proof_Display.pretty_goal_header goal] @ Goal_Display.pretty_goals ctxt goal @
          (map (fn msg => Position.setmp_thread_data pos msg ()) (rev messages))
      | prt_goal NONE = [];

    val prt_ctxt =
      if verbose orelse mode = Forward then Proof_Context.pretty_context ctxt
      else if mode = Backward then Proof_Context.pretty_ctxt ctxt
      else [];

    val position_markup = Position.markup (Position.thread_data ()) Markup.position;
  in
    [Pretty.block
      [Pretty.mark_str (position_markup, "proof"),
        Pretty.str (" (" ^ mode_name mode ^ "): depth " ^ string_of_int (level state div 2 - 1))],
      Pretty.str ""] @
    (if null prt_ctxt then [] else prt_ctxt @ [Pretty.str ""]) @
    (if verbose orelse mode = Forward then
       pretty_facts ctxt "" facts @ prt_goal (try find_goal state)
     else if mode = Chain then pretty_facts ctxt "picking" facts
     else prt_goal (try find_goal state))
  end;



(** proof steps **)

(* refine via method *)

local

fun goalN i = "goal" ^ string_of_int i;
fun goals st = map goalN (1 upto Thm.nprems_of st);

fun no_goal_cases st = map (rpair NONE) (goals st);

fun goal_cases st =
  Rule_Cases.make_common
    (Thm.theory_of_thm st, Thm.prop_of st) (map (rpair [] o rpair []) (goals st));

fun apply_method current_context method state =
  let
    val (goal_ctxt, (_, {statement, messages = _, using, goal, before_qed, after_qed})) =
      find_goal state;
    val ctxt = if current_context then context_of state else goal_ctxt;
  in
    Method.apply method ctxt using goal |> Seq.map (fn (meth_cases, goal') =>
      state
      |> map_goal
          (Proof_Context.update_cases false (no_goal_cases goal @ goal_cases goal') #>
           Proof_Context.update_cases true meth_cases)
          (K (statement, [], using, goal', before_qed, after_qed)) I)
  end;

fun select_goals n meth state =
  ALLGOALS Goal.conjunction_tac (#2 (#2 (get_goal state)))
  |> Seq.maps (fn goal =>
    state
    |> Seq.lift provide_goal ((PRIMITIVE (Goal.restrict 1 n) THEN Goal.conjunction_tac 1) goal)
    |> Seq.maps meth
    |> Seq.maps (fn state' => state'
      |> Seq.lift provide_goal (PRIMITIVE (Goal.unrestrict 1) (#2 (#2 (get_goal state')))))
    |> Seq.maps (apply_method true (K Method.succeed)));

fun apply_text current_context text state =
  let
    val ctxt = context_of state;

    fun eval (Method.Basic m) = apply_method current_context m
      | eval (Method.Source src) = apply_method current_context (Method.method_cmd ctxt src)
      | eval (Method.Then (_, txts)) = Seq.EVERY (map eval txts)
      | eval (Method.Orelse (_, txts)) = Seq.FIRST (map eval txts)
      | eval (Method.Try (_, txt)) = Seq.TRY (eval txt)
      | eval (Method.Repeat1 (_, txt)) = Seq.REPEAT1 (eval txt)
      | eval (Method.Select_Goals (_, n, txt)) = select_goals n (eval txt);
  in eval text state end;

in

val refine = apply_text true;
val refine_end = apply_text false;
fun refine_insert ths = Seq.hd o refine (Method.Basic (K (Method.insert ths)));

end;


(* refine via sub-proof *)

local

fun finish_tac _ 0 = K all_tac
  | finish_tac ctxt n =
      Goal.norm_hhf_tac ctxt THEN'
      SUBGOAL (fn (goal, i) =>
        if can Logic.unprotect (Logic.strip_assums_concl goal) then
          etac Drule.protectI i THEN finish_tac ctxt (n - 1) i
        else finish_tac ctxt (n - 1) (i + 1));

fun goal_tac ctxt rule =
  Goal.norm_hhf_tac ctxt THEN'
  rtac rule THEN'
  finish_tac ctxt (Thm.nprems_of rule);

fun FINDGOAL tac st =
  let fun find i n = if i > n then Seq.fail else Seq.APPEND (tac i, find (i + 1) n)
  in find 1 (Thm.nprems_of st) st end;

in

fun refine_goals print_rule inner raw_rules state =
  let
    val (outer, (_, goal)) = get_goal state;
    fun refine rule st = (print_rule outer rule; FINDGOAL (goal_tac outer rule) st);
  in
    raw_rules
    |> Proof_Context.goal_export inner outer
    |> (fn rules => Seq.lift provide_goal (EVERY (map refine rules) goal) state)
  end;

end;


(* conclude goal *)

fun conclude_goal ctxt goal propss =
  let
    val thy = Proof_Context.theory_of ctxt;

    val _ =
      Theory.subthy (theory_of_thm goal, thy) orelse error "Bad background theory of goal state";
    val _ = Thm.no_prems goal orelse error (Proof_Display.string_of_goal ctxt goal);

    fun lost_structure () = error ("Lost goal structure:\n" ^ Display.string_of_thm ctxt goal);

    val th =
      (Goal.conclude (if length (flat propss) > 1 then Thm.norm_proof goal else goal)
        handle THM _ => lost_structure ())
      |> Drule.flexflex_unique
      |> Thm.check_shyps (Variable.sorts_of ctxt)
      |> Thm.check_hyps (Context.Proof ctxt);

    val goal_propss = filter_out null propss;
    val results =
      Conjunction.elim_balanced (length goal_propss) th
      |> map2 Conjunction.elim_balanced (map length goal_propss)
      handle THM _ => lost_structure ();
    val _ = Unify.matches_list thy (flat goal_propss) (map Thm.prop_of (flat results)) orelse
      error ("Proved a different theorem:\n" ^ Display.string_of_thm ctxt th);

    fun recover_result ([] :: pss) thss = [] :: recover_result pss thss
      | recover_result (_ :: pss) (ths :: thss) = ths :: recover_result pss thss
      | recover_result [] [] = []
      | recover_result _ _ = lost_structure ();
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
  let val (ctxt, (facts, goal)) = get_goal state
  in {context = ctxt, facts = facts, goal = goal} end;

val goal = raw_goal o refine_insert [];

fun simple_goal state =
  let
    val (_, (facts, _)) = get_goal state;
    val (ctxt, (_, goal)) = get_goal (refine_insert facts state);
  in {context = ctxt, goal = goal} end;

fun status_markup state =
  (case try goal state of
    SOME {goal, ...} => Markup.proof_state (Thm.nprems_of goal)
  | NONE => Markup.empty);

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

val let_bind = gen_bind Proof_Context.match_bind_i;
val let_bind_cmd = gen_bind Proof_Context.match_bind;

end;


(* concrete syntax *)

local

fun gen_write prep_arg mode args =
  assert_forward
  #> map_context (fn ctxt => ctxt |> Proof_Context.notation true mode (map (prep_arg ctxt) args))
  #> reset_facts;

fun read_arg ctxt (c, mx) =
  (case Proof_Context.read_const {proper = false, strict = false} ctxt c of
    Free (x, _) =>
      let val T = Proof_Context.infer_type ctxt (x, Mixfix.mixfixT mx)
      in (Free (x, T), mx) end
  | t => (t, mx));

in

val write = gen_write (K I);
val write_cmd = gen_write read_arg;

end;


(* fix *)

local

fun gen_fix prep_vars args =
  assert_forward
  #> map_context (fn ctxt => snd (Proof_Context.add_fixes (fst (prep_vars args ctxt)) ctxt))
  #> reset_facts;

in

val fix = gen_fix Proof_Context.cert_vars;
val fix_cmd = gen_fix Proof_Context.read_vars;

end;


(* assume etc. *)

local

fun gen_assume asm prep_att exp args state =
  state
  |> assert_forward
  |> map_context_result (asm exp (Attrib.map_specs (map (prep_att (context_of state))) args))
  |> these_factss [] |> #2;

in

val assm = gen_assume Proof_Context.add_assms_i (K I);
val assm_cmd = gen_assume Proof_Context.add_assms Attrib.attribute_cmd;
val assume = assm Assumption.assume_export;
val assume_cmd = assm_cmd Assumption.assume_export;
val presume = assm Assumption.presume_export;
val presume_cmd = assm_cmd Assumption.presume_export;

end;


(* def *)

local

fun gen_def prep_att prep_vars prep_binds args state =
  let
    val _ = assert_forward state;
    val (raw_name_atts, (raw_vars, raw_rhss)) = args |> split_list ||> split_list;
    val name_atts = map (apsnd (map (prep_att (context_of state)))) raw_name_atts;
  in
    state
    |> map_context_result (prep_vars (map (fn (x, mx) => (x, NONE, mx)) raw_vars))
    |>> map (fn (x, _, mx) => (x, mx))
    |-> (fn vars =>
      map_context_result (prep_binds false (map swap raw_rhss))
      #-> (fn rhss =>
        let
          val defs = (vars ~~ (name_atts ~~ rhss)) |> map (fn ((x, mx), ((a, atts), rhs)) =>
            ((x, mx), ((Thm.def_binding_optional x a, atts), rhs)));
        in map_context_result (Local_Defs.add_defs defs) end))
    |-> (set_facts o map (#2 o #2))
  end;

in

val def = gen_def (K I) Proof_Context.cert_vars Proof_Context.match_bind_i;
val def_cmd = gen_def Attrib.attribute_cmd Proof_Context.read_vars Proof_Context.match_bind;

end;



(** facts **)

(* chain *)

fun clean_facts ctxt =
  set_facts (filter_out Thm.is_dummy (the_facts ctxt)) ctxt;

val chain =
  assert_forward
  #> clean_facts
  #> enter_chain;

fun chain_facts facts =
  set_facts facts
  #> chain;


(* note etc. *)

fun no_binding args = map (pair (Binding.empty, [])) args;

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

val from_thmss = gen_thmss (K []) chain #2 (K I) (K I) o no_binding;
val from_thmss_cmd =
  gen_thmss (K []) chain #2 Attrib.attribute_cmd Proof_Context.get_fact o no_binding;

val with_thmss = gen_thmss the_facts chain #2 (K I) (K I) o no_binding;
val with_thmss_cmd =
  gen_thmss the_facts chain #2 Attrib.attribute_cmd Proof_Context.get_fact o no_binding;

val local_results = gen_thmss (K []) I I (K I) (K I) o map (apsnd Thm.simple_fact);

end;


(* using/unfolding *)

local

fun gen_using f g prep_att prep_fact args state =
  state
  |> assert_backward
  |> map_context_result
    (fn ctxt => ctxt |> Proof_Context.note_thmss ""
      (Attrib.map_facts_refs (map (prep_att ctxt)) (prep_fact ctxt) (no_binding args)))
  |> (fn (named_facts, state') =>
    state' |> map_goal I (fn (statement, _, using, goal, before_qed, after_qed) =>
      let
        val ctxt = context_of state';
        val ths = maps snd named_facts;
      in (statement, [], f ctxt ths using, g ctxt ths goal, before_qed, after_qed) end) I);

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

fun gen_invoke_case internal prep_att ((name, pos), xs, raw_atts) state =
  let
    val atts = map (prep_att (context_of state)) raw_atts;
    val (asms, state') = state |> map_context_result (fn ctxt =>
      ctxt |> Proof_Context.apply_case (Proof_Context.check_case ctxt internal (name, pos) xs));
    val assumptions =
      asms |> map (fn (b, ts) => ((Binding.set_pos pos b, atts), map (rpair []) ts));
  in
    state'
    |> assume assumptions
    |> bind_terms Auto_Bind.no_facts
    |> `the_facts |-> (fn thms => note_thmss [((Binding.make (name, pos), []), [(thms, [])])])
  end;

in

val invoke_case = gen_invoke_case true (K I);
val invoke_case_cmd = gen_invoke_case false Attrib.attribute_cmd;

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
  assert_backward
  #> refine (the_default Method.default_text opt_text)
  #> Seq.map (using_facts [] #> enter_forward);

fun proof_results arg =
  Seq.APPEND (proof (Method.text arg) #> Seq.make_results,
    method_error "initial" (Method.position arg));

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
      |> Seq.maps (refine finish_text)
      |> Seq.make_results, method_error "terminal" terminal_pos)
    #> Seq.maps_results (Seq.single o finished_goal finished_pos)
  end;

fun check_result msg sq =
  (case Seq.pull sq of
    NONE => error msg
  | SOME (s, _) => s);


(* unstructured refinement *)

fun defer i =
  assert_no_chain #>
  refine (Method.Basic (fn _ => METHOD (fn _ => ASSERT_SUBGOAL defer_tac i))) #> Seq.hd;

fun prefer i =
  assert_no_chain #>
  refine (Method.Basic (fn _ => METHOD (fn _ => ASSERT_SUBGOAL prefer_tac i))) #> Seq.hd;

fun apply text = assert_backward #> refine text #> Seq.map (using_facts []);

fun apply_end text = assert_forward #> refine_end text;

fun apply_results (text, (pos, _)) =
  Seq.APPEND (apply text #> Seq.make_results, method_error "" pos);

fun apply_end_results (text, (pos, _)) =
  Seq.APPEND (apply_end text #> Seq.make_results, method_error "" pos);



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
  refine (Method.Basic (K (RAW_METHOD
    (K (HEADGOAL (PRECISE_CONJUNCTS n
      (HEADGOAL (CONJUNCTS (ALLGOALS (rtac Drule.termI))))))))))
  #> Seq.hd;

in

fun generic_goal prepp kind before_qed after_qed raw_propp state =
  let
    val thy = theory_of state;
    val cert = Thm.cterm_of thy;
    val chaining = can assert_chain state;
    val pos = Position.thread_data ();

    val ((propss, after_ctxt), goal_state) =
      state
      |> assert_forward_or_chain
      |> enter_forward
      |> open_block
      |> map_context_result (prepp raw_propp);
    val props = flat propss;

    val vars = implicit_vars props;
    val propss' = vars :: propss;
    val goal_propss = filter_out null propss';
    val goal =
      cert (Logic.mk_conjunction_balanced (map Logic.mk_conjunction_balanced goal_propss))
      |> Thm.weaken_sorts (Variable.sorts_of (context_of goal_state));
    val statement = ((kind, pos), propss', Thm.term_of goal);
    val after_qed' = after_qed |>> (fn after_local =>
      fn results => map_context after_ctxt #> after_local results);
  in
    goal_state
    |> map_context (init_context #> Variable.set_body true)
    |> set_goal (make_goal (statement, [], [], Goal.init goal, before_qed, after_qed'))
    |> map_context (Proof_Context.auto_bind_goal props)
    |> chaining ? (`the_facts #-> using_facts)
    |> reset_facts
    |> open_block
    |> reset_goal
    |> enter_backward
    |> not (null vars) ? refine_terms (length goal_propss)
    |> null props ? (refine (Method.Basic Method.assumption) #> Seq.hd)
  end;

fun generic_qed after_ctxt state =
  let
    val (goal_ctxt, {statement = (_, stmt, _), goal, after_qed, ...}) = current_goal state;
    val outer_state = state |> close_block;
    val outer_ctxt = context_of outer_state;

    val props =
      flat (tl stmt)
      |> Variable.exportT_terms goal_ctxt outer_ctxt;
    val results =
      tl (conclude_goal goal_ctxt goal stmt)
      |> burrow (Proof_Context.export goal_ctxt outer_ctxt);
  in
    outer_state
    |> map_context (after_ctxt props)
    |> pair (after_qed, results)
  end;

end;


(* local goals *)

fun local_goal print_results prep_att prepp kind before_qed after_qed stmt state =
  let
    val ((names, attss), propp) =
      Attrib.map_specs (map (prep_att (context_of state))) stmt |> split_list |>> split_list;

    fun after_qed' results =
      local_results ((names ~~ attss) ~~ results)
      #-> (fn res => tap (fn st => print_results (context_of st) ((kind, ""), res) : unit))
      #> after_qed results;
  in
    state
    |> generic_goal prepp kind before_qed (after_qed', K I) propp
    |> tap (Variable.warn_extra_tfrees (context_of state) o context_of)
  end;

fun local_qeds arg =
  end_proof false arg
  #> Seq.map_result (generic_qed Proof_Context.auto_bind_facts #->
    (fn ((after_qed, _), results) => after_qed results));

fun local_qed arg =
  local_qeds (Position.none, arg) #> Seq.the_result finished_goal_error;


(* global goals *)

fun prepp_auto_fixes prepp args =
  prepp args #>
  (fn ((propss, a), ctxt) => ((propss, a), (fold o fold) Variable.auto_fixes propss ctxt));

fun global_goal prepp before_qed after_qed propp =
  init #>
  generic_goal (prepp_auto_fixes prepp) "" before_qed (K I, after_qed) propp;

val theorem = global_goal Proof_Context.bind_propp_schematic_i;
val theorem_cmd = global_goal Proof_Context.bind_propp_schematic;

fun global_qeds arg =
  end_proof true arg
  #> Seq.map_result (generic_qed (K I) #> (fn (((_, after_qed), results), state) =>
    after_qed results (context_of state)));

fun global_qed arg =
  global_qeds (Position.none, arg) #> Seq.the_result finished_goal_error;


(* terminal proof steps *)

local

fun terminal_proof qeds initial terminal =
  proof_results (SOME initial) #> Seq.maps_results (qeds (#2 (#2 initial), terminal))
  #> Seq.the_result "";

in

fun local_terminal_proof (text, opt_text) = terminal_proof local_qeds text (opt_text, true);
val local_default_proof = local_terminal_proof ((Method.default_text, Position.no_range), NONE);
val local_immediate_proof = local_terminal_proof ((Method.this_text, Position.no_range), NONE);
val local_done_proof = terminal_proof local_qeds (Method.done_text, Position.no_range) (NONE, false);

fun global_terminal_proof (text, opt_text) = terminal_proof global_qeds text (opt_text, true);
val global_default_proof = global_terminal_proof ((Method.default_text, Position.no_range), NONE);
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

local

fun gen_have prep_att prepp before_qed after_qed stmt int =
  local_goal (Proof_Display.print_results int (Position.thread_data ()))
    prep_att prepp "have" before_qed after_qed stmt;

fun gen_show prep_att prepp before_qed after_qed stmt int state =
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
        |> Markup.markup Markup.state
        |> writeln
      else ();
    val test_proof =
      local_skip_proof true
      |> Unsynchronized.setmp testing true
      |> Exn.interruptible_capture;

    fun after_qed' results =
      refine_goals print_rule (context_of state) (flat results)
      #> check_result "Failed to refine any pending goal"
      #> after_qed results;
  in
    state
    |> local_goal print_results prep_att prepp "show" before_qed after_qed' stmt
    |> int ? (fn goal_state =>
      (case test_proof (map_context (Context_Position.set_visible false) goal_state) of
        Exn.Res _ => goal_state
      | Exn.Exn exn => raise Exn.EXCEPTIONS ([exn, ERROR (fail_msg (context_of goal_state))])))
  end;

in

val have = gen_have (K I) Proof_Context.bind_propp_i;
val have_cmd = gen_have Attrib.attribute_cmd Proof_Context.bind_propp;
val show = gen_show (K I) Proof_Context.bind_propp_i;
val show_cmd = gen_show Attrib.attribute_cmd Proof_Context.bind_propp;

end;



(** future proofs **)

(* relevant proof states *)

fun schematic_goal state =
  let val (_, (_, {statement = (_, _, prop), ...})) = find_goal state
  in Goal.is_schematic prop end;

fun is_relevant state =
  (case try find_goal state of
    NONE => true
  | SOME (_, (_, {statement = (_, _, prop), goal, ...})) =>
      Goal.is_schematic prop orelse not (Logic.protect prop aconv Thm.concl_of goal));


(* full proofs *)

local

structure Result = Proof_Data
(
  type T = thm option;
  val empty = NONE;
  fun init _ = empty;
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
    val (goal_ctxt, (_, goal)) = find_goal state;
    val {statement as (kind, _, prop), messages, using, goal, before_qed, after_qed} = goal;
    val goal_tfrees =
      fold Term.add_tfrees
        (prop :: map Thm.term_of (Assumption.all_assms_of goal_ctxt)) [];

    val _ = is_relevant state andalso error "Cannot fork relevant proof";

    val prop' = Logic.protect prop;
    val statement' = (kind, [[], [prop']], prop');
    val goal' = Thm.adjust_maxidx_thm (Thm.maxidx_of goal) (Goal.protect (Thm.nprems_of goal) goal);
    val after_qed' = (fn [[th]] => map_context (set_result th), fn [[th]] => set_result th);

    val result_ctxt =
      state
      |> map_context reset_result
      |> map_goal I (K (statement', messages, using, goal', before_qed, after_qed'))
        (fold (Variable.declare_typ o TFree) goal_tfrees)
      |> fork_proof;

    val future_thm = Future.map (the_result o snd) result_ctxt;
    val finished_goal = Goal.future_result goal_ctxt future_thm prop';
    val state' =
      state
      |> map_goal I (K (statement, messages, using, finished_goal, NONE, after_qed)) I;
  in (Future.map fst result_ctxt, state') end;

end;


(* terminal proofs *)  (* FIXME avoid toplevel imitation -- include in PIDE/document *)

local

fun future_terminal_proof proof1 proof2 done int state =
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

