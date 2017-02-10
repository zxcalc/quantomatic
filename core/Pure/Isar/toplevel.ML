(*  Title:      Pure/Isar/toplevel.ML
    Author:     Markus Wenzel, TU Muenchen

Isabelle/Isar toplevel transactions.
*)

signature TOPLEVEL =
sig
  exception UNDEF
  type state
  val theory_toplevel: theory -> state
  val toplevel: state
  val is_toplevel: state -> bool
  val is_theory: state -> bool
  val is_proof: state -> bool
  val is_skipped_proof: state -> bool
  val level: state -> int
  val presentation_context_of: state -> Proof.context
  val previous_context_of: state -> Proof.context option
  val context_of: state -> Proof.context
  val generic_theory_of: state -> generic_theory
  val theory_of: state -> theory
  val proof_of: state -> Proof.state
  val proof_position_of: state -> int
  val end_theory: Position.T -> state -> theory
  val pretty_context: state -> Pretty.T list
  val pretty_state: state -> Pretty.T list
  val string_of_state: state -> string
  val pretty_abstract: state -> Pretty.T
  type transition
  val empty: transition
  val name_of: transition -> string
  val pos_of: transition -> Position.T
  val type_error: transition -> string
  val name: string -> transition -> transition
  val position: Position.T -> transition -> transition
  val init_theory: (unit -> theory) -> transition -> transition
  val is_init: transition -> bool
  val modify_init: (unit -> theory) -> transition -> transition
  val exit: transition -> transition
  val keep: (state -> unit) -> transition -> transition
  val keep': (bool -> state -> unit) -> transition -> transition
  val keep_proof: (state -> unit) -> transition -> transition
  val ignored: Position.T -> transition
  val is_ignored: transition -> bool
  val malformed: Position.T -> string -> transition
  val generic_theory: (generic_theory -> generic_theory) -> transition -> transition
  val theory': (bool -> theory -> theory) -> transition -> transition
  val theory: (theory -> theory) -> transition -> transition
  val begin_local_theory: bool -> (theory -> local_theory) -> transition -> transition
  val end_local_theory: transition -> transition
  val open_target: (generic_theory -> local_theory) -> transition -> transition
  val close_target: transition -> transition
  val local_theory': (bool * Position.T) option -> (xstring * Position.T) option ->
    (bool -> local_theory -> local_theory) -> transition -> transition
  val local_theory: (bool * Position.T) option -> (xstring * Position.T) option ->
    (local_theory -> local_theory) -> transition -> transition
  val present_local_theory: (xstring * Position.T) option -> (state -> unit) ->
    transition -> transition
  val local_theory_to_proof': (bool * Position.T) option -> (xstring * Position.T) option ->
    (bool -> local_theory -> Proof.state) -> transition -> transition
  val local_theory_to_proof: (bool * Position.T) option -> (xstring * Position.T) option ->
    (local_theory -> Proof.state) -> transition -> transition
  val theory_to_proof: (theory -> Proof.state) -> transition -> transition
  val end_proof: (bool -> Proof.state -> Proof.context) -> transition -> transition
  val forget_proof: bool -> transition -> transition
  val proofs': (bool -> Proof.state -> Proof.state Seq.result Seq.seq) -> transition -> transition
  val proof': (bool -> Proof.state -> Proof.state) -> transition -> transition
  val proofs: (Proof.state -> Proof.state Seq.result Seq.seq) -> transition -> transition
  val proof: (Proof.state -> Proof.state) -> transition -> transition
  val actual_proof: (Proof_Node.T -> Proof_Node.T) -> transition -> transition
  val skip_proof: (unit -> unit) -> transition -> transition
  val skip_proof_open: transition -> transition
  val skip_proof_close: transition -> transition
  val exec_id: Document_ID.exec -> transition -> transition
  val setmp_thread_position: transition -> ('a -> 'b) -> 'a -> 'b
  val add_hook: (transition -> state -> state -> unit) -> unit
  val get_timing: transition -> Time.time option
  val put_timing: Time.time option -> transition -> transition
  val transition: bool -> transition -> state -> state * (exn * string) option
  val command_errors: bool -> transition -> state -> Runtime.error list * state option
  val command_exception: bool -> transition -> state -> state
  val reset_theory: state -> state option
  val reset_proof: state -> state option
  type result
  val join_results: result -> (transition * state) list
  val element_result: Keyword.keywords -> transition Thy_Syntax.element -> state -> result * state
end;

structure Toplevel: TOPLEVEL =
struct

(** toplevel state **)

exception UNDEF = Runtime.UNDEF;


(* datatype node *)

datatype node =
  Theory of generic_theory * Proof.context option
    (*theory with presentation context*) |
  Proof of Proof_Node.T * ((Proof.context -> generic_theory) * generic_theory)
    (*proof node, finish, original theory*) |
  Skipped_Proof of int * (generic_theory * generic_theory);
    (*proof depth, resulting theory, original theory*)

val theory_node = fn Theory (gthy, _) => SOME gthy | _ => NONE;
val proof_node = fn Proof (prf, _) => SOME prf | _ => NONE;
val skipped_proof_node = fn Skipped_Proof _ => true | _ => false;

fun cases_node f _ (Theory (gthy, _)) = f gthy
  | cases_node _ g (Proof (prf, _)) = g (Proof_Node.current prf)
  | cases_node f _ (Skipped_Proof (_, (gthy, _))) = f gthy;

val context_node = cases_node Context.proof_of Proof.context_of;


(* datatype state *)

datatype state = State of node option * node option;  (*current, previous*)

fun theory_toplevel thy = State (SOME (Theory (Context.Theory thy, NONE)), NONE);

val toplevel = State (NONE, NONE);

fun is_toplevel (State (NONE, _)) = true
  | is_toplevel _ = false;

fun level (State (NONE, _)) = 0
  | level (State (SOME (Theory _), _)) = 0
  | level (State (SOME (Proof (prf, _)), _)) = Proof.level (Proof_Node.current prf)
  | level (State (SOME (Skipped_Proof (d, _)), _)) = d + 1;   (*different notion of proof depth!*)

fun str_of_state (State (NONE, SOME (Theory (Context.Theory thy, _)))) =
      "at top level, result theory " ^ quote (Context.theory_name thy)
  | str_of_state (State (NONE, _)) = "at top level"
  | str_of_state (State (SOME (Theory (Context.Theory _, _)), _)) = "in theory mode"
  | str_of_state (State (SOME (Theory (Context.Proof _, _)), _)) = "in local theory mode"
  | str_of_state (State (SOME (Proof _), _)) = "in proof mode"
  | str_of_state (State (SOME (Skipped_Proof _), _)) = "in skipped proof mode";


(* current node *)

fun node_of (State (NONE, _)) = raise UNDEF
  | node_of (State (SOME node, _)) = node;

fun is_theory state = not (is_toplevel state) andalso is_some (theory_node (node_of state));
fun is_proof state = not (is_toplevel state) andalso is_some (proof_node (node_of state));
fun is_skipped_proof state = not (is_toplevel state) andalso skipped_proof_node (node_of state);

fun node_case f g state = cases_node f g (node_of state);

fun presentation_context_of state =
  (case try node_of state of
    SOME (Theory (_, SOME ctxt)) => ctxt
  | SOME node => context_node node
  | NONE => raise UNDEF);

fun previous_context_of (State (_, NONE)) = NONE
  | previous_context_of (State (_, SOME prev)) = SOME (context_node prev);

val context_of = node_case Context.proof_of Proof.context_of;
val generic_theory_of = node_case I (Context.Proof o Proof.context_of);
val theory_of = node_case Context.theory_of Proof.theory_of;
val proof_of = node_case (fn _ => error "No proof state") I;

fun proof_position_of state =
  (case node_of state of
    Proof (prf, _) => Proof_Node.position prf
  | _ => ~1);

fun end_theory _ (State (NONE, SOME (Theory (Context.Theory thy, _)))) = thy
  | end_theory pos (State (NONE, _)) = error ("Bad theory" ^ Position.here pos)
  | end_theory pos (State (SOME _, _)) = error ("Unfinished theory" ^ Position.here pos);


(* print state *)

fun pretty_context state =
  (case try node_of state of
    NONE => []
  | SOME node =>
      let
        val gthy =
          (case node of
            Theory (gthy, _) => gthy
          | Proof (_, (_, gthy)) => gthy
          | Skipped_Proof (_, (gthy, _)) => gthy);
        val lthy = Context.cases (Named_Target.theory_init) I gthy;
      in Local_Theory.pretty lthy end);

fun pretty_state state =
  (case try node_of state of
    NONE => []
  | SOME (Theory _) => []
  | SOME (Proof (prf, _)) => Proof.pretty_state (Proof_Node.current prf)
  | SOME (Skipped_Proof (d, _)) => [Pretty.str ("skipped proof: depth " ^ string_of_int d)]);

val string_of_state = pretty_state #> Pretty.chunks #> Pretty.string_of;

fun pretty_abstract state = Pretty.str ("<Isar " ^ str_of_state state ^ ">");

val _ = ML_system_pp (fn _ => fn _ => Pretty.to_polyml o pretty_abstract);



(** toplevel transitions **)

(* node transactions -- maintaining stable checkpoints *)

exception FAILURE of state * exn;

local

fun reset_presentation (Theory (gthy, _)) = Theory (gthy, NONE)
  | reset_presentation node = node;

in

fun apply_transaction f g node =
  let
    val cont_node = reset_presentation node;
    val context = cases_node I (Context.Proof o Proof.context_of) cont_node;
    fun state_error e nd = (State (SOME nd, SOME node), e);

    val (result, err) =
      cont_node
      |> Runtime.controlled_execution (SOME context) f
      |> state_error NONE
      handle exn => state_error (SOME exn) cont_node;
  in
    (case err of
      NONE => tap g result
    | SOME exn => raise FAILURE (result, exn))
  end;

val exit_transaction =
  apply_transaction
    (fn Theory (Context.Theory thy, _) => Theory (Context.Theory (Theory.end_theory thy), NONE)
      | node => node) (K ())
  #> (fn State (node', _) => State (NONE, node'));

end;


(* primitive transitions *)

datatype trans =
  Init of unit -> theory |               (*init theory*)
  Exit |                                 (*formal exit of theory*)
  Keep of bool -> state -> unit |        (*peek at state*)
  Transaction of (bool -> node -> node) * (state -> unit);  (*node transaction and presentation*)

local

fun apply_tr _ (Init f) (State (NONE, _)) =
      State (SOME (Theory (Context.Theory (Runtime.controlled_execution NONE f ()), NONE)), NONE)
  | apply_tr _ Exit (State (SOME (state as Theory (Context.Theory _, _)), _)) =
      exit_transaction state
  | apply_tr int (Keep f) state =
      Runtime.controlled_execution (try generic_theory_of state) (fn x => tap (f int) x) state
  | apply_tr int (Transaction (f, g)) (State (SOME state, _)) =
      apply_transaction (fn x => f int x) g state
  | apply_tr _ _ _ = raise UNDEF;

fun apply_union _ [] state = raise FAILURE (state, UNDEF)
  | apply_union int (tr :: trs) state =
      apply_union int trs state
        handle Runtime.UNDEF => apply_tr int tr state
          | FAILURE (alt_state, UNDEF) => apply_tr int tr alt_state
          | exn as FAILURE _ => raise exn
          | exn => raise FAILURE (state, exn);

in

fun apply_trans int trs state = (apply_union int trs state, NONE)
  handle FAILURE (alt_state, exn) => (alt_state, SOME exn) | exn => (state, SOME exn);

end;


(* datatype transition *)

datatype transition = Transition of
 {name: string,              (*command name*)
  pos: Position.T,           (*source position*)
  timing: Time.time option,  (*prescient timing information*)
  trans: trans list};        (*primitive transitions (union)*)

fun make_transition (name, pos, timing, trans) =
  Transition {name = name, pos = pos, timing = timing, trans = trans};

fun map_transition f (Transition {name, pos, timing, trans}) =
  make_transition (f (name, pos, timing, trans));

val empty = make_transition ("", Position.none, NONE, []);


(* diagnostics *)

fun name_of (Transition {name, ...}) = name;
fun pos_of (Transition {pos, ...}) = pos;

fun command_msg msg tr =
  msg ^ "command " ^ quote (Markup.markup Markup.keyword1 (name_of tr)) ^
    Position.here (pos_of tr);

fun at_command tr = command_msg "At " tr;
fun type_error tr = command_msg "Bad context for " tr;


(* modify transitions *)

fun name name = map_transition (fn (_, pos, timing, trans) =>
  (name, pos, timing, trans));

fun position pos = map_transition (fn (name, _, timing, trans) =>
  (name, pos, timing, trans));

fun add_trans tr = map_transition (fn (name, pos, timing, trans) =>
  (name, pos, timing, tr :: trans));

val reset_trans = map_transition (fn (name, pos, timing, _) =>
  (name, pos, timing, []));


(* basic transitions *)

fun init_theory f = add_trans (Init f);

fun is_init (Transition {trans = [Init _], ...}) = true
  | is_init _ = false;

fun modify_init f tr = if is_init tr then init_theory f (reset_trans tr) else tr;

val exit = add_trans Exit;
val keep' = add_trans o Keep;

fun present_transaction f g = add_trans (Transaction (f, g));
fun transaction f = present_transaction f (K ());

fun keep f = add_trans (Keep (fn _ => f));

fun keep_proof f =
  keep (fn st =>
    if is_proof st then f st
    else if is_skipped_proof st then ()
    else warning "No proof state");

fun ignored pos = empty |> name "<ignored>" |> position pos |> keep (fn _ => ());
fun is_ignored tr = name_of tr = "<ignored>";

fun malformed pos msg =
  empty |> name "<malformed>" |> position pos |> keep (fn _ => error msg);


(* theory transitions *)

fun generic_theory f = transaction (fn _ =>
  (fn Theory (gthy, _) => Theory (f gthy, NONE)
    | _ => raise UNDEF));

fun theory' f = transaction (fn int =>
  (fn Theory (Context.Theory thy, _) =>
      let val thy' = thy
        |> Sign.new_group
        |> f int
        |> Sign.reset_group;
      in Theory (Context.Theory thy', NONE) end
    | _ => raise UNDEF));

fun theory f = theory' (K f);

fun begin_local_theory begin f = transaction (fn _ =>
  (fn Theory (Context.Theory thy, _) =>
        let
          val lthy = f thy;
          val gthy = if begin then Context.Proof lthy else Context.Theory (Named_Target.exit lthy);
          val _ =
            (case Local_Theory.pretty lthy of
              [] => ()
            | prts => Output.state (Pretty.string_of (Pretty.chunks prts)));
        in Theory (gthy, SOME lthy) end
    | _ => raise UNDEF));

val end_local_theory = transaction (fn _ =>
  (fn Theory (Context.Proof lthy, _) => Theory (Context.Theory (Named_Target.exit lthy), SOME lthy)
    | _ => raise UNDEF));

fun open_target f = transaction (fn _ =>
  (fn Theory (gthy, _) =>
        let val lthy = f gthy
        in Theory (Context.Proof lthy, SOME lthy) end
    | _ => raise UNDEF));

val close_target = transaction (fn _ =>
  (fn Theory (Context.Proof lthy, _) =>
        (case try Local_Theory.close_target lthy of
          SOME ctxt' =>
            let
              val gthy' =
                if can Local_Theory.assert ctxt'
                then Context.Proof ctxt'
                else Context.Theory (Proof_Context.theory_of ctxt');
            in Theory (gthy', SOME lthy) end
        | NONE => raise UNDEF)
    | _ => raise UNDEF));

fun restricted_context (SOME (strict, scope)) =
      Proof_Context.map_naming (Name_Space.restricted strict scope)
  | restricted_context NONE = I;

fun local_theory' restricted target f = present_transaction (fn int =>
  (fn Theory (gthy, _) =>
        let
          val (finish, lthy) = Named_Target.switch target gthy;
          val lthy' = lthy
            |> restricted_context restricted
            |> Local_Theory.new_group
            |> f int
            |> Local_Theory.reset_group;
        in Theory (finish lthy', SOME lthy') end
    | _ => raise UNDEF))
  (K ());

fun local_theory restricted target f = local_theory' restricted target (K f);

fun present_local_theory target = present_transaction (fn int =>
  (fn Theory (gthy, _) =>
        let val (finish, lthy) = Named_Target.switch target gthy;
        in Theory (finish lthy, SOME lthy) end
    | _ => raise UNDEF));


(* proof transitions *)

fun end_proof f = transaction (fn int =>
  (fn Proof (prf, (finish, _)) =>
        let val state = Proof_Node.current prf in
          if can (Proof.assert_bottom true) state then
            let
              val ctxt' = f int state;
              val gthy' = finish ctxt';
            in Theory (gthy', SOME ctxt') end
          else raise UNDEF
        end
    | Skipped_Proof (0, (gthy, _)) => Theory (gthy, NONE)
    | _ => raise UNDEF));

local

fun begin_proof init = transaction (fn int =>
  (fn Theory (gthy, _) =>
    let
      val (finish, prf) = init int gthy;
      val skip = Goal.skip_proofs_enabled ();
      val schematic_goal = try Proof.schematic_goal prf;
      val _ =
        if skip andalso schematic_goal = SOME true then
          warning "Cannot skip proof of schematic goal statement"
        else ();
    in
      if skip andalso schematic_goal = SOME false then
        Skipped_Proof (0, (finish (Proof.global_skip_proof true prf), gthy))
      else Proof (Proof_Node.init prf, (finish, gthy))
    end
  | _ => raise UNDEF));

in

fun local_theory_to_proof' restricted target f = begin_proof
  (fn int => fn gthy =>
    let
      val (finish, lthy) = Named_Target.switch target gthy;
      val prf = lthy
        |> restricted_context restricted
        |> Local_Theory.new_group
        |> f int;
    in (finish o Local_Theory.reset_group, prf) end);

fun local_theory_to_proof restricted target f =
  local_theory_to_proof' restricted target (K f);

fun theory_to_proof f = begin_proof
  (fn _ => fn gthy =>
    (Context.Theory o Sign.reset_group o Sign.change_check o Proof_Context.theory_of,
      (case gthy of
        Context.Theory thy => f (Sign.new_group thy)
      | _ => raise UNDEF)));

end;

fun forget_proof strict = transaction (fn _ =>
  (fn Proof (prf, (_, orig_gthy)) =>
        if strict andalso not (Proof.has_bottom_goal (Proof_Node.current prf))
        then raise UNDEF else Theory (orig_gthy, NONE)
    | Skipped_Proof (_, (_, orig_gthy)) => Theory (orig_gthy, NONE)
    | _ => raise UNDEF));

fun proofs' f = transaction (fn int =>
  (fn Proof (prf, x) => Proof (Proof_Node.applys (f int) prf, x)
    | skip as Skipped_Proof _ => skip
    | _ => raise UNDEF));

fun proof' f = proofs' ((Seq.single o Seq.Result) oo f);
val proofs = proofs' o K;
val proof = proof' o K;


(* skipped proofs *)

fun actual_proof f = transaction (fn _ =>
  (fn Proof (prf, x) => Proof (f prf, x)
    | _ => raise UNDEF));

fun skip_proof f = transaction (fn _ =>
  (fn skip as Skipped_Proof _ => (f (); skip)
    | _ => raise UNDEF));

val skip_proof_open = transaction (fn _ =>
  (fn Skipped_Proof (d, x) => Skipped_Proof (d + 1, x)
    | _ => raise UNDEF));

val skip_proof_close = transaction (fn _ =>
  (fn Skipped_Proof (0, (gthy, _)) => Theory (gthy, NONE)
    | Skipped_Proof (d, x) => Skipped_Proof (d - 1, x)
    | _ => raise UNDEF));



(** toplevel transactions **)

(* runtime position *)

fun exec_id id (tr as Transition {pos, ...}) =
  position (Position.put_id (Document_ID.print id) pos) tr;

fun setmp_thread_position (Transition {pos, ...}) f x =
  Position.setmp_thread_data pos f x;


(* post-transition hooks *)

local
  val hooks =
    Synchronized.var "Toplevel.hooks" ([]: (transition -> state -> state -> unit) list);
in

fun add_hook hook = Synchronized.change hooks (cons hook);
fun get_hooks () = Synchronized.value hooks;

end;


(* apply transitions *)

fun get_timing (Transition {timing, ...}) = timing;
fun put_timing timing = map_transition (fn (name, pos, _, trans) => (name, pos, timing, trans));

local

fun app int (tr as Transition {name, trans, ...}) =
  setmp_thread_position tr (fn state =>
    let
      val timing_start = Timing.start ();
      val (result, opt_err) = apply_trans int trans state;
      val timing_result = Timing.result timing_start;
      val timing_props =
        Markup.command_timing :: (Markup.nameN, name_of tr) :: Position.properties_of (pos_of tr);
      val _ = Timing.protocol_message timing_props timing_result;
    in (result, Option.map (fn UNDEF => ERROR (type_error tr) | exn => exn) opt_err) end);

in

fun transition int tr st =
  let
    val (st', opt_err) =
      Context.setmp_generic_context (try (Context.Proof o presentation_context_of) st)
        (fn () => app int tr st) ();
    val opt_err' = opt_err |> Option.map
      (fn Runtime.EXCURSION_FAIL exn_info => exn_info
        | exn => (Runtime.exn_context (try context_of st) exn, at_command tr));
    val _ = get_hooks () |> List.app (fn f => (try (fn () => f tr st st') (); ()));
  in (st', opt_err') end;

end;


(* managed commands *)

fun command_errors int tr st =
  (case transition int tr st of
    (st', NONE) => ([], SOME st')
  | (_, SOME (exn, _)) => (Runtime.exn_messages_ids exn, NONE));

fun command_exception int tr st =
  (case transition int tr st of
    (st', NONE) => st'
  | (_, SOME (exn, info)) =>
      if Exn.is_interrupt exn then Exn.reraise exn
      else raise Runtime.EXCURSION_FAIL (exn, info));

val command = command_exception false;


(* reset state *)

local

fun reset_state check trans st =
  if check st then NONE
  else #2 (command_errors false (trans empty) st);

in

val reset_theory = reset_state is_theory (forget_proof false);

val reset_proof =
  reset_state is_proof
    (transaction (fn _ =>
      (fn Theory (gthy, _) => Skipped_Proof (0, (gthy, gthy))
        | _ => raise UNDEF)));

end;


(* scheduled proof result *)

datatype result =
  Result of transition * state |
  Result_List of result list |
  Result_Future of result future;

fun join_results (Result x) = [x]
  | join_results (Result_List xs) = maps join_results xs
  | join_results (Result_Future x) = join_results (Future.join x);

local

structure Result = Proof_Data
(
  type T = result;
  fun init _ = Result_List [];
);

val get_result = Result.get o Proof.context_of;
val put_result = Proof.map_context o Result.put;

fun timing_estimate include_head elem =
  let
    val trs = Thy_Syntax.flat_element elem |> not include_head ? tl;
    val timings = map get_timing trs;
  in
    if forall is_some timings then
      SOME (fold (curry (op +) o the) timings Time.zeroTime)
    else NONE
  end;

fun priority NONE = ~1
  | priority (SOME estimate) =
      Int.min (Real.floor (Real.max (Math.log10 (Time.toReal estimate), ~3.0)) - 3, ~1);

fun proof_future_enabled estimate st =
  (case try proof_of st of
    NONE => false
  | SOME state =>
      not (Proof.is_relevant state) andalso
       (if can (Proof.assert_bottom true) state
        then Goal.future_enabled 1
        else
          (case estimate of
            NONE => Goal.future_enabled 2
          | SOME t => Goal.future_enabled_timing t)));

fun atom_result keywords tr st =
  let
    val st' =
      if Goal.future_enabled 1 andalso Keyword.is_diag keywords (name_of tr) then
        (Execution.fork
          {name = "Toplevel.diag", pos = pos_of tr,
            pri = priority (timing_estimate true (Thy_Syntax.atom tr))}
          (fn () => command tr st); st)
      else command tr st;
  in (Result (tr, st'), st') end;

in

fun element_result keywords (Thy_Syntax.Element (tr, NONE)) st = atom_result keywords tr st
  | element_result keywords (elem as Thy_Syntax.Element (head_tr, SOME element_rest)) st =
      let
        val (head_result, st') = atom_result keywords head_tr st;
        val (body_elems, end_tr) = element_rest;
        val estimate = timing_estimate false elem;
      in
        if not (proof_future_enabled estimate st')
        then
          let
            val proof_trs = maps Thy_Syntax.flat_element body_elems @ [end_tr];
            val (proof_results, st'') = fold_map (atom_result keywords) proof_trs st';
          in (Result_List (head_result :: proof_results), st'') end
        else
          let
            val finish = Context.Theory o Proof_Context.theory_of;

            val future_proof =
              Proof.future_proof (fn state =>
                Execution.fork
                  {name = "Toplevel.future_proof", pos = pos_of head_tr, pri = priority estimate}
                  (fn () =>
                    let
                      val State (SOME (Proof (prf, (_, orig_gthy))), prev) = st';
                      val prf' = Proof_Node.apply (K state) prf;
                      val (result, result_state) =
                        State (SOME (Proof (prf', (finish, orig_gthy))), prev)
                        |> fold_map (element_result keywords) body_elems ||> command end_tr;
                    in (Result_List result, presentation_context_of result_state) end))
              #> (fn (res, state') => state' |> put_result (Result_Future res));

            val forked_proof =
              proof (future_proof #>
                (fn state => state |> Proof.local_done_proof |> put_result (get_result state))) o
              end_proof (fn _ => future_proof #>
                (fn state => state |> Proof.global_done_proof |> Result.put (get_result state)));

            val st'' = st'
              |> command (head_tr |> reset_trans |> forked_proof);
            val end_result = Result (end_tr, st'');
            val result =
              Result_List [head_result, Result.get (presentation_context_of st''), end_result];
          in (result, st'') end
      end;

end;

end;
