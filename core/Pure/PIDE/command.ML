(*  Title:      Pure/PIDE/command.ML
    Author:     Makarius

Prover command execution: read -- eval -- print.
*)

signature COMMAND =
sig
  type blob = (string * (SHA1.digest * string list) option) Exn.result
  val read_file: Path.T -> Position.T -> Path.T -> Token.file
  val read_thy: Toplevel.state -> theory
  val read: Keyword.keywords -> theory -> Path.T-> (unit -> theory) ->
    blob list * int -> Token.T list -> Toplevel.transition
  type eval
  val eval_eq: eval * eval -> bool
  val eval_running: eval -> bool
  val eval_finished: eval -> bool
  val eval_result_state: eval -> Toplevel.state
  val eval: Keyword.keywords -> Path.T -> (unit -> theory) ->
    blob list * int -> Token.T list -> eval -> eval
  type print
  val print: bool -> (string * string list) list -> Keyword.keywords -> string ->
    eval -> print list -> print list option
  type print_fn = Toplevel.transition -> Toplevel.state -> unit
  type print_function =
    {keywords: Keyword.keywords, command_name: string, args: string list, exec_id: Document_ID.exec} ->
      {delay: Time.time option, pri: int, persistent: bool, strict: bool, print_fn: print_fn} option
  val print_function: string -> print_function -> unit
  val no_print_function: string -> unit
  type exec = eval * print list
  val init_exec: theory option -> exec
  val no_exec: exec
  val exec_ids: exec option -> Document_ID.exec list
  val exec: Document_ID.execution -> exec -> unit
end;

structure Command: COMMAND =
struct

(** main phases of execution **)

fun task_context group f =
  f
  |> Future.interruptible_task
  |> Future.task_context "Command.run_process" group;


(* read *)

type blob =
  (string * (SHA1.digest * string list) option) Exn.result;  (*file node name, digest, lines*)

fun read_file_node file_node master_dir pos src_path =
  let
    val _ = Position.report pos Markup.language_path;
    val _ =
      (case try Url.explode file_node of
        NONE => ()
      | SOME (Url.File _) => ()
      | _ =>
          error ("Prover cannot load remote file " ^
            Markup.markup (Markup.path file_node) (quote file_node)));
    val full_path = File.check_file (File.full_path master_dir src_path);
    val text = File.read full_path;
    val lines = split_lines text;
    val digest = SHA1.digest text;
  in {src_path = src_path, lines = lines, digest = digest, pos = Path.position full_path} end
  handle ERROR msg => error (msg ^ Position.here pos);

val read_file = read_file_node "";

local

fun blob_file src_path lines digest file_node =
  let
    val file_pos =
      Position.file file_node |>
      (case Position.get_id (Position.thread_data ()) of
        NONE => I
      | SOME exec_id => Position.put_id exec_id);
  in {src_path = src_path, lines = lines, digest = digest, pos = file_pos} end

fun resolve_files keywords master_dir (blobs, blobs_index) toks =
  (case Outer_Syntax.parse_spans toks of
    [Command_Span.Span (Command_Span.Command_Span (cmd, _), _)] =>
      (case try (nth toks) blobs_index of
        SOME tok =>
          let
            val pos = Token.pos_of tok;
            val path = Path.explode (Token.content_of tok)
              handle ERROR msg => error (msg ^ Position.here pos);
            fun make_file src_path (Exn.Res (file_node, NONE)) =
                  Exn.interruptible_capture (fn () =>
                    read_file_node file_node master_dir pos src_path) ()
              | make_file src_path (Exn.Res (file_node, SOME (digest, lines))) =
                  (Position.report pos Markup.language_path;
                    Exn.Res (blob_file src_path lines digest file_node))
              | make_file _ (Exn.Exn e) = Exn.Exn e;
            val src_paths = Keyword.command_files keywords cmd path;
            val files =
              if null blobs then
                map2 make_file src_paths (map (K (Exn.Res ("", NONE))) src_paths)
              else if length src_paths = length blobs then
                map2 make_file src_paths blobs
              else error ("Misalignment of inlined files" ^ Position.here pos);
          in
            toks |> map_index (fn (i, tok) =>
              if i = blobs_index then Token.put_files files tok else tok)
          end
      | NONE => toks)
  | _ => toks);

fun reports_of_token keywords tok =
  let
    val malformed_symbols =
      Input.source_explode (Token.input_of tok)
      |> map_filter (fn (sym, pos) =>
          if Symbol.is_malformed sym
          then SOME ((pos, Markup.bad), "Malformed symbolic character") else NONE);
    val is_malformed = Token.is_error tok orelse not (null malformed_symbols);
    val reports = Token.reports keywords tok @ Token.completion_report tok @ malformed_symbols;
  in (is_malformed, reports) end;

in

fun read_thy st = Toplevel.theory_of st
  handle Toplevel.UNDEF => Pure_Syn.bootstrap_thy;

fun read keywords thy master_dir init blobs_info span =
  let
    val command_reports = Outer_Syntax.command_reports thy;

    val proper_range = Token.range_of (#1 (take_suffix Token.is_improper span));
    val pos =
      (case find_first Token.is_command span of
        SOME tok => Token.pos_of tok
      | NONE => #1 proper_range);

    val token_reports = map (reports_of_token keywords) span;
    val _ = Position.reports_text (maps #2 token_reports @ maps command_reports span);
  in
    if exists #1 token_reports then Toplevel.malformed pos "Malformed command syntax"
    else
      (case Outer_Syntax.parse_tokens thy (resolve_files keywords master_dir blobs_info span) of
        [tr] => Toplevel.modify_init init tr
      | [] => Toplevel.ignored (#1 (Token.range_of span))
      | _ => Toplevel.malformed (#1 proper_range) "Exactly one command expected")
      handle ERROR msg => Toplevel.malformed (#1 proper_range) msg
  end;

end;


(* eval *)

type eval_state = {failed: bool, command: Toplevel.transition, state: Toplevel.state};

fun init_eval_state opt_thy =
 {failed = false,
  command = Toplevel.empty,
  state = (case opt_thy of NONE => Toplevel.toplevel | SOME thy => Toplevel.theory_toplevel thy)};

datatype eval = Eval of {exec_id: Document_ID.exec, eval_process: eval_state lazy};

fun eval_exec_id (Eval {exec_id, ...}) = exec_id;
val eval_eq = op = o apply2 eval_exec_id;

val eval_running = Execution.is_running_exec o eval_exec_id;
fun eval_finished (Eval {eval_process, ...}) = Lazy.is_finished eval_process;

fun eval_result (Eval {eval_process, ...}) =
  task_context (Future.worker_subgroup ()) Lazy.force eval_process;

val eval_result_state = #state o eval_result;

local

fun reset_state keywords tr st0 = Toplevel.setmp_thread_position tr (fn () =>
  let
    val name = Toplevel.name_of tr;
    val res =
      if Keyword.is_theory_body keywords name then Toplevel.reset_theory st0
      else if Keyword.is_proof keywords name then Toplevel.reset_proof st0
      else NONE;
  in
    (case res of
      NONE => st0
    | SOME st => (Output.error_message (Toplevel.type_error tr ^ " -- using reset state"); st))
  end) ();

fun run keywords int tr st =
  if Goal.future_enabled 1 andalso Keyword.is_diag keywords (Toplevel.name_of tr) then
    (Execution.fork {name = "Toplevel.diag", pos = Toplevel.pos_of tr, pri = ~1}
      (fn () => Toplevel.command_exception int tr st); ([], SOME st))
  else Toplevel.command_errors int tr st;

fun check_cmts span tr st' =
  Toplevel.setmp_thread_position tr
    (fn () =>
      Outer_Syntax.side_comments span |> maps (fn cmt =>
        (Thy_Output.output_text st' {markdown = false} (Token.input_of cmt); [])
          handle exn =>
            if Exn.is_interrupt exn then Exn.reraise exn
            else Runtime.exn_messages_ids exn)) ();

fun report tr m =
  Toplevel.setmp_thread_position tr (fn () => Output.report [Markup.markup_only m]) ();

fun status tr m =
  Toplevel.setmp_thread_position tr (fn () => Output.status (Markup.markup_only m)) ();

fun command_indent tr st =
  (case try Toplevel.proof_of st of
    SOME prf =>
      let val keywords = Thy_Header.get_keywords (Proof.theory_of prf) in
        if Keyword.command_kind keywords (Toplevel.name_of tr) = SOME Keyword.prf_script then
          (case try Proof.goal prf of
            SOME {goal, ...} =>
              let val n = Thm.nprems_of goal
              in if n > 1 then report tr (Markup.command_indent (n - 1)) else () end
          | NONE => ())
        else ()
      end
  | NONE => ());


fun eval_state keywords span tr ({state, ...}: eval_state) =
  let
    val _ = Thread_Attributes.expose_interrupt ();

    val st = reset_state keywords tr state;

    val _ = command_indent tr st;
    val _ = status tr Markup.running;
    val (errs1, result) = run keywords true tr st;
    val errs2 = (case result of NONE => [] | SOME st' => check_cmts span tr st');
    val errs = errs1 @ errs2;
    val _ = List.app (Future.error_message (Toplevel.pos_of tr)) errs;
  in
    (case result of
      NONE =>
        let
          val _ = status tr Markup.failed;
          val _ = status tr Markup.finished;
          val _ = if null errs then (report tr Markup.bad; Exn.interrupt ()) else ();
        in {failed = true, command = tr, state = st} end
    | SOME st' =>
        let
          val _ = status tr Markup.finished;
        in {failed = false, command = tr, state = st'} end)
  end;

in

fun eval keywords master_dir init blobs_info span eval0 =
  let
    val exec_id = Document_ID.make ();
    fun process () =
      let
        val eval_state0 = eval_result eval0;
        val thy = read_thy (#state eval_state0);
        val tr =
          Position.setmp_thread_data (Position.id_only (Document_ID.print exec_id))
            (fn () =>
              read keywords thy master_dir init blobs_info span |> Toplevel.exec_id exec_id) ();
      in eval_state keywords span tr eval_state0 end;
  in Eval {exec_id = exec_id, eval_process = Lazy.lazy process} end;

end;


(* print *)

datatype print = Print of
 {name: string, args: string list, delay: Time.time option, pri: int, persistent: bool,
  exec_id: Document_ID.exec, print_process: unit lazy};

fun print_exec_id (Print {exec_id, ...}) = exec_id;
val print_eq = op = o apply2 print_exec_id;

type print_fn = Toplevel.transition -> Toplevel.state -> unit;

type print_function =
  {keywords: Keyword.keywords, command_name: string, args: string list, exec_id: Document_ID.exec} ->
    {delay: Time.time option, pri: int, persistent: bool, strict: bool, print_fn: print_fn} option;

local

val print_functions =
  Synchronized.var "Command.print_functions" ([]: (string * print_function) list);

fun print_error tr opt_context e =
  (Toplevel.setmp_thread_position tr o Runtime.controlled_execution opt_context) e ()
    handle exn =>
      if Exn.is_interrupt exn then Exn.reraise exn
      else List.app (Future.error_message (Toplevel.pos_of tr)) (Runtime.exn_messages_ids exn);

fun print_finished (Print {print_process, ...}) = Lazy.is_finished print_process;

fun print_persistent (Print {persistent, ...}) = persistent;

val overlay_ord = prod_ord string_ord (list_ord string_ord);

in

fun print command_visible command_overlays keywords command_name eval old_prints =
  let
    val print_functions = Synchronized.value print_functions;

    fun make_print name args {delay, pri, persistent, strict, print_fn} =
      let
        val exec_id = Document_ID.make ();
        fun process () =
          let
            val {failed, command, state = st', ...} = eval_result eval;
            val tr = Toplevel.exec_id exec_id command;
            val opt_context = try Toplevel.generic_theory_of st';
          in
            if failed andalso strict then ()
            else print_error tr opt_context (fn () => print_fn tr st')
          end;
      in
        Print {
          name = name, args = args, delay = delay, pri = pri, persistent = persistent,
          exec_id = exec_id, print_process = Lazy.lazy process}
      end;

    fun bad_print name args exn =
      make_print name args {delay = NONE, pri = 0, persistent = false,
        strict = false, print_fn = fn _ => fn _ => Exn.reraise exn};

    fun new_print name args get_pr =
      let
        val params =
         {keywords = keywords,
          command_name = command_name,
          args = args,
          exec_id = eval_exec_id eval};
      in
        (case Exn.capture (Runtime.controlled_execution NONE get_pr) params of
          Exn.Res NONE => NONE
        | Exn.Res (SOME pr) => SOME (make_print name args pr)
        | Exn.Exn exn => SOME (bad_print name args exn))
      end;

    fun get_print (a, b) =
      (case find_first (fn Print {name, args, ...} => name = a andalso args = b) old_prints of
        NONE =>
          (case AList.lookup (op =) print_functions a of
            NONE => SOME (bad_print a b (ERROR ("Missing print function " ^ quote a)))
          | SOME get_pr => new_print a b get_pr)
      | some => some);

    val new_prints =
      if command_visible then
        fold (fn (a, _) => cons (a, [])) print_functions command_overlays
        |> sort_distinct overlay_ord
        |> map_filter get_print
      else filter (fn print => print_finished print andalso print_persistent print) old_prints;
  in
    if eq_list print_eq (old_prints, new_prints) then NONE else SOME new_prints
  end;

fun print_function name f =
  Synchronized.change print_functions (fn funs =>
   (if not (AList.defined (op =) funs name) then ()
    else warning ("Redefining command print function: " ^ quote name);
    AList.update (op =) (name, f) funs));

fun no_print_function name =
  Synchronized.change print_functions (filter_out (equal name o #1));

end;

val _ =
  print_function "Execution.print"
    (fn {args, exec_id, ...} =>
      if null args then
        SOME {delay = NONE, pri = Task_Queue.urgent_pri + 2, persistent = false, strict = false,
          print_fn = fn _ => fn _ => Execution.fork_prints exec_id}
      else NONE);

val _ =
  print_function "print_state"
    (fn {keywords, command_name, ...} =>
      if Options.default_bool "editor_output_state" andalso Keyword.is_printed keywords command_name
      then
        SOME {delay = NONE, pri = Task_Queue.urgent_pri + 1, persistent = false, strict = false,
          print_fn = fn _ => fn st =>
            if Toplevel.is_proof st then Output.state (Toplevel.string_of_state st)
            else ()}
      else NONE);


(* combined execution *)

type exec = eval * print list;

fun init_exec opt_thy : exec =
  (Eval {exec_id = Document_ID.none, eval_process = Lazy.value (init_eval_state opt_thy)}, []);

val no_exec = init_exec NONE;

fun exec_ids NONE = []
  | exec_ids (SOME (eval, prints)) = eval_exec_id eval :: map print_exec_id prints;

local

fun run_process execution_id exec_id process =
  let val group = Future.worker_subgroup () in
    if Execution.running execution_id exec_id [group] then
      ignore (task_context group Lazy.force_result process)
    else ()
  end;

fun ignore_process process =
  Lazy.is_running process orelse Lazy.is_finished process;

fun run_eval execution_id (Eval {exec_id, eval_process}) =
  if Lazy.is_finished eval_process then ()
  else run_process execution_id exec_id eval_process;

fun run_print execution_id (Print {name, delay, pri, exec_id, print_process, ...}) =
  if ignore_process print_process then ()
  else if pri <= 0 orelse (Multithreading.enabled () andalso Options.default_bool "parallel_print")
  then
    let
      val group = Future.worker_subgroup ();
      fun fork () =
        ignore ((singleton o Future.forks)
          {name = name, group = SOME group, deps = [], pri = pri, interrupts = true}
          (fn () =>
            if ignore_process print_process then ()
            else run_process execution_id exec_id print_process));
    in
      (case delay of
        NONE => fork ()
      | SOME d => ignore (Event_Timer.request (Time.now () + d) fork))
    end
  else run_process execution_id exec_id print_process;

in

fun exec execution_id (eval, prints) =
  (run_eval execution_id eval; List.app (run_print execution_id) prints);

end;

end;

