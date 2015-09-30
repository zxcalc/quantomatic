(*  Title:      Pure/PIDE/execution.ML
    Author:     Makarius

Global management of execution.  Unique running execution serves as
barrier for further exploration of forked command execs.
*)

signature EXECUTION =
sig
  val start: unit -> Document_ID.execution
  val discontinue: unit -> unit
  val is_running: Document_ID.execution -> bool
  val is_running_exec: Document_ID.exec -> bool
  val running: Document_ID.execution -> Document_ID.exec -> Future.group list -> bool
  val peek: Document_ID.exec -> Future.group list
  val cancel: Document_ID.exec -> unit
  type params = {name: string, pos: Position.T, pri: int}
  val fork: params -> (unit -> 'a) -> 'a future
  val print: params -> (unit -> unit) -> unit
  val fork_prints: Document_ID.exec -> unit
  val purge: Document_ID.exec list -> unit
  val reset: unit -> Future.group list
  val shutdown: unit -> unit
end;

structure Execution: EXECUTION =
struct

(* global state *)

type print = {name: string, pri: int, body: unit -> unit};
type exec_state = Future.group list * print list;  (*active forks, prints*)
type state =
  Document_ID.execution * (*overall document execution*)
  exec_state Inttab.table;  (*running command execs*)

val init_state: state = (Document_ID.none, Inttab.make [(Document_ID.none, ([], []))]);
val state = Synchronized.var "Execution.state" init_state;

fun get_state () = Synchronized.value state;
fun change_state_result f = Synchronized.change_result state f;
fun change_state f = Synchronized.change state f;

fun unregistered exec_id = "Unregistered execution: " ^ Document_ID.print exec_id;


(* unique running execution *)

fun start () =
  let
    val execution_id = Document_ID.make ();
    val _ = change_state (apfst (K execution_id));
  in execution_id end;

fun discontinue () = change_state (apfst (K Document_ID.none));

fun is_running execution_id = execution_id = #1 (get_state ());


(* execs *)

fun is_running_exec exec_id =
  Inttab.defined (#2 (get_state ())) exec_id;

fun running execution_id exec_id groups =
  change_state_result (fn (execution_id', execs) =>
    let
      val ok = execution_id = execution_id' andalso not (Inttab.defined execs exec_id);
      val execs' = execs |> ok ? Inttab.update (exec_id, (groups, []));
    in (ok, (execution_id', execs')) end);

fun peek exec_id =
  (case Inttab.lookup (#2 (get_state ())) exec_id of
    SOME (groups, _) => groups
  | NONE => []);

fun cancel exec_id = List.app Future.cancel_group (peek exec_id);


(* fork *)

fun status task markups =
  let
    val props =
      if ! Multithreading.trace >= 2
      then [(Markup.taskN, Task_Queue.str_of_task task)] else [];
  in Output.status (implode (map (Markup.markup_only o Markup.properties props) markups)) end;

type params = {name: string, pos: Position.T, pri: int};

fun fork ({name, pos, pri}: params) e =
  uninterruptible (fn _ => Position.setmp_thread_data pos (fn () =>
    let
      val exec_id = the_default 0 (Position.parse_id pos);
      val group = Future.worker_subgroup ();
      val _ = change_state (apsnd (fn execs =>
        (case Inttab.lookup execs exec_id of
          SOME (groups, prints) =>
            Inttab.update (exec_id, (group :: groups, prints)) execs
        | NONE => raise Fail (unregistered exec_id))));

      val future =
        (singleton o Future.forks)
          {name = name, group = SOME group, deps = [], pri = pri, interrupts = false}
          (fn () =>
            let
              val task = the (Future.worker_task ());
              val _ = status task [Markup.running];
              val result =
                Exn.capture (Future.interruptible_task e) ()
                |> Future.identify_result pos;
              val _ = status task [Markup.joined];
              val _ =
                (case result of
                  Exn.Exn exn =>
                   (status task [Markup.failed];
                    status task [Markup.finished];
                    Output.report [Markup.markup_only Markup.bad];
                    if exec_id = 0 then ()
                    else List.app (Future.error_message pos) (Runtime.exn_messages_ids exn))
                | Exn.Res _ =>
                    status task [Markup.finished])
            in Exn.release result end);

      val _ = status (Future.task_of future) [Markup.forked];
    in future end)) ();


(* print *)

fun print ({name, pos, pri}: params) e =
  change_state (apsnd (fn execs =>
    let
      val exec_id = the_default 0 (Position.parse_id pos);
      val print = {name = name, pri = pri, body = e};
    in
      (case Inttab.lookup execs exec_id of
        SOME (groups, prints) => Inttab.update (exec_id, (groups, print :: prints)) execs
      | NONE => raise Fail (unregistered exec_id))
    end));

fun fork_prints exec_id =
  (case Inttab.lookup (#2 (get_state ())) exec_id of
    SOME (_, prints) =>
      if null prints orelse null (tl prints) orelse not (Multithreading.enabled ())
      then List.app (fn {body, ...} => body ()) (rev prints)
      else
        let val pos = Position.thread_data () in
          List.app (fn {name, pri, body} =>
            ignore (fork {name = name, pos = pos, pri = pri} body)) (rev prints)
        end
  | NONE => raise Fail (unregistered exec_id));


(* cleanup *)

fun purge exec_ids =
  (change_state o apsnd) (fn execs =>
    let
      val execs' = fold Inttab.delete_safe exec_ids execs;
      val () =
        (execs', ()) |-> Inttab.fold (fn (exec_id, (groups, _)) => fn () =>
          if Inttab.defined execs' exec_id then ()
          else groups |> List.app (fn group =>
            if Task_Queue.is_canceled group then ()
            else raise Fail ("Attempt to purge valid execution: " ^ Document_ID.print exec_id)));
    in execs' end);

fun reset () =
  change_state_result (fn (_, execs) =>
    let val groups = Inttab.fold (append o #1 o #2) execs []
    in (groups, init_state) end);

fun shutdown () =
  (Future.shutdown ();
    (case maps Task_Queue.group_status (reset ()) of
      [] => ()
    | exns => raise Par_Exn.make exns));

end;

