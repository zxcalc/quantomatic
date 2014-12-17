(*  Title:      Pure/Concurrent/future.ML
    Author:     Makarius

Value-oriented parallel execution via futures and promises.
*)

signature FUTURE =
sig
  type task = Task_Queue.task
  type group = Task_Queue.group
  val new_group: group option -> group
  val worker_task: unit -> task option
  val worker_group: unit -> group option
  val the_worker_group: unit -> group
  val worker_subgroup: unit -> group
  type 'a future
  val task_of: 'a future -> task
  val peek: 'a future -> 'a Exn.result option
  val is_finished: 'a future -> bool
  val ML_statistics: bool Unsynchronized.ref
  val interruptible_task: ('a -> 'b) -> 'a -> 'b
  val cancel_group: group -> unit
  val cancel: 'a future -> unit
  val error_message: Position.T -> (serial * string) * string option -> unit
  val identify_result: Position.T -> 'a Exn.result -> 'a Exn.result
  type params = {name: string, group: group option, deps: task list, pri: int, interrupts: bool}
  val default_params: params
  val forks: params -> (unit -> 'a) list -> 'a future list
  val fork: (unit -> 'a) -> 'a future
  val join_results: 'a future list -> 'a Exn.result list
  val join_result: 'a future -> 'a Exn.result
  val joins: 'a future list -> 'a list
  val join: 'a future -> 'a
  val join_tasks: task list -> unit
  val task_context: string -> group -> ('a -> 'b) -> 'a -> 'b
  val value_result: 'a Exn.result -> 'a future
  val value: 'a -> 'a future
  val cond_forks: params -> (unit -> 'a) list -> 'a future list
  val map: ('a -> 'b) -> 'a future -> 'b future
  val promise_group: group -> (unit -> unit) -> 'a future
  val promise: (unit -> unit) -> 'a future
  val fulfill_result: 'a future -> 'a Exn.result -> unit
  val fulfill: 'a future -> 'a -> unit
  val group_snapshot: group -> task list
  val terminate: group -> unit
  val shutdown: unit -> unit
end;

structure Future: FUTURE =
struct

(** future values **)

type task = Task_Queue.task;
type group = Task_Queue.group;
val new_group = Task_Queue.new_group;


(* identifiers *)

local
  val tag = Universal.tag () : task option Universal.tag;
in
  fun worker_task () = the_default NONE (Thread.getLocal tag);
  fun setmp_worker_task task f x = setmp_thread_data tag (worker_task ()) (SOME task) f x;
end;

val worker_group = Option.map Task_Queue.group_of_task o worker_task;

fun the_worker_group () =
  (case worker_group () of
    SOME group => group
  | NONE => raise Fail "Missing worker thread context");

fun worker_subgroup () = new_group (worker_group ());

fun worker_joining e =
  (case worker_task () of
    NONE => e ()
  | SOME task => Task_Queue.joining task e);

fun worker_waiting deps e =
  (case worker_task () of
    NONE => e ()
  | SOME task => Task_Queue.waiting task deps e);


(* datatype future *)

type 'a result = 'a Exn.result Single_Assignment.var;

datatype 'a future = Future of
 {promised: bool,
  task: task,
  result: 'a result};

fun task_of (Future {task, ...}) = task;
fun result_of (Future {result, ...}) = result;

fun peek x = Single_Assignment.peek (result_of x);
fun is_finished x = is_some (peek x);



(** scheduling **)

(* synchronization *)

val scheduler_event = ConditionVar.conditionVar ();
val work_available = ConditionVar.conditionVar ();
val work_finished = ConditionVar.conditionVar ();

local
  val lock = Mutex.mutex ();
in

fun SYNCHRONIZED name = Simple_Thread.synchronized name lock;

fun wait cond = (*requires SYNCHRONIZED*)
  Multithreading.sync_wait NONE NONE cond lock;

fun wait_timeout timeout cond = (*requires SYNCHRONIZED*)
  Multithreading.sync_wait NONE (SOME (Time.+ (Time.now (), timeout))) cond lock;

fun signal cond = (*requires SYNCHRONIZED*)
  ConditionVar.signal cond;

fun broadcast cond = (*requires SYNCHRONIZED*)
  ConditionVar.broadcast cond;

end;


(* global state *)

val queue = Unsynchronized.ref Task_Queue.empty;
val next = Unsynchronized.ref 0;
val scheduler = Unsynchronized.ref (NONE: Thread.thread option);
val canceled = Unsynchronized.ref ([]: group list);
val do_shutdown = Unsynchronized.ref false;
val max_workers = Unsynchronized.ref 0;
val max_active = Unsynchronized.ref 0;
val worker_trend = Unsynchronized.ref 0;

val status_ticks = Unsynchronized.ref 0;
val last_round = Unsynchronized.ref Time.zeroTime;
val next_round = seconds 0.05;

datatype worker_state = Working | Waiting | Sleeping;
val workers = Unsynchronized.ref ([]: (Thread.thread * worker_state Unsynchronized.ref) list);

fun count_workers state = (*requires SYNCHRONIZED*)
  fold (fn (_, state_ref) => fn i => if ! state_ref = state then i + 1 else i) (! workers) 0;



(* status *)

val ML_statistics = Unsynchronized.ref false;

fun report_status () = (*requires SYNCHRONIZED*)
  if ! ML_statistics then
    let
      val {ready, pending, running, passive} = Task_Queue.status (! queue);
      val total = length (! workers);
      val active = count_workers Working;
      val waiting = count_workers Waiting;
      val stats =
       [("now", Markup.print_real (Time.toReal (Time.now ()))),
        ("tasks_ready", Markup.print_int ready),
        ("tasks_pending", Markup.print_int pending),
        ("tasks_running", Markup.print_int running),
        ("tasks_passive", Markup.print_int passive),
        ("workers_total", Markup.print_int total),
        ("workers_active", Markup.print_int active),
        ("workers_waiting", Markup.print_int waiting)] @
        ML_Statistics.get ();
    in Output.try_protocol_message (Markup.ML_statistics :: stats) [] end
  else ();


(* cancellation primitives *)

fun cancel_now group = (*requires SYNCHRONIZED*)
  let
    val running = Task_Queue.cancel (! queue) group;
    val _ = running |> List.app (fn thread =>
      if Simple_Thread.is_self thread then ()
      else Simple_Thread.interrupt_unsynchronized thread);
  in running end;

fun cancel_all () = (*requires SYNCHRONIZED*)
  let
    val (groups, threads) = Task_Queue.cancel_all (! queue);
    val _ = List.app Simple_Thread.interrupt_unsynchronized threads;
  in groups end;

fun cancel_later group = (*requires SYNCHRONIZED*)
 (Unsynchronized.change canceled (insert Task_Queue.eq_group group);
  broadcast scheduler_event);

fun interruptible_task f x =
  (if Multithreading.available then
    Multithreading.with_attributes
      (if is_some (worker_task ())
       then Multithreading.private_interrupts
       else Multithreading.public_interrupts)
      (fn _ => f x)
   else interruptible f x)
  before Multithreading.interrupted ();


(* worker threads *)

fun worker_exec (task, jobs) =
  let
    val group = Task_Queue.group_of_task task;
    val valid = not (Task_Queue.is_canceled group);
    val ok =
      Task_Queue.running task (fn () =>
        setmp_worker_task task (fn () =>
          fold (fn job => fn ok => job valid andalso ok) jobs true) ());
    val _ =
      if ! Multithreading.trace >= 2 then
        Output.try_protocol_message (Markup.task_statistics :: Task_Queue.task_statistics task) []
      else ();
    val _ = SYNCHRONIZED "finish" (fn () =>
      let
        val maximal = Unsynchronized.change_result queue (Task_Queue.finish task);
        val test = Exn.capture Multithreading.interrupted ();
        val _ =
          if ok andalso not (Exn.is_interrupt_exn test) then ()
          else if null (cancel_now group) then ()
          else cancel_later group;
        val _ = broadcast work_finished;
        val _ = if maximal then () else signal work_available;
      in () end);
  in () end;

fun worker_wait active cond = (*requires SYNCHRONIZED*)
  (case AList.lookup Thread.equal (! workers) (Thread.self ()) of
    SOME state =>
     (state := (if active then Waiting else Sleeping);
      wait cond;
      state := Working)
  | NONE => ignore (wait cond));

fun worker_next () = (*requires SYNCHRONIZED*)
  if length (! workers) > ! max_workers then
    (Unsynchronized.change workers (AList.delete Thread.equal (Thread.self ()));
     signal work_available;
     NONE)
  else if count_workers Working > ! max_active then
    (worker_wait false work_available; worker_next ())
  else
    (case Unsynchronized.change_result queue (Task_Queue.dequeue (Thread.self ())) of
      NONE => (worker_wait false work_available; worker_next ())
    | some => (signal work_available; some));

fun worker_loop name =
  (case SYNCHRONIZED name (fn () => worker_next ()) of
    NONE => ()
  | SOME work => (worker_exec work; worker_loop name));

fun worker_start name = (*requires SYNCHRONIZED*)
  Unsynchronized.change workers (cons (Simple_Thread.fork false (fn () => worker_loop name),
    Unsynchronized.ref Working));


(* scheduler *)

fun scheduler_next () = (*requires SYNCHRONIZED*)
  let
    val now = Time.now ();
    val tick = Time.<= (Time.+ (! last_round, next_round), now);
    val _ = if tick then last_round := now else ();


    (* runtime status *)

    val _ =
      if tick then Unsynchronized.change status_ticks (fn i => i + 1) else ();
    val _ =
      if tick andalso ! status_ticks mod (if ! Multithreading.trace >= 1 then 2 else 10) = 0
      then report_status () else ();

    val _ =
      if forall (Thread.isActive o #1) (! workers) then ()
      else
        let
          val (alive, dead) = List.partition (Thread.isActive o #1) (! workers);
          val _ = workers := alive;
        in
          Multithreading.tracing 0 (fn () =>
            "SCHEDULER: disposed " ^ string_of_int (length dead) ^ " dead worker threads")
        end;


    (* worker pool adjustments *)

    val max_active0 = ! max_active;
    val max_workers0 = ! max_workers;

    val m = if ! do_shutdown then 0 else Multithreading.max_threads_value ();
    val _ = max_active := m;

    val mm =
      if ! do_shutdown then 0
      else Int.min (Int.max (count_workers Working + 2 * count_workers Waiting, m), 4 * m);
    val _ =
      if tick andalso mm > ! max_workers then
        Unsynchronized.change worker_trend (fn w => if w < 0 then 0 else w + 1)
      else if tick andalso mm < ! max_workers then
        Unsynchronized.change worker_trend (fn w => if w > 0 then 0 else w - 1)
      else ();
    val _ =
      if mm = 0 orelse ! worker_trend > 50 orelse ! worker_trend < ~50 then
        max_workers := mm
      else if ! worker_trend > 5 andalso ! max_workers < 2 * m orelse ! max_workers = 0 then
        max_workers := Int.min (mm, 2 * m)
      else ();

    val missing = ! max_workers - length (! workers);
    val _ =
      if missing > 0 then
        funpow missing (fn () =>
          ignore (worker_start ("worker " ^ string_of_int (Unsynchronized.inc next)))) ()
      else ();

    val _ =
      if ! max_active = max_active0 andalso ! max_workers = max_workers0 then ()
      else signal work_available;


    (* canceled groups *)

    val _ =
      if null (! canceled) then ()
      else
       (Multithreading.tracing 1 (fn () =>
          string_of_int (length (! canceled)) ^ " canceled groups");
        Unsynchronized.change canceled (filter_out (null o cancel_now));
        signal work_available);


    (* delay loop *)

    val _ = Exn.release (wait_timeout next_round scheduler_event);


    (* shutdown *)

    val _ = if Task_Queue.all_passive (! queue) then do_shutdown := true else ();
    val continue = not (! do_shutdown andalso null (! workers));
    val _ = if continue then () else (report_status (); scheduler := NONE);

    val _ = broadcast scheduler_event;
  in continue end
  handle exn =>
    if Exn.is_interrupt exn then
     (Multithreading.tracing 1 (fn () => "SCHEDULER: Interrupt");
      List.app cancel_later (cancel_all ());
      signal work_available; true)
    else reraise exn;

fun scheduler_loop () =
 (while
    Multithreading.with_attributes
      (Multithreading.sync_interrupts Multithreading.public_interrupts)
      (fn _ => SYNCHRONIZED "scheduler" (fn () => scheduler_next ()))
  do (); last_round := Time.zeroTime);

fun scheduler_active () = (*requires SYNCHRONIZED*)
  (case ! scheduler of NONE => false | SOME thread => Thread.isActive thread);

fun scheduler_check () = (*requires SYNCHRONIZED*)
 (do_shutdown := false;
  if scheduler_active () then ()
  else scheduler := SOME (Simple_Thread.fork false scheduler_loop));



(** futures **)

(* cancel *)

fun cancel_group_unsynchronized group = (*requires SYNCHRONIZED*)
  let
    val _ = if null (cancel_now group) then () else cancel_later group;
    val _ = signal work_available;
    val _ = scheduler_check ();
  in () end;

fun cancel_group group =
  SYNCHRONIZED "cancel_group" (fn () => cancel_group_unsynchronized group);

fun cancel x = cancel_group (Task_Queue.group_of_task (task_of x));


(* results *)

fun error_message pos ((serial, msg), exec_id) =
  Position.setmp_thread_data pos (fn () =>
    let val id = Position.get_id pos in
      if is_none id orelse is_none exec_id orelse id = exec_id
      then Output.error_message' (serial, msg) else ()
    end) ();

fun identify_result pos res =
  (case res of
    Exn.Exn exn =>
      let val exec_id =
        (case Position.get_id pos of
          NONE => []
        | SOME id => [(Markup.exec_idN, id)])
      in Exn.Exn (Par_Exn.identify exec_id exn) end
  | _ => res);

fun assign_result group result res =
  let
    val _ = Single_Assignment.assign result res
      handle exn as Fail _ =>
        (case Single_Assignment.peek result of
          SOME (Exn.Exn e) => reraise (if Exn.is_interrupt e then e else exn)
        | _ => reraise exn);
    val ok =
      (case the (Single_Assignment.peek result) of
        Exn.Exn exn =>
          (SYNCHRONIZED "cancel" (fn () => Task_Queue.cancel_group group exn); false)
      | Exn.Res _ => true);
  in ok end;


(* future jobs *)

fun future_job group atts (e: unit -> 'a) =
  let
    val result = Single_Assignment.var "future" : 'a result;
    val pos = Position.thread_data ();
    fun job ok =
      let
        val res =
          if ok then
            Exn.capture (fn () =>
              Multithreading.with_attributes atts (fn _ => Position.setmp_thread_data pos e ())) ()
          else Exn.interrupt_exn;
      in assign_result group result (identify_result pos res) end;
  in (result, job) end;


(* fork *)

type params = {name: string, group: group option, deps: task list, pri: int, interrupts: bool};
val default_params: params = {name = "", group = NONE, deps = [], pri = 0, interrupts = true};

fun forks ({name, group, deps, pri, interrupts}: params) es =
  if null es then []
  else
    let
      val grp =
        (case group of
          NONE => worker_subgroup ()
        | SOME grp => grp);
      fun enqueue e queue =
        let
          val atts =
            if interrupts
            then Multithreading.private_interrupts
            else Multithreading.no_interrupts;
          val (result, job) = future_job grp atts e;
          val (task, queue') = Task_Queue.enqueue name grp deps pri job queue;
          val future = Future {promised = false, task = task, result = result};
        in (future, queue') end;
    in
      SYNCHRONIZED "enqueue" (fn () =>
        let
          val (futures, queue') = fold_map enqueue es (! queue);
          val _ = queue := queue';
          val minimal = forall (not o Task_Queue.known_task queue') deps;
          val _ = if minimal then signal work_available else ();
          val _ = scheduler_check ();
        in futures end)
    end;

fun fork e =
  (singleton o forks) {name = "fork", group = NONE, deps = [], pri = 0, interrupts = true} e;


(* join *)

fun get_result x =
  (case peek x of
    NONE => Exn.Exn (Fail "Unfinished future")
  | SOME res =>
      if Exn.is_interrupt_exn res then
        (case Task_Queue.group_status (Task_Queue.group_of_task (task_of x)) of
          [] => res
        | exns => Exn.Exn (Par_Exn.make exns))
      else res);

local

fun join_next deps = (*requires SYNCHRONIZED*)
  if null deps then NONE
  else
    (case Unsynchronized.change_result queue (Task_Queue.dequeue_deps (Thread.self ()) deps) of
      (NONE, []) => NONE
    | (NONE, deps') =>
        (worker_waiting deps' (fn () => worker_wait true work_finished); join_next deps')
    | (SOME work, deps') => SOME (work, deps'));

fun execute_work NONE = ()
  | execute_work (SOME (work, deps')) =
      (worker_joining (fn () => worker_exec work); join_work deps')
and join_work deps =
  Multithreading.with_attributes Multithreading.no_interrupts
    (fn _ => execute_work (SYNCHRONIZED "join" (fn () => join_next deps)));

in

fun join_results xs =
  let
    val _ =
      if forall is_finished xs then ()
      else if Multithreading.self_critical () then
        raise Fail "Cannot join future values within critical section"
      else if is_some (worker_task ()) then join_work (map task_of xs)
      else List.app (ignore o Single_Assignment.await o result_of) xs;
  in map get_result xs end;

end;

fun join_result x = singleton join_results x;
fun joins xs = Par_Exn.release_all (join_results xs);
fun join x = Exn.release (join_result x);

fun join_tasks tasks =
  if null tasks then ()
  else
    (singleton o forks)
      {name = "join_tasks", group = SOME (new_group NONE),
        deps = tasks, pri = 0, interrupts = false} I
    |> join;


(* task context for running thread *)

fun task_context name group f x =
  Multithreading.with_attributes Multithreading.no_interrupts (fn orig_atts =>
    let
      val (result, job) = future_job group orig_atts (fn () => f x);
      val task =
        SYNCHRONIZED "enroll" (fn () =>
          Unsynchronized.change_result queue (Task_Queue.enroll (Thread.self ()) name group));
      val _ = worker_exec (task, [job]);
    in
      (case Single_Assignment.peek result of
        NONE => raise Fail "Missing task context result"
      | SOME res => Exn.release res)
    end);


(* fast-path operations -- bypass task queue if possible *)

fun value_result (res: 'a Exn.result) =
  let
    val task = Task_Queue.dummy_task;
    val group = Task_Queue.group_of_task task;
    val result = Single_Assignment.var "value" : 'a result;
    val _ = assign_result group result (identify_result (Position.thread_data ()) res);
  in Future {promised = false, task = task, result = result} end;

fun value x = value_result (Exn.Res x);

fun cond_forks args es =
  if Multithreading.enabled () then forks args es
  else map (fn e => value_result (Exn.interruptible_capture e ())) es;

fun map_future f x =
  if is_finished x then value_result (Exn.interruptible_capture (f o join) x)
  else
    let
      val task = task_of x;
      val group = Task_Queue.group_of_task task;
      val (result, job) =
        future_job group Multithreading.private_interrupts (fn () => f (join x));

      val extended = SYNCHRONIZED "extend" (fn () =>
        (case Task_Queue.extend task job (! queue) of
          SOME queue' => (queue := queue'; true)
        | NONE => false));
    in
      if extended then Future {promised = false, task = task, result = result}
      else
        (singleton o cond_forks)
          {name = "map_future", group = SOME group, deps = [task],
            pri = Task_Queue.pri_of_task task, interrupts = true}
          (fn () => f (join x))
    end;


(* promised futures -- fulfilled by external means *)

fun promise_group group abort : 'a future =
  let
    val result = Single_Assignment.var "promise" : 'a result;
    fun assign () = assign_result group result Exn.interrupt_exn
      handle Fail _ => true
        | exn =>
            if Exn.is_interrupt exn
            then raise Fail "Concurrent attempt to fulfill promise"
            else reraise exn;
    fun job () =
      Multithreading.with_attributes Multithreading.no_interrupts
        (fn _ => Exn.release (Exn.capture assign () before abort ()));
    val task = SYNCHRONIZED "enqueue_passive" (fn () =>
      Unsynchronized.change_result queue (Task_Queue.enqueue_passive group job));
  in Future {promised = true, task = task, result = result} end;

fun promise abort = promise_group (worker_subgroup ()) abort;

fun fulfill_result (Future {promised, task, result}) res =
  if not promised then raise Fail "Not a promised future"
  else
    let
      val group = Task_Queue.group_of_task task;
      val pos = Position.thread_data ();
      fun job ok =
        assign_result group result (if ok then identify_result pos res else Exn.interrupt_exn);
      val _ =
        Multithreading.with_attributes Multithreading.no_interrupts (fn _ =>
          let
            val passive_job =
              SYNCHRONIZED "fulfill_result" (fn () =>
                Unsynchronized.change_result queue
                  (Task_Queue.dequeue_passive (Thread.self ()) task));
          in
            (case passive_job of
              SOME true => worker_exec (task, [job])
            | SOME false => ()
            | NONE => ignore (job (not (Task_Queue.is_canceled group))))
          end);
      val _ =
        if is_some (Single_Assignment.peek result) then ()
        else worker_waiting [task] (fn () => ignore (Single_Assignment.await result));
    in () end;

fun fulfill x res = fulfill_result x (Exn.Res res);


(* group snapshot *)

fun group_snapshot group =
  SYNCHRONIZED "group_snapshot" (fn () =>
    Task_Queue.group_tasks (! queue) group);


(* terminate *)

fun terminate group =
  SYNCHRONIZED "terminate" (fn () =>
    let val _ = cancel_group_unsynchronized group;
    in Task_Queue.group_tasks (! queue) group end)
  |> join_tasks;


(* shutdown *)

fun shutdown () =
  if not Multithreading.available then ()
  else if is_some (worker_task ()) then
    raise Fail "Cannot shutdown while running as worker thread"
  else
    SYNCHRONIZED "shutdown" (fn () =>
      while scheduler_active () do
       (Multithreading.tracing 1 (fn () => "SHUTDOWN: wait");
        wait scheduler_event));


(*final declarations of this structure!*)
val map = map_future;

end;

type 'a future = 'a Future.future;

