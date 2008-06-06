(*  Title:      Pure/ML-Systems/multithreading_polyml.ML
    ID:         $Id: multithreading_polyml.ML,v 1.31 2008/03/31 21:08:55 wenzelm Exp $
    Author:     Makarius

Multithreading in Poly/ML 5.1 (cf. polyml/basis/Thread.sml).
*)

open Thread;

signature MULTITHREADING_POLYML =
sig
  val interruptible: ('a -> 'b) -> 'a -> 'b
  val uninterruptible: ((('c -> 'd) -> 'c -> 'd) -> 'a -> 'b) -> 'a -> 'b
  val system_out: string -> string * int
  structure TimeLimit: TIME_LIMIT
  val profile: int -> ('a -> 'b) -> 'a -> 'b
end;

signature BASIC_MULTITHREADING =
sig
  include BASIC_MULTITHREADING
  include MULTITHREADING_POLYML
end;

signature MULTITHREADING =
sig
  include MULTITHREADING
  include MULTITHREADING_POLYML
end;

structure Multithreading: MULTITHREADING =
struct

(* options *)

val trace = ref 0;
fun tracing level msg =
  if level <= ! trace
  then (TextIO.output (TextIO.stdErr, (">>> " ^ msg () ^ "\n")); TextIO.flushOut TextIO.stdErr)
  else ();

val available = true;

val max_threads = ref 1;

fun max_threads_value () =
  let val m = ! max_threads
  in if m <= 0 then Thread.numProcessors () else m end;


(* misc utils *)

fun cons x xs = x :: xs;

fun change r f = r := f (! r);

fun inc i = (i := ! i + 1; ! i);
fun dec i = (i := ! i - 1; ! i);

fun show "" = "" | show name = " " ^ name;
fun show' "" = "" | show' name = " [" ^ name ^ "]";

fun read_file name =
  let val is = TextIO.openIn name
  in Exn.release (Exn.capture TextIO.inputAll is before TextIO.closeIn is) end;

fun write_file name txt =
  let val os = TextIO.openOut name
  in Exn.release (Exn.capture TextIO.output (os, txt) before TextIO.closeOut os) end;


(* thread attributes *)

fun with_attributes new_atts f x =
  let
    val orig_atts = Thread.getAttributes ();
    fun restore () = Thread.setAttributes orig_atts;
  in
    Exn.release
    (*RACE for fully asynchronous interrupts!*)
    (let
        val _ = Thread.setAttributes new_atts;
        val result = Exn.capture (f orig_atts) x;
        val _ = restore ();
      in result end
      handle Interrupt => (restore (); Exn.Exn Interrupt))
  end;

fun interruptible f =
  with_attributes
    [Thread.EnableBroadcastInterrupt true, Thread.InterruptState Thread.InterruptAsynchOnce]
    (fn _ => f);

fun uninterruptible f =
  with_attributes
    [Thread.EnableBroadcastInterrupt false, Thread.InterruptState Thread.InterruptDefer]
    (fn atts => f (fn g => with_attributes atts (fn _ => g)));


(* execution with time limit *)

structure TimeLimit =
struct

exception TimeOut;

fun timeLimit time f x = uninterruptible (fn restore_attributes => fn () =>
  let
    val worker = Thread.self ();
    val timeout = ref false;
    val watchdog = Thread.fork (fn () =>
      (OS.Process.sleep time; timeout := true; Thread.interrupt worker), []);

    (*RACE! timeout signal vs. external Interrupt*)
    val result = Exn.capture (restore_attributes f) x;
    val was_timeout = (case result of Exn.Exn Interrupt => ! timeout | _ => false);

    val _ = Thread.interrupt watchdog handle Thread _ => ();
  in if was_timeout then raise TimeOut else Exn.release result end) ();

end;


(* system shell processes, with propagation of interrupts *)

fun system_out_threaded script = uninterruptible (fn restore_attributes => fn () =>
  let
    val script_name = OS.FileSys.tmpName ();
    val _ = write_file script_name script;

    val pid_name = OS.FileSys.tmpName ();
    val output_name = OS.FileSys.tmpName ();

    (*result state*)
    datatype result = Wait | Signal | Result of int;
    val result = ref Wait;
    val result_mutex = Mutex.mutex ();
    val result_cond = ConditionVar.conditionVar ();
    fun set_result res =
      (Mutex.lock result_mutex; result := res; Mutex.unlock result_mutex;
        ConditionVar.signal result_cond);

    val _ = Mutex.lock result_mutex;

    (*system thread*)
    val system_thread = Thread.fork (fn () =>
      let
        val status =
          OS.Process.system ("perl -w \"$ISABELLE_HOME/lib/scripts/system.pl\" group " ^
            script_name ^ " " ^ pid_name ^ " " ^ output_name);
        val res =
          (case Posix.Process.fromStatus status of
            Posix.Process.W_EXITED => Result 0
          | Posix.Process.W_EXITSTATUS 0wx82 => Signal
          | Posix.Process.W_EXITSTATUS w => Result (Word8.toInt w)
          | Posix.Process.W_SIGNALED s =>
              if s = Posix.Signal.int then Signal
              else Result (256 + LargeWord.toInt (Posix.Signal.toWord s))
          | Posix.Process.W_STOPPED s => Result (512 + LargeWord.toInt (Posix.Signal.toWord s)));
      in set_result res end handle _ => set_result (Result 2), []);

    (*main thread -- proxy for interrupts*)
    fun kill n =
      (case Int.fromString (read_file pid_name) of
        SOME pid =>
          Posix.Process.kill
            (Posix.Process.K_GROUP (Posix.Process.wordToPid (LargeWord.fromInt pid)),
              Posix.Signal.int)
      | NONE => ())
      handle OS.SysErr _ => () | IO.Io _ =>
        (OS.Process.sleep (Time.fromMilliseconds 100); if n > 0 then kill (n - 1) else ());

    val _ = while ! result = Wait do
      restore_attributes (fn () =>
        (ConditionVar.waitUntil (result_cond, result_mutex, Time.now () + Time.fromMilliseconds 100); ())
          handle Interrupt => kill 10) ();

    (*cleanup*)
    val output = read_file output_name handle IO.Io _ => "";
    val _ = OS.FileSys.remove script_name handle OS.SysErr _ => ();
    val _ = OS.FileSys.remove pid_name handle OS.SysErr _ => ();
    val _ = OS.FileSys.remove output_name handle OS.SysErr _ => ();
    val _ = Thread.interrupt system_thread handle Thread _ => ();
    val rc = (case ! result of Signal => raise Interrupt | Result rc => rc);
  in (output, rc) end) ();

val system_out =
  if PolyML.rtsVersion () = 510 then system_out  (*signals not propagated from root thread! for polyml 5.1 only, fixed in 5.2 *)
  else system_out_threaded;


(* critical section -- may be nested within the same thread *)

local

val critical_lock = Mutex.mutex ();
val critical_thread = ref (NONE: Thread.thread option);
val critical_name = ref "";

in

fun self_critical () =
  (case ! critical_thread of
    NONE => false
  | SOME id => Thread.equal (id, Thread.self ()));

fun NAMED_CRITICAL name e =
  if self_critical () then e ()
  else
    uninterruptible (fn restore_attributes => fn () =>
      let
        val name' = ! critical_name;
        val _ =
          if Mutex.trylock critical_lock then ()
          else
            let
              val timer = Timer.startRealTimer ();
              val _ = tracing 4 (fn () => "CRITICAL" ^ show name ^ show' name' ^ ": waiting");
              val _ = Mutex.lock critical_lock;
              val time = Timer.checkRealTimer timer;
              val trace_time =
                if Time.>= (time, Time.fromMilliseconds 1000) then 1
                else if Time.>= (time, Time.fromMilliseconds 100) then 2
                else if Time.>= (time, Time.fromMilliseconds 10) then 3 else 4;
              val _ = tracing trace_time (fn () =>
                "CRITICAL" ^ show name ^ show' name' ^ ": passed after " ^ Time.toString time);
            in () end;
        val _ = critical_thread := SOME (Thread.self ());
        val _ = critical_name := name;
        val result = Exn.capture (restore_attributes e) ();
        val _ = critical_name := "";
        val _ = critical_thread := NONE;
        val _ = Mutex.unlock critical_lock;
      in Exn.release result end) ();

fun CRITICAL e = NAMED_CRITICAL "" e;

end;


(* scheduling -- multiple threads working on a queue of tasks *)

datatype 'a task =
  Task of {body: unit -> unit, cont: 'a -> 'a, fail: 'a -> 'a} | Wait | Terminate;

fun schedule n next_task = uninterruptible (fn restore_attributes => fn tasks =>
  let
    (*protected execution*)
    val lock = Mutex.mutex ();
    val protected_name = ref "";
    fun PROTECTED name e =
      let
        val name' = ! protected_name;
        val _ =
          if Mutex.trylock lock then ()
          else
            let
              val _ = tracing 2 (fn () => "PROTECTED" ^ show name ^ show' name' ^ ": waiting");
              val _ = Mutex.lock lock;
              val _ = tracing 2 (fn () => "PROTECTED" ^ show name ^ show' name' ^ ": passed");
            in () end;
        val _ = protected_name := name;
        val res = Exn.capture e ();
        val _ = protected_name := "";
        val _ = Mutex.unlock lock;
      in Exn.release res end;

    (*wakeup condition*)
    val wakeup = ConditionVar.conditionVar ();
    fun wakeup_all () = ConditionVar.broadcast wakeup;
    fun wait () = ConditionVar.wait (wakeup, lock);
    fun wait_timeout () = ConditionVar.waitUntil (wakeup, lock, Time.now () + Time.fromSeconds 1);

    (*queue of tasks*)
    val queue = ref tasks;
    val active = ref 0;
    fun trace_active () = tracing 1 (fn () => "SCHEDULE: " ^ Int.toString (! active) ^ " active");
    fun dequeue () =
      let
        val (next, tasks') = next_task (! queue);
        val _ = queue := tasks';
      in
        (case next of Wait =>
          (dec active; trace_active ();
            wait ();
            inc active; trace_active ();
            dequeue ())
        | _ => next)
      end;

    (*pool of running threads*)
    val status = ref ([]: exn list);
    val running = ref ([]: Thread.thread list);
    fun start f =
      (inc active;
       change running (cons (Thread.fork (f, [Thread.InterruptState Thread.InterruptDefer]))));
    fun stop () =
      (dec active;
       change running (List.filter (fn t => not (Thread.equal (t, Thread.self ())))));

   (*worker thread*)
    fun worker () =
      (case PROTECTED "dequeue" dequeue of
        Task {body, cont, fail} =>
          (case Exn.capture (restore_attributes body) () of
            Exn.Result () =>
              (PROTECTED "cont" (fn () => (change queue cont; wakeup_all ())); worker ())
          | Exn.Exn exn =>
              PROTECTED "fail" (fn () =>
                (change status (cons exn); change queue fail; stop (); wakeup_all ())))
      | Terminate => PROTECTED "terminate" (fn () => (stop (); wakeup_all ())));

    (*main control: fork and wait*)
    fun fork 0 = ()
      | fork k = (start worker; fork (k - 1));
    val _ = PROTECTED "main" (fn () =>
     (fork (Int.max (n, 1));
      while not (List.null (! running)) do
      (trace_active ();
       if not (List.null (! status))
       then (List.app (fn t => Thread.interrupt t handle Thread _ => ()) (! running))
       else ();
       wait_timeout ())));

  in ! status end);


(* profiling *)

local val profile_orig = profile in

fun profile 0 f x = f x
  | profile n f x = NAMED_CRITICAL "profile" (fn () => profile_orig n f x);

end;


(* serial numbers *)

local

val serial_lock = Mutex.mutex ();
val serial_count = ref 0;

in

val serial = uninterruptible (fn _ => fn () =>
  let
    val _ = Mutex.lock serial_lock;
    val res = inc serial_count;
    val _ = Mutex.unlock serial_lock;
  in res end);

end;


(* thread data *)

val get_data = Thread.getLocal;
val put_data = Thread.setLocal;

end;

structure BasicMultithreading: BASIC_MULTITHREADING = Multithreading;
open BasicMultithreading;
