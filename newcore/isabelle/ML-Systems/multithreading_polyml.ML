(*  Title:      Pure/ML-Systems/multithreading_polyml.ML
    Author:     Makarius

Multithreading in Poly/ML (cf. polyml/basis/Thread.sml).
*)

signature MULTITHREADING_POLYML =
sig
  val interruptible: ('a -> 'b) -> 'a -> 'b
  val uninterruptible: ((('c -> 'd) -> 'c -> 'd) -> 'a -> 'b) -> 'a -> 'b
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

val available = true;

val max_threads = ref (Thread.numProcessors());

fun max_threads_value () =
  let val m = ! max_threads in
    if m > 0 then m
    else Int.max (Thread.numProcessors (), 1)
  end;

fun enabled () = max_threads_value () > 1;


(* thread attributes *)

val no_interrupts =
  [Thread.EnableBroadcastInterrupt false, Thread.InterruptState Thread.InterruptDefer];

val test_interrupts =
  [Thread.EnableBroadcastInterrupt false, Thread.InterruptState Thread.InterruptSynch];

val public_interrupts =
  [Thread.EnableBroadcastInterrupt true, Thread.InterruptState Thread.InterruptAsynchOnce];

val private_interrupts =
  [Thread.EnableBroadcastInterrupt false, Thread.InterruptState Thread.InterruptAsynchOnce];

val sync_interrupts = map
  (fn x as Thread.InterruptState Thread.InterruptDefer => x
    | Thread.InterruptState _ => Thread.InterruptState Thread.InterruptSynch
    | x => x);

val safe_interrupts = map
  (fn Thread.InterruptState Thread.InterruptAsynch =>
      Thread.InterruptState Thread.InterruptAsynchOnce
    | x => x);

fun interrupted () =
  let
    val orig_atts = safe_interrupts (Thread.getAttributes ());
    val _ = Thread.setAttributes test_interrupts;
    val test = Exn.capture Thread.testInterrupt ();
    val _ = Thread.setAttributes orig_atts;
  in Exn.release test end;

fun with_attributes new_atts e =
  let
    val orig_atts = safe_interrupts (Thread.getAttributes ());
    val result = Exn.capture (fn () =>
      (Thread.setAttributes (safe_interrupts new_atts); e orig_atts)) ();
    val _ = Thread.setAttributes orig_atts;
  in Exn.release result end;


(* portable wrappers *)

fun interruptible f x = with_attributes public_interrupts (fn _ => f x);

fun uninterruptible f x =
  with_attributes no_interrupts (fn atts =>
    f (fn g => fn y => with_attributes atts (fn _ => g y)) x);


(* synchronous wait *)

fun sync_wait opt_atts time cond lock =
  with_attributes
    (sync_interrupts (case opt_atts of SOME atts => atts | NONE => Thread.getAttributes ()))
    (fn _ =>
      (case time of
        SOME t => Exn.Res (ConditionVar.waitUntil (cond, lock, t))
      | NONE => (ConditionVar.wait (cond, lock); Exn.Res true))
      handle exn => Exn.Exn exn);


(* tracing *)

val trace = ref 0;

fun tracing level msg =
  if level > ! trace then ()
  else uninterruptible (fn _ => fn () =>
    (TextIO.output (TextIO.stdErr, (">>> " ^ msg () ^ "\n")); TextIO.flushOut TextIO.stdErr)
      handle _ (*sic*) => ()) ();

fun tracing_time detailed time =
  tracing
   (if not detailed then 5
    else if Time.>= (time, seconds 1.0) then 1
    else if Time.>= (time, seconds 0.1) then 2
    else if Time.>= (time, seconds 0.01) then 3
    else if Time.>= (time, seconds 0.001) then 4 else 5);

fun real_time f x =
  let
    val timer = Timer.startRealTimer ();
    val () = f x;
    val time = Timer.checkRealTimer timer;
  in time end;


(* critical section -- may be nested within the same thread *)

local

val critical_lock = Mutex.mutex ();
val critical_thread = ref (NONE: Thread.thread option);
val critical_name = ref "";

fun show "" = "" | show name = " " ^ name;
fun show' "" = "" | show' name = " [" ^ name ^ "]";

in

fun self_critical () =
  (case ! critical_thread of
    NONE => false
  | SOME t => Thread.equal (t, Thread.self ()));

fun NAMED_CRITICAL name e =
  if self_critical () then e ()
  else
    Exn.release (uninterruptible (fn restore_attributes => fn () =>
      let
        val name' = ! critical_name;
        val _ =
          if Mutex.trylock critical_lock then ()
          else
            let
              val _ = tracing 5 (fn () => "CRITICAL" ^ show name ^ show' name' ^ ": waiting");
              val time = real_time Mutex.lock critical_lock;
              val _ = tracing_time true time (fn () =>
                "CRITICAL" ^ show name ^ show' name' ^ ": passed after " ^ Time.toString time);
            in () end;
        val _ = critical_thread := SOME (Thread.self ());
        val _ = critical_name := name;
        val result = Exn.capture (restore_attributes e) ();
        val _ = critical_name := "";
        val _ = critical_thread := NONE;
        val _ = Mutex.unlock critical_lock;
      in result end) ());

fun CRITICAL e = NAMED_CRITICAL "" e;

end;


(* serial numbers *)

local

val serial_lock = Mutex.mutex ();
val serial_count = ref 0;

in

val serial = uninterruptible (fn _ => fn () =>
  let
    val _ = Mutex.lock serial_lock;
    val _ = serial_count := ! serial_count + 1;
    val res = ! serial_count;
    val _ = Mutex.unlock serial_lock;
  in res end);

end;

end;

structure Basic_Multithreading: BASIC_MULTITHREADING = Multithreading;
open Basic_Multithreading;
