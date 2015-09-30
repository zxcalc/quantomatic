(*  Title:      Pure/ML-Systems/multithreading_polyml.ML
    Author:     Makarius

Multithreading in Poly/ML (cf. polyml/basis/Thread.sml).
*)

signature MULTITHREADING =
sig
  include MULTITHREADING
  val interruptible: ('a -> 'b) -> 'a -> 'b
  val uninterruptible: ((('c -> 'd) -> 'c -> 'd) -> 'a -> 'b) -> 'a -> 'b
end;

structure Multithreading: MULTITHREADING =
struct

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


(* options *)

val available = true;

fun max_threads_result m =
  if m > 0 then m else Int.min (Int.max (Thread.numProcessors (), 1), 8);

val max_threads = ref 1;

fun max_threads_value () = ! max_threads;

fun max_threads_update m = max_threads := max_threads_result m;

fun max_threads_setmp m f x =
  uninterruptible (fn restore_attributes => fn () =>
    let
      val max_threads_orig = ! max_threads;
      val _ = max_threads_update m;
      val result = Exn.capture (restore_attributes f) x;
      val _ = max_threads := max_threads_orig;
    in Exn.release result end) ();

fun enabled () = max_threads_value () > 1;


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


(* synchronized evaluation *)

fun synchronized name lock e =
  Exn.release (uninterruptible (fn restore_attributes => fn () =>
    let
      val immediate =
        if Mutex.trylock lock then true
        else
          let
            val _ = tracing 5 (fn () => name ^ ": locking ...");
            val time = real_time Mutex.lock lock;
            val _ = tracing_time true time (fn () => name ^ ": locked after " ^ Time.toString time);
          in false end;
      val result = Exn.capture (restore_attributes e) ();
      val _ = if immediate then () else tracing 5 (fn () => name ^ ": unlocking ...");
      val _ = Mutex.unlock lock;
    in result end) ());


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

val interruptible = Multithreading.interruptible;
val uninterruptible = Multithreading.uninterruptible;

