(*  Title:      Pure/Concurrent/multithreading.ML
    Author:     Makarius

Multithreading in Poly/ML (cf. polyml/basis/Thread.sml).
*)

signature MULTITHREADING =
sig
  val max_threads: unit -> int
  val max_threads_update: int -> unit
  val enabled: unit -> bool
  val sync_wait: Time.time option -> ConditionVar.conditionVar -> Mutex.mutex -> bool Exn.result
  val trace: int ref
  val tracing: int -> (unit -> string) -> unit
  val tracing_time: bool -> Time.time -> (unit -> string) -> unit
  val synchronized: string -> Mutex.mutex -> (unit -> 'a) -> 'a
end;

structure Multithreading: MULTITHREADING =
struct

(* max_threads *)

local

fun num_processors () =
  (case Thread.numPhysicalProcessors () of
    SOME n => n
  | NONE => Thread.numProcessors ());

fun max_threads_result m =
  if Thread_Data.is_virtual then 1
  else if m > 0 then m
  else Int.min (Int.max (num_processors (), 1), 8);

val max_threads_state = ref 1;

in

fun max_threads () = ! max_threads_state;
fun max_threads_update m = max_threads_state := max_threads_result m;
fun enabled () = max_threads () > 1;

end;


(* synchronous wait *)

fun sync_wait time cond lock =
  Thread_Attributes.with_attributes
    (Thread_Attributes.sync_interrupts (Thread.getAttributes ()))
    (fn _ =>
      (case time of
        SOME t => Exn.Res (ConditionVar.waitUntil (cond, lock, t))
      | NONE => (ConditionVar.wait (cond, lock); Exn.Res true))
      handle exn => Exn.Exn exn);


(* tracing *)

val trace = ref 0;

fun tracing level msg =
  if level > ! trace then ()
  else Thread_Attributes.uninterruptible (fn _ => fn () =>
    (TextIO.output (TextIO.stdErr, (">>> " ^ msg () ^ "\n")); TextIO.flushOut TextIO.stdErr)
      handle _ (*sic*) => ()) ();

fun tracing_time detailed time =
  tracing
   (if not detailed then 5
    else if time >= seconds 1.0 then 1
    else if time >= seconds 0.1 then 2
    else if time >= seconds 0.01 then 3
    else if time >= seconds 0.001 then 4 else 5);


(* synchronized evaluation *)

fun synchronized name lock e =
  Exn.release (Thread_Attributes.uninterruptible (fn restore_attributes => fn () =>
    let
      val immediate =
        if Mutex.trylock lock then true
        else
          let
            val _ = tracing 5 (fn () => name ^ ": locking ...");
            val timer = Timer.startRealTimer ();
            val _ = Mutex.lock lock;
            val time = Timer.checkRealTimer timer;
            val _ = tracing_time true time (fn () => name ^ ": locked after " ^ Time.toString time);
          in false end;
      val result = Exn.capture (restore_attributes e) ();
      val _ = if immediate then () else tracing 5 (fn () => name ^ ": unlocking ...");
      val _ = Mutex.unlock lock;
    in result end) ());

end;
