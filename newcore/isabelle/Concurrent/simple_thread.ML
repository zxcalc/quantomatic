(*  Title:      Pure/Concurrent/simple_thread.ML
    Author:     Makarius

Simplified thread operations.
*)

signature SIMPLE_THREAD =
sig
  val is_self: Thread.thread -> bool
  val attributes: bool -> Thread.threadAttribute list
  val fork: bool -> (unit -> unit) -> Thread.thread
  val interrupt_unsynchronized: Thread.thread -> unit
  val synchronized: string -> Mutex.mutex -> (unit -> 'a) -> 'a
end;

structure Simple_Thread: SIMPLE_THREAD =
struct

fun is_self thread = Thread.equal (Thread.self (), thread);

fun attributes interrupts =
  if interrupts then Multithreading.public_interrupts else Multithreading.no_interrupts;

fun fork interrupts body =
  Thread.fork (fn () =>
    exception_trace (fn () =>
      body () handle exn => if Exn.is_interrupt exn then () else reraise exn),
    attributes interrupts);

fun interrupt_unsynchronized thread = Thread.interrupt thread handle Thread _ => ();


(* basic synchronization *)

fun synchronized name lock e =
  if Multithreading.available then
    Exn.release (uninterruptible (fn restore_attributes => fn () =>
      let
        val immediate =
          if Mutex.trylock lock then true
          else
            let
              val _ = Multithreading.tracing 5 (fn () => name ^ ": locking ...");
              val time = Multithreading.real_time Mutex.lock lock;
              val _ = Multithreading.tracing_time true time
                (fn () => name ^ ": locked after " ^ Time.toString time);
            in false end;
        val result = Exn.capture (restore_attributes e) ();
        val _ =
          if immediate then () else Multithreading.tracing 5 (fn () => name ^ ": unlocking ...");
        val _ = Mutex.unlock lock;
      in result end) ())
  else e ();

end;
