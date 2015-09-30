(*  Title:      Pure/Concurrent/simple_thread.ML
    Author:     Makarius

Simplified thread operations.
*)

signature SIMPLE_THREAD =
sig
  val is_self: Thread.thread -> bool
  type params = {stack_limit: int option, interrupts: bool}
  val attributes: params -> Thread.threadAttribute list
  val fork: params -> (unit -> unit) -> Thread.thread
  val join: Thread.thread -> unit
  val interrupt_unsynchronized: Thread.thread -> unit
end;

structure Simple_Thread: SIMPLE_THREAD =
struct

fun is_self thread = Thread.equal (Thread.self (), thread);

type params = {stack_limit: int option, interrupts: bool};

fun attributes {stack_limit, interrupts} =
  maximum_ml_stack stack_limit @
  (if interrupts then Multithreading.public_interrupts else Multithreading.no_interrupts);

fun fork params body =
  Thread.fork (fn () =>
    print_exception_trace General.exnMessage tracing (fn () =>
      body () handle exn => if Exn.is_interrupt exn then () (*sic!*) else reraise exn),
    attributes params);

fun join thread =
  while Thread.isActive thread
  do OS.Process.sleep (seconds 0.1);

fun interrupt_unsynchronized thread = Thread.interrupt thread handle Thread _ => ();

end;
