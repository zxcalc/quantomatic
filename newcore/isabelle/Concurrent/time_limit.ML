(*  Title:      Pure/Concurrent/time_limit.ML
    Author:     Makarius

Execution with time limit.

Notes:

  * There is considerable overhead due to fork of watchdog thread.

  * In rare situations asynchronous interrupts might be mistaken as
    timeout event, and turned into exception TimeOut accidentally.
*)

signature TIME_LIMIT =
sig
  exception TimeOut
  val timeLimit : Time.time -> ('a -> 'b) -> 'a -> 'b
end;

structure TimeLimit: TIME_LIMIT =
struct

exception TimeOut;

val wait_time = seconds 0.0001;

fun timeLimit time f x =
  Multithreading.with_attributes Multithreading.no_interrupts (fn orig_atts =>
    let
      val main = Thread.self ();
      val timeout = Unsynchronized.ref false;
      val watchdog = Simple_Thread.fork true (fn () =>
        (OS.Process.sleep time; timeout := true; Simple_Thread.interrupt_unsynchronized main));

      val result =
        Exn.capture (fn () => Multithreading.with_attributes orig_atts (fn _ => f x)) ();

      val _ = Simple_Thread.interrupt_unsynchronized watchdog;
      val _ = while Thread.isActive watchdog do OS.Process.sleep wait_time;

      val test = Exn.capture Multithreading.interrupted ();
    in
      if ! timeout andalso (Exn.is_interrupt_exn result orelse Exn.is_interrupt_exn test)
      then raise TimeOut
      else if Exn.is_interrupt_exn test then Exn.interrupt ()
      else Exn.release result
    end);

end;

