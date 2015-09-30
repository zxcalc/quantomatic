(*  Title:      Pure/Concurrent/time_limit.ML
    Author:     Makarius

Execution with time limit (relative timeout).
*)

signature TIME_LIMIT =
sig
  exception TimeOut
  val timeLimit : Time.time -> ('a -> 'b) -> 'a -> 'b
end;

structure TimeLimit: TIME_LIMIT =
struct

exception TimeOut;

fun timeLimit timeout f x =
  Multithreading.with_attributes Multithreading.no_interrupts (fn orig_atts =>
    let
      val self = Thread.self ();

      val request =
        Event_Timer.request (Time.+ (Time.now (), timeout))
          (fn () => Simple_Thread.interrupt_unsynchronized self);

      val result =
        Exn.capture (fn () => Multithreading.with_attributes orig_atts (fn _ => f x)) ();

      val was_timeout = not (Event_Timer.cancel request);
      val test = Exn.capture Multithreading.interrupted ();
    in
      if was_timeout andalso (Exn.is_interrupt_exn result orelse Exn.is_interrupt_exn test)
      then raise TimeOut
      else (Exn.release test; Exn.release result)
    end);

end;

