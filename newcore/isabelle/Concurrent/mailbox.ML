(*  Title:      Pure/Concurrent/mailbox.ML
    Author:     Makarius

Message exchange via mailbox, with non-blocking send (due to unbounded
queueing) and potentially blocking receive.
*)

signature MAILBOX =
sig
  type 'a T
  val create: unit -> 'a T
  val send: 'a T -> 'a -> unit
  val receive: 'a T -> 'a
  val receive_timeout: Time.time -> 'a T -> 'a option
end;

structure Mailbox: MAILBOX =
struct

datatype 'a T = Mailbox of 'a Queue.T Synchronized.var;

fun create () = Mailbox (Synchronized.var "mailbox" Queue.empty);

fun send (Mailbox mailbox) msg =
  Synchronized.change mailbox (Queue.enqueue msg);

fun receive (Mailbox mailbox) =
  Synchronized.guarded_access mailbox (try Queue.dequeue);

fun receive_timeout timeout (Mailbox mailbox) =
  Synchronized.timed_access mailbox
    (fn _ => SOME (Time.+ (Time.now (), timeout))) (try Queue.dequeue);

end;
