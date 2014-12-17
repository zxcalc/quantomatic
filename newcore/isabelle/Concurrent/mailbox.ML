(*  Title:      Pure/Concurrent/mailbox.ML
    Author:     Makarius

Message exchange via mailbox, with multiple senders (non-blocking,
unbounded buffering) and single receiver (bulk messages).
*)

signature MAILBOX =
sig
  type 'a T
  val create: unit -> 'a T
  val send: 'a T -> 'a -> unit
  val receive: Time.time option -> 'a T -> 'a list
  val await_empty: 'a T -> unit
end;

structure Mailbox: MAILBOX =
struct

datatype 'a T = Mailbox of 'a list Synchronized.var;

fun create () = Mailbox (Synchronized.var "mailbox" []);

fun send (Mailbox mailbox) msg = Synchronized.change mailbox (cons msg);

fun receive timeout (Mailbox mailbox) =
  Synchronized.timed_access mailbox
    (fn _ => Option.map (fn t => (Time.+ (Time.now (), t))) timeout)
    (fn [] => NONE | msgs => SOME (msgs, []))
  |> these |> rev;

fun await_empty (Mailbox mailbox) =
  Synchronized.guarded_access mailbox (fn [] => SOME ((), []) | _ => NONE);

end;
