(*  Title:      Pure/Concurrent/thread_attributes.ML
    Author:     Makarius

Thread attributes for interrupt handling.
*)

signature THREAD_ATTRIBUTES =
sig
  type attributes = Thread.Thread.threadAttribute list
  val no_interrupts: attributes
  val test_interrupts: attributes
  val public_interrupts: attributes
  val private_interrupts: attributes
  val sync_interrupts: attributes -> attributes
  val safe_interrupts: attributes -> attributes
  val with_attributes: attributes -> (attributes -> 'a) -> 'a
  val uninterruptible: ((('c -> 'd) -> 'c -> 'd) -> 'a -> 'b) -> 'a -> 'b
  val expose_interrupt: unit -> unit  (*exception Interrupt*)
end;

structure Thread_Attributes: THREAD_ATTRIBUTES =
struct

type attributes = Thread.Thread.threadAttribute list;

val no_interrupts =
  [Thread.Thread.EnableBroadcastInterrupt false,
    Thread.Thread.InterruptState Thread.Thread.InterruptDefer];

val test_interrupts =
  [Thread.Thread.EnableBroadcastInterrupt false,
    Thread.Thread.InterruptState Thread.Thread.InterruptSynch];

val public_interrupts =
  [Thread.Thread.EnableBroadcastInterrupt true,
    Thread.Thread.InterruptState Thread.Thread.InterruptAsynchOnce];

val private_interrupts =
  [Thread.Thread.EnableBroadcastInterrupt false,
    Thread.Thread.InterruptState Thread.Thread.InterruptAsynchOnce];

val sync_interrupts = map
  (fn x as Thread.Thread.InterruptState Thread.Thread.InterruptDefer => x
    | Thread.Thread.InterruptState _ => Thread.Thread.InterruptState Thread.Thread.InterruptSynch
    | x => x);

val safe_interrupts = map
  (fn Thread.Thread.InterruptState Thread.Thread.InterruptAsynch =>
      Thread.Thread.InterruptState Thread.Thread.InterruptAsynchOnce
    | x => x);

fun with_attributes new_atts e =
  let
    val orig_atts = safe_interrupts (Thread.Thread.getAttributes ());
    val result = Exn.capture (fn () =>
      (Thread.Thread.setAttributes (safe_interrupts new_atts); e orig_atts)) ();
    val _ = Thread.Thread.setAttributes orig_atts;
  in Exn.release result end;

fun uninterruptible f x =
  with_attributes no_interrupts (fn atts =>
    f (fn g => fn y => with_attributes atts (fn _ => g y)) x);

fun expose_interrupt () =
  let
    val orig_atts = safe_interrupts (Thread.Thread.getAttributes ());
    val _ = Thread.Thread.setAttributes test_interrupts;
    val test = Exn.capture Thread.Thread.testInterrupt ();
    val _ = Thread.Thread.setAttributes orig_atts;
  in Exn.release test end;

end;
