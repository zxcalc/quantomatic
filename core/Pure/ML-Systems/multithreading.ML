(*  Title:      Pure/ML-Systems/multithreading.ML
    Author:     Makarius

Dummy implementation of multithreading setup.
*)

signature MULTITHREADING =
sig
  val available: bool
  val max_threads_value: unit -> int
  val max_threads_update: int -> unit
  val max_threads_setmp: int -> ('a -> 'b) -> 'a -> 'b
  val enabled: unit -> bool
  val no_interrupts: Thread.threadAttribute list
  val public_interrupts: Thread.threadAttribute list
  val private_interrupts: Thread.threadAttribute list
  val sync_interrupts: Thread.threadAttribute list -> Thread.threadAttribute list
  val interrupted: unit -> unit  (*exception Interrupt*)
  val with_attributes: Thread.threadAttribute list -> (Thread.threadAttribute list -> 'a) -> 'a
  val sync_wait: Thread.threadAttribute list option -> Time.time option ->
    ConditionVar.conditionVar -> Mutex.mutex -> bool Exn.result
  val trace: int ref
  val tracing: int -> (unit -> string) -> unit
  val tracing_time: bool -> Time.time -> (unit -> string) -> unit
  val real_time: ('a -> unit) -> 'a -> Time.time
  val synchronized: string -> Mutex.mutex -> (unit -> 'a) -> 'a
  val serial: unit -> int
end;

structure Multithreading: MULTITHREADING =
struct

(* options *)

val available = false;
fun max_threads_value () = 1: int;
fun max_threads_update _ = ();
fun max_threads_setmp _ f x = f x;
fun enabled () = false;


(* attributes *)

val no_interrupts = [];
val public_interrupts = [];
val private_interrupts = [];
fun sync_interrupts _ = [];

fun interrupted () = ();

fun with_attributes _ e = e [];

fun sync_wait _ _ _ _ = Exn.Res true;


(* tracing *)

val trace = ref (0: int);
fun tracing _ _ = ();
fun tracing_time _ _ _ = ();
fun real_time f x = (f x; Time.zeroTime);


(* synchronized evaluation *)

fun synchronized _ _ e = e ();


(* serial numbers *)

local val count = ref (0: int)
in fun serial () = (count := ! count + 1; ! count) end;

end;

