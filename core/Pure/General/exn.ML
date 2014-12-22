(*  Title:      Pure/General/exn.ML
    Author:     Makarius

Support for exceptions.
*)

signature EXN =
sig
  datatype 'a result = Res of 'a | Exn of exn
  val get_res: 'a result -> 'a option
  val get_exn: 'a result -> exn option
  val capture: ('a -> 'b) -> 'a -> 'b result
  val release: 'a result -> 'a
  val map_result: ('a -> 'b) -> 'a result -> 'b result
  val maps_result: ('a -> 'b result) -> 'a result -> 'b result
  exception Interrupt
  val interrupt: unit -> 'a
  val is_interrupt: exn -> bool
  val interrupt_exn: 'a result
  val is_interrupt_exn: 'a result -> bool
  val interruptible_capture: ('a -> 'b) -> 'a -> 'b result
  val return_code: exn -> int -> int
  val capture_exit: int -> ('a -> 'b) -> 'a -> 'b
  exception EXCEPTIONS of exn list
end;

structure Exn: EXN =
struct

(* exceptions as values *)

datatype 'a result =
  Res of 'a |
  Exn of exn;

fun get_res (Res x) = SOME x
  | get_res _ = NONE;

fun get_exn (Exn exn) = SOME exn
  | get_exn _ = NONE;

fun capture f x = Res (f x) handle e => Exn e;

fun release (Res y) = y
  | release (Exn e) = reraise e;

fun map_result f (Res x) = Res (f x)
  | map_result f (Exn e) = Exn e;

fun maps_result f = (fn Res x => x | Exn e => Exn e) o map_result f;


(* interrupts *)

exception Interrupt = Interrupt;

fun interrupt () = raise Interrupt;

fun is_interrupt Interrupt = true
  | is_interrupt (IO.Io {cause, ...}) = is_interrupt cause
  | is_interrupt _ = false;

val interrupt_exn = Exn Interrupt;

fun is_interrupt_exn (Exn exn) = is_interrupt exn
  | is_interrupt_exn _ = false;

fun interruptible_capture f x =
  Res (f x) handle e => if is_interrupt e then reraise e else Exn e;


(* POSIX return code *)

fun return_code exn rc =
  if is_interrupt exn then (130: int) else rc;

fun capture_exit rc f x =
  f x handle exn => exit (return_code exn rc);


(* concatenated exceptions *)

exception EXCEPTIONS of exn list;

end;

datatype illegal = Interrupt;

