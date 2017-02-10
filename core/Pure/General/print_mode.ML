(*  Title:      Pure/General/print_mode.ML
    Author:     Makarius

Generic print mode as thread-local value derived from global template;
provides implicit configuration for various output mechanisms.

The special print mode "input" is never enabled for output.
*)

signature BASIC_PRINT_MODE =
sig
  val print_mode: string list Unsynchronized.ref  (*global template*)
  val print_mode_value: unit -> string list       (*thread-local value*)
  val print_mode_active: string -> bool           (*thread-local value*)
end;

signature PRINT_MODE =
sig
  include BASIC_PRINT_MODE
  val input: string
  val ASCII: string
  val internal: string
  val setmp: string list -> ('a -> 'b) -> 'a -> 'b
  val with_modes: string list -> ('a -> 'b) -> 'a -> 'b
  val closure: ('a -> 'b) -> 'a -> 'b
  val add_modes: string list -> unit
end;

structure Print_Mode: PRINT_MODE =
struct

val input = "input";
val ASCII = "ASCII";
val internal = "internal";

val print_mode = Unsynchronized.ref ([]: string list);
val print_mode_var = Thread_Data.var () : string list Thread_Data.var;

fun print_mode_value () =
  let val modes =
    (case Thread_Data.get print_mode_var of
      SOME modes => modes
    | NONE => ! print_mode)
  in subtract (op =) [input, internal] modes end;

fun print_mode_active mode = member (op =) (print_mode_value ()) mode;

fun setmp modes f x = Thread_Data.setmp print_mode_var (SOME modes) f x;

fun with_modes modes f x = setmp (modes @ print_mode_value ()) f x;
fun closure f = with_modes [] f;

fun add_modes modes = Unsynchronized.change print_mode (append modes);

end;

structure Basic_Print_Mode: BASIC_PRINT_MODE = Print_Mode;
open Basic_Print_Mode;
