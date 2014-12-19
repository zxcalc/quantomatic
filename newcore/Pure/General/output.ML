(*  Title:      Pure/General/output.ML
    Author:     Makarius, Hagia Maria Sion Abbey (Jerusalem)

Isabelle channels for diagnostic output.
*)

signature BASIC_OUTPUT =
sig
  val writeln: string -> unit
  val tracing: string -> unit
  val warning: string -> unit
end;

signature OUTPUT =
sig
  include BASIC_OUTPUT
  type output = string
  val default_output: string -> output * int
  val default_escape: output -> string
  val add_mode: string -> (string -> output * int) -> (output -> string) -> unit
  val output_width: string -> output * int
  val output: string -> output
  val escape: output -> string
  val physical_stdout: output -> unit
  val physical_stderr: output -> unit
  val physical_writeln: output -> unit
  exception Protocol_Message of Properties.T
  val writelns: string list -> unit
  val urgent_message: string -> unit
  val error_message': serial * string -> unit
  val error_message: string -> unit
  val system_message: string -> unit
  val prompt: string -> unit
  val status: string -> unit
  val report: string list -> unit
  val result: Properties.T -> string list -> unit
  val protocol_message: Properties.T -> string list -> unit
  val try_protocol_message: Properties.T -> string list -> unit
end;

signature PRIVATE_OUTPUT =
sig
  include OUTPUT
  val writeln_fn: (output list -> unit) Unsynchronized.ref
  val urgent_message_fn: (output list -> unit) Unsynchronized.ref
  val tracing_fn: (output list -> unit) Unsynchronized.ref
  val warning_fn: (output list -> unit) Unsynchronized.ref
  val error_message_fn: (serial * output list -> unit) Unsynchronized.ref
  val system_message_fn: (output list -> unit) Unsynchronized.ref
  val prompt_fn: (output -> unit) Unsynchronized.ref
  val status_fn: (output list -> unit) Unsynchronized.ref
  val report_fn: (output list -> unit) Unsynchronized.ref
  val result_fn: (Properties.T -> output list -> unit) Unsynchronized.ref
  val protocol_message_fn: (Properties.T -> output list -> unit) Unsynchronized.ref
end;

structure Output: PRIVATE_OUTPUT =
struct

(** print modes **)

type output = string;  (*raw system output*)

fun default_output s = (s, size s);
fun default_escape (s: output) = s;

local
  val default = {output = default_output, escape = default_escape};
  val modes = Synchronized.var "Output.modes" (Symtab.make [("", default)]);
in
  fun add_mode name output escape =
    Synchronized.change modes (Symtab.update_new (name, {output = output, escape = escape}));
  fun get_mode () =
    the_default default
      (Library.get_first (Symtab.lookup (Synchronized.value modes)) (print_mode_value ()));
end;

fun output_width x = #output (get_mode ()) x;
val output = #1 o output_width;

fun escape x = #escape (get_mode ()) x;



(** output channels **)

(* raw output primitives -- not to be used in user-space *)

fun physical_stdout s = (TextIO.output (TextIO.stdOut, s); TextIO.flushOut TextIO.stdOut);
fun physical_stderr s = (TextIO.output (TextIO.stdErr, s); TextIO.flushOut TextIO.stdErr);

fun physical_writeln "" = ()
  | physical_writeln s = physical_stdout (suffix "\n" s);  (*atomic output!*)


(* Isabelle output channels *)

exception Protocol_Message of Properties.T;

val writeln_fn = Unsynchronized.ref (physical_writeln o implode);
val urgent_message_fn = Unsynchronized.ref (fn ss => ! writeln_fn ss);  (*Proof General legacy*)
val tracing_fn = Unsynchronized.ref (fn ss => ! writeln_fn ss);
val warning_fn = Unsynchronized.ref (physical_writeln o prefix_lines "### " o implode);
val error_message_fn =
  Unsynchronized.ref (fn (_: serial, ss) => physical_writeln (prefix_lines "*** " (implode ss)));
val system_message_fn = Unsynchronized.ref (fn ss => ! writeln_fn ss);
val prompt_fn = Unsynchronized.ref physical_stdout;  (*Proof General legacy*)
val status_fn = Unsynchronized.ref (fn _: output list => ());
val report_fn = Unsynchronized.ref (fn _: output list => ());
val result_fn = Unsynchronized.ref (fn _: Properties.T => fn _: output list => ());
val protocol_message_fn: (Properties.T -> output list -> unit) Unsynchronized.ref =
  Unsynchronized.ref (fn props => fn _ => raise Protocol_Message props);

fun writelns ss = ! writeln_fn (map output ss);
fun writeln s = writelns [s];
fun urgent_message s = ! urgent_message_fn [output s];  (*Proof General legacy*)
fun tracing s = ! tracing_fn [output s];
fun warning s = ! warning_fn [output s];
fun error_message' (i, s) = ! error_message_fn (i, [output s]);
fun error_message s = error_message' (serial (), s);
fun system_message s = ! system_message_fn [output s];
fun prompt s = ! prompt_fn (output s);
fun status s = ! status_fn [output s];
fun report ss = ! report_fn (map output ss);
fun result props ss = ! result_fn props (map output ss);
fun protocol_message props ss = ! protocol_message_fn props (map output ss);
fun try_protocol_message props ss = protocol_message props ss handle Protocol_Message _ => ();

end;

structure Basic_Output: BASIC_OUTPUT = Output;
open Basic_Output;
