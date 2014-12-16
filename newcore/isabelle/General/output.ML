(*  Title:      Pure/General/output.ML
    Author:     Makarius, Hagia Maria Sion Abbey (Jerusalem)

Isabelle output channels.
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
  structure Private_Hooks:
  sig
    val writeln_fn: (output -> unit) Unsynchronized.ref
    val urgent_message_fn: (output -> unit) Unsynchronized.ref
    val tracing_fn: (output -> unit) Unsynchronized.ref
    val warning_fn: (output -> unit) Unsynchronized.ref
    val error_fn: (serial * output -> unit) Unsynchronized.ref
    val prompt_fn: (output -> unit) Unsynchronized.ref
    val status_fn: (output -> unit) Unsynchronized.ref
    val report_fn: (output -> unit) Unsynchronized.ref
    val result_fn: (serial * output -> unit) Unsynchronized.ref
    val protocol_message_fn: (Properties.T -> output -> unit) Unsynchronized.ref
  end
  val urgent_message: string -> unit
  val error_msg': serial * string -> unit
  val error_msg: string -> unit
  val prompt: string -> unit
  val status: string -> unit
  val report: string -> unit
  val result: serial * string -> unit
  val protocol_message: Properties.T -> string -> unit
end;

structure Output: OUTPUT =
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

structure Private_Hooks =
struct
  val writeln_fn = Unsynchronized.ref physical_writeln;
  val urgent_message_fn = Unsynchronized.ref (fn s => ! writeln_fn s);
  val tracing_fn = Unsynchronized.ref (fn s => ! writeln_fn s);
  val warning_fn = Unsynchronized.ref (physical_writeln o prefix_lines "### ");
  val error_fn = Unsynchronized.ref (fn (_: serial, s) => physical_writeln (prefix_lines "*** " s));
  val prompt_fn = Unsynchronized.ref physical_stdout;
  val status_fn = Unsynchronized.ref (fn _: output => ());
  val report_fn = Unsynchronized.ref (fn _: output => ());
  val result_fn = Unsynchronized.ref (fn _: serial * output => ());
  val protocol_message_fn: (Properties.T -> output -> unit) Unsynchronized.ref =
    Unsynchronized.ref (fn _ => fn _ => raise Fail "Output.protocol_message undefined in TTY mode");
end;

fun writeln s = ! Private_Hooks.writeln_fn (output s);
fun urgent_message s = ! Private_Hooks.urgent_message_fn (output s);
fun tracing s = ! Private_Hooks.tracing_fn (output s);
fun warning s = ! Private_Hooks.warning_fn (output s);
fun error_msg' (i, s) = ! Private_Hooks.error_fn (i, output s);
fun error_msg s = error_msg' (serial (), s);
fun prompt s = ! Private_Hooks.prompt_fn (output s);
fun status s = ! Private_Hooks.status_fn (output s);
fun report s = ! Private_Hooks.report_fn (output s);
fun result (i, s) = ! Private_Hooks.result_fn (i, output s);
fun protocol_message props s = ! Private_Hooks.protocol_message_fn props (output s);

end;

structure Basic_Output: BASIC_OUTPUT = Output;
open Basic_Output;
