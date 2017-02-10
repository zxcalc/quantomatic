(*  Title:      Pure/General/output.ML
    Author:     Makarius

Isabelle output channels.
*)

signature BASIC_OUTPUT =
sig
  val writeln: string -> unit
  val tracing: string -> unit
  val warning: string -> unit
  val legacy_feature: string -> unit
  val profile_time: ('a -> 'b) -> 'a -> 'b
  val profile_time_thread: ('a -> 'b) -> 'a -> 'b
  val profile_allocations: ('a -> 'b) -> 'a -> 'b
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
  val state: string -> unit
  val information: string -> unit
  val error_message': serial * string -> unit
  val error_message: string -> unit
  val system_message: string -> unit
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
  val state_fn: (output list -> unit) Unsynchronized.ref
  val information_fn: (output list -> unit) Unsynchronized.ref
  val tracing_fn: (output list -> unit) Unsynchronized.ref
  val warning_fn: (output list -> unit) Unsynchronized.ref
  val legacy_fn: (output list -> unit) Unsynchronized.ref
  val error_message_fn: (serial * output list -> unit) Unsynchronized.ref
  val system_message_fn: (output list -> unit) Unsynchronized.ref
  val status_fn: (output list -> unit) Unsynchronized.ref
  val report_fn: (output list -> unit) Unsynchronized.ref
  val result_fn: (Properties.T -> output list -> unit) Unsynchronized.ref
  val protocol_message_fn: (Properties.T -> output list -> unit) Unsynchronized.ref
end;

structure Private_Output: PRIVATE_OUTPUT =
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
    if Thread_Data.is_virtual then ()
    else Synchronized.change modes (Symtab.update_new (name, {output = output, escape = escape}));
  fun get_mode () =
    the_default default
      (Library.get_first (Symtab.lookup (Synchronized.value modes)) (print_mode_value ()));
end;

fun output_width x = #output (get_mode ()) x;
val output = #1 o output_width;

fun escape x = #escape (get_mode ()) x;



(** output channels **)

(* primitives -- provided via bootstrap environment *)

val writeln_fn = Unsynchronized.ref Output_Primitives.writeln_fn;
val state_fn = Unsynchronized.ref Output_Primitives.state_fn;
val information_fn = Unsynchronized.ref Output_Primitives.information_fn;
val tracing_fn = Unsynchronized.ref Output_Primitives.tracing_fn;
val warning_fn = Unsynchronized.ref Output_Primitives.warning_fn;
val legacy_fn = Unsynchronized.ref Output_Primitives.legacy_fn;
val error_message_fn = Unsynchronized.ref Output_Primitives.error_message_fn;
val system_message_fn = Unsynchronized.ref Output_Primitives.system_message_fn;
val status_fn = Unsynchronized.ref Output_Primitives.status_fn;
val report_fn = Unsynchronized.ref Output_Primitives.report_fn;
val result_fn = Unsynchronized.ref Output_Primitives.result_fn;
val protocol_message_fn = Unsynchronized.ref Output_Primitives.protocol_message_fn;


(* physical output -- not to be used in user-space *)

fun physical_stdout s = (TextIO.output (TextIO.stdOut, s); TextIO.flushOut TextIO.stdOut);
fun physical_stderr s = (TextIO.output (TextIO.stdErr, s); TextIO.flushOut TextIO.stdErr);

fun physical_writeln "" = ()
  | physical_writeln s = physical_stdout (suffix "\n" s);  (*atomic output!*)


(* Isabelle output channels *)

exception Protocol_Message of Properties.T;

val _ =
  if Thread_Data.is_virtual then ()
  else
   (writeln_fn := (physical_writeln o implode);
    state_fn := (fn ss => ! writeln_fn ss);
    tracing_fn := (fn ss => ! writeln_fn ss);
    warning_fn := (physical_writeln o prefix_lines "### " o implode);
    legacy_fn := (fn ss => ! warning_fn ss);
    error_message_fn := (fn (_, ss) => physical_writeln (prefix_lines "*** " (implode ss)));
    system_message_fn := (fn ss => ! writeln_fn ss);
    protocol_message_fn := (fn props => fn _ => raise Protocol_Message props));

fun writelns ss = ! writeln_fn (map output ss);
fun writeln s = writelns [s];
fun state s = ! state_fn [output s];
fun information s = ! information_fn [output s];
fun tracing s = ! tracing_fn [output s];
fun warning s = ! warning_fn [output s];
fun legacy_feature s = ! legacy_fn [output ("Legacy feature! " ^ s)];
fun error_message' (i, s) = ! error_message_fn (i, [output s]);
fun error_message s = error_message' (serial (), s);
fun system_message s = ! system_message_fn [output s];
fun status s = ! status_fn [output s];
fun report ss = ! report_fn (map output ss);
fun result props ss = ! result_fn props (map output ss);
fun protocol_message props ss = ! protocol_message_fn props (map output ss);
fun try_protocol_message props ss = protocol_message props ss handle Protocol_Message _ => ();


(* profiling *)

local

fun output_profile title entries =
  let
    val body = entries
      |> sort (int_ord o apply2 fst)
      |> map (fn (count, name) =>
          let
            val c = string_of_int count;
            val prefix = replicate_string (Int.max (0, 10 - size c)) " ";
          in prefix ^ c ^ " " ^ name end);
    val total = fold (curry (op +) o fst) entries 0;
  in
    if total = 0 then ()
    else warning (cat_lines (title :: (body @ ["total: " ^ string_of_int total])))
  end;

in

fun profile_time f x =
  ML_Profiling.profile_time (output_profile "time profile:") f x;

fun profile_time_thread f x =
  ML_Profiling.profile_time_thread (output_profile "time profile (this thread):") f x;

fun profile_allocations f x =
  ML_Profiling.profile_allocations (output_profile "allocations profile:") f x;

end;


end;

structure Output: OUTPUT = Private_Output;
structure Basic_Output: BASIC_OUTPUT = Output;
open Basic_Output;
