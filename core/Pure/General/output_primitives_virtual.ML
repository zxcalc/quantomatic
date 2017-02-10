(*  Title:      Pure/General/output_primitives_virtual.ML
    Author:     Makarius

Primitives for Isabelle output channels -- virtual version within Pure environment.
*)

structure Output_Primitives_Virtual: OUTPUT_PRIMITIVES =
struct

open Output_Primitives;

fun writeln_fn x = ! Private_Output.writeln_fn x;
fun state_fn x = ! Private_Output.state_fn x;
fun information_fn x = ! Private_Output.information_fn x;
fun tracing_fn x = ! Private_Output.tracing_fn x;
fun warning_fn x = ! Private_Output.warning_fn x;
fun legacy_fn x = ! Private_Output.legacy_fn x;
fun error_message_fn x = ! Private_Output.error_message_fn x;
fun system_message_fn x = ! Private_Output.system_message_fn x;
fun status_fn x = ! Private_Output.status_fn x;
fun report_fn x = ! Private_Output.report_fn x;
fun result_fn x y = ! Private_Output.result_fn x y;
fun protocol_message_fn x y = ! Private_Output.protocol_message_fn x y;

val markup_fn = Markup.output;

end;
