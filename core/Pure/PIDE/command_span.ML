(*  Title:      Pure/PIDE/command_span.ML
    Author:     Makarius

Syntactic representation of command spans.
*)

signature COMMAND_SPAN =
sig
  datatype kind = Command_Span of string * Position.T | Ignored_Span | Malformed_Span
  datatype span = Span of kind * Token.T list
  val kind: span -> kind
  val content: span -> Token.T list
end;

structure Command_Span: COMMAND_SPAN =
struct

datatype kind = Command_Span of string * Position.T | Ignored_Span | Malformed_Span;
datatype span = Span of kind * Token.T list;

fun kind (Span (k, _)) = k;
fun content (Span (_, toks)) = toks;

end;
