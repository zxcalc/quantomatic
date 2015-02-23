(*  Title:      Pure/ML/exn_output.ML
    Author:     Makarius

Auxiliary operations for exception output -- generic version.
*)

signature EXN_OUTPUT =
sig
  val position: exn -> Position.T
  val pretty: exn -> Pretty.T
end

structure Exn_Output: EXN_OUTPUT =
struct

fun position (_: exn) = Position.none
val pretty = Pretty.str o General.exnMessage;

end;

