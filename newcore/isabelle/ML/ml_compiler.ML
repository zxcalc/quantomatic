(*  Title:      Pure/ML/ml_compiler.ML
    Author:     Makarius

Runtime compilation -- generic version.
*)

signature ML_COMPILER =
sig
  val exn_position: exn -> Position.T
  val exn_messages: exn -> string list
  val exn_message: exn -> string
  val eval: bool -> Position.T -> ML_Lex.token list -> unit
end

structure ML_Compiler: ML_COMPILER =
struct

fun exn_position _ = Position.none;
val exn_messages = Runtime.exn_messages exn_position;
val exn_message = Runtime.exn_message exn_position;

fun eval verbose pos toks =
  let
    val line = the_default 1 (Position.line_of pos);
    val file = the_default "ML" (Position.file_of pos);
    val text = ML_Lex.flatten toks;
  in Secure.use_text ML_Env.local_context (line, file) verbose text end;

end;

