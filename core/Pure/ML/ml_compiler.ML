(*  Title:      Pure/ML/ml_compiler.ML
    Author:     Makarius

Runtime compilation and evaluation -- generic version.
*)

signature ML_COMPILER =
sig
  type flags = {SML: bool, exchange: bool, redirect: bool, verbose: bool}
  val flags: flags
  val verbose: bool -> flags -> flags
  val eval: flags -> Position.T -> ML_Lex.token list -> unit
end

structure ML_Compiler: ML_COMPILER =
struct

type flags = {SML: bool, exchange: bool, redirect: bool, verbose: bool};
val flags = {SML = false, exchange = false, redirect = false, verbose = false};

fun verbose b (flags: flags) =
  {SML = #SML flags, exchange = #exchange flags, redirect = #redirect flags, verbose = b};

fun eval (flags: flags) pos toks =
  let
    val _ = if #SML flags then error ("Standard ML is unsupported on " ^ ML_System.name) else ();
    val line = the_default 1 (Position.line_of pos);
    val file = the_default "ML" (Position.file_of pos);
    val text = ML_Lex.flatten toks;
  in Secure.use_text ML_Env.local_context (line, file) (#verbose flags) text end;

end;

