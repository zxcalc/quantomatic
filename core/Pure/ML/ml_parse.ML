(*  Title:      Pure/ML/ml_parse.ML
    Author:     Makarius

Minimal parsing for SML -- fixing integer numerals.
*)

signature ML_PARSE =
sig
  val fix_ints: string -> string
  val global_context: use_context
end;

structure ML_Parse: ML_PARSE =
struct

(** error handling **)

fun !!! scan =
  let
    fun get_pos [] = " (end-of-input)"
      | get_pos (tok :: _) = Position.here (ML_Lex.pos_of tok);

    fun err (toks, NONE) = (fn () => "SML syntax error" ^ get_pos toks)
      | err (toks, SOME msg) = (fn () => "SML syntax error" ^ get_pos toks ^ ": " ^ msg ());
  in Scan.!! err scan end;

fun bad_input x =
  (Scan.some (fn tok => (case ML_Lex.kind_of tok of ML_Lex.Error msg => SOME msg | _ => NONE)) :|--
    (fn msg => Scan.fail_with (K (fn () => msg)))) x;


(** basic parsers **)

fun $$$ x =
  Scan.one (fn tok => ML_Lex.kind_of tok = ML_Lex.Keyword andalso ML_Lex.content_of tok = x)
    >> ML_Lex.content_of;

val int = Scan.one (fn tok => ML_Lex.kind_of tok = ML_Lex.Int) >> ML_Lex.content_of;

val regular = Scan.one ML_Lex.is_regular >> ML_Lex.content_of;
val improper = Scan.one ML_Lex.is_improper >> ML_Lex.content_of;

val blanks = Scan.repeat improper >> implode;


(* fix_ints *)

(*approximation only -- corrupts numeric record field patterns *)
val fix_int =
  $$$ "#" ^^ blanks ^^ int ||
  ($$$ "infix" || $$$ "infixr") ^^ blanks ^^ int ||
  int >> (fn x => "(" ^ x ^ ":int)") ||
  regular ||
  bad_input;

val fix_ints =
  ML_System.is_smlnj ?
   (Source.of_string #>
    ML_Lex.source #>
    Source.source ML_Lex.stopper (Scan.bulk (!!! fix_int)) NONE #>
    Source.exhaust #>
    implode);


(* global use_context *)

val global_context: use_context =
 {tune_source = fix_ints,
  name_space = ML_Name_Space.global,
  str_of_pos = Position.here oo Position.line_file,
  print = writeln,
  error = error};

end;
