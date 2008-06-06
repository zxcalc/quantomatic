(*  Title:      Pure/ML/ml_parse.ML
    ID:         $Id: ml_parse.ML,v 1.1 2007/09/16 12:52:28 wenzelm Exp $
    Author:     Makarius

Minimal parsing for SML -- fixing integer numerals.
*)

signature ML_PARSE =
sig
  val fix_ints: string -> string
end;

structure ML_Parse: ML_PARSE =
struct

structure T = ML_Lex;


(** error handling **)

fun !!! scan =
  let
    fun get_pos [] = " (past end-of-file!)"
      | get_pos (tok :: _) = T.pos_of tok;

    fun err (toks, NONE) = "SML syntax error" ^ get_pos toks
      | err (toks, SOME msg) = "SML syntax error" ^ get_pos toks ^ ": " ^ msg;
  in Scan.!! err scan end;

fun bad_input x =
  (Scan.some (fn tok => (case T.kind_of tok of T.Error msg => SOME msg | _ => NONE)) :|--
    (fn msg => Scan.fail_with (K msg))) x;


(** basic parsers **)

fun $$$ x = Scan.one (fn tok => T.kind_of tok = T.Keyword andalso T.val_of tok = x) >> T.val_of;
val int = Scan.one (fn tok => T.kind_of tok = T.Int) >> T.val_of;

val regular = Scan.one T.is_regular >> T.val_of;
val improper = Scan.one T.is_improper >> T.val_of;

val blanks = Scan.repeat improper >> implode;


(* fix_ints *)

(*approximation only -- corrupts numeric record field patterns *)
val fix_int =
  $$$ "#" ^^ blanks ^^ int ||
  ($$$ "infix" || $$$ "infixr") ^^ blanks ^^ int ||
  int >> (fn x => "(" ^ x ^ ":int)") ||
  regular ||
  bad_input;

fun do_fix_ints s =
  Source.of_string s
  |> T.source
  |> Source.source T.stopper (Scan.bulk (!!! fix_int)) NONE
  |> Source.exhaust
  |> implode;

val fix_ints = if ml_system_fix_ints then do_fix_ints else I;

end;
