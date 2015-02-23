(*  Title:      Pure/ML-Systems/ml_positions.ML
    Author:     Makarius

Approximate ML antiquotation @{here} for Isabelle/Pure bootstrap.
*)

fun ml_positions start_line name txt =
  let
    fun positions line (#"@" :: #"{" :: #"h" :: #"e" :: #"r" :: #"e" :: #"}" :: cs) res =
          let val s = "(Position.line_file_only " ^ Int.toString line ^ " \"" ^ name ^ "\")"
          in positions line cs (s :: res) end
      | positions line (c :: cs) res =
          positions (if c = #"\n" then line + 1 else line) cs (str c :: res)
      | positions _ [] res = rev res;
  in String.concat (positions start_line (String.explode txt) []) end;

