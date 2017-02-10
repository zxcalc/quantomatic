(*  Title:      Pure/System/bash_syntax.ML
    Author:     Makarius

Syntax for GNU bash (see also Pure/System/bash.ML).
*)

signature BASH_SYNTAX =
sig
  val string: string -> string
  val strings: string list -> string
end;

structure Bash_Syntax: BASH_SYNTAX =
struct

fun string "" = "\"\""
  | string str =
      str |> translate_string (fn ch =>
        let val c = ord ch in
          (case ch of
            "\t" => "$'\\t'"
          | "\n" => "$'\\n'"
          | "\f" => "$'\\f'"
          | "\r" => "$'\\r'"
          | _ =>
              if Symbol.is_ascii_letter ch orelse Symbol.is_ascii_digit ch orelse
                exists_string (fn c => c = ch) "-./:_" then ch
              else if c < 16 then "$'\\x0" ^ Int.fmt StringCvt.HEX c ^ "'"
              else if c < 32 orelse c >= 127 then "$'\\x" ^ Int.fmt StringCvt.HEX c ^ "'"
              else "\\" ^ ch)
        end);

val strings = space_implode " " o map string;

end;
