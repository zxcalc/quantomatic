(*  Title:      Pure/General/symbol_explode.ML
    Author:     Makarius

String explode operation for Isabelle symbols (see also symbol.ML).
*)

structure Symbol: sig val explode: string -> string list end =
struct

fun is_ascii_letter c = #"A" <= c andalso c <= #"Z" orelse #"a" <= c andalso c <= #"z";
fun is_ascii_letdig c =
  is_ascii_letter c orelse #"0" <= c andalso c <= #"9" orelse c = #"_" orelse c = #"'";

fun is_utf8 c = c >= #"\128";
fun is_utf8_trailer c = #"\128" <= c andalso c < #"\192";
fun is_utf8_control c = #"\128" <= c andalso c < #"\160";

fun explode string =
  let
    fun char i = String.sub (string, i);
    fun string_range i j = String.substring (string, i, j - i);

    val n = size string;
    fun test pred i = i < n andalso pred (char i);
    fun many pred i = if test pred i then many pred (i + 1) else i;
    fun maybe pred i = if test pred i then i + 1 else i;
    fun maybe_char c = maybe (fn c' => c = c');
    fun maybe_ascii_id i = if test is_ascii_letter i then many is_ascii_letdig (i + 1) else i;

    fun scan i =
      if i < n then
        let val ch = char i in
          (*encoded newline*)
          if ch = #"\^M" then "\n" :: scan (maybe_char #"\n" (i + 1))
          (*pseudo utf8: encoded ascii control*)
          else if ch = #"\192" andalso
            test is_utf8_control (i + 1) andalso not (test is_utf8 (i + 2))
          then chr (Char.ord (char (i + 1)) - 128) :: scan (i + 2)
          (*utf8*)
          else if is_utf8 ch then
            let val j = many is_utf8_trailer (i + 1)
            in string_range i j :: scan j end
          (*named symbol*)
          else if ch = #"\\" andalso test (fn c => c = #"<") (i + 1) then
            let val j = (i + 2) |> maybe_char #"^" |> maybe_ascii_id |> maybe_char #">"
            in string_range i j :: scan j end
          (*single character*)
          else String.str ch :: scan (i + 1)
        end
      else [];
  in scan 0 end;

end;
