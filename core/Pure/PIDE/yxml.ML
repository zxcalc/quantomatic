(*  Title:      Pure/PIDE/yxml.ML
    Author:     Makarius

Efficient text representation of XML trees using extra characters X
and Y -- no escaping, may nest marked text verbatim.  Suitable for
direct inlining into plain text.

Markup <elem att="val" ...>...body...</elem> is encoded as:

  X Y name Y att=val ... X
  ...
  body
  ...
  X Y X
*)

signature YXML =
sig
  val X: Symbol.symbol
  val Y: Symbol.symbol
  val embed_controls: string -> string
  val detect: string -> bool
  val output_markup: Markup.T -> string * string
  val string_of_body: XML.body -> string
  val string_of: XML.tree -> string
  val output_markup_elem: Markup.T -> (string * string) * string
  val parse_body: string -> XML.body
  val parse: string -> XML.tree
end;

structure YXML: YXML =
struct

(** string representation **)

(* idempotent recoding of certain low ASCII control characters *)

fun pseudo_utf8 c =
  if Symbol.is_ascii_control c
  then chr 192 ^ chr (128 + ord c)
  else c;

fun embed_controls str =
  if exists_string Symbol.is_ascii_control str
  then translate_string pseudo_utf8 str
  else str;


(* markers *)

val X = chr 5;
val Y = chr 6;
val XY = X ^ Y;
val XYX = XY ^ X;

val detect = exists_string (fn s => s = X orelse s = Y);


(* output *)

fun output_markup (markup as (name, atts)) =
  if Markup.is_empty markup then Markup.no_output
  else (XY ^ name ^ implode (map (fn (a, x) => Y ^ a ^ "=" ^ x) atts) ^ X, XYX);

fun string_of_body body =
  let
    fun attrib (a, x) = Buffer.add Y #> Buffer.add a #> Buffer.add "=" #> Buffer.add x;
    fun tree (XML.Elem ((name, atts), ts)) =
          Buffer.add XY #> Buffer.add name #> fold attrib atts #> Buffer.add X #>
          trees ts #>
          Buffer.add XYX
      | tree (XML.Text s) = Buffer.add s
    and trees ts = fold tree ts;
  in Buffer.empty |> trees body |> Buffer.content end;

val string_of = string_of_body o single;


(* wrapped elements *)

val Z = chr 0;
val Z_text = [XML.Text Z];

fun output_markup_elem markup =
  let val [bg1, bg2, en] = space_explode Z (string_of (XML.wrap_elem ((markup, Z_text), Z_text)))
  in ((bg1, bg2), en) end;



(** efficient YXML parsing **)

local

(* splitting *)

fun is_char s c = ord s = Char.ord c;

val split_string =
  Substring.full #>
  Substring.tokens (is_char X) #>
  map (Substring.fields (is_char Y) #> map Substring.string);


(* structural errors *)

fun err msg = raise Fail ("Malformed YXML: " ^ msg);
fun err_attribute () = err "bad attribute";
fun err_element () = err "bad element";
fun err_unbalanced "" = err "unbalanced element"
  | err_unbalanced name = err ("unbalanced element " ^ quote name);


(* stack operations *)

fun add x ((elem, body) :: pending) = (elem, x :: body) :: pending;

fun push "" _ _ = err_element ()
  | push name atts pending = ((name, atts), []) :: pending;

fun pop ((("", _), _) :: _) = err_unbalanced ""
  | pop ((markup, body) :: pending) = add (XML.Elem (markup, rev body)) pending;


(* parsing *)

fun parse_attrib s =
  (case first_field "=" s of
    NONE => err_attribute ()
  | SOME ("", _) => err_attribute ()
  | SOME att => att);

fun parse_chunk ["", ""] = pop
  | parse_chunk ("" :: name :: atts) = push name (map parse_attrib atts)
  | parse_chunk txts = fold (add o XML.Text) txts;

in

fun parse_body source =
  (case fold parse_chunk (split_string source) [(("", []), [])] of
    [(("", _), result)] => rev result
  | ((name, _), _) :: _ => err_unbalanced name);

fun parse source =
  (case parse_body source of
    [result] => result
  | [] => XML.Text ""
  | _ => err "multiple results");

end;

end;

