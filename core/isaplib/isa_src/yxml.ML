(*  Title:      Pure/General/yxml.ML
    ID:         $Id: yxml.ML,v 1.4 2008/04/16 15:40:40 wenzelm Exp $
    Author:     Makarius

Efficient text representation of XML trees using extra characters X
and Y -- no escaping, may nest marked text verbatim.

Markup <elem att="val" ...>...body...</elem> is encoded as:

  X Y name Y att=val ... X
  ...
  body
  ...
  X Y X
*)

signature YXML =
sig
  val detect: string -> bool
  val output_markup: Markup.T -> string * string
  val element: string -> XML.attributes -> string list -> string
  val string_of: XML.tree -> string
  val parse_body: string -> XML.tree list
  val parse_element: string -> string * XML.attributes * XML.tree list
  val parse: string -> XML.tree
end;

structure YXML: YXML =
struct

(** string representation **)

(* markers *)

val X = Symbol.ENQ;
val Y = Symbol.ACK;
val XY = X ^ Y;
val XYX = XY ^ X;

val detect = String.isPrefix XY;


(* output *)

fun output_markup (name, atts) =
  (XY ^ name ^ implode (map (fn (a, x) => Y ^ a ^ "=" ^ x) atts) ^ X, XYX);

fun element name atts body =
  let val (pre, post) = output_markup (name, atts)
  in pre ^ implode body ^ post end;

fun string_of t =
  let
    fun attrib (a, x) =
      Buffer.add Y #>
      Buffer.add a #> Buffer.add "=" #> Buffer.add x;
    fun tree (XML.Elem (name, atts, ts)) =
          Buffer.add XY #> Buffer.add name #> fold attrib atts #> Buffer.add X #>
          fold tree ts #>
          Buffer.add XYX
      | tree (XML.Text s) = Buffer.add s
      | tree (XML.Output s) = Buffer.add s;
  in Buffer.empty |> tree t |> Buffer.content end;



(** efficient YXML parsing **)

local

(* splitting *)

fun is_char s c = ord s = Char.ord c;

val split_string =
  Substring.full #>
  Substring.tokens (is_char X) #>
  map (Substring.fields (is_char Y) #> map Substring.string);


(* structural errors *)

fun err msg = raise Fail ("Malformed YXML encoding: " ^ msg);
fun err_attribute () = err "bad attribute";
fun err_element () = err "bad element";
fun err_unbalanced "" = err "unbalanced element"
  | err_unbalanced name = err ("unbalanced element " ^ quote name);


(* stack operations *)

fun add x ((elem, body) :: pending) = (elem, x :: body) :: pending;

fun push "" _ _ = err_element ()
  | push name atts pending = ((name, atts), []) :: pending;

fun pop ((("", _), _) :: _) = err_unbalanced ""
  | pop (((name, atts), body) :: pending) = add (XML.Elem (name, atts, rev body)) pending;


(* parsing *)

fun parse_attrib s =
  (case space_explode "=" s of
    [] => err_attribute ()
  | "" :: _ => err_attribute ()
  | a :: xs => (a, space_implode "=" xs));

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
    [result as XML.Elem _] => result
  | _ => err "no root element");

val parse_element = parse #> (fn XML.Elem elem => elem);

end;

end;

