(*  Title:      Pure/General/input.ML
    Author:     Makarius

Generic input with position information.
*)

signature INPUT =
sig
  type source
  val is_delimited: source -> bool
  val text_of: source -> Symbol_Pos.text
  val pos_of: source -> Position.T
  val range_of: source -> Position.range
  val source: bool -> Symbol_Pos.text -> Position.range -> source
  val string: string -> source
  val reset_pos: source -> source
  val source_explode: source -> Symbol_Pos.T list
  val source_content: source -> string
  val equal_content: source * source -> bool
end;

structure Input: INPUT =
struct

abstype source = Source of {delimited: bool, text: Symbol_Pos.text, range: Position.range}
with


(* source *)

fun is_delimited (Source {delimited, ...}) = delimited;
fun text_of (Source {text, ...}) = text;
fun pos_of (Source {range = (pos, _), ...}) = pos;
fun range_of (Source {range, ...}) = range;

fun source delimited text range =
  Source {delimited = delimited, text = text, range = range};

fun string text = source true text Position.no_range;

fun reset_pos (Source {delimited, text, ...}) = source delimited text Position.no_range;


(* content *)

fun source_explode (Source {text, range = (pos, _), ...}) =
  Symbol_Pos.explode (text, pos);

val source_content = source_explode #> Symbol_Pos.content;

val equal_content = (op =) o apply2 source_content;

end;

end;
