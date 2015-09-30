(*  Title:      Pure/PIDE/protocol_message.ML
    Author:     Makarius

Auxiliary operations on protocol messages.
*)

signature PROTOCOL_MESSAGE =
sig
  val command_positions: string -> XML.body -> XML.body
  val command_positions_yxml: string -> string -> string
end;

structure Protocol_Message: PROTOCOL_MESSAGE =
struct

fun command_positions id =
  let
    fun attribute (a, b) =
      if a = Markup.idN andalso b = Markup.commandN then (a, id) else (a, b);
    fun tree (XML.Elem ((a, atts), ts)) = XML.Elem ((a, map attribute atts), map tree ts)
      | tree text = text;
  in map tree end;

fun command_positions_yxml id =
  YXML.string_of_body o command_positions id o YXML.parse_body;

end;
