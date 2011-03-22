signature XML_OUTPUT_UTILS = 
sig 
  (* NOTE: Markup.T = string * Properties.T *)

  val wrap_text_simple : string -> string -> XML.tree
  val wrap_bool_simple : string -> bool -> XML.tree
  val wrap_int_simple : string -> int -> XML.tree
  val wrap_el_simple : string -> XML.tree -> XML.tree
  val wrap_els_simple : string -> XML.tree list -> XML.tree
end;

structure XML_Output_Utils 
: XML_OUTPUT_UTILS  
= struct

open XML;

fun wrap_els_simple name elem = (Elem ((name, []), elem))

fun wrap_el_simple name elem = wrap_els_simple name [elem]

fun wrap_text_simple name text = wrap_el_simple name (Text text)

fun wrap_bool_simple name true = wrap_text_simple name "true"
  | wrap_bool_simple name false = wrap_text_simple name "false"

fun wrap_int_simple name i = wrap_text_simple name (Int.toString i)

end

(* vim:et:sts=2:sw=2
*)
