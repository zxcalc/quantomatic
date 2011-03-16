signature XML_OUTPUT_UTILS = 
sig 
  include XML
  (* NOTE: Markup.T = string * Properties.T *)

  val start_tag : string -> (string*string) list -> string
  val empty_tag : string -> (string*string) list -> string
  val end_tag : string -> string
  val pretty_text : string -> (string*string) list -> string -> Pretty.T
  val pretty_item : string -> (string*string) list -> Pretty.T -> Pretty.T
  val pretty_list : string -> (string*string) list -> Pretty.T list -> Pretty.T
end;

structure XML_Output_Utils 
: XML_OUTPUT_UTILS  
= struct

open XML;

fun attr_list ((name,value)::xs) = (" "^name^"=\""^(text value)^"\"")
  | attr_list [] = ""

fun start_tag name [] = ("<"^name^">")
  | start_tag name attrs = ("<"^name^(attr_list attrs)^">");

fun empty_tag name [] = ("<"^name^">")
  | empty_tag name attrs = ("<"^name^(attr_list attrs)^"/>");

fun end_tag name = ("</"^name^">");

fun pretty_text name attrs text = Pretty.str ((start_tag name attrs)^text^(end_tag name));

fun pretty_item name attrs item = Pretty.enclose (start_tag name attrs) (end_tag name) [item];

fun pretty_list name attrs items = Pretty.chunks [Pretty.str (start_tag name attrs),
                                                  Pretty.indent 2 (Pretty.chunks items),
                                                  Pretty.str (end_tag name)];

end

(* vim:et:sts=2:sw=2
*)
