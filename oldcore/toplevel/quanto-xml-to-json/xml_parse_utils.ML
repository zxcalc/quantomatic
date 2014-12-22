signature XML_PARSE_UTILS =
sig
  include XML
  exception bad_input_exp of string*string
  (* NOTE:     Markup.T = string * Properties.T
           Properties.T = XML attributes = (string * string) list *)

  (* tree = Text ... | Elem ... , see xml.ML *)
  val is_text : tree -> bool (* is just text *)
  val is_elem : tree -> bool (* is a tag *)

  (* if it's an elem and has the given tag... *)
  val test_tag : string -> tree -> bool (* elem has tag = string *)
  val assert_tag : string -> tree -> unit (* raises *)

  (* typical usage interface *)
  val get_text : tree -> string (* raises if elem *)
  val get_elem : tree -> Markup.T * tree list (* raises if text *)

  val get_tag : tree -> string (* the tagname part of the Markup.T; raises if text *)
  val get_attributes : tree -> Properties.T (* raises if text *)
  val lookup_attribute : tree -> string -> string option
  val get_attribute : tree -> string -> string (* raises if text *)

  val get_children : tree -> tree list (* raises if text *)
  val get_filtered_children : string -> tree -> tree list (* raises if text *)
  val get_filtered_children_text : string -> tree -> string list (* raises if text *)

  val get_unique_child : tree -> tree (* raises if not a unique child *)
  val get_unique_child_text : tree -> string (* raises if the unique child is not text *)
  val get_unique_child_ignore_wsp : tree -> tree (* raises if not a unique child *)

  (* other helpful stuff *)
  val filter_elems : string -> tree list -> tree list
  val lookup_first_elem : string -> tree list -> tree option
  val lookup_unique_elem : string -> tree list -> tree option (* SOME if exactly one *)
  val get_first_elem : string -> tree list -> tree (* raises if text or none *)
  val get_unique_elem : string -> tree list -> tree (* raises if not exactly one *)
  val apply_to_unique_elem : (tree -> 'a) -> string -> tree list -> 'a  (* raises if not unique *)

  (* direct conversion from XML text elements to value *)
  val get_boolean : tree -> bool (* raises if not "true" or "false" (case-insensitive) *)
  val get_int : tree -> int (* raises if not text or cannot parse to integer *)

end

structure XML_Parse_Utils
: XML_PARSE_UTILS
= struct

open XML

exception bad_input_exp of string*string

val tolower = String.map Char.toLower

fun is_text (Text _) = true | is_text _ = false
fun is_elem (Elem _) = true | is_elem _ = false

fun get_elem (Text _) = raise bad_input_exp ("Can't get elem of text","")
  | get_elem (Elem x) = x

fun get_text (Text s) = s
  | get_text (Elem ((tag,_),_)) = raise bad_input_exp ("Expected text but found <"^ tag ^"> element.","")

fun get_boolean (Text s) =
      let
        val t = String.map Char.toLower s
      in
        if (t = "true") then true
        else if (t = "false") then false
        else raise bad_input_exp ("Expected 'true' or 'false' but found '"^t^"'.","")
      end
  | get_boolean (Elem ((tag,_),_)) = raise bad_input_exp ("Expected 'true' or 'false' but found <"^ tag ^"> element.","")

fun get_int (Text s) =
      (case Int.fromString s of
        SOME i => i
      | NONE => raise bad_input_exp  ("Expected integer but found '"^s^"'.",""))
  | get_int (Elem ((tag,_),_)) = raise bad_input_exp ("Expected integer but found <"^ tag ^"> element.","")

fun get_tag (Text _) = raise bad_input_exp ("Can't get tag of text","")
  | get_tag (Elem ((tag, attr),children)) = tag

fun get_attributes (Text _) = raise bad_input_exp ("Can't get attribute of text","")
  | get_attributes (Elem ((elem, attr),children)) = attr

fun lookup_attribute xml attr_name =
    AList.lookup (op =) (get_attributes xml) attr_name

fun get_attribute xml attr_name =
    (case lookup_attribute xml attr_name
       of NONE =>
          raise bad_input_exp ("Missing attribute: '" ^ attr_name ^ "'","")
        | SOME s => s)

fun get_children (Text _) = raise bad_input_exp ("Can't get children of text","")
  | get_children (Elem ((elem, attr),children)) = children

fun get_unique_child (Text _) = raise bad_input_exp ("Can't get unique child of text","")
  | get_unique_child (Elem ((elem, attr),[child])) = child
  | get_unique_child (Elem ((elem, attr),_)) = raise bad_input_exp ("<"^elem^"> does not have unique child","")

fun is_whitespace_char " " = true
  | is_whitespace_char "\n" = true
  | is_whitespace_char "\r" = true
  | is_whitespace_char _ = false

fun filter_whitespace ((Text s)::remaining) =
	if (forall_string is_whitespace_char s) then (filter_whitespace remaining)
        else ((Text s)::(filter_whitespace remaining))
  | filter_whitespace (e::remaining) = (e::(filter_whitespace remaining))
  | filter_whitespace [] = []

fun get_unique_child_ignore_wsp (Text _) = raise bad_input_exp ("Can't get unique child of text","")
  | get_unique_child_ignore_wsp (Elem ((elem, attr),children)) = get_unique_child (Elem ((elem, attr),(filter_whitespace children)))

val get_unique_child_text = get_text o get_unique_child


fun test_tag tag xml = is_elem xml andalso (tolower tag = tolower (get_tag xml))

fun assert_tag tag xml =
    if test_tag tag xml then ()
    else raise bad_input_exp ("found "^(if is_text xml then "Text"  else "<"^(get_tag xml)^">")^" but expected <"^tag^">.","")

(* get the first element in the list  which is of type "wanted"
   return the rest of the list in the second coord
   if none is found either return NONE or fail depending
   on value of boolean param force
*)
fun filter_elems tag trees = List.filter (test_tag tag) trees

fun get_filtered_children tag tree =
    filter_elems tag (get_children tree)

fun get_filtered_children_text tag tree =
    map get_text (get_filtered_children tag tree);


fun lookup_first_elem tag trees = List.find (test_tag tag) trees
fun get_first_elem tag trees =
    case lookup_first_elem tag trees of SOME x => x
       | NONE => raise bad_input_exp ("Element with tag <"^tag^"> not found.","")

fun lookup_unique_elem tag trees =
    case List.filter (test_tag tag) trees
      of [x] => SOME x
       | _ => NONE
fun get_unique_elem tag trees =
    case lookup_unique_elem tag trees
      of NONE => raise bad_input_exp ("Unique <"^tag^"> tag not found.","")
       | SOME x => x

fun apply_to_unique_elem f tag trees =
    let
      val elem = get_unique_elem tag trees
    in
      (f elem) handle bad_input_exp (s,p) => raise bad_input_exp (s,tag^"/"^p)
    end
end

(* vim:et:sts=2:sw=2
*)
