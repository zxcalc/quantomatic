(*  Title:      Pure/General/incr_xml.ML
    Author:     Lucas Dixon
    based on:   Isabelle's xml.ML

Incremental parsing of simple XML tree values.
*)

signature INCR_XML =
sig
  (* NOTE: type of attributes = Properties.T *)
  datatype T (* partially given XML *)

  val get_position : T -> Position.T (* not in the middle of something XML *)

  val xml_so_far : T -> XML.tree (* partially given XML *)
  val depth : T -> int (* number of tags above us. *)
  val is_valid_xml : T -> bool (* not in the middle of something XML: implies depth = 0 *)

  exception bad_xml_exp of Position.T * string;
  (* think of symbol as being a single character *)
  val add_symbol : string -> T -> T (* raises bad_xml_exp *)


  type body = tree list
  val add_content: tree -> Buffer.T -> Buffer.T
  val content_of: body -> string
  val header: string
  val text: string -> string
  val element: string -> attributes -> string list -> string
  val output_markup: Markup.T -> Output.output * Output.output
  val string_of: tree -> string
  val output: tree -> TextIO.outstream -> unit
  val parse_comments: string list -> unit * string list
  val parse_string : string -> string option
  val parse_element: string list -> tree * string list
  val parse_document: string list -> tree * string list
  val parse: string -> tree
end;

structure PartialXML: INCR_XML =
struct


(* IDEA: functional, dependently typed, incremental parsing algorithm *)
datatype 'sym 'a parse_chunk = 
  Done 'a 
| Waiting (string (* informal description of state *) 
           * ('sym -> 'a parse_chunk))

datatype 'a parse_state = 
  ('sym1 'sym2 parse_chunk) :: ('sym2 'sym3 parse_chunk) :: ...  


(** XML trees **)

type attributes = Properties.T;

(* 
datatype tree =
    Elem of Markup.T * tree list
  | Text of string;
*)

(* IDEA: there should be an automatic transformation to go from a grammar to a lazy parser 
   for that grammar... essentially create the zipper for. This means we could also create the text editor, which *knows* the structure of text even as we are typing and editing it. With a bit of thought this could be made very very cool... *)

(* type used in edit point for when we are the middle of editing a propety 
   (which is called an attribute in XML, but I'm 
   usiung Isabelle's terminology here to fit the type names) *)
datatype edit_property = 
    (* at: "...<name ... part_of_prop_name_" word-chars until we get a "=" *)
    Edit_PropName of string             
    (* at: "...<name ... part_of_prop_name='part of value"; until we get a ' chat *)
  | Edit_PropValue of string * string;  

(* editing something in XML *)
datatype edit_point =
    (* at: "...<in_this_name"; until non-word char, i.e. ">" or space *)
    Edit_ElemName of string 
    (* at: "...<name prop='x' "; either ">" or start of new property/attribute *)
  | Edit_ElemProp of string * Properties.T * edit_property
    (* at: "...<name>... "; until we get to a "<" or end of file. *)
  | Edit_Text of string;

(* a tag we are inside of, and the list of tags above it, which are also children of 
   the same parent node. *)
datatype tree_ctxt = TreeCtxt of (Markup.T * tree list);

(* partial tree, the zipper context above us, and this edit point. *)
datatype partial_tree = 
  PartialTree of {
    context: TreeCtxt list, (* context of tree above us *)
    needed : string list, (* stuff we need to get now continue *)
    edit : edit_point, (* particular point being edited *)
  };

(* boiler plate get, update, set *)
fun get_needed (PartialTree rep) = #needed rep;
fun get_context (PartialTree rep) = #context rep;
fun get_edit (PartialTree rep) = #edit rep;

fun update_context f (PartialTree rep) = 
    PartialTree {context = f(#context rep), edit = #edit rep, needed = #needed rep };
fun update_edit f (PartialTree rep) = 
    PartialTree {context = #context rep, edit = f (#edit rep), needed = #needed rep };
fun update_needed f (PartialTree rep) = 
    PartialTree {context = #context rep, edit = #edit rep, needed = f(#needed rep) };

val set_context = update_context o K;
val set_edit = update_edit o K;
val set_needed = update_needed o K;


fun add_next_sibling xml_tree = update_context 
      (fn TreeCtxt (tag,above_siblings) => (TreeCtxt(tag,xml_tree :: above_siblings)));

fun push_new_parent s_and_properties = update_context (fn ctxt => (s_and_properties,[])::ctxt);

(* *)
exception bad_xml_exp of (Position.T * string);
exception internal_bad_xml_exp of string;

fun is_space_sym

(* NOTE: assume symbol is a single char, else maybe you get bad looking xml...
   also assumes that position has already been incremeneted *)

fun add_symbol' sym (st as PartialTree {needed = h::t, ...}) = 
    if sym = h then st |> update_needed tl else 
    raise internal_bad_xml_exp ("We found '"^ sym ^"' where you need to have '"^String.concat (h::t)^"' (State: needed).")
  | add_symbol' sym (st as PartialTree {edit = Edit_Text s, ...}) = 
    (* at: "...<name>... "; until we get to a "<" or end of file. *)
    (case sym 
     (* enter a new tag *)
     of "<" => if s = "" then st |> set_edit (Edit_ElemName "")
               else st |> add_next_sibling (XML.Text s)
                       |> set_edit (Edit_ElemName "")
      | ">" => raise internal_bad_xml_exp ("Bad '>' character, expected '<' or normal text (State: Edit_Text).")
      (* add sym to a text element (unless there is no text and symbol is blank). *)
      | _ => if s = "" andalso Symbol.is_blank sym then st else st |> set_edit (Edit_Text (s ^ sym))
    )
  | add_symbol sym (st as PartialTree {edit = Edit_ElemName s, ...}) = 
    (* at: "...<in_this_name"; until non-word char, i.e. ">" or space *)
    if Symbol.is_blank sym then st |> set_edit (Edit_ElemProp (s, [], Edit_PropName ""))
    else if Symbol.is_quasi_letter sym orelse Symbol.is_digit sym then ;
      st |> set_edit (Edit_ElemName (s ^ sym))
    else if sym = ">" then 
      if s = "" then raise internal_bad_xml_exp ("Empty-string XML tags, as in '<>', are not allowed (State: Edit_ElemName).")
      else st |> push_new_parent (s,[])
              |> set_edit (Edit_Text "")
    else if sym = "/" then st |> add_next_sibling (XML.Elem((s,[]),[]))
                              |> set_edit (Edit_Text "")
                              |> set_needed ">"
    else raise internal_bad_xml_exp ("Strange character in xml-tag name: '" ^ sym ^ "', this is not allowed (State: Edit_ElemName).")
  | add_symbol sym (st as PartialTree {edit = Edit_ElemProp (tag,props,Edit_PropName s), ...}) =
    if Symbol.is_blank sym then st
    else if sym = "=" then st |> set_edit (Edit_PropValue (s,""))
                              |> set_needed "\""

(* at: "...<name prop='x' "; either ">" or start of new property/attribute *)




set_edit (Edit_Text (s ^ sym)))
     

    (Text s)


local open XML in  

Position.start

end;

type body = tree list;

fun add_content (Elem (_, ts)) = fold add_content ts
  | add_content (Text s) = Buffer.add s;

fun content_of body = Buffer.empty |> fold add_content body |> Buffer.content;



(** string representation **)

val header = "<?xml version=\"1.0\"?>\n";


(* escaped text *)

fun decode "&lt;" = "<"
  | decode "&gt;" = ">"
  | decode "&amp;" = "&"
  | decode "&apos;" = "'"
  | decode "&quot;" = "\""
  | decode c = c;

fun encode "<" = "&lt;"
  | encode ">" = "&gt;"
  | encode "&" = "&amp;"
  | encode "'" = "&apos;"
  | encode "\"" = "&quot;"
  | encode c = c;

val text = translate_string encode;


(* elements *)

fun elem name atts =
  space_implode " " (name :: map (fn (a, x) => a ^ "=\"" ^ text x ^ "\"") atts);

fun element name atts body =
  let val b = implode body in
    if b = "" then enclose "<" "/>" (elem name atts)
    else enclose "<" ">" (elem name atts) ^ b ^ enclose "</" ">" name
  end;

fun output_markup (markup as (name, atts)) =
  if Markup.is_empty markup then Markup.no_output
  else (enclose "<" ">" (elem name atts), enclose "</" ">" name);


(* output *)

fun buffer_of tree =
  let
    fun traverse (Elem ((name, atts), [])) =
          Buffer.add "<" #> Buffer.add (elem name atts) #> Buffer.add "/>"
      | traverse (Elem ((name, atts), ts)) =
          Buffer.add "<" #> Buffer.add (elem name atts) #> Buffer.add ">" #>
          fold traverse ts #>
          Buffer.add "</" #> Buffer.add name #> Buffer.add ">"
      | traverse (Text s) = Buffer.add (text s);
  in Buffer.empty |> traverse tree end;

val string_of = Buffer.content o buffer_of;
val output = Buffer.output o buffer_of;



(** XML parsing (slow) **)

local

fun err s (xs, _) =
  "XML parsing error: " ^ s ^ "\nfound: " ^ quote (Symbol.beginning 100 xs);

fun ignored _ = [];

val blanks = Scan.many Symbol.is_blank;
val special = $$ "&" ^^ Symbol.scan_id ^^ $$ ";" >> decode;
val regular = Scan.one Symbol.is_regular;
fun regular_except x = Scan.one (fn c => Symbol.is_regular c andalso c <> x);

val parse_chars = Scan.repeat1 (special || regular_except "<") >> implode;

val parse_cdata =
  Scan.this_string "<![CDATA[" |--
  (Scan.repeat (Scan.unless (Scan.this_string "]]>") regular) >> implode) --|
  Scan.this_string "]]>";

val parse_att =
  (Symbol.scan_id --| (blanks -- $$ "=" -- blanks)) --
  (($$ "\"" || $$ "'") :|-- (fn s =>
    (Scan.repeat (special || regular_except s) >> implode) --| $$ s));

val parse_comment =
  Scan.this_string "<!--" --
  Scan.repeat (Scan.unless (Scan.this_string "-->") regular) --
  Scan.this_string "-->" >> ignored;

val parse_processing_instruction =
  Scan.this_string "<?" --
  Scan.repeat (Scan.unless (Scan.this_string "?>") regular) --
  Scan.this_string "?>" >> ignored;

val parse_doctype =
  Scan.this_string "<!DOCTYPE" --
  Scan.repeat (Scan.unless ($$ ">") regular) --
  $$ ">" >> ignored;

val parse_misc =
  Scan.one Symbol.is_blank >> ignored ||
  parse_processing_instruction ||
  parse_comment;

val parse_optional_text =
  Scan.optional (parse_chars >> (single o Text)) [];

in

val parse_comments =
  blanks -- Scan.repeat (parse_comment -- blanks >> K ()) >> K ();

val parse_string = Scan.read Symbol.stopper parse_chars o raw_explode;

fun parse_content xs =
  (parse_optional_text @@@
    (Scan.repeat
      ((parse_element >> single ||
        parse_cdata >> (single o Text) ||
        parse_processing_instruction ||
        parse_comment)
      @@@ parse_optional_text) >> flat)) xs

and parse_element xs =
  ($$ "<" |-- Symbol.scan_id --
    Scan.repeat (blanks |-- parse_att) --| blanks :-- (fn (s, _) =>
      !! (err "Expected > or />")
        (Scan.this_string "/>" >> ignored
         || $$ ">" |-- parse_content --|
            !! (err ("Expected </" ^ s ^ ">"))
              (Scan.this_string ("</" ^ s) --| blanks --| $$ ">"))) >> Elem) xs;

val parse_document =
  (Scan.repeat parse_misc -- Scan.option parse_doctype -- Scan.repeat parse_misc)
  |-- parse_element;

fun parse s =
  (case Scan.finite Symbol.stopper (Scan.error (!! (err "Malformed element")
      (blanks |-- parse_document --| blanks))) (raw_explode s) of
    (x, []) => x
  | (_, ys) => error ("XML parsing error: Unprocessed input\n" ^ Symbol.beginning 100 ys));

end;

end;
