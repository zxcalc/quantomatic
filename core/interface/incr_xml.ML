(*  Title:      Pure/General/incr_xml.ML
    Author:     Lucas Dixon
    based on:   Isabelle's xml.ML

Incremental parsing of simple XML tree values.
*)

signature INCR_XML =
sig
  type attributes = Properties.T
  datatype tree_zipper =

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

structure XML: XML =
struct

(** XML trees **)

type attributes = Properties.T;

(* 
datatype tree =
    Elem of Markup.T * tree list
  | Text of string;
*)

(* IDEA: there should be an automatic transformation to go from a grammar to a lazy parser 
   for that grammar... *)

datatype edit_property = 
    Edit_PropName of string
    Edit_PropValue of string * string;

datatype edit_point =
    Edit_ElemName of string 
  | Edit_ElemProp of string * Properties.T * edit_property
  | Edit_Text of string;

datatype tree_ctxt = TreeCtxt of (Markup.T * tree list);

datatype partial_tree = PartialTree of TreeCtxt list * edit_point

local open XML in  
  fun parse (PartialTree (ctxt, Edit_Text s)) = 
      (Text s)
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
