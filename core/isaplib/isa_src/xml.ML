(*  Title:      Pure/General/xml.ML
    ID:         $Id: xml.ML,v 1.39 2008/05/24 18:12:18 wenzelm Exp $
    Author:     David Aspinall, Stefan Berghofer and Markus Wenzel

Basic support for XML.
*)

signature XML =
sig
  type attributes = Markup.property list
  datatype tree =
      Elem of string * attributes * tree list
    | Text of string
    | Output of output
  val add_content: tree -> Buffer.T -> Buffer.T
  val detect: string -> bool
  val header: string
  val text: string -> string
  val element: string -> attributes -> string list -> string
  val output_markup: Markup.T -> output * output
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

type attributes = Markup.property list;

datatype tree =
    Elem of string * attributes * tree list
  | Text of string
  | Output of output;   (* FIXME !? *)

fun add_content (Elem (_, _, ts)) = fold add_content ts
  | add_content (Text s) = Buffer.add s
  | add_content (Output _) = I;    (* FIXME !? *)



(** string representation **)

val detect = String.isPrefix "<?xml";
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

fun output_markup (name, atts) =
 (enclose "<" ">" (elem name atts),
  enclose "</" ">" name);


(* output *)

fun buffer_of tree =
  let
    fun traverse (Elem (name, atts, [])) =
          Buffer.add "<" #> Buffer.add (elem name atts) #> Buffer.add "/>"
      | traverse (Elem (name, atts, ts)) =
          Buffer.add "<" #> Buffer.add (elem name atts) #> Buffer.add ">" #>
          fold traverse ts #>
          Buffer.add "</" #> Buffer.add name #> Buffer.add ">"
      | traverse (Text s) = Buffer.add (text s)
      | traverse (Output s) = Buffer.add s;
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

val parse_string = Scan.read Symbol.stopper parse_chars o explode;

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
              (Scan.this_string ("</" ^ s) --| blanks --| $$ ">"))) >>
    (fn ((s, atts), ts) => Elem (s, atts, ts))) xs;

val parse_document =
  (Scan.repeat parse_misc -- Scan.option parse_doctype -- Scan.repeat parse_misc)
  |-- parse_element;

fun parse s =
  (case Scan.finite Symbol.stopper (Scan.error (!! (err "Malformed element")
      (blanks |-- parse_document --| blanks))) (explode s) of
    (x, []) => x
  | (_, ys) => error ("XML parsing error: Unprocessed input\n" ^ Symbol.beginning 100 ys));

end;

end;
