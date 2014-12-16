(*  Title:      Pure/PIDE/xml.ML
    Author:     David Aspinall
    Author:     Stefan Berghofer
    Author:     Makarius

Untyped XML trees and representation of ML values.
*)

signature XML_DATA_OPS =
sig
  type 'a A
  type 'a T
  type 'a V
  val int_atom: int A
  val bool_atom: bool A
  val unit_atom: unit A
  val properties: Properties.T T
  val string: string T
  val int: int T
  val bool: bool T
  val unit: unit T
  val pair: 'a T -> 'b T -> ('a * 'b) T
  val triple: 'a T -> 'b T -> 'c T -> ('a * 'b * 'c) T
  val list: 'a T -> 'a list T
  val option: 'a T -> 'a option T
  val variant: 'a V list -> 'a T
end;

signature XML =
sig
  type attributes = (string * string) list
  datatype tree =
      Elem of (string * attributes) * tree list
    | Text of string
  type body = tree list
  val wrap_elem: ((string * attributes) * tree list) * tree list -> tree
  val unwrap_elem: tree -> (((string * attributes) * tree list) * tree list) option
  val add_content: tree -> Buffer.T -> Buffer.T
  val content_of: body -> string
  val header: string
  val text: string -> string
  val element: string -> attributes -> string list -> string
  val output_markup: Markup.T -> Output.output * Output.output
  val string_of: tree -> string
  val pretty: int -> tree -> Pretty.T
  val output: tree -> TextIO.outstream -> unit
  val parse_comments: string list -> unit * string list
  val parse_string : string -> string option
  val parse_element: string list -> tree * string list
  val parse_document: string list -> tree * string list
  val parse: string -> tree
  exception XML_ATOM of string
  exception XML_BODY of body
  structure Encode: XML_DATA_OPS
  structure Decode: XML_DATA_OPS
end;

structure XML: XML =
struct

(** XML trees **)

type attributes = (string * string) list;

datatype tree =
    Elem of (string * attributes) * tree list
  | Text of string;

type body = tree list;


(* wrapped elements *)

val xml_elemN = "xml_elem";
val xml_nameN = "xml_name";
val xml_bodyN = "xml_body";

fun wrap_elem (((a, atts), body1), body2) =
  Elem ((xml_elemN, (xml_nameN, a) :: atts), Elem ((xml_bodyN, []), body1) :: body2);

fun unwrap_elem (Elem ((name, (n, a) :: atts), Elem ((name', atts'), body1) :: body2)) =
      if name = xml_elemN andalso n = xml_nameN andalso name' = xml_bodyN andalso null atts'
      then SOME (((a, atts), body1), body2) else NONE
  | unwrap_elem _ = NONE;


(* text context *)

fun add_content tree =
  (case unwrap_elem tree of
    SOME (_, ts) => fold add_content ts
  | NONE =>
      (case tree of
        Elem (_, ts) => fold add_content ts
      | Text s => Buffer.add s));

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

fun buffer_of depth tree =
  let
    fun traverse _ (Elem ((name, atts), [])) =
          Buffer.add "<" #> Buffer.add (elem name atts) #> Buffer.add "/>"
      | traverse d (Elem ((name, atts), ts)) =
          Buffer.add "<" #> Buffer.add (elem name atts) #> Buffer.add ">" #>
          traverse_body d ts #>
          Buffer.add "</" #> Buffer.add name #> Buffer.add ">"
      | traverse _ (Text s) = Buffer.add (text s)
    and traverse_body 0 _ = Buffer.add "..."
      | traverse_body d ts = fold (traverse (d - 1)) ts;
  in Buffer.empty |> traverse depth tree end;

val string_of = Buffer.content o buffer_of ~1;
val output = Buffer.output o buffer_of ~1;

fun pretty depth tree =
  Pretty.str (Buffer.content (buffer_of (Int.max (0, depth)) tree));



(** XML parsing **)

local

fun err msg (xs, _) =
  fn () => "XML parsing error: " ^ msg () ^ "\nfound: " ^ quote (Symbol.beginning 100 xs);

fun ignored _ = [];

fun name_start_char c = Symbol.is_ascii_letter c orelse c = ":" orelse c = "_";
fun name_char c = name_start_char c orelse Symbol.is_ascii_digit c orelse c = "-" orelse c = ".";
val parse_name = Scan.one name_start_char ::: Scan.many name_char;

val blanks = Scan.many Symbol.is_blank;
val special = $$ "&" ^^ (parse_name >> implode) ^^ $$ ";" >> decode;
val regular = Scan.one Symbol.is_regular;
fun regular_except x = Scan.one (fn c => Symbol.is_regular c andalso c <> x);

val parse_chars = Scan.repeat1 (special || regular_except "<") >> implode;

val parse_cdata =
  Scan.this_string "<![CDATA[" |--
  (Scan.repeat (Scan.unless (Scan.this_string "]]>") regular) >> implode) --|
  Scan.this_string "]]>";

val parse_att =
  ((parse_name >> implode) --| (blanks -- $$ "=" -- blanks)) --
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
  ($$ "<" |-- parse_name -- Scan.repeat (blanks |-- parse_att) --| blanks :--
    (fn (name, _) =>
      !! (err (fn () => "Expected > or />"))
       ($$ "/" -- $$ ">" >> ignored ||
        $$ ">" |-- parse_content --|
          !! (err (fn () => "Expected </" ^ implode name ^ ">"))
              ($$ "<" -- $$ "/" -- Scan.this name -- blanks -- $$ ">")))
    >> (fn ((name, atts), body) => Elem ((implode name, atts), body))) xs;

val parse_document =
  (Scan.repeat parse_misc -- Scan.option parse_doctype -- Scan.repeat parse_misc)
  |-- parse_element;

fun parse s =
  (case Scan.finite Symbol.stopper (Scan.error (!! (err (fn () => "Malformed element"))
      (blanks |-- parse_document --| blanks))) (raw_explode s) of
    (x, []) => x
  | (_, ys) => error ("XML parsing error: unprocessed input\n" ^ Symbol.beginning 100 ys));

end;



(** XML as data representation language **)

exception XML_ATOM of string;
exception XML_BODY of tree list;


structure Encode =
struct

type 'a A = 'a -> string;
type 'a T = 'a -> body;
type 'a V = 'a -> string list * body;


(* atomic values *)

fun int_atom i = signed_string_of_int i;

fun bool_atom false = "0"
  | bool_atom true = "1";

fun unit_atom () = "";


(* structural nodes *)

fun node ts = Elem ((":", []), ts);

fun vector xs = map_index (fn (i, x) => (int_atom i, x)) xs;

fun tagged (tag, (xs, ts)) = Elem ((int_atom tag, vector xs), ts);


(* representation of standard types *)

fun properties props = [Elem ((":", props), [])];

fun string "" = []
  | string s = [Text s];

val int = string o int_atom;

val bool = string o bool_atom;

val unit = string o unit_atom;

fun pair f g (x, y) = [node (f x), node (g y)];

fun triple f g h (x, y, z) = [node (f x), node (g y), node (h z)];

fun list f xs = map (node o f) xs;

fun option _ NONE = []
  | option f (SOME x) = [node (f x)];

fun variant fs x =
  [tagged (the (get_index (fn f => SOME (f x) handle General.Match => NONE) fs))];

end;


structure Decode =
struct

type 'a A = string -> 'a;
type 'a T = body -> 'a;
type 'a V = string list * body -> 'a;


(* atomic values *)

fun int_atom s =
  Markup.parse_int s
    handle Fail _ => raise XML_ATOM s;

fun bool_atom "0" = false
  | bool_atom "1" = true
  | bool_atom s = raise XML_ATOM s;

fun unit_atom "" = ()
  | unit_atom s = raise XML_ATOM s;


(* structural nodes *)

fun node (Elem ((":", []), ts)) = ts
  | node t = raise XML_BODY [t];

fun vector atts =
  map_index (fn (i, (a, x)) => if int_atom a = i then x else raise XML_ATOM a) atts;

fun tagged (Elem ((name, atts), ts)) = (int_atom name, (vector atts, ts))
  | tagged t = raise XML_BODY [t];


(* representation of standard types *)

fun properties [Elem ((":", props), [])] = props
  | properties ts = raise XML_BODY ts;

fun string [] = ""
  | string [Text s] = s
  | string ts = raise XML_BODY ts;

val int = int_atom o string;

val bool = bool_atom o string;

val unit = unit_atom o string;

fun pair f g [t1, t2] = (f (node t1), g (node t2))
  | pair _ _ ts = raise XML_BODY ts;

fun triple f g h [t1, t2, t3] = (f (node t1), g (node t2), h (node t3))
  | triple _ _ _ ts = raise XML_BODY ts;

fun list f ts = map (f o node) ts;

fun option _ [] = NONE
  | option f [t] = SOME (f (node t))
  | option _ ts = raise XML_BODY ts;

fun variant fs [t] =
      let
        val (tag, (xs, ts)) = tagged t;
        val f = nth fs tag handle General.Subscript => raise XML_BODY [t];
      in f (xs, ts) end
  | variant _ ts = raise XML_BODY ts;

end;

end;
