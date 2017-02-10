(*  Title:      Pure/Thy/markdown.ML
    Author:     Makarius

Minimal support for Markdown documents (see also http://commonmark.org)
that consist only of paragraphs and (nested) lists:

  * list items start with marker \<^item> (itemize), \<^enum> (enumerate), \<^descr> (description)
  * adjacent list items with same indentation and same marker are grouped
    into a single list
  * singleton blank lines separate paragraphs
  * multiple blank lines escape from the current list hierarchy

Notable differences to official Markdown:

  * indentation of list items needs to match exactly
  * indentation is unlimited (Markdown interprets 4 spaces as block quote)
  * list items always consist of paragraphs -- no notion of "tight" list
*)

signature MARKDOWN =
sig
  datatype kind = Itemize | Enumerate | Description
  val print_kind: kind -> string
  val is_control: Symbol.symbol -> bool
  type line
  val line_source: line -> Antiquote.text_antiquote list
  val line_is_item: line -> bool
  val line_content: line -> Antiquote.text_antiquote list
  val make_line: Antiquote.text_antiquote list -> line
  val empty_line: line
  datatype block = Par of line list | List of {indent: int, kind: kind, body: block list}
  val read_lines: line list -> block list
  val read_antiquotes: Antiquote.text_antiquote list -> block list
  val read_source: Input.source -> block list
  val text_reports: Antiquote.text_antiquote list -> Position.report list
  val reports: block list -> Position.report list
end;

structure Markdown: MARKDOWN =
struct

(* item kinds *)

datatype kind = Itemize | Enumerate | Description;

fun print_kind Itemize = "itemize"
  | print_kind Enumerate = "enumerate"
  | print_kind Description = "description";

val kinds = [("item", Itemize), ("enum", Enumerate), ("descr", Description)];

val is_control = member (op =) ["\<^item>", "\<^enum>", "\<^descr>"];


(* document lines *)

datatype line =
  Line of
   {source: Antiquote.text_antiquote list,
    is_empty: bool,
    indent: int,
    item: kind option,
    item_pos: Position.T,
    content: Antiquote.text_antiquote list};

val eof_line =
  Line {source = [Antiquote.Text [(Symbol.eof, Position.none)]],
    is_empty = false, indent = 0, item = NONE, item_pos = Position.none, content = []};

fun line_source (Line {source, ...}) = source;
fun line_is_empty (Line {is_empty, ...}) = is_empty;
fun line_is_item (Line {item, ...}) = is_some item;
fun line_content (Line {content, ...}) = content;


(* make line *)

local

fun bad_blank ((s, _): Symbol_Pos.T) = Symbol.is_ascii_blank s andalso s <> Symbol.space;
val bad_blanks = maps (fn Antiquote.Text ss => filter bad_blank ss | _ => []);

fun check_blanks source =
  (case bad_blanks source of
    [] => ()
  | (c, pos) :: _ =>
      error ("Bad blank character " ^ quote (ML_Syntax.print_char c) ^ Position.here pos));

val is_space = Symbol.is_space o Symbol_Pos.symbol;
val is_empty = forall (fn Antiquote.Text ss => forall is_space ss | _ => false);

fun strip_spaces (Antiquote.Text ss :: rest) =
      let val (sp, ss') = take_prefix is_space ss
      in (length sp, if null ss' then rest else Antiquote.Text ss' :: rest) end
  | strip_spaces source = (0, source);

fun read_marker source =
  let val (indent, source') = strip_spaces source in
    (case source' of
      (control as Antiquote.Control {name = (name, pos), body = [], ...}) :: rest =>
        let
          val item = AList.lookup (op =) kinds name;
          val item_pos = if is_some item then pos else Position.none;
          val (_, rest') = strip_spaces (if is_some item then rest else control :: rest);
        in ((indent, item, item_pos), rest') end
    | _ => ((indent, NONE, Position.none), source'))
  end;

in

fun make_line source =
  let
    val _ = check_blanks source;
    val ((indent, item, item_pos), content) = read_marker source;
  in
    Line {source = source, is_empty = is_empty source, indent = indent,
      item = item, item_pos = item_pos, content = content}
  end;

val empty_line = make_line [];

end;


(* document blocks *)

datatype block = Par of line list | List of {indent: int, kind: kind, body: block list};

fun block_lines (Par lines) = lines
  | block_lines (List {body, ...}) = maps block_lines body;

fun block_range (Par lines) = Antiquote.range (maps line_content lines)
  | block_range (List {body, ...}) = Antiquote.range (maps line_source (maps block_lines body));

fun block_indent (List {indent, ...}) = indent
  | block_indent (Par (Line {indent, ...} :: _)) = indent
  | block_indent _ = 0;

fun block_list indent0 kind0 (List {indent, kind, body}) =
      if indent0 = indent andalso kind0 = kind then SOME body else NONE
  | block_list _ _ _ = NONE;

val is_list = fn List _ => true | _ => false;


(* read document *)

local

fun build (indent, item, rev_body) document =
  (case (item, document) of
    (SOME kind, block :: blocks) =>
      (case block_list indent kind block of
        SOME list => List {indent = indent, kind = kind, body = fold cons rev_body list} :: blocks
      | NONE =>
          if (if is_list block then indent < block_indent block else indent <= block_indent block)
          then build (indent, item, block :: rev_body) blocks
          else List {indent = indent, kind = kind, body = rev rev_body} :: document)
  | (SOME kind, []) => [List {indent = indent, kind = kind, body = rev rev_body}]
  | (NONE, _) => fold cons rev_body document);

fun plain_line (line as Line {is_empty, item, ...}) =
  not is_empty andalso is_none item andalso line <> eof_line;

val parse_paragraph =
  Scan.one (fn line => line <> eof_line) -- Scan.many plain_line >> (fn (line, lines) =>
    let
      val Line {indent, item, ...} = line;
      val block = Par (line :: lines);
    in (indent, item, [block]) end);

val parse_document =
  parse_paragraph ::: Scan.repeat (Scan.option (Scan.one line_is_empty) |-- parse_paragraph)
    >> (fn pars => fold_rev build pars []);

in

val read_lines =
  Scan.read (Scan.stopper (K eof_line) (fn line => line = eof_line))
    (Scan.repeat (Scan.many line_is_empty |-- parse_document) --| Scan.many line_is_empty) #>
  the_default [] #> flat;

val read_antiquotes = Antiquote.split_lines #> map make_line #> read_lines;
val read_source = Antiquote.read #> read_antiquotes;

end;


(* PIDE reports *)

val text_reports =
  maps (fn Antiquote.Text ss => [(#1 (Symbol_Pos.range ss), Markup.words)] | _ => []);

local

fun line_reports depth (Line {item_pos, content, ...}) =
  cons (item_pos, Markup.markdown_item depth) #> append (text_reports content);

fun block_reports depth block =
  (case block of
    Par lines =>
      cons (#1 (block_range block), Markup.markdown_paragraph) #>
      fold (line_reports depth) lines
  | List {kind, body, ...} =>
      cons (#1 (block_range block), Markup.markdown_list (print_kind kind)) #>
      fold (block_reports (depth + 1)) body);

in

fun reports blocks =
  filter (Position.is_reported o #1) (fold (block_reports 0) blocks []);

end;

end;
