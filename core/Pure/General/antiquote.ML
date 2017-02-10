(*  Title:      Pure/General/antiquote.ML
    Author:     Makarius

Antiquotations within plain text.
*)

signature ANTIQUOTE =
sig
  type control = {range: Position.range, name: string * Position.T, body: Symbol_Pos.T list}
  type antiq = {start: Position.T, stop: Position.T, range: Position.range, body: Symbol_Pos.T list}
  datatype 'a antiquote = Text of 'a | Control of control | Antiq of antiq
  type text_antiquote = Symbol_Pos.T list antiquote
  val range: text_antiquote list -> Position.range
  val split_lines: text_antiquote list -> text_antiquote list list
  val antiq_reports: 'a antiquote list -> Position.report list
  val scan_control: Symbol_Pos.T list -> control * Symbol_Pos.T list
  val scan_antiq: Symbol_Pos.T list -> antiq * Symbol_Pos.T list
  val scan_antiquote: Symbol_Pos.T list -> text_antiquote * Symbol_Pos.T list
  val parse: Position.T -> Symbol_Pos.T list -> text_antiquote list
  val read: Input.source -> text_antiquote list
end;

structure Antiquote: ANTIQUOTE =
struct

(* datatype antiquote *)

type control = {range: Position.range, name: string * Position.T, body: Symbol_Pos.T list};
type antiq = {start: Position.T, stop: Position.T, range: Position.range, body: Symbol_Pos.T list};
datatype 'a antiquote = Text of 'a | Control of control | Antiq of antiq;

type text_antiquote = Symbol_Pos.T list antiquote;

fun antiquote_range (Text ss) = Symbol_Pos.range ss
  | antiquote_range (Control {range, ...}) = range
  | antiquote_range (Antiq {range, ...}) = range;

fun range ants =
  if null ants then Position.no_range
  else Position.range (#1 (antiquote_range (hd ants)), #2 (antiquote_range (List.last ants)));


(* split lines *)

fun split_lines input =
  let
    fun add a (line, lines) = (a :: line, lines);
    fun flush (line, lines) = ([], rev line :: lines);
    fun split (a as Text ss) =
          (case take_prefix (fn ("\n", _) => false | _ => true) ss of
            ([], []) => I
          | (_, []) => add a
          | ([], _ :: rest) => flush #> split (Text rest)
          | (prefix, _ :: rest) => add (Text prefix) #> flush #> split (Text rest))
      | split a = add a;
  in if null input then [] else rev (#2 (flush (fold split input ([], [])))) end;


(* reports *)

fun antiq_reports ants = ants |> maps
  (fn Text _ => []
    | Control {range = (pos, _), ...} => [(pos, Markup.antiquoted)]
    | Antiq {start, stop, range = (pos, _), ...} =>
        [(start, Markup.antiquote),
         (stop, Markup.antiquote),
         (pos, Markup.antiquoted),
         (pos, Markup.language_antiquotation)]);


(* scan *)

open Basic_Symbol_Pos;

local

val err_prefix = "Antiquotation lexical error: ";

val scan_txt =
  Scan.repeats1
   (Scan.many1 (fn (s, _) =>
      not (Symbol.is_control s) andalso s <> Symbol.open_ andalso s <> "@" andalso Symbol.not_eof s) ||
    $$$ "@" --| Scan.ahead (~$$ "{"));

val scan_antiq_body =
  Scan.trace (Symbol_Pos.scan_string_qq err_prefix || Symbol_Pos.scan_string_bq err_prefix) >> #2 ||
  Symbol_Pos.scan_cartouche err_prefix ||
  Scan.one (fn (s, _) => s <> "}" andalso Symbol.not_eof s) >> single;

fun control_name sym = (case Symbol.decode sym of Symbol.Control name => name);

in

val scan_control =
  Scan.option (Scan.one (Symbol.is_control o Symbol_Pos.symbol)) --
  Symbol_Pos.scan_cartouche err_prefix >>
    (fn (opt_control, body) =>
      let
        val (name, range) =
          (case opt_control of
            SOME (sym, pos) => ((control_name sym, pos), Symbol_Pos.range ((sym, pos) :: body))
          | NONE => (("cartouche", #2 (hd body)), Symbol_Pos.range body));
      in {name = name, range = range, body = body} end) ||
  Scan.one (Symbol.is_control o Symbol_Pos.symbol) >>
    (fn (sym, pos) =>
      {name = (control_name sym, pos), range = Symbol_Pos.range [(sym, pos)], body = []});

val scan_antiq =
  Symbol_Pos.scan_pos -- ($$ "@" |-- $$ "{" |-- Symbol_Pos.scan_pos --
    Symbol_Pos.!!! (fn () => err_prefix ^ "missing closing brace")
      (Scan.repeats scan_antiq_body -- Symbol_Pos.scan_pos -- ($$ "}" |-- Symbol_Pos.scan_pos))) >>
    (fn (pos1, (pos2, ((body, pos3), pos4))) =>
      {start = Position.range_position (pos1, pos2),
       stop = Position.range_position (pos3, pos4),
       range = Position.range (pos1, pos4),
       body = body});

val scan_antiquote =
  scan_txt >> Text || scan_control >> Control || scan_antiq >> Antiq;

end;


(* read *)

fun parse pos syms =
  (case Scan.read Symbol_Pos.stopper (Scan.repeat scan_antiquote) syms of
    SOME ants => ants
  | NONE => error ("Malformed quotation/antiquotation source" ^ Position.here pos));

fun read source =
  let
    val ants = parse (Input.pos_of source) (Input.source_explode source);
    val _ = Position.reports (antiq_reports ants);
  in ants end;

end;
