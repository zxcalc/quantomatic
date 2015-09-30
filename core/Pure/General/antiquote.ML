(*  Title:      Pure/General/antiquote.ML
    Author:     Makarius

Antiquotations within plain text.
*)

signature ANTIQUOTE =
sig
  type antiq = Symbol_Pos.T list * {start: Position.T, stop: Position.T, range: Position.range}
  datatype 'a antiquote = Text of 'a | Antiq of antiq
  val antiq_reports: antiq -> Position.report list
  val antiquote_reports: ('a -> Position.report_text list) ->
    'a antiquote list -> Position.report_text list
  val scan_antiq: Symbol_Pos.T list -> antiq * Symbol_Pos.T list
  val scan_antiquote: Symbol_Pos.T list -> Symbol_Pos.T list antiquote * Symbol_Pos.T list
  val read: Input.source -> Symbol_Pos.T list antiquote list
end;

structure Antiquote: ANTIQUOTE =
struct

(* datatype antiquote *)

type antiq = Symbol_Pos.T list * {start: Position.T, stop: Position.T, range: Position.range};
datatype 'a antiquote = Text of 'a | Antiq of antiq;


(* reports *)

fun antiq_reports ((_, {start, stop, range = (pos, _)}): antiq) =
  [(start, Markup.antiquote), (stop, Markup.antiquote),
   (pos, Markup.antiquoted), (pos, Markup.language_antiquotation)];

fun antiquote_reports text =
  maps (fn Text x => text x | Antiq antiq => map (rpair "") (antiq_reports antiq));


(* scan *)

open Basic_Symbol_Pos;

local

val err_prefix = "Antiquotation lexical error: ";

val scan_txt =
  Scan.repeat1 ($$$ "@" --| Scan.ahead (~$$ "{") ||
    Scan.many1 (fn (s, _) => s <> "@" andalso Symbol.not_eof s)) >> flat;

val scan_antiq_body =
  Scan.trace (Symbol_Pos.scan_string_qq err_prefix || Symbol_Pos.scan_string_bq err_prefix) >> #2 ||
  Scan.trace (Symbol_Pos.scan_cartouche err_prefix) >> #2 ||
  Scan.one (fn (s, _) => s <> "}" andalso Symbol.not_eof s) >> single;

in

val scan_antiq =
  Symbol_Pos.scan_pos -- ($$ "@" |-- $$ "{" |-- Symbol_Pos.scan_pos --
    Symbol_Pos.!!! (fn () => err_prefix ^ "missing closing brace")
      (Scan.repeat scan_antiq_body -- Symbol_Pos.scan_pos -- ($$ "}" |-- Symbol_Pos.scan_pos)))
  >> (fn (pos1, (pos2, ((body, pos3), pos4))) =>
      (flat body,
        {start = Position.set_range (pos1, pos2),
         stop = Position.set_range (pos3, pos4),
         range = Position.range pos1 pos4}));

val scan_antiquote = scan_antiq >> Antiq || scan_txt >> Text;

end;


(* read *)

fun read source =
  (case Scan.read Symbol_Pos.stopper (Scan.repeat scan_antiquote) (Input.source_explode source) of
    SOME xs => (Position.reports_text (antiquote_reports (K []) xs); xs)
  | NONE =>
      error ("Malformed quotation/antiquotation source" ^ Position.here (Input.pos_of source)));

end;
