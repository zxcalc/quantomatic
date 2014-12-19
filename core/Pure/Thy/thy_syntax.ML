(*  Title:      Pure/Thy/thy_syntax.ML
    Author:     Makarius

Superficial theory syntax: tokens and spans.
*)

signature THY_SYNTAX =
sig
  val parse_tokens: Scan.lexicon * Scan.lexicon -> Position.T -> string -> Token.T list
  val reports_of_tokens: Token.T list -> bool * Position.report_text list
  val present_token: Token.T -> Output.output
  datatype span_kind = Command of string * Position.T | Ignored | Malformed
  datatype span = Span of span_kind * Token.T list
  val span_kind: span -> span_kind
  val span_content: span -> Token.T list
  val present_span: span -> Output.output
  val parse_spans: Token.T list -> span list
  val resolve_files: (string -> Path.T * Position.T -> Token.file Exn.result list) -> span -> span
  datatype 'a element = Element of 'a * ('a element list * 'a) option
  val atom: 'a -> 'a element
  val map_element: ('a -> 'b) -> 'a element -> 'b element
  val flat_element: 'a element -> 'a list
  val last_element: 'a element -> 'a
  val parse_elements: span list -> span element list
end;

structure Thy_Syntax: THY_SYNTAX =
struct

(** tokens **)

(* parse *)

fun parse_tokens lexs pos =
  Source.of_string #>
  Symbol.source #>
  Token.source {do_recover = SOME false} (K lexs) pos #>
  Source.exhaust;


(* present *)

local

fun reports_of_token tok =
  let
    val {text, pos, ...} = Token.source_position_of tok;
    val malformed_symbols =
      Symbol_Pos.explode (text, pos)
      |> map_filter (fn (sym, pos) =>
          if Symbol.is_malformed sym
          then SOME ((pos, Markup.bad), "Malformed symbolic character") else NONE);
    val is_malformed = Token.is_error tok orelse not (null malformed_symbols);
    val reports = Token.report tok :: Token.completion_report tok @ malformed_symbols;
  in (is_malformed, reports) end;

in

fun reports_of_tokens toks =
  let val results = map reports_of_token toks
  in (exists fst results, maps snd results) end;

fun present_token tok =
  Markup.enclose (Token.markup tok) (Output.output (Token.unparse tok));

end;



(** spans **)

(* type span *)

datatype span_kind = Command of string * Position.T | Ignored | Malformed;
datatype span = Span of span_kind * Token.T list;

fun span_kind (Span (k, _)) = k;
fun span_content (Span (_, toks)) = toks;

val present_span = implode o map present_token o span_content;


(* parse *)

local

fun make_span toks =
  if not (null toks) andalso Token.is_command (hd toks) then
    Span (Command (Token.content_of (hd toks), Token.pos_of (hd toks)), toks)
  else if forall Token.is_improper toks then Span (Ignored, toks)
  else Span (Malformed, toks);

fun flush (result, span, improper) =
  result
  |> not (null span) ? cons (rev span)
  |> not (null improper) ? cons (rev improper);

fun parse tok (result, span, improper) =
  if Token.is_command tok then (flush (result, span, improper), [tok], [])
  else if Token.is_improper tok then (result, span, tok :: improper)
  else (result, tok :: (improper @ span), []);

in

fun parse_spans toks =
  fold parse toks ([], [], [])
  |> flush |> rev |> map make_span;

end;


(* inlined files *)

local

fun clean ((i1, t1) :: (i2, t2) :: toks) =
      if Token.keyword_with (fn s => s = "%" orelse s = "--") t1 then clean toks
      else (i1, t1) :: clean ((i2, t2) :: toks)
  | clean toks = toks;

fun clean_tokens toks =
  ((0 upto length toks - 1) ~~ toks)
  |> filter (fn (_, tok) => Token.is_proper tok)
  |> clean;

fun find_file ((_, tok) :: toks) =
      if Token.is_command tok then
        toks |> get_first (fn (i, tok) =>
          if Token.is_name tok then
            SOME (i, (Path.explode (Token.content_of tok), Token.pos_of tok))
              handle ERROR msg => error (msg ^ Position.here (Token.pos_of tok))
          else NONE)
      else NONE
  | find_file [] = NONE;

in

fun resolve_files read_files span =
  (case span of
    Span (Command (cmd, pos), toks) =>
      if Keyword.is_theory_load cmd then
        (case find_file (clean_tokens toks) of
          NONE => error ("Bad file argument of command " ^ quote cmd ^ Position.here pos)
        | SOME (i, path) =>
            let
              val toks' = toks |> map_index (fn (j, tok) =>
                if i = j then Token.put_files (read_files cmd path) tok
                else tok);
            in Span (Command (cmd, pos), toks') end)
      else span
  | _ => span);

end;



(** specification elements: commands with optional proof **)

datatype 'a element = Element of 'a * ('a element list * 'a) option;

fun element (a, b) = Element (a, SOME b);
fun atom a = Element (a, NONE);

fun map_element f (Element (a, NONE)) = Element (f a, NONE)
  | map_element f (Element (a, SOME (elems, b))) =
      Element (f a, SOME ((map o map_element) f elems, f b));

fun flat_element (Element (a, NONE)) = [a]
  | flat_element (Element (a, SOME (elems, b))) = a :: maps flat_element elems @ [b];

fun last_element (Element (a, NONE)) = a
  | last_element (Element (_, SOME (_, b))) = b;


(* scanning spans *)

val eof = Span (Command ("", Position.none), []);

fun is_eof (Span (Command ("", _), _)) = true
  | is_eof _ = false;

val not_eof = not o is_eof;

val stopper = Scan.stopper (K eof) is_eof;


(* parse *)

local

fun command_with pred =
  Scan.one (fn (Span (Command (name, _), _)) => pred name | _ => false);

val proof_atom =
  Scan.one (fn (Span (Command (name, _), _)) => Keyword.is_proof_body name | _ => true) >> atom;

fun proof_element x = (command_with Keyword.is_proof_goal -- proof_rest >> element || proof_atom) x
and proof_rest x = (Scan.repeat proof_element -- command_with Keyword.is_qed) x;

val other_element =
  command_with Keyword.is_theory_goal -- proof_rest >> element ||
  Scan.one not_eof >> atom;

in

val parse_elements =
  Source.of_list #>
  Source.source stopper (Scan.bulk other_element) NONE #>
  Source.exhaust;

end;

end;
