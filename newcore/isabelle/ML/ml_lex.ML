(*  Title:      Pure/ML/ml_lex.ML
    Author:     Makarius

Lexical syntax for SML.
*)

signature ML_LEX =
sig
  datatype token_kind =
    Keyword | Ident | LongIdent | TypeVar | Word | Int | Real | Char | String |
    Space | Comment | Error of string | EOF
  eqtype token
  val stopper: token Scan.stopper
  val is_regular: token -> bool
  val is_improper: token -> bool
  val set_range: Position.range -> token -> token
  val pos_of: token -> Position.T
  val end_pos_of: token -> Position.T
  val kind_of: token -> token_kind
  val content_of: token -> string
  val check_content_of: token -> string
  val flatten: token list -> string
  val keywords: string list
  val source: (Symbol.symbol, 'a) Source.source ->
    (token, (Symbol_Pos.T, Position.T * (Symbol.symbol, 'a) Source.source)
      Source.source) Source.source
  val tokenize: string -> token list
  val read: Position.T -> Symbol_Pos.text -> token Antiquote.antiquote list
end;

structure ML_Lex: ML_LEX =
struct

(** tokens **)

(* datatype token *)

datatype token_kind =
  Keyword | Ident | LongIdent | TypeVar | Word | Int | Real | Char | String |
  Space | Comment | Error of string | EOF;

datatype token = Token of Position.range * (token_kind * string);


(* position *)

fun set_range range (Token (_, x)) = Token (range, x);

fun pos_of (Token ((pos, _), _)) = pos;
fun end_pos_of (Token ((_, pos), _)) = pos;


(* control tokens *)

fun mk_eof pos = Token ((pos, Position.none), (EOF, ""));
val eof = mk_eof Position.none;

fun is_eof (Token (_, (EOF, _))) = true
  | is_eof _ = false;

val stopper =
  Scan.stopper (fn [] => eof | toks => mk_eof (end_pos_of (List.last toks))) is_eof;


(* token content *)

fun kind_of (Token (_, (k, _))) = k;

fun content_of (Token (_, (_, x))) = x;
fun token_leq (tok, tok') = content_of tok <= content_of tok';

fun is_regular (Token (_, (Error _, _))) = false
  | is_regular (Token (_, (EOF, _))) = false
  | is_regular _ = true;

fun is_improper (Token (_, (Space, _))) = true
  | is_improper (Token (_, (Comment, _))) = true
  | is_improper _ = false;

fun warn tok =
  (case tok of
    Token (_, (Keyword, ":>")) =>
      warning ("Opaque signature matching (:>) fails to work with ML pretty printing --\n\
        \prefer non-opaque matching (:) possibly with abstype" ^
        Position.here (pos_of tok))
  | _ => ());

fun check_content_of tok =
  (case kind_of tok of
    Error msg => error msg
  | _ => content_of tok);

fun flatten_content (tok :: (toks as tok' :: _)) =
      Symbol.escape (check_content_of tok) ::
        (if is_improper tok orelse is_improper tok' then flatten_content toks
         else Symbol.space :: flatten_content toks)
  | flatten_content toks = map (Symbol.escape o check_content_of) toks;

val flatten = implode o flatten_content;


(* markup *)

local

val token_kind_markup =
 fn Keyword   => (Markup.ML_keyword, "")
  | Ident     => (Markup.empty, "")
  | LongIdent => (Markup.empty, "")
  | TypeVar   => (Markup.ML_tvar, "")
  | Word      => (Markup.ML_numeral, "")
  | Int       => (Markup.ML_numeral, "")
  | Real      => (Markup.ML_numeral, "")
  | Char      => (Markup.ML_char, "")
  | String    => (Markup.ML_string, "")
  | Space     => (Markup.empty, "")
  | Comment   => (Markup.ML_comment, "")
  | Error msg => (Markup.bad, msg)
  | EOF       => (Markup.empty, "");

fun token_markup kind x =
  if kind = Keyword andalso exists_string (not o Symbol.is_ascii_letter) x
  then (Markup.ML_delimiter, "")
  else token_kind_markup kind;

in

fun report_of_token (Token ((pos, _), (kind, x))) =
  let val (markup, txt) = token_markup kind x
  in ((pos, markup), txt) end;

end;



(** scanners **)

open Basic_Symbol_Pos;

fun !!! msg = Symbol_Pos.!!! (fn () => "SML lexical error: " ^ msg);


(* blanks *)

val scan_blank = Scan.one (Symbol.is_ascii_blank o Symbol_Pos.symbol);
val scan_blanks1 = Scan.repeat1 scan_blank;


(* keywords *)

val keywords = ["#", "(", ")", ",", "->", "...", ":", ":>", ";", "=",
  "=>", "[", "]", "_", "{", "|", "}", "abstype", "and", "andalso", "as",
  "case", "datatype", "do", "else", "end", "eqtype", "exception", "fn",
  "fun", "functor", "handle", "if", "in", "include", "infix", "infixr",
  "let", "local", "nonfix", "of", "op", "open", "orelse", "raise", "rec",
  "sharing", "sig", "signature", "struct", "structure", "then", "type",
  "val", "where", "while", "with", "withtype"];

val lex = Scan.make_lexicon (map raw_explode keywords);
fun scan_keyword x = Scan.literal lex x;


(* identifiers *)

local

val scan_letdigs =
  Scan.many
    ((Symbol.is_ascii_letter orf Symbol.is_ascii_digit orf Symbol.is_ascii_quasi) o
      Symbol_Pos.symbol);

val scan_alphanumeric =
  Scan.one (Symbol.is_ascii_letter o Symbol_Pos.symbol) -- scan_letdigs >> op ::;

val scan_symbolic =
  Scan.many1 (member (op =) (raw_explode "!#$%&*+-/:<=>?@\\^`|~") o Symbol_Pos.symbol);

in

val scan_ident = scan_alphanumeric || scan_symbolic;

val scan_longident =
  (Scan.repeat1 (scan_alphanumeric @@@ $$$ ".") >> flat) @@@ (scan_ident || $$$ "=");

val scan_typevar = $$$ "'" @@@ scan_letdigs;

end;


(* numerals *)

local

val scan_dec = Scan.many1 (Symbol.is_ascii_digit o Symbol_Pos.symbol);
val scan_hex = Scan.many1 (Symbol.is_ascii_hex o Symbol_Pos.symbol);
val scan_sign = Scan.optional ($$$ "~") [];
val scan_decint = scan_sign @@@ scan_dec;

in

val scan_word =
  $$$ "0" @@@ $$$ "w" @@@ $$$ "x" @@@ scan_hex ||
  $$$ "0" @@@ $$$ "w" @@@ scan_dec;

val scan_int = scan_sign @@@ ($$$ "0" @@@ $$$ "x" @@@ scan_hex || scan_dec);

val scan_exp = ($$$ "E" || $$$ "e") @@@ scan_decint;

val scan_real =
  scan_decint @@@ $$$ "." @@@ scan_dec @@@ Scan.optional scan_exp [] ||
  scan_decint @@@ scan_exp;

end;


(* chars and strings *)

local

val scan_escape =
  Scan.one (member (op =) (raw_explode "\"\\abtnvfr") o Symbol_Pos.symbol) >> single ||
  $$$ "^" @@@ (Scan.one (fn (s, _) => ord "@" <= ord s andalso ord s <= ord "_") >> single) ||
  Scan.one (Symbol.is_ascii_digit o Symbol_Pos.symbol) --
    Scan.one (Symbol.is_ascii_digit o Symbol_Pos.symbol) --
    Scan.one (Symbol.is_ascii_digit o Symbol_Pos.symbol) >> (fn ((a, b), c) => [a, b, c]);

val scan_str =
  Scan.one (fn (s, _) => Symbol.is_regular s andalso s <> "\"" andalso s <> "\\" andalso
    (not (Symbol.is_char s) orelse Symbol.is_printable s)) >> single ||
  $$$ "\\" @@@ !!! "bad escape character in string" scan_escape;

val scan_gap = $$$ "\\" @@@ scan_blanks1 @@@ $$$ "\\";
val scan_gaps = Scan.repeat scan_gap >> flat;

in

val scan_char =
  $$$ "#" @@@ $$$ "\"" @@@ scan_gaps @@@ scan_str @@@ scan_gaps @@@ $$$ "\"";

val recover_char =
  $$$ "#" @@@ $$$ "\"" @@@ scan_gaps @@@ Scan.optional (Scan.permissive scan_str @@@ scan_gaps) [];

val scan_string =
  $$$ "\"" @@@ !!! "missing quote at end of string"
    ((Scan.repeat (scan_gap || scan_str) >> flat) @@@ $$$ "\"");

val recover_string =
  $$$ "\"" @@@ (Scan.repeat (scan_gap || Scan.permissive scan_str) >> flat);

end;


(* scan tokens *)

local

fun token k ss = Token (Symbol_Pos.range ss, (k, Symbol_Pos.content ss));

val scan_ml =
 (scan_char >> token Char ||
  scan_string >> token String ||
  scan_blanks1 >> token Space ||
  Symbol_Pos.scan_comment !!! >> token Comment ||
  Scan.max token_leq
   (scan_keyword >> token Keyword)
   (scan_word >> token Word ||
    scan_real >> token Real ||
    scan_int >> token Int ||
    scan_longident >> token LongIdent ||
    scan_ident >> token Ident ||
    scan_typevar >> token TypeVar));

val scan_antiq = Antiquote.scan || scan_ml >> Antiquote.Text;

fun recover msg =
 (recover_char ||
  recover_string ||
  Symbol_Pos.recover_comment ||
  Scan.one (Symbol.not_eof o Symbol_Pos.symbol) >> single)
  >> (single o token (Error msg));

in

fun source src =
  Symbol_Pos.source (Position.line 1) src
  |> Source.source Symbol_Pos.stopper (Scan.bulk (!!! "bad input" scan_ml)) (SOME (false, recover));

val tokenize = Source.of_string #> Symbol.source #> source #> Source.exhaust;

fun read pos txt =
  let
    val _ = Position.report pos Markup.ML_source;
    val syms = Symbol_Pos.explode (txt, pos);
    val termination =
      if null syms then []
      else
        let
          val pos1 = List.last syms |-> Position.advance;
          val pos2 = Position.advance Symbol.space pos1;
        in [Antiquote.Text (Token (Position.range pos1 pos2, (Space, Symbol.space)))] end;
    val input =
      (Source.of_list syms
        |> Source.source Symbol_Pos.stopper (Scan.bulk (!!! "bad input" scan_antiq))
          (SOME (false, fn msg => recover msg >> map Antiquote.Text))
        |> Source.exhaust
        |> tap (Position.reports_text o Antiquote.reports_of (single o report_of_token))
        |> tap Antiquote.check_nesting
        |> tap (List.app (fn Antiquote.Text tok => (check_content_of tok; warn tok) | _ => ())))
      handle ERROR msg =>
        cat_error msg ("The error(s) above occurred in ML source" ^ Position.here pos);
  in input @ termination end;

end;

end;

