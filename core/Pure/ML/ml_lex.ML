(*  Title:      Pure/ML/ml_lex.ML
    Author:     Makarius

Lexical syntax for Isabelle/ML and Standard ML.
*)

signature ML_LEX =
sig
  val keywords: string list
  datatype token_kind =
    Keyword | Ident | Long_Ident | Type_Var | Word | Int | Real | Char | String |
    Space | Cartouche | Comment | Error of string | EOF
  eqtype token
  val stopper: token Scan.stopper
  val is_cartouche: token -> bool
  val is_regular: token -> bool
  val is_improper: token -> bool
  val set_range: Position.range -> token -> token
  val range_of: token -> Position.range
  val pos_of: token -> Position.T
  val end_pos_of: token -> Position.T
  val kind_of: token -> token_kind
  val content_of: token -> string
  val check_content_of: token -> string
  val flatten: token list -> string
  val source: (Symbol.symbol, 'a) Source.source ->
    (token, (Symbol_Pos.T, Position.T * (Symbol.symbol, 'a) Source.source)
      Source.source) Source.source
  val tokenize: string -> token list
  val read_pos: Position.T -> Symbol_Pos.text -> token Antiquote.antiquote list
  val read: Symbol_Pos.text -> token Antiquote.antiquote list
  val read_set_range: Position.range -> Symbol_Pos.text -> token Antiquote.antiquote list
  val read_source: bool -> Input.source -> token Antiquote.antiquote list
end;

structure ML_Lex: ML_LEX =
struct

(** keywords **)

val keywords =
 ["#", "(", ")", ",", "->", "...", ":", ":>", ";", "=", "=>", "[",
  "]", "_", "{", "|", "}", "abstype", "and", "andalso", "as", "case",
  "datatype", "do", "else", "end", "eqtype", "exception", "fn", "fun",
  "functor", "handle", "if", "in", "include", "infix", "infixr",
  "let", "local", "nonfix", "of", "op", "open", "orelse", "raise",
  "rec", "sharing", "sig", "signature", "struct", "structure", "then",
  "type", "val", "where", "while", "with", "withtype"];

val keywords2 =
 ["and", "case", "do", "else", "end", "if", "in", "let", "local", "of",
  "sig", "struct", "then", "while", "with"];

val keywords3 =
 ["handle", "open", "raise"];

val lexicon = Scan.make_lexicon (map raw_explode keywords);



(** tokens **)

(* datatype token *)

datatype token_kind =
  Keyword | Ident | Long_Ident | Type_Var | Word | Int | Real | Char | String |
  Space | Cartouche | Comment | Error of string | EOF;

datatype token = Token of Position.range * (token_kind * string);


(* position *)

fun set_range range (Token (_, x)) = Token (range, x);
fun range_of (Token (range, _)) = range;

val pos_of = #1 o range_of;
val end_pos_of = #2 o range_of;


(* stopper *)

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

fun is_keyword (Token (_, (Keyword, _))) = true
  | is_keyword _ = false;

fun is_delimiter (Token (_, (Keyword, x))) = not (Symbol.is_ascii_identifier x)
  | is_delimiter _ = false;

fun is_cartouche (Token (_, (Cartouche, _))) = true
  | is_cartouche _ = false;

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


(* flatten *)

fun flatten_content (tok :: (toks as tok' :: _)) =
      Symbol.escape (check_content_of tok) ::
        (if is_improper tok orelse is_improper tok' then flatten_content toks
         else Symbol.space :: flatten_content toks)
  | flatten_content toks = map (Symbol.escape o check_content_of) toks;

val flatten = implode o flatten_content;


(* markup *)

local

fun token_kind_markup SML =
 fn Type_Var => (Markup.ML_tvar, "")
  | Word => (Markup.ML_numeral, "")
  | Int => (Markup.ML_numeral, "")
  | Real => (Markup.ML_numeral, "")
  | Char => (Markup.ML_char, "")
  | String => (if SML then Markup.SML_string else Markup.ML_string, "")
  | Cartouche => (Markup.ML_cartouche, "")
  | Comment => (if SML then Markup.SML_comment else Markup.ML_comment, "")
  | Error msg => (Markup.bad, msg)
  | _ => (Markup.empty, "");

in

fun report_of_token SML (tok as Token ((pos, _), (kind, x))) =
  let
    val (markup, txt) =
      if not (is_keyword tok) then token_kind_markup SML kind
      else if is_delimiter tok then (Markup.ML_delimiter, "")
      else if member (op =) keywords2 x then (Markup.ML_keyword2, "")
      else if member (op =) keywords3 x then (Markup.ML_keyword3, "")
      else (Markup.ML_keyword1, "");
  in ((pos, markup), txt) end;

end;



(** scanners **)

open Basic_Symbol_Pos;

val err_prefix = "SML lexical error: ";

fun !!! msg = Symbol_Pos.!!! (fn () => err_prefix ^ msg);


(* identifiers *)

local

val scan_letdigs =
  Scan.many (Symbol.is_ascii_letdig o Symbol_Pos.symbol);

val scan_alphanumeric =
  Scan.one (Symbol.is_ascii_letter o Symbol_Pos.symbol) -- scan_letdigs >> op ::;

val scan_symbolic =
  Scan.many1 (member (op =) (raw_explode "!#$%&*+-/:<=>?@\\^`|~") o Symbol_Pos.symbol);

in

val scan_ident = scan_alphanumeric || scan_symbolic;

val scan_long_ident =
  (Scan.repeat1 (scan_alphanumeric @@@ $$$ ".") >> flat) @@@ (scan_ident || $$$ "=");

val scan_type_var = $$$ "'" @@@ scan_letdigs;

end;


(* numerals *)

local

val scan_dec = Scan.many1 (Symbol.is_ascii_digit o Symbol_Pos.symbol);
val scan_hex = Scan.many1 (Symbol.is_ascii_hex o Symbol_Pos.symbol);
val scan_sign = Scan.optional ($$$ "~") [];
val scan_decint = scan_sign @@@ scan_dec;
val scan_exp = ($$$ "E" || $$$ "e") @@@ scan_decint;

in

val scan_word =
  $$$ "0" @@@ $$$ "w" @@@ $$$ "x" @@@ scan_hex ||
  $$$ "0" @@@ $$$ "w" @@@ scan_dec;

val scan_int = scan_sign @@@ ($$$ "0" @@@ $$$ "x" @@@ scan_hex || scan_dec);

val scan_real =
  scan_decint @@@ $$$ "." @@@ scan_dec @@@ Scan.optional scan_exp [] ||
  scan_decint @@@ scan_exp;

end;


(* chars and strings *)

val scan_blanks1 = Scan.many1 (Symbol.is_ascii_blank o Symbol_Pos.symbol);

local

val scan_escape =
  Scan.one (member (op =) (raw_explode "\"\\abtnvfr") o Symbol_Pos.symbol) >> single ||
  $$$ "^" @@@ (Scan.one (fn (s, _) => ord "@" <= ord s andalso ord s <= ord "_") >> single) ||
  Scan.one (Symbol.is_ascii_digit o Symbol_Pos.symbol) --
    Scan.one (Symbol.is_ascii_digit o Symbol_Pos.symbol) --
    Scan.one (Symbol.is_ascii_digit o Symbol_Pos.symbol) >> (fn ((a, b), c) => [a, b, c]);

val scan_str =
  Scan.one (fn (s, _) => Symbol.not_eof s andalso s <> "\"" andalso s <> "\\" andalso
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
  Scan.ahead ($$ "\"") |--
    !!! "unclosed string literal"
      ($$$ "\"" @@@ (Scan.repeat (scan_gap || scan_str) >> flat) @@@ $$$ "\"");

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
  Symbol_Pos.scan_comment err_prefix >> token Comment ||
  Scan.max token_leq
   (Scan.literal lexicon >> token Keyword)
   (scan_word >> token Word ||
    scan_real >> token Real ||
    scan_int >> token Int ||
    scan_long_ident >> token Long_Ident ||
    scan_ident >> token Ident ||
    scan_type_var >> token Type_Var));

val scan_sml = scan_ml >> Antiquote.Text;

val scan_ml_antiq =
  Antiquote.scan_antiq >> Antiquote.Antiq ||
  Symbol_Pos.scan_cartouche err_prefix >> (Antiquote.Text o token Cartouche) ||
  scan_ml >> Antiquote.Text;

fun recover msg =
 (recover_char ||
  recover_string ||
  Symbol_Pos.recover_cartouche ||
  Symbol_Pos.recover_comment ||
  Scan.one (Symbol.not_eof o Symbol_Pos.symbol) >> single)
  >> (single o token (Error msg));

fun gen_read SML pos text =
  let
    val syms =
      Symbol_Pos.explode (text, pos)
      |> SML ? maps (fn (s, p) => raw_explode s |> map (rpair p));

    val termination =
      if null syms then []
      else
        let
          val pos1 = List.last syms |-> Position.advance;
          val pos2 = Position.advance Symbol.space pos1;
        in [Antiquote.Text (Token (Position.range pos1 pos2, (Space, Symbol.space)))] end;

    val scan = if SML then scan_sml else scan_ml_antiq;
    fun check (Antiquote.Text tok) = (check_content_of tok; if SML then () else warn tok)
      | check _ = ();
    val input =
      Source.of_list syms
      |> Source.source Symbol_Pos.stopper
        (Scan.recover (Scan.bulk (!!! "bad input" scan))
          (fn msg => recover msg >> map Antiquote.Text))
      |> Source.exhaust
      |> tap (Position.reports_text o Antiquote.antiquote_reports (single o report_of_token SML))
      |> tap (List.app check);
  in input @ termination end;

in

fun source src =
  Symbol_Pos.source (Position.line 1) src
  |> Source.source Symbol_Pos.stopper (Scan.recover (Scan.bulk (!!! "bad input" scan_ml)) recover);

val tokenize = Source.of_string #> Symbol.source #> source #> Source.exhaust;

val read_pos = gen_read false;

val read = read_pos Position.none;

fun read_set_range range =
  read #> map (fn Antiquote.Text tok => Antiquote.Text (set_range range tok) | antiq => antiq);

fun read_source SML source =
  let
    val pos = Input.pos_of source;
    val language = if SML then Markup.language_SML else Markup.language_ML;
    val _ =
      if Position.is_reported_range pos
      then Position.report pos (language (Input.is_delimited source))
      else ();
  in gen_read SML pos (Input.text_of source) end;

end;

end;

