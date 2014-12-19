(*  Title:      Pure/Isar/token.ML
    Author:     Markus Wenzel, TU Muenchen

Outer token syntax for Isabelle/Isar.
*)

signature TOKEN =
sig
  datatype kind =
    Command | Keyword | Ident | LongIdent | SymIdent | Var | TypeIdent | TypeVar |
    Nat | Float | String | AltString | Verbatim | Cartouche | Space | Comment | InternalValue |
    Error of string | Sync | EOF
  type file = {src_path: Path.T, lines: string list, digest: SHA1.digest, pos: Position.T}
  datatype value =
    Literal of bool * Markup.T | Text of string | Typ of typ | Term of term | Fact of thm list |
    Attribute of morphism -> attribute | Files of file Exn.result list
  type T
  val str_of_kind: kind -> string
  val pos_of: T -> Position.T
  val range_of: T list -> Position.range
  val eof: T
  val is_eof: T -> bool
  val not_eof: T -> bool
  val not_sync: T -> bool
  val stopper: T Scan.stopper
  val kind_of: T -> kind
  val is_kind: kind -> T -> bool
  val keyword_with: (string -> bool) -> T -> bool
  val ident_with: (string -> bool) -> T -> bool
  val is_command: T -> bool
  val is_name: T -> bool
  val is_proper: T -> bool
  val is_improper: T -> bool
  val is_semicolon: T -> bool
  val is_comment: T -> bool
  val is_begin_ignore: T -> bool
  val is_end_ignore: T -> bool
  val is_error: T -> bool
  val is_space: T -> bool
  val is_blank: T -> bool
  val is_newline: T -> bool
  val inner_syntax_of: T -> string
  val source_position_of: T -> Symbol_Pos.source
  val content_of: T -> string
  val keyword_markup: bool * Markup.T -> string -> Markup.T
  val completion_report: T -> Position.report_text list
  val report: T -> Position.report_text
  val markup: T -> Markup.T
  val unparse: T -> string
  val print: T -> string
  val text_of: T -> string * string
  val get_files: T -> file Exn.result list
  val put_files: file Exn.result list -> T -> T
  val get_value: T -> value option
  val map_value: (value -> value) -> T -> T
  val reports_of_value: T -> Position.report list
  val mk_text: string -> T
  val mk_typ: typ -> T
  val mk_term: term -> T
  val mk_fact: thm list -> T
  val mk_attribute: (morphism -> attribute) -> T
  val init_assignable: T -> T
  val assign: value option -> T -> unit
  val closure: T -> T
  val ident_or_symbolic: string -> bool
  val source_proper: (T, 'a) Source.source -> (T, (T, 'a) Source.source) Source.source
  val source': {do_recover: bool option} -> (unit -> Scan.lexicon * Scan.lexicon) ->
    (Symbol_Pos.T, 'a) Source.source -> (T, (Symbol_Pos.T, 'a) Source.source) Source.source
  val source: {do_recover: bool option} -> (unit -> Scan.lexicon * Scan.lexicon) ->
    Position.T -> (Symbol.symbol, 'a) Source.source -> (T,
      (Symbol_Pos.T, Position.T * (Symbol.symbol, 'a) Source.source) Source.source) Source.source
  val read_antiq: Scan.lexicon -> (T list -> 'a * T list) -> Symbol_Pos.T list * Position.T -> 'a
end;

structure Token: TOKEN =
struct

(** tokens **)

(* token values *)

(*The value slot assigns an (optional) internal value to a token,
  usually as a side-effect of special scanner setup (see also
  args.ML).  Note that an assignable ref designates an intermediate
  state of internalization -- it is NOT meant to persist.*)

type file = {src_path: Path.T, lines: string list, digest: SHA1.digest, pos: Position.T};

datatype value =
  Literal of bool * Markup.T |
  Text of string |
  Typ of typ |
  Term of term |
  Fact of thm list |
  Attribute of morphism -> attribute |
  Files of file Exn.result list;

datatype slot =
  Slot |
  Value of value option |
  Assignable of value option Unsynchronized.ref;


(* datatype token *)

datatype kind =
  Command | Keyword | Ident | LongIdent | SymIdent | Var | TypeIdent | TypeVar |
  Nat | Float | String | AltString | Verbatim | Cartouche | Space | Comment | InternalValue |
  Error of string | Sync | EOF;

datatype T = Token of (Symbol_Pos.text * Position.range) * (kind * string) * slot;

val str_of_kind =
 fn Command => "command"
  | Keyword => "keyword"
  | Ident => "identifier"
  | LongIdent => "long identifier"
  | SymIdent => "symbolic identifier"
  | Var => "schematic variable"
  | TypeIdent => "type variable"
  | TypeVar => "schematic type variable"
  | Nat => "natural number"
  | Float => "floating-point number"
  | String => "quoted string"
  | AltString => "back-quoted string"
  | Verbatim => "verbatim text"
  | Cartouche => "text cartouche"
  | Space => "white space"
  | Comment => "comment text"
  | InternalValue => "internal value"
  | Error _ => "bad input"
  | Sync => "sync marker"
  | EOF => "end-of-input";

val delimited_kind = member (op =) [String, AltString, Verbatim, Cartouche, Comment];


(* position *)

fun pos_of (Token ((_, (pos, _)), _, _)) = pos;
fun end_pos_of (Token ((_, (_, pos)), _, _)) = pos;

fun range_of (toks as tok :: _) =
      let val pos' = end_pos_of (List.last toks)
      in Position.range (pos_of tok) pos' end
  | range_of [] = Position.no_range;


(* control tokens *)

fun mk_eof pos = Token (("", (pos, Position.none)), (EOF, ""), Slot);
val eof = mk_eof Position.none;

fun is_eof (Token (_, (EOF, _), _)) = true
  | is_eof _ = false;

val not_eof = not o is_eof;

fun not_sync (Token (_, (Sync, _), _)) = false
  | not_sync _ = true;

val stopper =
  Scan.stopper (fn [] => eof | toks => mk_eof (end_pos_of (List.last toks))) is_eof;


(* kind of token *)

fun kind_of (Token (_, (k, _), _)) = k;
fun is_kind k (Token (_, (k', _), _)) = k = k';

val is_command = is_kind Command;
val is_name = is_kind Ident orf is_kind SymIdent orf is_kind String orf is_kind Nat;

fun keyword_with pred (Token (_, (Keyword, x), _)) = pred x
  | keyword_with _ _ = false;

fun ident_with pred (Token (_, (Ident, x), _)) = pred x
  | ident_with _ _ = false;

fun is_proper (Token (_, (Space, _), _)) = false
  | is_proper (Token (_, (Comment, _), _)) = false
  | is_proper _ = true;

val is_improper = not o is_proper;

fun is_semicolon (Token (_, (Keyword, ";"), _)) = true
  | is_semicolon _ = false;

fun is_comment (Token (_, (Comment, _), _)) = true
  | is_comment _ = false;

fun is_begin_ignore (Token (_, (Comment, "<"), _)) = true
  | is_begin_ignore _ = false;

fun is_end_ignore (Token (_, (Comment, ">"), _)) = true
  | is_end_ignore _ = false;

fun is_error (Token (_, (Error _, _), _)) = true
  | is_error _ = false;


(* blanks and newlines -- space tokens obey lines *)

fun is_space (Token (_, (Space, _), _)) = true
  | is_space _ = false;

fun is_blank (Token (_, (Space, x), _)) = not (String.isSuffix "\n" x)
  | is_blank _ = false;

fun is_newline (Token (_, (Space, x), _)) = String.isSuffix "\n" x
  | is_newline _ = false;


(* token content *)

fun inner_syntax_of (Token ((source, (pos, _)), (kind, x), _)) =
  if YXML.detect x then x
  else
    let
      val delimited = delimited_kind kind;
      val tree = XML.Elem (Markup.token delimited (Position.properties_of pos), [XML.Text source]);
    in YXML.string_of tree end;

fun source_position_of (Token ((source, (pos, _)), (kind, _), _)) =
  {delimited = delimited_kind kind, text = source, pos = pos};

fun content_of (Token (_, (_, x), _)) = x;


(* markup reports *)

local

val token_kind_markup =
 fn Command       => (Markup.command, "")
  | Keyword       => (Markup.keyword2, "")
  | Ident         => (Markup.empty, "")
  | LongIdent     => (Markup.empty, "")
  | SymIdent      => (Markup.empty, "")
  | Var           => (Markup.var, "")
  | TypeIdent     => (Markup.tfree, "")
  | TypeVar       => (Markup.tvar, "")
  | Nat           => (Markup.empty, "")
  | Float         => (Markup.empty, "")
  | String        => (Markup.string, "")
  | AltString     => (Markup.altstring, "")
  | Verbatim      => (Markup.verbatim, "")
  | Cartouche     => (Markup.cartouche, "")
  | Space         => (Markup.empty, "")
  | Comment       => (Markup.comment, "")
  | InternalValue => (Markup.empty, "")
  | Error msg     => (Markup.bad, msg)
  | Sync          => (Markup.control, "")
  | EOF           => (Markup.control, "");

in

fun keyword_markup (important, keyword) x =
  if important orelse Symbol.is_ascii_identifier x then keyword else Markup.delimiter;

fun completion_report tok =
  if is_kind Keyword tok
  then map (fn m => ((pos_of tok, m), "")) (Completion.suppress_abbrevs (content_of tok))
  else [];

fun report tok =
  if is_kind Keyword tok then
    ((pos_of tok, keyword_markup (false, Markup.keyword2) (content_of tok)), "")
  else
    let val (m, text) = token_kind_markup (kind_of tok)
    in ((pos_of tok, m), text) end;

val markup = #2 o #1 o report;

end;


(* unparse *)

fun unparse (Token (_, (kind, x), _)) =
  (case kind of
    String => Symbol_Pos.quote_string_qq x
  | AltString => Symbol_Pos.quote_string_bq x
  | Verbatim => enclose "{*" "*}" x
  | Cartouche => cartouche x
  | Comment => enclose "(*" "*)" x
  | Sync => ""
  | EOF => ""
  | _ => x);

fun print tok = Markup.markup (markup tok) (unparse tok);

fun text_of tok =
  if is_semicolon tok then ("terminator", "")
  else
    let
      val k = str_of_kind (kind_of tok);
      val m = markup tok;
      val s = unparse tok;
    in
      if s = "" then (k, "")
      else if size s < 40 andalso not (exists_string (fn c => c = "\n") s)
      then (k ^ " " ^ Markup.markup m s, "")
      else (k, Markup.markup m s)
    end;



(** associated values **)

(* inlined file content *)

fun get_files (Token (_, _, Value (SOME (Files files)))) = files
  | get_files _ = [];

fun put_files [] tok = tok
  | put_files files (Token (x, y, Slot)) = Token (x, y, Value (SOME (Files files)))
  | put_files _ tok = raise Fail ("Cannot put inlined files here" ^ Position.here (pos_of tok));


(* access values *)

fun get_value (Token (_, _, Value v)) = v
  | get_value _ = NONE;

fun map_value f (Token (x, y, Value (SOME v))) = Token (x, y, Value (SOME (f v)))
  | map_value _ tok = tok;

fun reports_of_value tok =
  (case get_value tok of
    SOME (Literal markup) =>
      let
        val pos = pos_of tok;
        val x = content_of tok;
      in
        if Position.is_reported pos then
          map (pair pos) (keyword_markup markup x :: Completion.suppress_abbrevs x)
        else []
      end
  | _ => []);


(* make values *)

fun mk_value k v = Token ((k, Position.no_range), (InternalValue, k), Value (SOME v));

val mk_text = mk_value "<text>" o Text;
val mk_typ = mk_value "<typ>" o Typ;
val mk_term = mk_value "<term>" o Term;
val mk_fact = mk_value "<fact>" o Fact;
val mk_attribute = mk_value "<attribute>" o Attribute;


(* static binding *)

(*1st stage: initialize assignable slots*)
fun init_assignable (Token (x, y, Slot)) = Token (x, y, Assignable (Unsynchronized.ref NONE))
  | init_assignable (tok as Token (_, _, Assignable r)) = (r := NONE; tok)
  | init_assignable tok = tok;

(*2nd stage: assign values as side-effect of scanning*)
fun assign v (Token (_, _, Assignable r)) = r := v
  | assign _ _ = ();

(*3rd stage: static closure of final values*)
fun closure (Token (x, y, Assignable (Unsynchronized.ref v))) = Token (x, y, Value v)
  | closure tok = tok;



(** scanners **)

open Basic_Symbol_Pos;

val err_prefix = "Outer lexical error: ";

fun !!! msg = Symbol_Pos.!!! (fn () => err_prefix ^ msg);


(* scan symbolic idents *)

val scan_symid =
  Scan.many1 (Symbol.is_symbolic_char o Symbol_Pos.symbol) ||
  Scan.one (Symbol.is_symbolic o Symbol_Pos.symbol) >> single;

fun is_symid str =
  (case try Symbol.explode str of
    SOME [s] => Symbol.is_symbolic s orelse Symbol.is_symbolic_char s
  | SOME ss => forall Symbol.is_symbolic_char ss
  | _ => false);

fun ident_or_symbolic "begin" = false
  | ident_or_symbolic ":" = true
  | ident_or_symbolic "::" = true
  | ident_or_symbolic s = Symbol_Pos.is_identifier s orelse is_symid s;


(* scan verbatim text *)

val scan_verb =
  $$$ "*" --| Scan.ahead (~$$ "}") ||
  Scan.one (fn (s, _) => s <> "*" andalso Symbol.is_regular s) >> single;

val scan_verbatim =
  Scan.ahead ($$ "{" -- $$ "*") |--
    !!! "unclosed verbatim text"
      ((Symbol_Pos.scan_pos --| $$ "{" --| $$ "*") --
        Symbol_Pos.change_prompt
          ((Scan.repeat scan_verb >> flat) -- ($$ "*" |-- $$ "}" |-- Symbol_Pos.scan_pos)));

val recover_verbatim =
  $$$ "{" @@@ $$$ "*" @@@ (Scan.repeat scan_verb >> flat);


(* scan cartouche *)

val scan_cartouche =
  Symbol_Pos.scan_pos --
    ((Symbol_Pos.scan_cartouche err_prefix >> Symbol_Pos.cartouche_content) -- Symbol_Pos.scan_pos);


(* scan space *)

fun space_symbol (s, _) = Symbol.is_blank s andalso s <> "\n";

val scan_space =
  Scan.many1 space_symbol @@@ Scan.optional ($$$ "\n") [] ||
  Scan.many space_symbol @@@ $$$ "\n";


(* scan comment *)

val scan_comment =
  Symbol_Pos.scan_pos -- (Symbol_Pos.scan_comment_body err_prefix -- Symbol_Pos.scan_pos);



(** token sources **)

fun source_proper src = src |> Source.filter is_proper;

local

fun token_leq ((_, syms1), (_, syms2)) = length syms1 <= length syms2;

fun token k ss =
  Token ((Symbol_Pos.implode ss, Symbol_Pos.range ss), (k, Symbol_Pos.content ss), Slot);

fun token_range k (pos1, (ss, pos2)) =
  Token (Symbol_Pos.implode_range pos1 pos2 ss, (k, Symbol_Pos.content ss), Slot);

fun scan (lex1, lex2) = !!! "bad input"
  (Symbol_Pos.scan_string_qq err_prefix >> token_range String ||
    Symbol_Pos.scan_string_bq err_prefix >> token_range AltString ||
    scan_verbatim >> token_range Verbatim ||
    scan_cartouche >> token_range Cartouche ||
    scan_comment >> token_range Comment ||
    scan_space >> token Space ||
    Scan.one (Symbol.is_sync o Symbol_Pos.symbol) >> (token Sync o single) ||
    (Scan.max token_leq
      (Scan.max token_leq
        (Scan.literal lex2 >> pair Command)
        (Scan.literal lex1 >> pair Keyword))
      (Lexicon.scan_longid >> pair LongIdent ||
        Lexicon.scan_id >> pair Ident ||
        Lexicon.scan_var >> pair Var ||
        Lexicon.scan_tid >> pair TypeIdent ||
        Lexicon.scan_tvar >> pair TypeVar ||
        Lexicon.scan_float >> pair Float ||
        Lexicon.scan_nat >> pair Nat ||
        scan_symid >> pair SymIdent) >> uncurry token));

fun recover msg =
  (Symbol_Pos.recover_string_qq ||
    Symbol_Pos.recover_string_bq ||
    recover_verbatim ||
    Symbol_Pos.recover_cartouche ||
    Symbol_Pos.recover_comment ||
    Scan.one (Symbol.is_regular o Symbol_Pos.symbol) >> single)
  >> (single o token (Error msg));

in

fun source' {do_recover} get_lex =
  Source.source Symbol_Pos.stopper (Scan.bulk (fn xs => scan (get_lex ()) xs))
    (Option.map (rpair recover) do_recover);

fun source do_recover get_lex pos src =
  Symbol_Pos.source pos src
  |> source' do_recover get_lex;

end;


(* read_antiq *)

fun read_antiq lex scan (syms, pos) =
  let
    fun err msg = cat_error msg ("Malformed antiquotation" ^ Position.here pos ^ ":\n" ^
      "@{" ^ Symbol_Pos.content syms ^ "}");

    val res =
      Source.of_list syms
      |> source' {do_recover = NONE} (K (lex, Scan.empty_lexicon))
      |> source_proper
      |> Source.source stopper (Scan.error (Scan.bulk scan)) NONE
      |> Source.exhaust;
  in (case res of [x] => x | _ => err "") handle ERROR msg => err msg end;

end;
