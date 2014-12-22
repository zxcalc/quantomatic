(*  Title:      Pure/Syntax/lexicon.ML
    Author:     Tobias Nipkow and Markus Wenzel, TU Muenchen

Lexer for the inner Isabelle syntax (terms and types).
*)

signature LEXICON =
sig
  structure Syntax:
  sig
    val const: string -> term
    val free: string -> term
    val var: indexname -> term
  end
  val scan_id: Symbol_Pos.T list -> Symbol_Pos.T list * Symbol_Pos.T list
  val scan_longid: Symbol_Pos.T list -> Symbol_Pos.T list * Symbol_Pos.T list
  val scan_tid: Symbol_Pos.T list -> Symbol_Pos.T list * Symbol_Pos.T list
  val scan_nat: Symbol_Pos.T list -> Symbol_Pos.T list * Symbol_Pos.T list
  val scan_int: Symbol_Pos.T list -> Symbol_Pos.T list * Symbol_Pos.T list
  val scan_float: Symbol_Pos.T list -> Symbol_Pos.T list * Symbol_Pos.T list
  val scan_hex: Symbol_Pos.T list -> Symbol_Pos.T list * Symbol_Pos.T list
  val scan_bin: Symbol_Pos.T list -> Symbol_Pos.T list * Symbol_Pos.T list
  val scan_var: Symbol_Pos.T list -> Symbol_Pos.T list * Symbol_Pos.T list
  val scan_tvar: Symbol_Pos.T list -> Symbol_Pos.T list * Symbol_Pos.T list
  val is_tid: string -> bool
  datatype token_kind =
    Literal | IdentSy | LongIdentSy | VarSy | TFreeSy | TVarSy | NumSy |
    FloatSy | XNumSy | StrSy | StringSy | Cartouche | Space | Comment | EOF
  datatype token = Token of token_kind * string * Position.range
  val str_of_token: token -> string
  val pos_of_token: token -> Position.T
  val is_proper: token -> bool
  val mk_eof: Position.T -> token
  val eof: token
  val is_eof: token -> bool
  val stopper: token Scan.stopper
  val terminals: string list
  val is_terminal: string -> bool
  val literal_markup: string -> Markup.T
  val report_of_token: token -> Position.report
  val reported_token_range: Proof.context -> token -> string
  val matching_tokens: token * token -> bool
  val valued_token: token -> bool
  val predef_term: string -> token option
  val implode_string: Symbol.symbol list -> string
  val explode_string: string * Position.T -> Symbol_Pos.T list
  val implode_str: Symbol.symbol list -> string
  val explode_str: string * Position.T -> Symbol_Pos.T list
  val tokenize: Scan.lexicon -> bool -> Symbol_Pos.T list -> token list
  val read_indexname: string -> indexname
  val read_var: string -> term
  val read_variable: string -> indexname option
  val read_nat: string -> int option
  val read_int: string -> int option
  val read_xnum: string -> {radix: int, leading_zeros: int, value: int}
  val read_float: string -> {mant: int, exp: int}
  val mark_class: string -> string val unmark_class: string -> string
  val mark_type: string -> string val unmark_type: string -> string
  val mark_const: string -> string val unmark_const: string -> string
  val mark_fixed: string -> string val unmark_fixed: string -> string
  val unmark:
   {case_class: string -> 'a,
    case_type: string -> 'a,
    case_const: string -> 'a,
    case_fixed: string -> 'a,
    case_default: string -> 'a} -> string -> 'a
  val is_marked: string -> bool
  val dummy_type: term
  val fun_type: term
end;

structure Lexicon: LEXICON =
struct

(** syntaxtic terms **)

structure Syntax =
struct

fun const c = Const (c, dummyT);
fun free x = Free (x, dummyT);
fun var xi = Var (xi, dummyT);

end;



(** basic scanners **)

open Basic_Symbol_Pos;

val err_prefix = "Inner lexical error: ";

fun !!! msg = Symbol_Pos.!!! (fn () => err_prefix ^ msg);

val scan_id = Symbol_Pos.scan_ident;
val scan_longid = scan_id @@@ (Scan.repeat1 ($$$ "." @@@ scan_id) >> flat);
val scan_tid = $$$ "'" @@@ scan_id;

val scan_nat = Scan.many1 (Symbol.is_digit o Symbol_Pos.symbol);
val scan_int = $$$ "-" @@@ scan_nat || scan_nat;
val scan_natdot = scan_nat @@@ $$$ "." @@@ scan_nat;
val scan_float = $$$ "-" @@@ scan_natdot || scan_natdot;
val scan_hex = $$$ "0" @@@ $$$ "x" @@@ Scan.many1 (Symbol.is_ascii_hex o Symbol_Pos.symbol);
val scan_bin = $$$ "0" @@@ $$$ "b" @@@ Scan.many1 (fn (s, _) => s = "0" orelse s = "1");

val scan_id_nat = scan_id @@@ Scan.optional ($$$ "." @@@ scan_nat) [];
val scan_var = $$$ "?" @@@ scan_id_nat;
val scan_tvar = $$$ "?" @@@ $$$ "'" @@@ scan_id_nat;

fun is_tid s =
  (case try (unprefix "'") s of
    SOME s' => Symbol_Pos.is_identifier s'
  | NONE => false);



(** datatype token **)

datatype token_kind =
  Literal | IdentSy | LongIdentSy | VarSy | TFreeSy | TVarSy | NumSy |
  FloatSy | XNumSy | StrSy | StringSy | Cartouche | Space | Comment | EOF;

datatype token = Token of token_kind * string * Position.range;

fun str_of_token (Token (_, s, _)) = s;
fun pos_of_token (Token (_, _, (pos, _))) = pos;

fun is_proper (Token (Space, _, _)) = false
  | is_proper (Token (Comment, _, _)) = false
  | is_proper _ = true;


(* stopper *)

fun mk_eof pos = Token (EOF, "", (pos, Position.none));
val eof = mk_eof Position.none;

fun is_eof (Token (EOF, _, _)) = true
  | is_eof _ = false;

val stopper = Scan.stopper (K eof) is_eof;


(* terminal arguments *)

val terminal_kinds =
 [("id", IdentSy),
  ("longid", LongIdentSy),
  ("var", VarSy),
  ("tid", TFreeSy),
  ("tvar", TVarSy),
  ("num_token", NumSy),
  ("float_token", FloatSy),
  ("xnum_token", XNumSy),
  ("str_token", StrSy),
  ("string_token", StringSy),
  ("cartouche", Cartouche)];

val terminals = map #1 terminal_kinds;
val is_terminal = member (op =) terminals;


(* markup *)

fun literal_markup s =
  if Symbol.is_ascii_identifier s orelse exists Symbol.is_letter (Symbol.explode s)
  then Markup.literal
  else Markup.delimiter;

val token_kind_markup =
 fn TFreeSy => Markup.tfree
  | TVarSy => Markup.tvar
  | NumSy => Markup.numeral
  | FloatSy => Markup.numeral
  | XNumSy => Markup.numeral
  | StrSy => Markup.inner_string
  | StringSy => Markup.inner_string
  | Cartouche => Markup.inner_cartouche
  | Comment => Markup.inner_comment
  | _ => Markup.empty;

fun report_of_token (Token (kind, s, (pos, _))) =
  let val markup = if kind = Literal then literal_markup s else token_kind_markup kind
  in (pos, markup) end;

fun reported_token_range ctxt tok =
  if is_proper tok
  then Context_Position.reported_text ctxt (pos_of_token tok) Markup.token_range ""
  else "";


(* matching_tokens *)

fun matching_tokens (Token (Literal, x, _), Token (Literal, y, _)) = x = y
  | matching_tokens (Token (k, _, _), Token (k', _, _)) = k = k';


(* valued_token *)

fun valued_token (Token (Literal, _, _)) = false
  | valued_token (Token (EOF, _, _)) = false
  | valued_token _ = true;


(* predef_term *)

fun predef_term s =
  (case AList.lookup (op =) terminal_kinds s of
    SOME sy => SOME (Token (sy, s, Position.no_range))
  | NONE => NONE);



(** string literals **)

fun explode_literal scan_body (str, pos) =
  (case Scan.read Symbol_Pos.stopper scan_body (Symbol_Pos.explode (str, pos)) of
    SOME ss => ss
  | _ => error (err_prefix ^ "malformed string literal " ^ quote str ^ Position.here pos));


(* string *)

val scan_string = Scan.trace (Symbol_Pos.scan_string_qq err_prefix) >> #2;
val scan_string_body = Symbol_Pos.scan_string_qq err_prefix >> (#1 o #2);

fun implode_string ss = quote (implode (map (fn "\"" => "\\\"" | s => s) ss));
val explode_string = explode_literal scan_string_body;


(* str *)

val scan_chr =
  $$ "\\" |-- $$$ "'" ||
  Scan.one
    ((fn s => s <> "\\" andalso s <> "'" andalso Symbol.is_regular s) o
      Symbol_Pos.symbol) >> single ||
  $$$ "'" --| Scan.ahead (~$$ "'");

val scan_str =
  Scan.ahead ($$ "'" -- $$ "'") |--
    !!! "unclosed string literal"
      ($$$ "'" @@@ $$$ "'" @@@ (Scan.repeat scan_chr >> flat) @@@ $$$ "'" @@@ $$$ "'");

val scan_str_body =
  Scan.ahead ($$ "'" |-- $$ "'") |--
    !!! "unclosed string literal"
      ($$ "'" |-- $$ "'" |-- (Scan.repeat scan_chr >> flat) --| $$ "'" --| $$ "'");

fun implode_str ss = enclose "''" "''" (implode (map (fn "'" => "\\'" | s => s) ss));
val explode_str = explode_literal scan_str_body;



(** tokenize **)

fun token_leq (Token (_, s1, _), Token (_, s2, _)) = s1 <= s2;
fun token kind ss = Token (kind, Symbol_Pos.content ss, Symbol_Pos.range ss);

fun tokenize lex xids syms =
  let
    val scan_xid =
      (if xids then $$$ "_" @@@ scan_id || scan_id else scan_id) ||
      $$$ "_" @@@ $$$ "_";

    val scan_num = scan_hex || scan_bin || scan_int;

    val scan_val =
      scan_tvar >> token TVarSy ||
      scan_var >> token VarSy ||
      scan_tid >> token TFreeSy ||
      scan_float >> token FloatSy ||
      scan_num >> token NumSy ||
      $$$ "#" @@@ scan_num >> token XNumSy ||
      scan_longid >> token LongIdentSy ||
      scan_xid >> token IdentSy;

    val scan_lit = Scan.literal lex >> token Literal;

    val scan_token =
      Symbol_Pos.scan_cartouche err_prefix >> token Cartouche ||
      Symbol_Pos.scan_comment err_prefix >> token Comment ||
      Scan.max token_leq scan_lit scan_val ||
      scan_string >> token StringSy ||
      scan_str >> token StrSy ||
      Scan.many1 (Symbol.is_blank o Symbol_Pos.symbol) >> token Space;
  in
    (case Scan.error
        (Scan.finite Symbol_Pos.stopper (Scan.repeat scan_token)) syms of
      (toks, []) => toks
    | (_, ss) =>
        error ("Inner lexical error" ^ Position.here (#1 (Symbol_Pos.range ss)) ^
          Markup.markup Markup.no_report ("\nat " ^ quote (Symbol_Pos.content ss))))

  end;



(** scan variables **)

(* scan_indexname *)

local

val scan_vname =
  let
    fun nat n [] = n
      | nat n (c :: cs) = nat (n * 10 + (ord c - ord "0")) cs;

    fun idxname cs ds = (implode (rev cs), nat 0 ds);
    fun chop_idx [] ds = idxname [] ds
      | chop_idx (cs as (_ :: "\\<^sub>" :: _)) ds = idxname cs ds
      | chop_idx (c :: cs) ds =
          if Symbol.is_digit c then chop_idx cs (c :: ds)
          else idxname (c :: cs) ds;

    val scan =
      (scan_id >> map Symbol_Pos.symbol) --
      Scan.optional ($$ "." |-- scan_nat >> (nat 0 o map Symbol_Pos.symbol)) ~1;
  in
    scan >>
      (fn (cs, ~1) => chop_idx (rev cs) []
        | (cs, i) => (implode cs, i))
  end;

in

val scan_indexname = $$ "'" |-- scan_vname >> (fn (x, i) => ("'" ^ x, i)) || scan_vname;

end;


(* indexname *)

fun read_indexname s =
  (case Scan.read Symbol_Pos.stopper scan_indexname (Symbol_Pos.explode (s, Position.none)) of
    SOME xi => xi
  | _ => error ("Lexical error in variable name: " ^ quote s));


(* read_var *)

fun read_var str =
  let
    val scan =
      $$ "?" |-- scan_indexname --| Scan.ahead (Scan.one Symbol_Pos.is_eof)
        >> Syntax.var ||
      Scan.many (Symbol.is_regular o Symbol_Pos.symbol)
        >> (Syntax.free o implode o map Symbol_Pos.symbol);
  in the (Scan.read Symbol_Pos.stopper scan (Symbol_Pos.explode (str, Position.none))) end;


(* read_variable *)

fun read_variable str =
  let val scan = $$ "?" |-- scan_indexname || scan_indexname
  in Scan.read Symbol_Pos.stopper scan (Symbol_Pos.explode (str, Position.none)) end;


(* read numbers *)

local

fun nat cs =
  Option.map (#1 o Library.read_int o map Symbol_Pos.symbol)
    (Scan.read Symbol_Pos.stopper scan_nat cs);

in

fun read_nat s = nat (Symbol_Pos.explode (s, Position.none));

fun read_int s =
  (case Symbol_Pos.explode (s, Position.none) of
    ("-", _) :: cs => Option.map ~ (nat cs)
  | cs => nat cs);

end;


(* read_xnum: hex/bin/decimal *)

local

val ten = ord "0" + 10;
val a = ord "a";
val A = ord "A";
val _ = a > A orelse raise Fail "Bad ASCII";

fun remap_hex c =
  let val x = ord c in
    if x >= a then chr (x - a + ten)
    else if x >= A then chr (x - A + ten)
    else c
  end;

fun leading_zeros ["0"] = 0
  | leading_zeros ("0" :: cs) = 1 + leading_zeros cs
  | leading_zeros _ = 0;

in

fun read_xnum str =
  let
    val (sign, radix, digs) =
      (case Symbol.explode (perhaps (try (unprefix "#")) str) of
        "0" :: "x" :: cs => (1, 16, map remap_hex cs)
      | "0" :: "b" :: cs => (1, 2, cs)
      | "-" :: cs => (~1, 10, cs)
      | cs => (1, 10, cs));
  in
   {radix = radix,
    leading_zeros = leading_zeros digs,
    value = sign * #1 (Library.read_radix_int radix digs)}
  end;

end;

fun read_float str =
  let
    val (sign, cs) =
      (case Symbol.explode str of
        "-" :: cs => (~1, cs)
      | cs => (1, cs));
    val (intpart, fracpart) =
      (case take_prefix Symbol.is_digit cs of
        (intpart, "." :: fracpart) => (intpart, fracpart)
      | _ => raise Fail "read_float");
  in
   {mant = sign * #1 (Library.read_int (intpart @ fracpart)),
    exp = length fracpart}
  end;


(* marked logical entities *)

fun marker s = (prefix s, unprefix s);

val (mark_class, unmark_class) = marker "\\<^class>";
val (mark_type, unmark_type) = marker "\\<^type>";
val (mark_const, unmark_const) = marker "\\<^const>";
val (mark_fixed, unmark_fixed) = marker "\\<^fixed>";

fun unmark {case_class, case_type, case_const, case_fixed, case_default} s =
  (case try unmark_class s of
    SOME c => case_class c
  | NONE =>
      (case try unmark_type s of
        SOME c => case_type c
      | NONE =>
          (case try unmark_const s of
            SOME c => case_const c
          | NONE =>
              (case try unmark_fixed s of
                SOME c => case_fixed c
              | NONE => case_default s))));

val is_marked =
  unmark {case_class = K true, case_type = K true, case_const = K true,
    case_fixed = K true, case_default = K false};

val dummy_type = Syntax.const (mark_type "dummy");
val fun_type = Syntax.const (mark_type "fun");

end;
