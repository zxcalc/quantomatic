(*  Title:      Pure/General/symbol_pos.ML
    Author:     Makarius

Symbols with explicit position information.
*)

signature SYMBOL_POS =
sig
  type T = Symbol.symbol * Position.T
  val symbol: T -> Symbol.symbol
  val $$$ : Symbol.symbol -> T list -> T list * T list
  val ~$$$ : Symbol.symbol -> T list -> T list * T list
  val content: T list -> string
  val is_eof: T -> bool
  val stopper: T Scan.stopper
  val !!! : Scan.message -> (T list -> 'a) -> T list -> 'a
  val change_prompt: ('a -> 'b) -> 'a -> 'b
  val scan_pos: T list -> Position.T * T list
  val scan_string_q: string -> T list -> (Position.T * (T list * Position.T)) * T list
  val scan_string_qq: string -> T list -> (Position.T * (T list * Position.T)) * T list
  val scan_string_bq: string -> T list -> (Position.T * (T list * Position.T)) * T list
  val recover_string_q: T list -> T list * T list
  val recover_string_qq: T list -> T list * T list
  val recover_string_bq: T list -> T list * T list
  val quote_string_q: string -> string
  val quote_string_qq: string -> string
  val quote_string_bq: string -> string
  val scan_comment: (string -> (T list -> T list * T list) -> T list -> T list * T list) ->
    T list -> T list * T list
  val scan_comment_body: (string -> (T list -> T list * T list) -> T list -> T list * T list) ->
    T list -> T list * T list
  val recover_comment: T list -> T list * T list
  val source: Position.T -> (Symbol.symbol, 'a) Source.source ->
    (T, Position.T * (Symbol.symbol, 'a) Source.source) Source.source
  type text = string
  val implode: T list -> text
  val range: T list -> Position.range
  val implode_range: Position.T -> Position.T -> T list -> text * Position.range
  val explode: text * Position.T -> T list
  val scan_ident: T list -> T list * T list
  val is_identifier: string -> bool
end;

structure Symbol_Pos: SYMBOL_POS =
struct

(* type T *)

type T = Symbol.symbol * Position.T;

fun symbol ((s, _): T) = s;

val content = implode o map symbol;


(* stopper *)

fun mk_eof pos = (Symbol.eof, pos);
val eof = mk_eof Position.none;

val is_eof = Symbol.is_eof o symbol;

val stopper =
  Scan.stopper (fn [] => eof | inp => mk_eof (List.last inp |-> Position.advance)) is_eof;


(* basic scanners *)

fun !!! text scan =
  let
    fun get_pos [] = " (end-of-input)"
      | get_pos ((_, pos) :: _) = Position.here pos;

    fun err (syms, msg) = fn () =>
      text () ^ get_pos syms ^
      Markup.markup Markup.no_report (" at " ^ Symbol.beginning 10 (map symbol syms)) ^
      (case msg of NONE => "" | SOME m => "\n" ^ m ());
  in Scan.!! err scan end;

fun change_prompt scan = Scan.prompt "# " scan;

fun $$$ s = Scan.one (fn x => symbol x = s) >> single;
fun ~$$$ s = Scan.one (fn x => symbol x <> s) >> single;

val scan_pos = Scan.ahead (Scan.one (K true)) >> (fn (_, pos): T => pos);


(* scan string literals *)

local

val char_code =
  Scan.one (Symbol.is_ascii_digit o symbol) --
  Scan.one (Symbol.is_ascii_digit o symbol) --
  Scan.one (Symbol.is_ascii_digit o symbol) :|--
  (fn (((a, pos), (b, _)), (c, _)) =>
    let val (n, _) = Library.read_int [a, b, c]
    in if n <= 255 then Scan.succeed [(chr n, pos)] else Scan.fail end);

fun scan_str q err_prefix =
  $$$ "\\" |-- !!! (fn () => err_prefix ^ "bad escape character in string")
    ($$$ q || $$$ "\\" || char_code) ||
  Scan.one (fn (s, _) => s <> q andalso s <> "\\" andalso Symbol.is_regular s) >> single;

fun scan_strs q err_prefix =
  (scan_pos --| $$$ q) -- !!! (fn () => err_prefix ^ "missing quote at end of string")
    (change_prompt ((Scan.repeat (scan_str q err_prefix) >> flat) -- ($$$ q |-- scan_pos)));

fun recover_strs q =
  $$$ q @@@ (Scan.repeat (Scan.permissive (scan_str q "")) >> flat);

in

val scan_string_q = scan_strs "'";
val scan_string_qq = scan_strs "\"";
val scan_string_bq = scan_strs "`";

val recover_string_q = recover_strs "'";
val recover_string_qq = recover_strs "\"";
val recover_string_bq = recover_strs "`";

end;


(* quote string literals *)

local

fun char_code i =
  (if i < 10 then "00" else if i < 100 then "0" else "") ^ string_of_int i;

fun quote_str q s =
  if Symbol.is_ascii_control s then "\\" ^ char_code (ord s)
  else if s = q orelse s = "\\" then "\\" ^ s
  else s;

fun quote_string q = enclose q q o implode o map (quote_str q) o Symbol.explode;

in

val quote_string_q = quote_string "'";
val quote_string_qq = quote_string "\"";
val quote_string_bq = quote_string "`";

end;


(* ML-style comments *)

local

val scan_cmt =
  Scan.depend (fn (d: int) => $$$ "(" @@@ $$$ "*" >> pair (d + 1)) ||
  Scan.depend (fn 0 => Scan.fail | d => $$$ "*" @@@ $$$ ")" >> pair (d - 1)) ||
  Scan.lift ($$$ "*" --| Scan.ahead (~$$$ ")")) ||
  Scan.lift (Scan.one (fn (s, _) => s <> "*" andalso Symbol.is_regular s)) >> single;

val scan_cmts = Scan.pass 0 (Scan.repeat scan_cmt >> flat);

val scan_body = change_prompt scan_cmts;

in

fun scan_comment cut =
  $$$ "(" @@@ $$$ "*" @@@ cut "missing end of comment" (scan_body @@@ $$$ "*" @@@ $$$ ")");

fun scan_comment_body cut =
  $$$ "(" |-- $$$ "*" |-- cut "missing end of comment" (scan_body --| $$$ "*" --| $$$ ")");

val recover_comment =
  $$$ "(" @@@ $$$ "*" @@@ scan_cmts;

end;


(* source *)

fun source pos =
  Source.source' pos Symbol.stopper (Scan.bulk (Scan.depend (fn pos =>
    Scan.one Symbol.not_eof >> (fn s => (Position.advance s pos, (s, pos)))))) NONE;


(* compact representation -- with Symbol.DEL padding *)

type text = string;

fun pad [] = []
  | pad [(s, _)] = [s]
  | pad ((s1, pos1) :: (rest as (_, pos2) :: _)) =
      let
        val end_pos1 = Position.advance s1 pos1;
        val d = Int.max (0, Position.distance_of end_pos1 pos2);
      in s1 :: replicate d Symbol.DEL @ pad rest end;

val implode = implode o pad;

fun range (syms as (_, pos) :: _) =
      let val pos' = List.last syms |-> Position.advance
      in Position.range pos pos' end
  | range [] = Position.no_range;

fun implode_range pos1 pos2 syms =
  let val syms' = (("", pos1) :: syms @ [("", pos2)])
  in (implode syms', range syms') end;

fun explode (str, pos) =
  let
    val (res, _) =
      fold (fn s => fn (res, p) => ((s, p) :: res, Position.advance s p))
        (Symbol.explode str) ([], Position.reset_range pos);
  in fold (fn (s, p) => if s = Symbol.DEL then I else cons (s, p)) res [] end;


(* identifiers *)

local

val latin = Symbol.is_ascii_letter;
val digit = Symbol.is_ascii_digit;
fun underscore s = s = "_";
fun prime s = s = "'";
fun subscript s = s = "\\<^sub>" orelse s = "\\<^isub>";
fun script s = s = "\\<^sub>" orelse s = "\\<^isub>" orelse s = "\\<^isup>";
fun special_letter s = Symbol.is_letter_symbol s andalso not (script s);

val scan_plain = Scan.one ((latin orf digit orf prime) o symbol) >> single;
val scan_digit = Scan.one (digit o symbol) >> single;
val scan_prime = Scan.one (prime o symbol) >> single;
val scan_extended =
  Scan.one ((latin orf digit orf prime orf underscore orf special_letter) o symbol) >> single;

val scan_subscript =
  Scan.one (subscript o symbol) --
  Scan.one ((latin orf digit orf prime orf special_letter) o symbol)
  >> (fn (x, y) => [x, y]);

val scan_ident_part1 =
  Scan.one (latin o symbol) ::: (Scan.repeat (scan_plain || scan_subscript) >> flat) ||
  Scan.one (special_letter o symbol) :::
    (Scan.repeat (scan_digit || scan_prime || scan_subscript) >> flat);

val scan_ident_part2 =
  Scan.repeat1 (scan_plain || scan_subscript) >> flat ||
  scan_ident_part1;

in

val scan_ident0 =
  Scan.one (Symbol.is_letter o symbol) ::: Scan.many (Symbol.is_letdig o symbol);

val scan_ident1 =
  Scan.one ((latin orf special_letter) o symbol) :::
    (Scan.repeat (scan_extended || Scan.one (subscript o symbol) ::: scan_extended) >> flat);

val scan_ident2 =
  scan_ident_part1 @@@
    (Scan.repeat (Scan.many1 (underscore o symbol) @@@ scan_ident_part2) >> flat);

end;

val scan_ident = scan_ident0;

fun is_identifier s =
  Symbol.is_ascii_identifier s orelse
    (case try (Scan.finite stopper scan_ident) (explode (s, Position.none)) of
      SOME (_, []) => true
    | _ => false);

end;

structure Basic_Symbol_Pos =   (*not open by default*)
struct
  val $$$ = Symbol_Pos.$$$;
  val ~$$$ = Symbol_Pos.~$$$;
end;

