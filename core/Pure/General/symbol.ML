(*  Title:      Pure/General/symbol.ML
    Author:     Makarius

Generalized characters with infinitely many named symbols.
*)

signature SYMBOL =
sig
  type symbol = string
  val explode: string -> symbol list
  val spaces: int -> string
  val STX: symbol
  val DEL: symbol
  val open_: symbol
  val close: symbol
  val space: symbol
  val is_space: symbol -> bool
  val comment: symbol
  val is_char: symbol -> bool
  val is_utf8: symbol -> bool
  val is_symbolic: symbol -> bool
  val is_symbolic_char: symbol -> bool
  val is_printable: symbol -> bool
  val is_control: symbol -> bool
  val eof: symbol
  val is_eof: symbol -> bool
  val not_eof: symbol -> bool
  val stopper: symbol Scan.stopper
  val is_malformed: symbol -> bool
  val malformed_msg: symbol -> string
  val is_ascii: symbol -> bool
  val is_ascii_letter: symbol -> bool
  val is_ascii_digit: symbol -> bool
  val is_ascii_hex: symbol -> bool
  val is_ascii_quasi: symbol -> bool
  val is_ascii_blank: symbol -> bool
  val is_ascii_control: symbol -> bool
  val is_ascii_letdig: symbol -> bool
  val is_ascii_lower: symbol -> bool
  val is_ascii_upper: symbol -> bool
  val to_ascii_lower: symbol -> symbol
  val to_ascii_upper: symbol -> symbol
  val is_ascii_identifier: string -> bool
  val scan_ascii_id: string list -> string * string list
  datatype sym =
    Char of string | UTF8 of string | Sym of string | Control of string | Malformed of string | EOF
  val decode: symbol -> sym
  datatype kind = Letter | Digit | Quasi | Blank | Other
  val kind: symbol -> kind
  val is_letter: symbol -> bool
  val is_digit: symbol -> bool
  val is_quasi: symbol -> bool
  val is_blank: symbol -> bool
  val is_block_ctrl: symbol -> bool
  val is_quasi_letter: symbol -> bool
  val is_letdig: symbol -> bool
  val beginning: int -> symbol list -> string
  val esc: symbol -> string
  val escape: string -> string
  val scanner: string -> (string list -> 'a * string list) -> symbol list -> 'a
  val split_words: symbol list -> string list
  val explode_words: string -> string list
  val trim_blanks: string -> string
  val bump_init: string -> string
  val bump_string: string -> string
  val length: symbol list -> int
  val xsymbolsN: string
  val output: string -> Output.output * int
end;

structure Symbol: SYMBOL =
struct

(** type symbol **)

(*Symbols, which are considered the smallest entities of any Isabelle
  string, may be of the following form:

    (1) ASCII: a
    (2) UTF-8: ä
    (2) regular symbols: \<ident>
    (3) control symbols: \<^ident>

  Output is subject to the print_mode variable (default: verbatim),
  actual interpretation in display is up to front-end tools.
*)

type symbol = string;

val STX = chr 2;
val DEL = chr 127;

val open_ = "\<open>";
val close = "\<close>";

val space = chr 32;
fun is_space s = s = space;

local
  val small_spaces = Vector.tabulate (65, fn i => replicate_string i space);
in
  fun spaces n =
    if n < 64 then Vector.sub (small_spaces, n)
    else
      replicate_string (n div 64) (Vector.sub (small_spaces, 64)) ^
        Vector.sub (small_spaces, n mod 64);
end;

val comment = "\<comment>";

fun is_char s = size s = 1;

fun is_utf8 s = size s > 0 andalso forall_string (fn c => ord c >= 128) s;

fun raw_symbolic s =
  String.isPrefix "\092<" s andalso String.isSuffix ">" s andalso not (String.isPrefix "\092<^" s);

fun is_symbolic s =
  s <> open_ andalso s <> close andalso raw_symbolic s;

val is_symbolic_char = member (op =) (raw_explode "!#$%&*+-/<=>?@^_|~");

fun is_printable s =
  if is_char s then 32 <= ord s andalso ord s <= Char.ord #"~"
  else is_utf8 s orelse raw_symbolic s;

fun is_control s =
  String.isPrefix "\092<^" s andalso String.isSuffix ">" s;


(* input source control *)

val eof = "";
fun is_eof s = s = eof;
fun not_eof s = s <> eof;
val stopper = Scan.stopper (K eof) is_eof;

fun is_malformed s =
  String.isPrefix "\092<" s andalso not (String.isSuffix ">" s)
  orelse s = "\092<>" orelse s = "\092<^>";

fun malformed_msg s = "Malformed symbolic character: " ^ quote s;


(* ASCII symbols *)

fun is_ascii s = is_char s andalso ord s < 128;

fun is_ascii_letter s =
  is_char s andalso
   (Char.ord #"A" <= ord s andalso ord s <= Char.ord #"Z" orelse
    Char.ord #"a" <= ord s andalso ord s <= Char.ord #"z");

fun is_ascii_digit s =
  is_char s andalso Char.ord #"0" <= ord s andalso ord s <= Char.ord #"9";

fun is_ascii_hex s =
  is_char s andalso
   (Char.ord #"0" <= ord s andalso ord s <= Char.ord #"9" orelse
    Char.ord #"A" <= ord s andalso ord s <= Char.ord #"F" orelse
    Char.ord #"a" <= ord s andalso ord s <= Char.ord #"f");

fun is_ascii_quasi "_" = true
  | is_ascii_quasi "'" = true
  | is_ascii_quasi _ = false;

val is_ascii_blank =
  fn " " => true | "\t" => true | "\n" => true | "\^K" => true | "\f" => true | "\^M" => true
    | _ => false;

fun is_ascii_control s = is_char s andalso ord s < 32 andalso not (is_ascii_blank s);

fun is_ascii_letdig s = is_ascii_letter s orelse is_ascii_digit s orelse is_ascii_quasi s;

fun is_ascii_lower s = is_char s andalso (Char.ord #"a" <= ord s andalso ord s <= Char.ord #"z");
fun is_ascii_upper s = is_char s andalso (Char.ord #"A" <= ord s andalso ord s <= Char.ord #"Z");

fun to_ascii_lower s = if is_ascii_upper s then chr (ord s + Char.ord #"a" - Char.ord #"A") else s;
fun to_ascii_upper s = if is_ascii_lower s then chr (ord s + Char.ord #"A" - Char.ord #"a") else s;

fun is_ascii_identifier s =
  size s > 0 andalso is_ascii_letter (String.substring (s, 0, 1)) andalso
  forall_string is_ascii_letdig s;

val scan_ascii_id = Scan.one is_ascii_letter ^^ (Scan.many is_ascii_letdig >> implode);


(* diagnostics *)

fun beginning n cs =
  let
    val drop_blanks = #1 o take_suffix is_ascii_blank;
    val all_cs = drop_blanks cs;
    val dots = if length all_cs > n then " ..." else "";
  in
    (drop_blanks (take n all_cs)
      |> map (fn c => if is_ascii_blank c then space else c)
      |> implode) ^ dots
  end;


(* symbol variants *)

datatype sym =
  Char of string | UTF8 of string | Sym of string | Control of string | Malformed of string | EOF;

fun decode s =
  if s = "" then EOF
  else if is_char s then Char s
  else if is_utf8 s then UTF8 s
  else if is_malformed s then Malformed s
  else if is_control s then Control (String.substring (s, 3, size s - 4))
  else Sym (String.substring (s, 2, size s - 3));


(* standard symbol kinds *)

local
  val letter_symbols =
    Symtab.make_set [
      "\<A>",
      "\<B>",
      "\<C>",
      "\<D>",
      "\<E>",
      "\<F>",
      "\<G>",
      "\<H>",
      "\<I>",
      "\<J>",
      "\<K>",
      "\<L>",
      "\<M>",
      "\<N>",
      "\<O>",
      "\<P>",
      "\<Q>",
      "\<R>",
      "\<S>",
      "\<T>",
      "\<U>",
      "\<V>",
      "\<W>",
      "\<X>",
      "\<Y>",
      "\<Z>",
      "\<a>",
      "\<b>",
      "\<c>",
      "\<d>",
      "\<e>",
      "\<f>",
      "\<g>",
      "\<h>",
      "\<i>",
      "\<j>",
      "\<k>",
      "\<l>",
      "\<m>",
      "\<n>",
      "\<o>",
      "\<p>",
      "\<q>",
      "\<r>",
      "\<s>",
      "\<t>",
      "\<u>",
      "\<v>",
      "\<w>",
      "\<x>",
      "\<y>",
      "\<z>",
      "\<AA>",
      "\<BB>",
      "\<CC>",
      "\<DD>",
      "\<EE>",
      "\<FF>",
      "\<GG>",
      "\<HH>",
      "\<II>",
      "\<JJ>",
      "\<KK>",
      "\<LL>",
      "\<MM>",
      "\<NN>",
      "\<OO>",
      "\<PP>",
      "\<QQ>",
      "\<RR>",
      "\<SS>",
      "\<TT>",
      "\<UU>",
      "\<VV>",
      "\<WW>",
      "\<XX>",
      "\<YY>",
      "\<ZZ>",
      "\<aa>",
      "\<bb>",
      "\<cc>",
      "\<dd>",
      "\<ee>",
      "\<ff>",
      "\<gg>",
      "\<hh>",
      "\<ii>",
      "\<jj>",
      "\<kk>",
      "\<ll>",
      "\<mm>",
      "\<nn>",
      "\<oo>",
      "\<pp>",
      "\<qq>",
      "\<rr>",
      "\<ss>",
      "\<tt>",
      "\<uu>",
      "\<vv>",
      "\<ww>",
      "\<xx>",
      "\<yy>",
      "\<zz>",
      "\<alpha>",
      "\<beta>",
      "\<gamma>",
      "\<delta>",
      "\<epsilon>",
      "\<zeta>",
      "\<eta>",
      "\<theta>",
      "\<iota>",
      "\<kappa>",
      (*"\<lambda>", sic!*)
      "\<mu>",
      "\<nu>",
      "\<xi>",
      "\<pi>",
      "\<rho>",
      "\<sigma>",
      "\<tau>",
      "\<upsilon>",
      "\<phi>",
      "\<chi>",
      "\<psi>",
      "\<omega>",
      "\<Gamma>",
      "\<Delta>",
      "\<Theta>",
      "\<Lambda>",
      "\<Xi>",
      "\<Pi>",
      "\<Sigma>",
      "\<Upsilon>",
      "\<Phi>",
      "\<Psi>",
      "\<Omega>"
    ];
in

val is_letter_symbol = Symtab.defined letter_symbols;

end;

datatype kind = Letter | Digit | Quasi | Blank | Other;

fun kind s =
  if is_ascii_letter s then Letter
  else if is_ascii_digit s then Digit
  else if is_ascii_quasi s then Quasi
  else if is_ascii_blank s then Blank
  else if is_char s then Other
  else if is_letter_symbol s then Letter
  else Other;

fun is_letter s = kind s = Letter;
fun is_digit s = kind s = Digit;
fun is_quasi s = kind s = Quasi;
fun is_blank s = kind s = Blank;

val is_block_ctrl = member (op =) ["\<^bsub>", "\<^esub>", "\<^bsup>", "\<^esup>"];

fun is_quasi_letter s = let val k = kind s in k = Letter orelse k = Quasi end;
fun is_letdig s = let val k = kind s in k = Letter orelse k = Digit orelse k = Quasi end;


(* escape *)

val esc = fn s =>
  if is_char s then s
  else if is_utf8 s then translate_string (fn c => "\\" ^ string_of_int (ord c)) s
  else "\\" ^ s;

val escape = implode o map esc o Symbol.explode;



(** scanning through symbols **)

(* scanner *)

fun scanner msg scan syms =
  let
    fun message (ss, NONE) = (fn () => msg ^ ": " ^ quote (beginning 10 ss))
      | message (ss, SOME msg') = (fn () => msg ^ ", " ^ msg' () ^ ": " ^ quote (beginning 10 ss));
    val finite_scan = Scan.error (Scan.finite stopper (!! message scan));
  in
    (case finite_scan syms of
      (result, []) => result
    | (_, rest) => error (message (rest, NONE) ()))
  end;


(* space-separated words *)

val scan_word =
  Scan.many1 is_ascii_blank >> K NONE ||
  Scan.many1 (fn s => not (is_ascii_blank s) andalso not_eof s) >> (SOME o implode);

val split_words = scanner "Bad text" (Scan.repeat scan_word >> map_filter I);

val explode_words = split_words o Symbol.explode;


(* blanks *)

val trim_blanks = Symbol.explode #> trim is_blank #> implode;


(* bump string -- treat as base 26 or base 1 numbers *)

fun symbolic_end (_ :: "\<^sub>" :: _) = true
  | symbolic_end ("'" :: ss) = symbolic_end ss
  | symbolic_end (s :: _) = raw_symbolic s
  | symbolic_end [] = false;

fun bump_init str =
  if symbolic_end (rev (Symbol.explode str)) then str ^ "'"
  else str ^ "a";

fun bump_string str =
  let
    fun bump [] = ["a"]
      | bump ("z" :: ss) = "a" :: bump ss
      | bump (s :: ss) =
          if is_char s andalso Char.ord #"a" <= ord s andalso ord s < Char.ord #"z"
          then chr (ord s + 1) :: ss
          else "a" :: s :: ss;

    val (ss, qs) = apfst rev (take_suffix is_quasi (Symbol.explode str));
    val ss' = if symbolic_end ss then "'" :: ss else bump ss;
  in implode (rev ss' @ qs) end;



(** symbol output **)

(* length *)

fun sym_len s =
  if not (is_printable s) then (0: int)
  else if String.isPrefix "\092<long" s then 2
  else if String.isPrefix "\092<Long" s then 2
  else 1;

fun sym_length ss = fold (fn s => fn n => sym_len s + n) ss 0;


(* print mode *)

val xsymbolsN = "xsymbols";

fun output s = (s, sym_length (Symbol.explode s));


(*final declarations of this structure!*)
val explode = Symbol.explode;
val length = sym_length;

end;
