(*  Title:      Pure/General/symbol.ML
    Author:     Markus Wenzel, TU Muenchen

Generalized characters with infinitely many named symbols.
*)

signature SYMBOL =
sig
  type symbol = string
  val STX: symbol
  val DEL: symbol
  val space: symbol
  val is_char: symbol -> bool
  val is_utf8: symbol -> bool
  val is_symbolic: symbol -> bool
  val is_symbolic_char: symbol -> bool
  val is_printable: symbol -> bool
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
  val is_raw: symbol -> bool
  val decode_raw: symbol -> string
  val encode_raw: string -> string
  datatype sym =
    Char of string | UTF8 of string | Sym of string | Ctrl of string | Raw of string |
    Malformed of string | EOF
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
  val source: (string, 'a) Source.source -> (symbol, (string, 'a) Source.source) Source.source
  val explode: string -> symbol list
  val esc: symbol -> string
  val escape: string -> string
  val scanner: string -> (string list -> 'a * string list) -> symbol list -> 'a
  val split_words: symbol list -> string list
  val explode_words: string -> string list
  val strip_blanks: string -> string
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

    (1) ASCII symbols: a
    (2) regular symbols: \<ident>
    (3) control symbols: \<^ident>
    (4) raw control symbols: \<^raw:...>, where "..." may be any printable
        character (excluding ".", ">"), or \<^raw000>

  Output is subject to the print_mode variable (default: verbatim),
  actual interpretation in display is up to front-end tools.
*)

type symbol = string;

val STX = chr 2;
val DEL = chr 127;

val space = chr 32;

fun is_char s = size s = 1;

fun is_utf8 s = size s > 0 andalso forall_string (fn c => ord c >= 128) s;

fun raw_symbolic s =
  String.isPrefix "\\<" s andalso String.isSuffix ">" s andalso not (String.isPrefix "\\<^" s);

fun is_symbolic s =
  s <> "\\<open>" andalso s <> "\\<close>" andalso raw_symbolic s;

val is_symbolic_char = member (op =) (raw_explode "!#$%&*+-/<=>?@^_|~");

fun is_printable s =
  if is_char s then ord space <= ord s andalso ord s <= ord "~"
  else is_utf8 s orelse raw_symbolic s;


(* input source control *)

val eof = "";
fun is_eof s = s = eof;
fun not_eof s = s <> eof;
val stopper = Scan.stopper (K eof) is_eof;

fun is_malformed s =
  String.isPrefix "\\<" s andalso not (String.isSuffix ">" s)
  orelse s = "\\<>" orelse s = "\\<^>";

fun malformed_msg s = "Malformed symbolic character: " ^ quote s;


(* ASCII symbols *)

fun is_ascii s = is_char s andalso ord s < 128;

fun is_ascii_letter s =
  is_char s andalso
   (ord "A" <= ord s andalso ord s <= ord "Z" orelse
    ord "a" <= ord s andalso ord s <= ord "z");

fun is_ascii_digit s =
  is_char s andalso ord "0" <= ord s andalso ord s <= ord "9";

fun is_ascii_hex s =
  is_char s andalso
   (ord "0" <= ord s andalso ord s <= ord "9" orelse
    ord "A" <= ord s andalso ord s <= ord "F" orelse
    ord "a" <= ord s andalso ord s <= ord "f");

fun is_ascii_quasi "_" = true
  | is_ascii_quasi "'" = true
  | is_ascii_quasi _ = false;

val is_ascii_blank =
  fn " " => true | "\t" => true | "\n" => true | "\^K" => true | "\f" => true | "\^M" => true
    | _ => false;

fun is_ascii_control s = is_char s andalso ord s < 32 andalso not (is_ascii_blank s);

fun is_ascii_letdig s = is_ascii_letter s orelse is_ascii_digit s orelse is_ascii_quasi s;

fun is_ascii_lower s = is_char s andalso (ord "a" <= ord s andalso ord s <= ord "z");
fun is_ascii_upper s = is_char s andalso (ord "A" <= ord s andalso ord s <= ord "Z");

fun to_ascii_lower s = if is_ascii_upper s then chr (ord s + ord "a" - ord "A") else s;
fun to_ascii_upper s = if is_ascii_lower s then chr (ord s + ord "A" - ord "a") else s;

fun is_ascii_identifier s =
  size s > 0 andalso is_ascii_letter (String.substring (s, 0, 1)) andalso
  forall_string is_ascii_letdig s;

val scan_ascii_id = Scan.one is_ascii_letter ^^ (Scan.many is_ascii_letdig >> implode);


(* encode_raw *)

fun raw_chr c =
  is_char c andalso
    (ord space <= ord c andalso ord c <= ord "~" andalso c <> "." andalso c <> ">"
      orelse ord c >= 128);

fun encode_raw "" = ""
  | encode_raw str =
      let
        val raw0 = enclose "\\<^raw:" ">";
        val raw1 = raw0 o implode;
        val raw2 = enclose "\\<^raw" ">" o string_of_int o ord;

        fun encode cs = enc (take_prefix raw_chr cs)
        and enc ([], []) = []
          | enc (cs, []) = [raw1 cs]
          | enc ([], d :: ds) = raw2 d :: encode ds
          | enc (cs, d :: ds) = raw1 cs :: raw2 d :: encode ds;
      in
        if exists_string (not o raw_chr) str then implode (encode (raw_explode str))
        else raw0 str
      end;


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


(* decode_raw *)

fun is_raw s =
  String.isPrefix "\\<^raw" s andalso String.isSuffix ">" s;

fun decode_raw s =
  if not (is_raw s) then error (malformed_msg s)
  else if String.isPrefix "\\<^raw:" s then String.substring (s, 7, size s - 8)
  else chr (#1 (Library.read_int (raw_explode (String.substring (s, 6, size s - 7)))));


(* symbol variants *)

datatype sym =
  Char of string | UTF8 of string | Sym of string | Ctrl of string | Raw of string |
  Malformed of string | EOF;

fun decode s =
  if s = "" then EOF
  else if is_char s then Char s
  else if is_utf8 s then UTF8 s
  else if is_raw s then Raw (decode_raw s)
  else if is_malformed s then Malformed s
  else if String.isPrefix "\\<^" s then Ctrl (String.substring (s, 3, size s - 4))
  else Sym (String.substring (s, 2, size s - 3));


(* standard symbol kinds *)

local
  val letter_symbols =
    Symtab.make_set [
      "\\<A>",
      "\\<B>",
      "\\<C>",
      "\\<D>",
      "\\<E>",
      "\\<F>",
      "\\<G>",
      "\\<H>",
      "\\<I>",
      "\\<J>",
      "\\<K>",
      "\\<L>",
      "\\<M>",
      "\\<N>",
      "\\<O>",
      "\\<P>",
      "\\<Q>",
      "\\<R>",
      "\\<S>",
      "\\<T>",
      "\\<U>",
      "\\<V>",
      "\\<W>",
      "\\<X>",
      "\\<Y>",
      "\\<Z>",
      "\\<a>",
      "\\<b>",
      "\\<c>",
      "\\<d>",
      "\\<e>",
      "\\<f>",
      "\\<g>",
      "\\<h>",
      "\\<i>",
      "\\<j>",
      "\\<k>",
      "\\<l>",
      "\\<m>",
      "\\<n>",
      "\\<o>",
      "\\<p>",
      "\\<q>",
      "\\<r>",
      "\\<s>",
      "\\<t>",
      "\\<u>",
      "\\<v>",
      "\\<w>",
      "\\<x>",
      "\\<y>",
      "\\<z>",
      "\\<AA>",
      "\\<BB>",
      "\\<CC>",
      "\\<DD>",
      "\\<EE>",
      "\\<FF>",
      "\\<GG>",
      "\\<HH>",
      "\\<II>",
      "\\<JJ>",
      "\\<KK>",
      "\\<LL>",
      "\\<MM>",
      "\\<NN>",
      "\\<OO>",
      "\\<PP>",
      "\\<QQ>",
      "\\<RR>",
      "\\<SS>",
      "\\<TT>",
      "\\<UU>",
      "\\<VV>",
      "\\<WW>",
      "\\<XX>",
      "\\<YY>",
      "\\<ZZ>",
      "\\<aa>",
      "\\<bb>",
      "\\<cc>",
      "\\<dd>",
      "\\<ee>",
      "\\<ff>",
      "\\<gg>",
      "\\<hh>",
      "\\<ii>",
      "\\<jj>",
      "\\<kk>",
      "\\<ll>",
      "\\<mm>",
      "\\<nn>",
      "\\<oo>",
      "\\<pp>",
      "\\<qq>",
      "\\<rr>",
      "\\<ss>",
      "\\<tt>",
      "\\<uu>",
      "\\<vv>",
      "\\<ww>",
      "\\<xx>",
      "\\<yy>",
      "\\<zz>",
      "\\<alpha>",
      "\\<beta>",
      "\\<gamma>",
      "\\<delta>",
      "\\<epsilon>",
      "\\<zeta>",
      "\\<eta>",
      "\\<theta>",
      "\\<iota>",
      "\\<kappa>",
      (*"\\<lambda>", sic!*)
      "\\<mu>",
      "\\<nu>",
      "\\<xi>",
      "\\<pi>",
      "\\<rho>",
      "\\<sigma>",
      "\\<tau>",
      "\\<upsilon>",
      "\\<phi>",
      "\\<chi>",
      "\\<psi>",
      "\\<omega>",
      "\\<Gamma>",
      "\\<Delta>",
      "\\<Theta>",
      "\\<Lambda>",
      "\\<Xi>",
      "\\<Pi>",
      "\\<Sigma>",
      "\\<Upsilon>",
      "\\<Phi>",
      "\\<Psi>",
      "\\<Omega>"
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

val is_block_ctrl = member (op =) ["\\<^bsub>", "\\<^esub>", "\\<^bsup>", "\\<^esup>"];

fun is_quasi_letter s = let val k = kind s in k = Letter orelse k = Quasi end;
fun is_letdig s = let val k = kind s in k = Letter orelse k = Digit orelse k = Quasi end;



(** symbol input **)

(* source *)

local

fun is_plain s = is_ascii s andalso s <> "\^M" andalso s <> "\\";

fun is_utf8_trailer s = is_char s andalso 128 <= ord s andalso ord s < 192;

fun implode_pseudo_utf8 (cs as ["\192", c]) =
      if ord c < 160 then chr (ord c - 128) else implode cs
  | implode_pseudo_utf8 cs = implode cs;

val scan_encoded_newline =
  $$ "\^M" -- $$ "\n" >> K "\n" ||
  $$ "\^M" >> K "\n";

val scan_raw =
  Scan.this_string "raw:" ^^ (Scan.many raw_chr >> implode) ||
  Scan.this_string "raw" ^^ (Scan.many1 is_ascii_digit >> implode);

val scan_total =
  Scan.one is_plain ||
  Scan.one is_utf8 ::: Scan.many is_utf8_trailer >> implode_pseudo_utf8 ||
  scan_encoded_newline ||
  ($$ "\\" ^^ $$ "<" ^^
    (($$ "^" ^^ Scan.optional (scan_raw || scan_ascii_id) "" || Scan.optional scan_ascii_id "") ^^
      Scan.optional ($$ ">") "")) ||
  Scan.one not_eof;

in

fun source src = Source.source stopper (Scan.bulk scan_total) src;

end;


(* explode *)

local

fun no_explode [] = true
  | no_explode ("\\" :: "<" :: _) = false
  | no_explode ("\^M" :: _) = false
  | no_explode (c :: cs) = is_ascii c andalso no_explode cs;

in

fun sym_explode str =
  let val chs = raw_explode str in
    if no_explode chs then chs
    else Source.exhaust (source (Source.of_list chs))
  end;

end;


(* escape *)

val esc = fn s =>
  if is_char s then s
  else if is_utf8 s then translate_string (fn c => "\\" ^ string_of_int (ord c)) s
  else "\\" ^ s;

val escape = implode o map esc o sym_explode;



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

val explode_words = split_words o sym_explode;


(* blanks *)

fun strip_blanks s =
  sym_explode s
  |> take_prefix is_blank |> #2
  |> take_suffix is_blank |> #1
  |> implode;


(* bump string -- treat as base 26 or base 1 numbers *)

fun symbolic_end (_ :: "\\<^sub>" :: _) = true
  | symbolic_end ("'" :: ss) = symbolic_end ss
  | symbolic_end (s :: _) = raw_symbolic s
  | symbolic_end [] = false;

fun bump_init str =
  if symbolic_end (rev (sym_explode str)) then str ^ "'"
  else str ^ "a";

fun bump_string str =
  let
    fun bump [] = ["a"]
      | bump ("z" :: ss) = "a" :: bump ss
      | bump (s :: ss) =
          if is_char s andalso ord "a" <= ord s andalso ord s < ord "z"
          then chr (ord s + 1) :: ss
          else "a" :: s :: ss;

    val (ss, qs) = apfst rev (take_suffix is_quasi (sym_explode str));
    val ss' = if symbolic_end ss then "'" :: ss else bump ss;
  in implode (rev ss' @ qs) end;



(** symbol output **)

(* length *)

fun sym_len s =
  if not (is_printable s) then (0: int)
  else if String.isPrefix "\\<long" s then 2
  else if String.isPrefix "\\<Long" s then 2
  else 1;

fun sym_length ss = fold (fn s => fn n => sym_len s + n) ss 0;


(* print mode *)

val xsymbolsN = "xsymbols";

fun output s = (s, sym_length (sym_explode s));


(*final declarations of this structure!*)
val explode = sym_explode;
val length = sym_length;

end;
