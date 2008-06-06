(*  Title:      Pure/General/symbol.ML
    ID:         $Id: symbol.ML,v 1.81 2008/04/12 15:00:47 wenzelm Exp $
    Author:     Markus Wenzel, TU Muenchen

Generalized characters with infinitely many named symbols.
*)

signature SYMBOL =
sig
  type symbol
  val SOH: symbol
  val STX: symbol
  val ENQ: symbol
  val ACK: symbol
  val DEL: symbol
  val space: symbol
  val spaces: int -> string
  val is_char: symbol -> bool
  val is_symbolic: symbol -> bool
  val is_printable: symbol -> bool
  val is_utf8_trailer: symbol -> bool
  val eof: symbol
  val is_eof: symbol -> bool
  val stopper: symbol * (symbol -> bool)
  val sync: symbol
  val is_sync: symbol -> bool
  val malformed: symbol
  val end_malformed: symbol
  val separate_chars: string -> string
  val is_regular: symbol -> bool
  val is_ascii: symbol -> bool
  val is_ascii_letter: symbol -> bool
  val is_ascii_digit: symbol -> bool
  val is_ascii_hex: symbol -> bool
  val is_ascii_quasi: symbol -> bool
  val is_ascii_blank: symbol -> bool
  val is_ascii_lower: symbol -> bool
  val is_ascii_upper: symbol -> bool
  val to_ascii_lower: symbol -> symbol
  val to_ascii_upper: symbol -> symbol
  val is_raw: symbol -> bool
  val decode_raw: symbol -> string
  val encode_raw: string -> string
  datatype sym = Char of string | Sym of string | Ctrl of string | Raw of string
  val decode: symbol -> sym
  datatype kind = Letter | Digit | Quasi | Blank | Other
  val kind: symbol -> kind
  val is_letter: symbol -> bool
  val is_digit: symbol -> bool
  val is_quasi: symbol -> bool
  val is_blank: symbol -> bool
  val is_quasi_letter: symbol -> bool
  val is_letdig: symbol -> bool
  val is_ident: symbol list -> bool
  val beginning: int -> symbol list -> string
  val scanner: string -> (string list -> 'a * string list) -> symbol list -> 'a
  val scan_id: string list -> string * string list
  val source: bool -> (string, 'a) Source.source ->
    (symbol, (string, 'a) Source.source) Source.source
  val explode: string -> symbol list
  val escape: string -> string
  val strip_blanks: string -> string
  val bump_init: string -> string
  val bump_string: string -> string
  val length: symbol list -> int
  val xsymbolsN: string
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

val SOH = chr 1;
val STX = chr 2;
val ENQ = chr 5;
val ACK = chr 6;
val DEL = chr 127;

val space = chr 32;

local
  val small_spaces = Vector.tabulate (65, fn i => Library.replicate_string i space);
in
  fun spaces k =
    if k < 64 then Vector.sub (small_spaces, k)
    else Library.replicate_string (k div 64) (Vector.sub (small_spaces, 64)) ^
      Vector.sub (small_spaces, k mod 64);
end;

fun is_char s = size s = 1;

fun is_symbolic s =
  String.isPrefix "\\<" s andalso not (String.isPrefix "\\<^" s);

fun is_printable s =
  if is_char s then ord space <= ord s andalso ord s <= ord "~"
  else not (String.isPrefix "\\<^" s);

fun is_utf8_trailer s = is_char s andalso 128 <= ord s andalso ord s < 192;


(* input source control *)

val eof = "";
fun is_eof s = s = eof;
fun not_eof s = s <> eof;
val stopper = (eof, is_eof);

val sync = "\\<^sync>";
fun is_sync s = s = sync;

val malformed = "[[";
val end_malformed = "]]";

val separate_chars = explode #> space_implode space;
fun malformed_msg s = "Malformed symbolic character: " ^ quote (separate_chars s);

fun is_regular s =
  not_eof s andalso s <> sync andalso s <> malformed andalso s <> end_malformed;


(* ascii symbols *)

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
  fn " " => true | "\t" => true | "\n" => true | "\^K" => true | "\^L" => true | "\^M" => true
    | _ => false;

fun is_ascii_lower s = is_char s andalso (ord "a" <= ord s andalso ord s <= ord "z");
fun is_ascii_upper s = is_char s andalso (ord "A" <= ord s andalso ord s <= ord "Z");

fun to_ascii_lower s = if is_ascii_upper s then chr (ord s + ord "a" - ord "A") else s;
fun to_ascii_upper s = if is_ascii_lower s then chr (ord s + ord "A" - ord "a") else s;


(* encode_raw *)

fun raw_chr c =
  ord space <= ord c andalso ord c <= ord "~" andalso c <> "." andalso c <> ">"
  orelse ord c >= 128;

fun encode_raw str =
  let
    val raw0 = enclose "\\<^raw:" ">";
    val raw1 = raw0 o implode;
    val raw2 = enclose "\\<^raw" ">" o string_of_int o ord;

    fun encode cs = enc (Library.take_prefix raw_chr cs)
    and enc ([], []) = []
      | enc (cs, []) = [raw1 cs]
      | enc ([], d :: ds) = raw2 d :: encode ds
      | enc (cs, d :: ds) = raw1 cs :: raw2 d :: encode ds;
  in
    if exists_string (not o raw_chr) str then implode (encode (explode str))
    else raw0 str
  end;


(* diagnostics *)

fun beginning n cs =
  let
    val drop_blanks = #1 o Library.take_suffix is_ascii_blank;
    val all_cs = drop_blanks cs;
    val dots = if length all_cs > n then " ..." else "";
  in
    (drop_blanks (Library.take (n, all_cs))
      |> map (fn c => if is_ascii_blank c then space else c)
      |> implode) ^ dots
  end;


(* decode_raw *)

fun is_raw s =
  String.isPrefix "\\<^raw" s andalso String.isSuffix ">" s;

fun decode_raw s =
  if not (is_raw s) then error (malformed_msg s)
  else if String.isPrefix "\\<^raw:" s then String.substring (s, 7, size s - 8)
  else chr (#1 (Library.read_int (explode (String.substring (s, 6, size s - 7)))));


(* symbol variants *)

datatype sym = Char of string | Sym of string | Ctrl of string | Raw of string;

fun decode s =
  if is_char s then Char s
  else if is_raw s then Raw (decode_raw s)
  else if String.isPrefix "\\<^" s then Ctrl (String.substring (s, 3, size s - 4))
  else if String.isPrefix "\\<" s then Sym (String.substring (s, 2, size s - 3))
  else error (malformed_msg s);


(* standard symbol kinds *)

datatype kind = Letter | Digit | Quasi | Blank | Other;

local
  val symbol_kinds = Symtab.make
   [("\\<A>", Letter),
    ("\\<B>", Letter),
    ("\\<C>", Letter),
    ("\\<D>", Letter),
    ("\\<E>", Letter),
    ("\\<F>", Letter),
    ("\\<G>", Letter),
    ("\\<H>", Letter),
    ("\\<I>", Letter),
    ("\\<J>", Letter),
    ("\\<K>", Letter),
    ("\\<L>", Letter),
    ("\\<M>", Letter),
    ("\\<N>", Letter),
    ("\\<O>", Letter),
    ("\\<P>", Letter),
    ("\\<Q>", Letter),
    ("\\<R>", Letter),
    ("\\<S>", Letter),
    ("\\<T>", Letter),
    ("\\<U>", Letter),
    ("\\<V>", Letter),
    ("\\<W>", Letter),
    ("\\<X>", Letter),
    ("\\<Y>", Letter),
    ("\\<Z>", Letter),
    ("\\<a>", Letter),
    ("\\<b>", Letter),
    ("\\<c>", Letter),
    ("\\<d>", Letter),
    ("\\<e>", Letter),
    ("\\<f>", Letter),
    ("\\<g>", Letter),
    ("\\<h>", Letter),
    ("\\<i>", Letter),
    ("\\<j>", Letter),
    ("\\<k>", Letter),
    ("\\<l>", Letter),
    ("\\<m>", Letter),
    ("\\<n>", Letter),
    ("\\<o>", Letter),
    ("\\<p>", Letter),
    ("\\<q>", Letter),
    ("\\<r>", Letter),
    ("\\<s>", Letter),
    ("\\<t>", Letter),
    ("\\<u>", Letter),
    ("\\<v>", Letter),
    ("\\<w>", Letter),
    ("\\<x>", Letter),
    ("\\<y>", Letter),
    ("\\<z>", Letter),
    ("\\<AA>", Letter),
    ("\\<BB>", Letter),
    ("\\<CC>", Letter),
    ("\\<DD>", Letter),
    ("\\<EE>", Letter),
    ("\\<FF>", Letter),
    ("\\<GG>", Letter),
    ("\\<HH>", Letter),
    ("\\<II>", Letter),
    ("\\<JJ>", Letter),
    ("\\<KK>", Letter),
    ("\\<LL>", Letter),
    ("\\<MM>", Letter),
    ("\\<NN>", Letter),
    ("\\<OO>", Letter),
    ("\\<PP>", Letter),
    ("\\<QQ>", Letter),
    ("\\<RR>", Letter),
    ("\\<SS>", Letter),
    ("\\<TT>", Letter),
    ("\\<UU>", Letter),
    ("\\<VV>", Letter),
    ("\\<WW>", Letter),
    ("\\<XX>", Letter),
    ("\\<YY>", Letter),
    ("\\<ZZ>", Letter),
    ("\\<aa>", Letter),
    ("\\<bb>", Letter),
    ("\\<cc>", Letter),
    ("\\<dd>", Letter),
    ("\\<ee>", Letter),
    ("\\<ff>", Letter),
    ("\\<gg>", Letter),
    ("\\<hh>", Letter),
    ("\\<ii>", Letter),
    ("\\<jj>", Letter),
    ("\\<kk>", Letter),
    ("\\<ll>", Letter),
    ("\\<mm>", Letter),
    ("\\<nn>", Letter),
    ("\\<oo>", Letter),
    ("\\<pp>", Letter),
    ("\\<qq>", Letter),
    ("\\<rr>", Letter),
    ("\\<ss>", Letter),
    ("\\<tt>", Letter),
    ("\\<uu>", Letter),
    ("\\<vv>", Letter),
    ("\\<ww>", Letter),
    ("\\<xx>", Letter),
    ("\\<yy>", Letter),
    ("\\<zz>", Letter),
    ("\\<alpha>", Letter),
    ("\\<beta>", Letter),
    ("\\<gamma>", Letter),
    ("\\<delta>", Letter),
    ("\\<epsilon>", Letter),
    ("\\<zeta>", Letter),
    ("\\<eta>", Letter),
    ("\\<theta>", Letter),
    ("\\<iota>", Letter),
    ("\\<kappa>", Letter),
    ("\\<lambda>", Other),      (*sic!*)
    ("\\<mu>", Letter),
    ("\\<nu>", Letter),
    ("\\<xi>", Letter),
    ("\\<pi>", Letter),
    ("\\<rho>", Letter),
    ("\\<sigma>", Letter),
    ("\\<tau>", Letter),
    ("\\<upsilon>", Letter),
    ("\\<phi>", Letter),
    ("\\<chi>", Letter),
    ("\\<psi>", Letter),
    ("\\<omega>", Letter),
    ("\\<Gamma>", Letter),
    ("\\<Delta>", Letter),
    ("\\<Theta>", Letter),
    ("\\<Lambda>", Letter),
    ("\\<Xi>", Letter),
    ("\\<Pi>", Letter),
    ("\\<Sigma>", Letter),
    ("\\<Upsilon>", Letter),
    ("\\<Phi>", Letter),
    ("\\<Psi>", Letter),
    ("\\<Omega>", Letter),
    ("\\<^isub>", Letter),
    ("\\<^isup>", Letter),
    ("\\<spacespace>", Blank)];
in
  fun kind s =
    if is_ascii_letter s then Letter
    else if is_ascii_digit s then Digit
    else if is_ascii_quasi s then Quasi
    else if is_ascii_blank s then Blank
    else if is_char s then Other
    else the_default Other (Symtab.lookup symbol_kinds s);
end;

fun is_letter s = kind s = Letter;
fun is_digit s = kind s = Digit;
fun is_quasi s = kind s = Quasi;
fun is_blank s = kind s = Blank;

fun is_quasi_letter s = let val k = kind s in k = Letter orelse k = Quasi end;
fun is_letdig s = let val k = kind s in k = Letter orelse k = Digit orelse k = Quasi end;

fun is_ident [] = false
  | is_ident (c :: cs) = is_letter c andalso forall is_letdig cs;



(** symbol input **)

(* scanning through symbols *)

fun scanner msg scan chs =
  let
    fun message (cs, NONE) = msg ^ ": " ^ quote (beginning 10 cs)
      | message (cs, SOME msg') = msg ^ ", " ^ msg' ^ ": " ^ quote (beginning 10 cs);
    val fin_scan = Scan.error (Scan.finite stopper (!! message scan));
  in
    (case fin_scan chs of
      (result, []) => result
    | (_, rest) => error (message (rest, NONE)))
  end;

val scan_id = Scan.one is_letter ^^ (Scan.many is_letdig >> implode);


(* source *)

local

fun is_plain s = s <> "\^M" andalso s <> "\\" andalso not_eof s;

val scan_encoded_newline =
  $$ "\^M" -- $$ "\n" >> K "\n" ||
  $$ "\^M" >> K "\n" ||
  $$ "\\" -- Scan.optional ($$ "\\") "" -- Scan.this_string "<^newline>" >> K "\n";

val scan_raw =
  Scan.this_string "raw:" ^^ (Scan.many raw_chr >> implode) ||
  Scan.this_string "raw" ^^ (Scan.many1 is_ascii_digit >> implode);

val scan =
  Scan.one is_plain ||
  scan_encoded_newline ||
  (($$ "\\" --| Scan.optional ($$ "\\") "") ^^ $$ "<" ^^
    !! (fn (cs, _) => malformed_msg (beginning 10 ("\\" :: "<" :: cs)))
      (($$ "^" ^^ (scan_raw || scan_id) || scan_id) ^^ $$ ">")) ||
  Scan.one not_eof;

val recover =
  Scan.many (fn s => not (is_blank s) andalso s <> "\"" andalso s <> "`" andalso not_eof s)
    >> (fn ss => malformed :: ss @ [end_malformed]);

in

fun source do_recover src =
  Source.source stopper (Scan.bulk scan)
    (if do_recover then SOME (false, K recover) else NONE) src;

end;


(* explode *)

local

fun no_explode [] = true
  | no_explode ("\\" :: "<" :: _) = false
  | no_explode ("\^M" :: _) = false
  | no_explode (_ :: cs) = no_explode cs;

in

fun sym_explode str =
  let val chs = explode str in
    if no_explode chs then chs
    else Source.exhaust (source false (Source.of_list chs))
  end;

end;


(* escape *)

val escape = implode o map (fn s => if is_char s then s else "\\" ^ s) o sym_explode;


(* blanks *)

fun strip_blanks s =
  sym_explode s
  |> Library.take_prefix is_blank |> #2
  |> Library.take_suffix is_blank |> #1
  |> implode;


(* bump string -- treat as base 26 or base 1 numbers *)

fun symbolic_end (_ :: "\\<^isub>" :: _) = true
  | symbolic_end (_ :: "\\<^isup>" :: _) = true
  | symbolic_end (s :: _) = is_symbolic s
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

    val (ss, qs) = apfst rev (Library.take_suffix is_quasi (sym_explode str));
    val ss' = if symbolic_end ss then "'" :: ss else bump ss;
  in implode (rev ss' @ qs) end;



(** xsymbols **)

val xsymbolsN = "xsymbols";

fun sym_len s =
  if not (is_printable s) then (0: int)
  else if String.isPrefix "\\<long" s then 2
  else if String.isPrefix "\\<Long" s then 2
  else if s = "\\<spacespace>" then 2
  else 1;

fun sym_length ss = fold (fn s => fn n => sym_len s + n) ss 0;

(*final declarations of this structure!*)
val length = sym_length;
val explode = sym_explode;

end;
