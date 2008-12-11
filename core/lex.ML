
signature SIMPLE_LEXER =
sig
    exception MoreTokens
    type elem
    datatype token = INT of int | ID of string | LIT of string | SYM of string | NOOP
    val ident : elem list -> token * elem list
    val noop : elem list -> token * elem list
    val num : elem list -> token * elem list
    val space : elem list -> token * elem list
    val literal : elem list -> token * elem list
    val sym : elem -> elem list -> token * elem list
    val parse_with : (elem list -> 'a * elem list) -> string -> 'a
end

structure SimpleLexer : SIMPLE_LEXER =
struct
exception MoreTokens
type elem = Symbol.symbol
datatype token = INT of int | ID of string | LIT of string | SYM of string | NOOP
val wordchar = fn c => (Symbol.is_quasi_letter c) orelse (Symbol.is_digit c)

(* terminal definitions *)
val space = Scan.many Symbol.is_blank >> (fn _ => NOOP)
fun wrap_term t = space |-- t --| space
val num = wrap_term (Scan.many1
			 Symbol.is_digit
			 >> implode
			 >> (the o Int.fromString)
			 >> INT)

val ident = wrap_term ((Scan.one Symbol.is_letter ::: Scan.many wordchar)
			   >> implode
			   >> ID)
val sym = fn str => (wrap_term (Scan.this_string str >> (fn _ => SYM str)))
val noop = fn toks => (NOOP, toks)

(* string literal code stolen from isabella/ml_parse.ML *)
local
    val scan_escape =
	Scan.one (member (op =) (explode "\"\\abtnvfr")) ||
		 $$ "^" ^^ Scan.one (fn s => ord "@" <= ord s andalso ord s <= ord "_") ||
		 Scan.one Symbol.is_ascii_digit ^^
		 Scan.one Symbol.is_ascii_digit ^^
		 Scan.one Symbol.is_ascii_digit;
	
    val scan_str = Scan.one
		       (fn s => Symbol.is_printable s andalso
				s <> "\"" andalso s <> "\\") ||
		       ($$ "\\") ^^ scan_escape;
	
    (*val scan_gap = ($$ "\\") ^^ scan_blanks1 ^^ ($$ "\\");
    val scan_gaps = Scan.repeat scan_gap >> implode;*)
	
in
val literal = (($$ "\"") |-- 
	      (Scan.repeat scan_str >> implode) --|
	      ($$ "\"")) >> LIT;
    
end;

fun parse_with p str = let
    val (thing, toks) = p ((Symbol.explode str)@[Symbol.eof])
in if toks = [Symbol.eof] then thing else
 raise MoreTokens
end

end
