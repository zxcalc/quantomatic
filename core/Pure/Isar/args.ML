(*  Title:      Pure/Isar/args.ML
    Author:     Markus Wenzel, TU Muenchen

Quasi-inner syntax based on outer tokens: concrete argument syntax of
attributes, methods etc.
*)

signature ARGS =
sig
  val context: Proof.context context_parser
  val theory: theory context_parser
  val $$$ : string -> string parser
  val add: string parser
  val del: string parser
  val colon: string parser
  val query: string parser
  val bang: string parser
  val query_colon: string parser
  val bang_colon: string parser
  val parens: 'a parser -> 'a parser
  val bracks: 'a parser -> 'a parser
  val mode: string -> bool parser
  val maybe: 'a parser -> 'a option parser
  val cartouche_inner_syntax: string parser
  val cartouche_input: Input.source parser
  val text_token: Token.T parser
  val text_input: Input.source parser
  val text: string parser
  val name_inner_syntax: string parser
  val name_input: Input.source parser
  val name: string parser
  val binding: binding parser
  val alt_name: string parser
  val symbol: string parser
  val liberal_name: string parser
  val var: indexname parser
  val internal_source: Token.src parser
  val internal_name: (string * morphism) parser
  val internal_typ: typ parser
  val internal_term: term parser
  val internal_fact: thm list parser
  val internal_attribute: (morphism -> attribute) parser
  val internal_declaration: declaration parser
  val named_source: (Token.T -> Token.src) -> Token.src parser
  val named_typ: (string -> typ) -> typ parser
  val named_term: (string -> term) -> term parser
  val named_fact: (string -> string option * thm list) -> thm list parser
  val named_attribute: (string * Position.T -> morphism -> attribute) ->
    (morphism -> attribute) parser
  val text_declaration: (Input.source -> declaration) -> declaration parser
  val cartouche_declaration: (Input.source -> declaration) -> declaration parser
  val typ_abbrev: typ context_parser
  val typ: typ context_parser
  val term: term context_parser
  val term_pattern: term context_parser
  val term_abbrev: term context_parser
  val prop: term context_parser
  val type_name: {proper: bool, strict: bool} -> string context_parser
  val const: {proper: bool, strict: bool} -> string context_parser
  val goal_spec: ((int -> tactic) -> tactic) context_parser
  val attribs: (xstring * Position.T -> string) -> Token.src list parser
  val opt_attribs: (xstring * Position.T -> string) -> Token.src list parser
  val checked_name: (xstring * Position.T -> string) -> string parser
end;

structure Args: ARGS =
struct

(** argument scanners **)

(* context *)

fun context x = (Scan.state >> Context.proof_of) x;
fun theory x = (Scan.state >> Context.theory_of) x;


(* basic *)

val ident = Parse.token
  (Parse.short_ident || Parse.long_ident || Parse.sym_ident || Parse.term_var ||
    Parse.type_ident || Parse.type_var || Parse.number);

val string = Parse.token Parse.string;
val alt_string = Parse.token (Parse.alt_string || Parse.cartouche);
val symbolic = Parse.token (Parse.keyword_with Token.ident_or_symbolic);

fun $$$ x =
  (ident || Parse.token Parse.keyword) :|-- (fn tok =>
    let val y = Token.content_of tok in
      if x = y
      then (Token.assign (SOME (Token.Literal (false, Markup.quasi_keyword))) tok; Scan.succeed x)
      else Scan.fail
    end);

val named = ident || string;

val add = $$$ "add";
val del = $$$ "del";
val colon = $$$ ":";
val query = $$$ "?";
val bang = $$$ "!";
val query_colon = $$$ "?" ^^ $$$ ":";
val bang_colon = $$$ "!" ^^ $$$ ":";

fun parens scan = $$$ "(" |-- scan --| $$$ ")";
fun bracks scan = $$$ "[" |-- scan --| $$$ "]";
fun mode s = Scan.optional (parens ($$$ s) >> K true) false;
fun maybe scan = $$$ "_" >> K NONE || scan >> SOME;

val cartouche = Parse.token Parse.cartouche;
val cartouche_inner_syntax = cartouche >> Token.inner_syntax_of;
val cartouche_input = cartouche >> Token.input_of;

val text_token = named || Parse.token (Parse.verbatim || Parse.cartouche);
val text_input = text_token >> Token.input_of;
val text = text_token >> Token.content_of;

val name_inner_syntax = named >> Token.inner_syntax_of;
val name_input = named >> Token.input_of;

val name = named >> Token.content_of;
val binding = Parse.position name >> Binding.make;
val alt_name = alt_string >> Token.content_of;
val symbol = symbolic >> Token.content_of;
val liberal_name = symbol || name;

val var = (ident >> Token.content_of) :|-- (fn x =>
  (case Lexicon.read_variable x of SOME v => Scan.succeed v | NONE => Scan.fail));


(* values *)

fun value dest = Scan.some (fn arg =>
  (case Token.get_value arg of SOME v => (SOME (dest v) handle Match => NONE) | NONE => NONE));

fun evaluate mk eval arg =
  let val x = eval arg in (Token.assign (SOME (mk x)) arg; x) end;

val internal_source = value (fn Token.Source src => src);
val internal_name = value (fn Token.Name a => a);
val internal_typ = value (fn Token.Typ T => T);
val internal_term = value (fn Token.Term t => t);
val internal_fact = value (fn Token.Fact (_, ths) => ths);
val internal_attribute = value (fn Token.Attribute att => att);
val internal_declaration = value (fn Token.Declaration decl => decl);

fun named_source read = internal_source || named >> evaluate Token.Source read;

fun named_typ read = internal_typ || named >> evaluate Token.Typ (read o Token.inner_syntax_of);
fun named_term read = internal_term || named >> evaluate Token.Term (read o Token.inner_syntax_of);

fun named_fact get =
  internal_fact ||
  named >> evaluate Token.Fact (get o Token.content_of) >> #2 ||
  alt_string >> evaluate Token.Fact (get o Token.inner_syntax_of) >> #2;

fun named_attribute att =
  internal_attribute ||
  named >> evaluate Token.Attribute (fn tok => att (Token.content_of tok, Token.pos_of tok));

fun text_declaration read =
  internal_declaration || text_token >> evaluate Token.Declaration (read o Token.input_of);

fun cartouche_declaration read =
  internal_declaration || cartouche >> evaluate Token.Declaration (read o Token.input_of);


(* terms and types *)

val typ_abbrev = Scan.peek (named_typ o Proof_Context.read_typ_abbrev o Context.proof_of);
val typ = Scan.peek (named_typ o Syntax.read_typ o Context.proof_of);
val term = Scan.peek (named_term o Syntax.read_term o Context.proof_of);
val term_pattern = Scan.peek (named_term o Proof_Context.read_term_pattern o Context.proof_of);
val term_abbrev = Scan.peek (named_term o Proof_Context.read_term_abbrev o Context.proof_of);
val prop = Scan.peek (named_term o Syntax.read_prop o Context.proof_of);


(* type and constant names *)

fun type_name flags =
  Scan.peek (named_typ o Proof_Context.read_type_name flags o Context.proof_of)
  >> (fn Type (c, _) => c | TFree (a, _) => a | _ => "");

fun const flags =
  Scan.peek (named_term o Proof_Context.read_const flags o Context.proof_of)
  >> (fn Const (c, _) => c | Free (x, _) => x | _ => "");


(* improper method arguments *)

val from_to =
  Parse.nat -- ($$$ "-" |-- Parse.nat) >> (fn (i, j) => fn tac => Seq.INTERVAL tac i j) ||
  Parse.nat --| $$$ "-" >> (fn i => fn tac => fn st => Seq.INTERVAL tac i (Thm.nprems_of st) st) ||
  Parse.nat >> (fn i => fn tac => tac i) ||
  $$$ "!" >> K ALLGOALS;

val goal = Parse.keyword_improper "[" |-- Parse.!!! (from_to --| Parse.keyword_improper "]");
fun goal_spec x = Scan.lift (Scan.optional goal (fn tac => tac 1)) x;


(* attributes *)

fun attribs check =
  let
    fun intern tok = check (Token.content_of tok, Token.pos_of tok);
    val attrib_name = internal_name >> #1 || (symbolic || named) >> evaluate Token.name0 intern;
    val attrib = Parse.position attrib_name -- Parse.!!! Parse.args >> uncurry Token.src;
  in $$$ "[" |-- Parse.!!! (Parse.list attrib --| $$$ "]") end;

fun opt_attribs check = Scan.optional (attribs check) [];


(* checked name *)

fun checked_name check =
  let fun intern tok = check (Token.content_of tok, Token.pos_of tok);
  in internal_name >> #1 || Parse.token Parse.xname >> evaluate Token.name0 intern end;

end;
