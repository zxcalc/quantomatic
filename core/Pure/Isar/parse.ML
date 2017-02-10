(*  Title:      Pure/Isar/parse.ML
    Author:     Markus Wenzel, TU Muenchen

Generic parsers for Isabelle/Isar outer syntax.
*)

signature PARSE =
sig
  val group: (unit -> string) -> (Token.T list -> 'a) -> Token.T list -> 'a
  val !!! : (Token.T list -> 'a) -> Token.T list -> 'a
  val !!!! : (Token.T list -> 'a) -> Token.T list -> 'a
  val not_eof: Token.T parser
  val token: 'a parser -> Token.T parser
  val range: 'a parser -> ('a * Position.range) parser
  val position: 'a parser -> ('a * Position.T) parser
  val input: 'a parser -> Input.source parser
  val inner_syntax: 'a parser -> string parser
  val command_: string parser
  val keyword: string parser
  val short_ident: string parser
  val long_ident: string parser
  val sym_ident: string parser
  val dots: string parser
  val minus: string parser
  val term_var: string parser
  val type_ident: string parser
  val type_var: string parser
  val number: string parser
  val float_number: string parser
  val string: string parser
  val alt_string: string parser
  val verbatim: string parser
  val cartouche: string parser
  val eof: string parser
  val command: string -> string parser
  val keyword_with: (string -> bool) -> string parser
  val keyword_markup: bool * Markup.T -> string -> string parser
  val keyword_improper: string -> string parser
  val $$$ : string -> string parser
  val reserved: string -> string parser
  val underscore: string parser
  val maybe: 'a parser -> 'a option parser
  val tag_name: string parser
  val tags: string list parser
  val opt_keyword: string -> bool parser
  val opt_bang: bool parser
  val begin: string parser
  val opt_begin: bool parser
  val nat: int parser
  val int: int parser
  val real: real parser
  val enum_positions: string -> 'a parser -> ('a list * Position.T list) parser
  val enum1_positions: string -> 'a parser -> ('a list * Position.T list) parser
  val enum: string -> 'a parser -> 'a list parser
  val enum1: string -> 'a parser -> 'a list parser
  val and_list: 'a parser -> 'a list parser
  val and_list1: 'a parser -> 'a list parser
  val enum': string -> 'a context_parser -> 'a list context_parser
  val enum1': string -> 'a context_parser -> 'a list context_parser
  val and_list': 'a context_parser -> 'a list context_parser
  val and_list1': 'a context_parser -> 'a list context_parser
  val list: 'a parser -> 'a list parser
  val list1: 'a parser -> 'a list parser
  val properties: Properties.T parser
  val name: string parser
  val binding: binding parser
  val embedded: string parser
  val text: string parser
  val path: string parser
  val theory_name: string parser
  val liberal_name: string parser
  val parname: string parser
  val parbinding: binding parser
  val class: string parser
  val sort: string parser
  val type_const: string parser
  val arity: (string * string list * string) parser
  val multi_arity: (string list * string list * string) parser
  val type_args: string list parser
  val type_args_constrained: (string * string option) list parser
  val typ: string parser
  val mixfix: mixfix parser
  val mixfix': mixfix parser
  val opt_mixfix: mixfix parser
  val opt_mixfix': mixfix parser
  val syntax_mode: Syntax.mode parser
  val where_: string parser
  val const_decl: (string * string * mixfix) parser
  val const_binding: (binding * string * mixfix) parser
  val params: (binding * string option * mixfix) list parser
  val vars: (binding * string option * mixfix) list parser
  val for_fixes: (binding * string option * mixfix) list parser
  val ML_source: Input.source parser
  val document_source: Input.source parser
  val const: string parser
  val term: string parser
  val prop: string parser
  val literal_fact: string parser
  val propp: (string * string list) parser
  val termp: (string * string list) parser
  val private: Position.T parser
  val qualified: Position.T parser
  val target: (string * Position.T) parser
  val opt_target: (string * Position.T) option parser
  val args: Token.T list parser
  val args1: (string -> bool) -> Token.T list parser
  val attribs: Token.src list parser
  val opt_attribs: Token.src list parser
  val thm_sel: Facts.interval list parser
  val thm: (Facts.ref * Token.src list) parser
  val thms1: (Facts.ref * Token.src list) list parser
end;

structure Parse: PARSE =
struct

(** error handling **)

(* group atomic parsers (no cuts!) *)

fun group s scan = scan || Scan.fail_with
  (fn [] => (fn () => s () ^ " expected,\nbut end-of-input was found")
    | tok :: _ =>
        (fn () =>
          (case Token.text_of tok of
            (txt, "") =>
              s () ^ " expected,\nbut " ^ txt ^ Position.here (Token.pos_of tok) ^
              " was found"
          | (txt1, txt2) =>
              s () ^ " expected,\nbut " ^ txt1 ^ Position.here (Token.pos_of tok) ^
              " was found:\n" ^ txt2)));


(* cut *)

fun cut kind scan =
  let
    fun get_pos [] = " (end-of-input)"
      | get_pos (tok :: _) = Position.here (Token.pos_of tok);

    fun err (toks, NONE) = (fn () => kind ^ get_pos toks)
      | err (toks, SOME msg) =
          (fn () =>
            let val s = msg () in
              if String.isPrefix kind s then s
              else kind ^ get_pos toks ^ ": " ^ s
            end);
  in Scan.!! err scan end;

fun !!! scan = cut "Outer syntax error" scan;
fun !!!! scan = cut "Corrupted outer syntax in presentation" scan;



(** basic parsers **)

(* tokens *)

fun RESET_VALUE atom = (*required for all primitive parsers*)
  Scan.ahead (Scan.one (K true)) -- atom >> (fn (arg, x) => (Token.assign NONE arg; x));


val not_eof = RESET_VALUE (Scan.one Token.not_eof);

fun token atom = Scan.ahead not_eof --| atom;

fun range scan = (Scan.ahead not_eof >> (Token.range_of o single)) -- scan >> Library.swap;
fun position scan = (Scan.ahead not_eof >> Token.pos_of) -- scan >> Library.swap;
fun input atom = Scan.ahead atom |-- not_eof >> Token.input_of;
fun inner_syntax atom = Scan.ahead atom |-- not_eof >> Token.inner_syntax_of;

fun kind k =
  group (fn () => Token.str_of_kind k)
    (RESET_VALUE (Scan.one (Token.is_kind k) >> Token.content_of));

val command_ = kind Token.Command;
val keyword = kind Token.Keyword;
val short_ident = kind Token.Ident;
val long_ident = kind Token.Long_Ident;
val sym_ident = kind Token.Sym_Ident;
val term_var = kind Token.Var;
val type_ident = kind Token.Type_Ident;
val type_var = kind Token.Type_Var;
val number = kind Token.Nat;
val float_number = kind Token.Float;
val string = kind Token.String;
val alt_string = kind Token.Alt_String;
val verbatim = kind Token.Verbatim;
val cartouche = kind Token.Cartouche;
val eof = kind Token.EOF;

fun command x =
  group (fn () => Token.str_of_kind Token.Command ^ " " ^ quote x)
    (RESET_VALUE (Scan.one (fn tok => Token.is_command tok andalso Token.content_of tok = x)))
  >> Token.content_of;

fun keyword_with pred = RESET_VALUE (Scan.one (Token.keyword_with pred) >> Token.content_of);

fun keyword_markup markup x =
  group (fn () => Token.str_of_kind Token.Keyword ^ " " ^ quote x)
    (Scan.ahead not_eof -- keyword_with (fn y => x = y))
  >> (fn (tok, x) => (Token.assign (SOME (Token.Literal markup)) tok; x));

val keyword_improper = keyword_markup (true, Markup.improper);
val $$$ = keyword_markup (false, Markup.quasi_keyword);

fun reserved x =
  group (fn () => "reserved identifier " ^ quote x)
    (RESET_VALUE (Scan.one (Token.ident_with (fn y => x = y)) >> Token.content_of));

val dots = sym_ident :-- (fn "\<dots>" => Scan.succeed () | _ => Scan.fail) >> #1;

val minus = sym_ident :-- (fn "-" => Scan.succeed () | _ => Scan.fail) >> #1;

val underscore = sym_ident :-- (fn "_" => Scan.succeed () | _ => Scan.fail) >> #1;
fun maybe scan = underscore >> K NONE || scan >> SOME;

val nat = number >> (#1 o Library.read_int o Symbol.explode);
val int = Scan.optional (minus >> K ~1) 1 -- nat >> op *;
val real = float_number >> Value.parse_real || int >> Real.fromInt;

val tag_name = group (fn () => "tag name") (short_ident || string);
val tags = Scan.repeat ($$$ "%" |-- !!! tag_name);

fun opt_keyword s = Scan.optional ($$$ "(" |-- !!! (($$$ s >> K true) --| $$$ ")")) false;
val opt_bang = Scan.optional ($$$ "!" >> K true) false;

val begin = $$$ "begin";
val opt_begin = Scan.optional (begin >> K true) false;


(* enumerations *)

fun enum1_positions sep scan =
  scan -- Scan.repeat (position ($$$ sep) -- !!! scan) >>
    (fn (x, ys) => (x :: map #2 ys, map (#2 o #1) ys));
fun enum_positions sep scan =
  enum1_positions sep scan || Scan.succeed ([], []);

fun enum1 sep scan = scan ::: Scan.repeat ($$$ sep |-- !!! scan);
fun enum sep scan = enum1 sep scan || Scan.succeed [];

fun enum1' sep scan = scan ::: Scan.repeat (Scan.lift ($$$ sep) |-- scan);
fun enum' sep scan = enum1' sep scan || Scan.succeed [];

fun and_list1 scan = enum1 "and" scan;
fun and_list scan = enum "and" scan;

fun and_list1' scan = enum1' "and" scan;
fun and_list' scan = enum' "and" scan;

fun list1 scan = enum1 "," scan;
fun list scan = enum "," scan;

val properties = $$$ "(" |-- !!! (list (string -- ($$$ "=" |-- string)) --| $$$ ")");


(* names and embedded content *)

val name =
  group (fn () => "name")
    (short_ident || long_ident || sym_ident || number || string);

val binding = position name >> Binding.make;

val embedded =
  group (fn () => "embedded content")
    (cartouche || string || short_ident || long_ident || sym_ident ||
      term_var || type_ident || type_var || number);

val text = group (fn () => "text") (embedded || verbatim);

val path = group (fn () => "file name/path specification") embedded;

val theory_name = group (fn () => "theory name") name;

val liberal_name = keyword_with Token.ident_or_symbolic || name;

val parname = Scan.optional ($$$ "(" |-- name --| $$$ ")") "";
val parbinding = Scan.optional ($$$ "(" |-- binding --| $$$ ")") Binding.empty;


(* type classes *)

val class = group (fn () => "type class") (inner_syntax embedded);

val sort = group (fn () => "sort") (inner_syntax embedded);

val type_const = group (fn () => "type constructor") (inner_syntax embedded);

val arity = type_const -- ($$$ "::" |-- !!!
  (Scan.optional ($$$ "(" |-- !!! (list1 sort --| $$$ ")")) [] -- sort)) >> Scan.triple2;

val multi_arity = and_list1 type_const -- ($$$ "::" |-- !!!
  (Scan.optional ($$$ "(" |-- !!! (list1 sort --| $$$ ")")) [] -- sort)) >> Scan.triple2;


(* types *)

val typ = group (fn () => "type") (inner_syntax embedded);

fun type_arguments arg =
  arg >> single ||
  $$$ "(" |-- !!! (list1 arg --| $$$ ")") ||
  Scan.succeed [];

val type_args = type_arguments type_ident;
val type_args_constrained = type_arguments (type_ident -- Scan.option ($$$ "::" |-- !!! sort));


(* mixfix annotations *)

local

val mfix =
  input string --
    !!! (Scan.optional ($$$ "[" |-- !!! (list nat --| $$$ "]")) [] -- Scan.optional nat 1000)
  >> (fn (sy, (ps, p)) => fn range => Mixfix (sy, ps, p, range));

val infx =
  $$$ "infix" |-- !!! (input string -- nat >> (fn (sy, p) => fn range => Infix (sy, p, range)));

val infxl =
  $$$ "infixl" |-- !!! (input string -- nat >> (fn (sy, p) => fn range => Infixl (sy, p, range)));

val infxr =
  $$$ "infixr" |-- !!! (input string -- nat >> (fn (sy, p) => fn range => Infixr (sy, p, range)));

val strcture = $$$ "structure" >> K Structure;

val binder =
  $$$ "binder" |--
    !!! (input string -- ($$$ "[" |-- nat --| $$$ "]" -- nat || nat >> (fn n => (n, n))))
  >> (fn (sy, (p, q)) => fn range => Binder (sy, p, q, range));

val mixfix_body = mfix || strcture || binder || infxl || infxr || infx;

fun annotation guard body =
  Scan.trace ($$$ "(" |-- guard (body --| $$$ ")"))
    >> (fn (mx, toks) => mx (Token.range_of toks));

fun opt_annotation guard body = Scan.optional (annotation guard body) NoSyn;

in

val mixfix = annotation !!! mixfix_body;
val mixfix' = annotation I mixfix_body;
val opt_mixfix = opt_annotation !!! mixfix_body;
val opt_mixfix' = opt_annotation I mixfix_body;

end;


(* syntax mode *)

val syntax_mode_spec =
  ($$$ "output" >> K ("", false)) || name -- Scan.optional ($$$ "output" >> K false) true;

val syntax_mode =
  Scan.optional ($$$ "(" |-- !!! (syntax_mode_spec --| $$$ ")")) Syntax.mode_default;


(* fixes *)

val where_ = $$$ "where";

val const_decl = name -- ($$$ "::" |-- !!! typ) -- opt_mixfix >> Scan.triple1;
val const_binding = binding -- ($$$ "::" |-- !!! typ) -- opt_mixfix >> Scan.triple1;

val param_mixfix = binding -- Scan.option ($$$ "::" |-- typ) -- mixfix' >> (single o Scan.triple1);

val params =
  (binding -- Scan.repeat binding) -- Scan.option ($$$ "::" |-- !!! (Scan.ahead typ -- embedded))
    >> (fn ((x, ys), T) =>
        (x, Option.map #1 T, NoSyn) :: map (fn y => (y, Option.map #2 T, NoSyn)) ys);

val vars = and_list1 (param_mixfix || params) >> flat;

val for_fixes = Scan.optional ($$$ "for" |-- !!! vars) [];


(* embedded source text *)

val ML_source = input (group (fn () => "ML source") text);
val document_source = input (group (fn () => "document source") text);


(* terms *)

val const = group (fn () => "constant") (inner_syntax embedded);
val term = group (fn () => "term") (inner_syntax embedded);
val prop = group (fn () => "proposition") (inner_syntax embedded);

val literal_fact = inner_syntax (group (fn () => "literal fact") (alt_string || cartouche));


(* patterns *)

val is_terms = Scan.repeat1 ($$$ "is" |-- term);
val is_props = Scan.repeat1 ($$$ "is" |-- prop);

val propp = prop -- Scan.optional ($$$ "(" |-- !!! (is_props --| $$$ ")")) [];
val termp = term -- Scan.optional ($$$ "(" |-- !!! (is_terms --| $$$ ")")) [];


(* target information *)

val private = position ($$$ "private") >> #2;
val qualified = position ($$$ "qualified") >> #2;

val target = ($$$ "(" -- $$$ "in") |-- !!! (position name --| $$$ ")");
val opt_target = Scan.option target;


(* arguments within outer syntax *)

local

val argument_kinds =
 [Token.Ident, Token.Long_Ident, Token.Sym_Ident, Token.Var, Token.Type_Ident, Token.Type_Var,
  Token.Nat, Token.Float, Token.String, Token.Alt_String, Token.Cartouche, Token.Verbatim];

fun arguments is_symid =
  let
    fun argument blk =
      group (fn () => "argument")
        (Scan.one (fn tok =>
          let val kind = Token.kind_of tok in
            member (op =) argument_kinds kind orelse
            Token.keyword_with is_symid tok orelse
            (blk andalso Token.keyword_with (fn s => s = ",") tok)
          end));

    fun args blk x = Scan.optional (args1 blk) [] x
    and args1 blk x =
      (Scan.repeats1 (Scan.repeat1 (argument blk) || argsp "(" ")" || argsp "[" "]")) x
    and argsp l r x = (token ($$$ l) ::: !!! (args true @@@ (token ($$$ r) >> single))) x;
  in (args, args1) end;

in

val args = #1 (arguments Token.ident_or_symbolic) false;
fun args1 is_symid = #2 (arguments is_symid) false;

end;


(* attributes *)

val attrib = token liberal_name ::: !!! args;
val attribs = $$$ "[" |-- list attrib --| $$$ "]";
val opt_attribs = Scan.optional attribs [];


(* theorem references *)

val thm_sel = $$$ "(" |-- list1
 (nat --| minus -- nat >> Facts.FromTo ||
  nat --| minus >> Facts.From ||
  nat >> Facts.Single) --| $$$ ")";

val thm =
  $$$ "[" |-- attribs --| $$$ "]" >> pair (Facts.named "") ||
  (literal_fact >> Facts.Fact ||
    position name -- Scan.option thm_sel >> Facts.Named) -- opt_attribs;

val thms1 = Scan.repeat1 thm;

end;
