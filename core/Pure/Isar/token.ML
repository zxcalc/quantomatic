(*  Title:      Pure/Isar/token.ML
    Author:     Markus Wenzel, TU Muenchen

Outer token syntax for Isabelle/Isar.
*)

signature TOKEN =
sig
  datatype kind =
    (*immediate source*)
    Command | Keyword | Ident | Long_Ident | Sym_Ident | Var | Type_Ident | Type_Var | Nat |
    Float | Space |
    (*delimited content*)
    String | Alt_String | Verbatim | Cartouche | Comment |
    (*special content*)
    Error of string | Internal_Value | EOF
  val str_of_kind: kind -> string
  type file = {src_path: Path.T, lines: string list, digest: SHA1.digest, pos: Position.T}
  type T
  type src
  datatype value =
    Source of src |
    Literal of bool * Markup.T |
    Name of string * morphism |
    Typ of typ |
    Term of term |
    Fact of string option * thm list |
    Attribute of morphism -> attribute |
    Declaration of declaration |
    Files of file Exn.result list
  val name0: string -> value
  val pos_of: T -> Position.T
  val range_of: T list -> Position.range
  val src: xstring * Position.T -> T list -> src
  val src_checked: string * Position.T -> T list -> string * Markup.T -> src
  val name_of_src: src -> string * Position.T
  val args_of_src: src -> T list
  val range_of_src: src -> Position.T
  val check_src: Proof.context -> 'a Name_Space.table -> src -> src * 'a
  val eof: T
  val is_eof: T -> bool
  val not_eof: T -> bool
  val stopper: T Scan.stopper
  val kind_of: T -> kind
  val is_kind: kind -> T -> bool
  val is_command: T -> bool
  val is_name: T -> bool
  val keyword_with: (string -> bool) -> T -> bool
  val is_command_modifier: T -> bool
  val ident_with: (string -> bool) -> T -> bool
  val is_proper: T -> bool
  val is_improper: T -> bool
  val is_comment: T -> bool
  val is_begin_ignore: T -> bool
  val is_end_ignore: T -> bool
  val is_error: T -> bool
  val is_space: T -> bool
  val is_blank: T -> bool
  val is_newline: T -> bool
  val content_of: T -> string
  val input_of: T -> Input.source
  val inner_syntax_of: T -> string
  val keyword_markup: bool * Markup.T -> string -> Markup.T
  val completion_report: T -> Position.report_text list
  val reports: Keyword.keywords -> T -> Position.report_text list
  val markups: Keyword.keywords -> T -> Markup.T list
  val unparse: T -> string
  val unparse_src: src -> string list
  val print: T -> string
  val text_of: T -> string * string
  val get_files: T -> file Exn.result list
  val put_files: file Exn.result list -> T -> T
  val make_value: string -> value -> T
  val get_value: T -> value option
  val map_value: (value -> value) -> T -> T
  val reports_of_value: T -> Position.report list
  val map_values: (value -> value) -> src -> src
  val declare_maxidx: T -> Proof.context -> Proof.context
  val declare_maxidx_src: src -> Proof.context -> Proof.context
  val transform: morphism -> T -> T
  val transform_src: morphism -> src -> src
  val init_assignable: T -> T
  val init_assignable_src: src -> src
  val assign: value option -> T -> unit
  val closure: T -> T
  val closure_src: src -> src
  val pretty_value: Proof.context -> T -> Pretty.T
  val pretty_src: Proof.context -> src -> Pretty.T
  val ident_or_symbolic: string -> bool
  val source_proper: (T, 'a) Source.source -> (T, (T, 'a) Source.source) Source.source
  val source: Keyword.keywords ->
    Position.T -> (Symbol.symbol, 'a) Source.source -> (T,
      (Symbol_Pos.T, Position.T * (Symbol.symbol, 'a) Source.source) Source.source) Source.source
  val source_strict: Keyword.keywords ->
    Position.T -> (Symbol.symbol, 'a) Source.source -> (T,
      (Symbol_Pos.T, Position.T * (Symbol.symbol, 'a) Source.source) Source.source) Source.source
  val explode: Keyword.keywords -> Position.T -> string -> T list
  val make: (int * int) * string -> Position.T -> T * Position.T
  type 'a parser = T list -> 'a * T list
  type 'a context_parser = Context.generic * T list -> 'a * (Context.generic * T list)
  val read_no_commands: Keyword.keywords -> 'a parser -> Symbol_Pos.T list -> 'a list
  val read_antiq: Keyword.keywords -> 'a parser -> Symbol_Pos.T list * Position.T -> 'a
  val syntax_generic: 'a context_parser -> src -> Context.generic -> 'a * Context.generic
  val syntax: 'a context_parser -> src -> Proof.context -> 'a * Proof.context
end;

structure Token: TOKEN =
struct

(** tokens **)

(* token kind *)

datatype kind =
  (*immediate source*)
  Command | Keyword | Ident | Long_Ident | Sym_Ident | Var | Type_Ident | Type_Var | Nat |
  Float | Space |
  (*delimited content*)
  String | Alt_String | Verbatim | Cartouche | Comment |
  (*special content*)
  Error of string | Internal_Value | EOF;

val str_of_kind =
 fn Command => "command"
  | Keyword => "keyword"
  | Ident => "identifier"
  | Long_Ident => "long identifier"
  | Sym_Ident => "symbolic identifier"
  | Var => "schematic variable"
  | Type_Ident => "type variable"
  | Type_Var => "schematic type variable"
  | Nat => "natural number"
  | Float => "floating-point number"
  | Space => "white space"
  | String => "quoted string"
  | Alt_String => "back-quoted string"
  | Verbatim => "verbatim text"
  | Cartouche => "text cartouche"
  | Comment => "comment text"
  | Internal_Value => "internal value"
  | Error _ => "bad input"
  | EOF => "end-of-input";

val immediate_kinds =
  Vector.fromList
    [Command, Keyword, Ident, Long_Ident, Sym_Ident, Var, Type_Ident, Type_Var, Nat, Float, Space];

val delimited_kind = member (op =) [String, Alt_String, Verbatim, Cartouche, Comment];


(* datatype token *)

(*The value slot assigns an (optional) internal value to a token,
  usually as a side-effect of special scanner setup (see also
  args.ML).  Note that an assignable ref designates an intermediate
  state of internalization -- it is NOT meant to persist.*)

type file = {src_path: Path.T, lines: string list, digest: SHA1.digest, pos: Position.T};

datatype T = Token of (Symbol_Pos.text * Position.range) * (kind * string) * slot

and src =
  Src of
   {name: string * Position.T,
    args: T list,
    checked: (string * Markup.T) option}

and slot =
  Slot |
  Value of value option |
  Assignable of value option Unsynchronized.ref

and value =
  Source of src |
  Literal of bool * Markup.T |
  Name of string * morphism |
  Typ of typ |
  Term of term |
  Fact of string option * thm list |  (*optional name for dynamic fact, i.e. fact "variable"*)
  Attribute of morphism -> attribute |
  Declaration of declaration |
  Files of file Exn.result list;

fun name0 a = Name (a, Morphism.identity);


(* position *)

fun pos_of (Token ((_, (pos, _)), _, _)) = pos;
fun end_pos_of (Token ((_, (_, pos)), _, _)) = pos;

fun range_of (toks as tok :: _) =
      let val pos' = end_pos_of (List.last toks)
      in Position.range (pos_of tok) pos' end
  | range_of [] = Position.no_range;


(* src *)

fun src name args = Src {name = name, args = args, checked = NONE};
fun src_checked name args checked = Src {name = name, args = args, checked = SOME checked};

fun map_args f (Src {name, args, checked}) =
  Src {name = name, args = map f args, checked = checked};

fun name_of_src (Src {name, ...}) = name;
fun args_of_src (Src {args, ...}) = args;

fun range_of_src (Src {name = (_, pos), args, ...}) =
  if null args then pos
  else Position.set_range (pos, #2 (range_of args));

fun check_src _ table (src as Src {name = (name, _), checked = SOME _, ...}) =
      (src, Name_Space.get table name)
  | check_src ctxt table (Src {name = (xname, pos), args, checked = NONE}) =
      let
        val (name, x) = Name_Space.check (Context.Proof ctxt) table (xname, pos);
        val space = Name_Space.space_of_table table;
        val kind = Name_Space.kind_of space;
        val markup = Name_Space.markup space name;
      in (src_checked (name, pos) args (kind, markup), x) end;


(* stopper *)

fun mk_eof pos = Token (("", (pos, Position.none)), (EOF, ""), Slot);
val eof = mk_eof Position.none;

fun is_eof (Token (_, (EOF, _), _)) = true
  | is_eof _ = false;

val not_eof = not o is_eof;

val stopper =
  Scan.stopper (fn [] => eof | toks => mk_eof (end_pos_of (List.last toks))) is_eof;


(* kind of token *)

fun kind_of (Token (_, (k, _), _)) = k;
fun is_kind k (Token (_, (k', _), _)) = k = k';

val is_command = is_kind Command;

val is_name = is_kind Ident orf is_kind Sym_Ident orf is_kind String orf is_kind Nat;

fun keyword_with pred (Token (_, (Keyword, x), _)) = pred x
  | keyword_with _ _ = false;

val is_command_modifier = keyword_with (fn x => x = "private" orelse x = "qualified");

fun ident_with pred (Token (_, (Ident, x), _)) = pred x
  | ident_with _ _ = false;

fun is_proper (Token (_, (Space, _), _)) = false
  | is_proper (Token (_, (Comment, _), _)) = false
  | is_proper _ = true;

val is_improper = not o is_proper;

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

fun content_of (Token (_, (_, x), _)) = x;

fun input_of (Token ((source, range), (kind, _), _)) =
  Input.source (delimited_kind kind) source range;

fun inner_syntax_of tok =
  let val x = content_of tok
  in if YXML.detect x then x else Syntax.implode_input (input_of tok) end;


(* markup reports *)

local

val token_kind_markup =
 fn Var => (Markup.var, "")
  | Type_Ident => (Markup.tfree, "")
  | Type_Var => (Markup.tvar, "")
  | String => (Markup.string, "")
  | Alt_String => (Markup.alt_string, "")
  | Verbatim => (Markup.verbatim, "")
  | Cartouche => (Markup.cartouche, "")
  | Comment => (Markup.comment, "")
  | Error msg => (Markup.bad, msg)
  | _ => (Markup.empty, "");

fun keyword_reports tok = map (fn markup => ((pos_of tok, markup), ""));

fun command_markups keywords x =
  if Keyword.is_theory_end keywords x then [Markup.keyword2]
  else if Keyword.is_proof_asm keywords x then [Markup.keyword3]
  else if Keyword.is_improper keywords x then [Markup.keyword1, Markup.improper]
  else [Markup.keyword1];

in

fun keyword_markup (important, keyword) x =
  if important orelse Symbol.is_ascii_identifier x then keyword else Markup.delimiter;

fun completion_report tok =
  if is_kind Keyword tok
  then map (fn m => ((pos_of tok, m), "")) (Completion.suppress_abbrevs (content_of tok))
  else [];

fun reports keywords tok =
  if is_command tok then
    keyword_reports tok (command_markups keywords (content_of tok))
  else if is_kind Keyword tok then
    keyword_reports tok [keyword_markup (false, Markup.keyword2) (content_of tok)]
  else
    let val (m, text) = token_kind_markup (kind_of tok)
    in [((pos_of tok, m), text)] end;

fun markups keywords = map (#2 o #1) o reports keywords;

end;


(* unparse *)

fun unparse (Token (_, (kind, x), _)) =
  (case kind of
    String => Symbol_Pos.quote_string_qq x
  | Alt_String => Symbol_Pos.quote_string_bq x
  | Verbatim => enclose "{*" "*}" x
  | Cartouche => cartouche x
  | Comment => enclose "(*" "*)" x
  | EOF => ""
  | _ => x);

fun unparse_src (Src {args, ...}) = map unparse args;

fun print tok = Markup.markups (markups Keyword.empty_keywords tok) (unparse tok);

fun text_of tok =
  let
    val k = str_of_kind (kind_of tok);
    val ms = markups Keyword.empty_keywords tok;
    val s = unparse tok;
  in
    if s = "" then (k, "")
    else if size s < 40 andalso not (exists_string (fn c => c = "\n") s)
    then (k ^ " " ^ Markup.markups ms s, "")
    else (k, Markup.markups ms s)
  end;



(** associated values **)

(* inlined file content *)

fun get_files (Token (_, _, Value (SOME (Files files)))) = files
  | get_files _ = [];

fun put_files [] tok = tok
  | put_files files (Token (x, y, Slot)) = Token (x, y, Value (SOME (Files files)))
  | put_files _ tok = raise Fail ("Cannot put inlined files here" ^ Position.here (pos_of tok));


(* access values *)

fun make_value name v =
  Token ((name, Position.no_range), (Internal_Value, name), Value (SOME v));

fun get_value (Token (_, _, Value v)) = v
  | get_value _ = NONE;

fun map_value f (Token (x, y, Value (SOME v))) = Token (x, y, Value (SOME (f v)))
  | map_value _ tok = tok;

fun map_values f =
  (map_args o map_value) (fn Source src => Source (map_values f src) | x => f x);


(* reports of value *)

fun get_assignable_value (Token (_, _, Assignable r)) = ! r
  | get_assignable_value (Token (_, _, Value v)) = v
  | get_assignable_value _ = NONE;

fun reports_of_value tok =
  (case get_assignable_value tok of
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


(* maxidx *)

fun declare_maxidx tok =
  (case get_value tok of
    SOME (Source src) => declare_maxidx_src src
  | SOME (Typ T) => Variable.declare_maxidx (Term.maxidx_of_typ T)
  | SOME (Term t) => Variable.declare_maxidx (Term.maxidx_of_term t)
  | SOME (Fact (_, ths)) => fold (Variable.declare_maxidx o Thm.maxidx_of) ths
  | SOME (Attribute _) => I  (* FIXME !? *)
  | SOME (Declaration decl) =>
      (fn ctxt =>
        let val ctxt' = Context.proof_map (Morphism.form decl) ctxt
        in Variable.declare_maxidx (Variable.maxidx_of ctxt') ctxt end)
  | _ => I)
and declare_maxidx_src src = fold declare_maxidx (args_of_src src);


(* transform *)

fun transform phi =
  map_value (fn v =>
    (case v of
      Source src => Source (transform_src phi src)
    | Literal _ => v
    | Name (a, psi) => Name (a, psi $> phi)
    | Typ T => Typ (Morphism.typ phi T)
    | Term t => Term (Morphism.term phi t)
    | Fact (a, ths) => Fact (a, Morphism.fact phi ths)
    | Attribute att => Attribute (Morphism.transform phi att)
    | Declaration decl => Declaration (Morphism.transform phi decl)
    | Files _ => v))
and transform_src phi = map_args (transform phi);


(* static binding *)

(*1st stage: initialize assignable slots*)
fun init_assignable (Token (x, y, Slot)) = Token (x, y, Assignable (Unsynchronized.ref NONE))
  | init_assignable (tok as Token (_, _, Assignable r)) = (r := NONE; tok)
  | init_assignable tok = tok;

val init_assignable_src = map_args init_assignable;

(*2nd stage: assign values as side-effect of scanning*)
fun assign v (Token (_, _, Assignable r)) = r := v
  | assign _ _ = ();

(*3rd stage: static closure of final values*)
fun closure (Token (x, y, Assignable (Unsynchronized.ref v))) = Token (x, y, Value v)
  | closure tok = tok;

val closure_src = map_args closure;


(* pretty *)

fun pretty_value ctxt tok =
  (case get_value tok of
    SOME (Literal markup) =>
      let val x = content_of tok
      in Pretty.mark_str (keyword_markup markup x, x) end
  | SOME (Name (a, _)) => Pretty.str (quote a)
  | SOME (Typ T) => Syntax.pretty_typ ctxt T
  | SOME (Term t) => Syntax.pretty_term ctxt t
  | SOME (Fact (_, ths)) =>
      Pretty.enclose "(" ")" (Pretty.breaks (map (Pretty.backquote o Display.pretty_thm ctxt) ths))
  | _ => Pretty.marks_str (markups Keyword.empty_keywords tok, unparse tok));

fun pretty_src ctxt src =
  let
    val Src {name = (name, _), args, checked} = src;
    val prt_name =
      (case checked of
        NONE => Pretty.str name
      | SOME (_, markup) => Pretty.mark_str (markup, name));
    val prt_arg = pretty_value ctxt;
  in Pretty.block (Pretty.breaks (prt_name :: map prt_arg args)) end;



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
  Scan.one (fn (s, _) => s <> "*" andalso Symbol.not_eof s) >> single;

val scan_verbatim =
  Scan.ahead ($$ "{" -- $$ "*") |--
    !!! "unclosed verbatim text"
      ((Symbol_Pos.scan_pos --| $$ "{" --| $$ "*") --
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
  Token (Symbol_Pos.implode_range (pos1, pos2) ss, (k, Symbol_Pos.content ss), Slot);

fun scan_token keywords = !!! "bad input"
  (Symbol_Pos.scan_string_qq err_prefix >> token_range String ||
    Symbol_Pos.scan_string_bq err_prefix >> token_range Alt_String ||
    scan_verbatim >> token_range Verbatim ||
    scan_cartouche >> token_range Cartouche ||
    scan_comment >> token_range Comment ||
    scan_space >> token Space ||
    (Scan.max token_leq
      (Scan.max token_leq
        (Scan.literal (Keyword.major_keywords keywords) >> pair Command)
        (Scan.literal (Keyword.minor_keywords keywords) >> pair Keyword))
      (Lexicon.scan_longid >> pair Long_Ident ||
        Lexicon.scan_id >> pair Ident ||
        Lexicon.scan_var >> pair Var ||
        Lexicon.scan_tid >> pair Type_Ident ||
        Lexicon.scan_tvar >> pair Type_Var ||
        Lexicon.scan_float >> pair Float ||
        Lexicon.scan_nat >> pair Nat ||
        scan_symid >> pair Sym_Ident) >> uncurry token));

fun recover msg =
  (Symbol_Pos.recover_string_qq ||
    Symbol_Pos.recover_string_bq ||
    recover_verbatim ||
    Symbol_Pos.recover_cartouche ||
    Symbol_Pos.recover_comment ||
    Scan.one (Symbol.not_eof o Symbol_Pos.symbol) >> single)
  >> (single o token (Error msg));

in

fun source' strict keywords =
  let
    val scan_strict = Scan.bulk (scan_token keywords);
    val scan = if strict then scan_strict else Scan.recover scan_strict recover;
  in Source.source Symbol_Pos.stopper scan end;

fun source keywords pos src = Symbol_Pos.source pos src |> source' false keywords;
fun source_strict keywords pos src = Symbol_Pos.source pos src |> source' true keywords;

end;


(* explode *)

fun explode keywords pos =
  Source.of_string #>
  Symbol.source #>
  source keywords pos #>
  Source.exhaust;


(* make *)

fun make ((k, n), s) pos =
  let
    val pos' = Position.advance_offset n pos;
    val range = Position.range pos pos';
    val tok =
      if k < Vector.length immediate_kinds then
        Token ((s, range), (Vector.sub (immediate_kinds, k), s), Slot)
      else
        (case explode Keyword.empty_keywords pos s of
          [tok] => tok
        | _ => Token ((s, range), (Error (err_prefix ^ "exactly one token expected"), s), Slot))
  in (tok, pos') end;



(** parsers **)

type 'a parser = T list -> 'a * T list;
type 'a context_parser = Context.generic * T list -> 'a * (Context.generic * T list);


(* read source *)

fun read_no_commands keywords scan syms =
  Source.of_list syms
  |> source' true (Keyword.no_command_keywords keywords)
  |> source_proper
  |> Source.source stopper (Scan.error (Scan.bulk scan))
  |> Source.exhaust;

fun read_antiq keywords scan (syms, pos) =
  let
    fun err msg =
      cat_error msg ("Malformed antiquotation" ^ Position.here pos ^ ":\n" ^
        "@{" ^ Symbol_Pos.content syms ^ "}");
    val res = read_no_commands keywords scan syms handle ERROR msg => err msg;
  in (case res of [x] => x | _ => err "") end;


(* wrapped syntax *)

fun syntax_generic scan (Src {name = (name, pos), args = args0, checked}) context =
  let
    val args1 = map init_assignable args0;
    fun reported_text () =
      if Context_Position.is_visible_generic context then
        ((pos, Markup.operator) :: maps (reports_of_value o closure) args1)
        |> map (fn (p, m) => Position.reported_text p m "")
      else [];
  in
    (case Scan.error (Scan.finite' stopper (Scan.option scan)) (context, args1) of
      (SOME x, (context', [])) =>
        let val _ = Output.report (reported_text ())
        in (x, context') end
    | (_, (_, args2)) =>
        let
          val print_name =
            (case checked of
              NONE => quote name
            | SOME (kind, markup) => plain_words kind ^ " " ^ quote (Markup.markup markup name));
          val print_args =
            if null args2 then "" else ":\n  " ^ space_implode " " (map print args2);
        in
          error ("Bad arguments for " ^ print_name ^ Position.here pos ^ print_args ^
            Markup.markup_report (implode (reported_text ())))
        end)
  end;

fun syntax scan src = apsnd Context.the_proof o syntax_generic scan src o Context.Proof;

end;

type 'a parser = 'a Token.parser;
type 'a context_parser = 'a Token.context_parser;
