(*  Title:      Pure/Isar/args.ML
    Author:     Markus Wenzel, TU Muenchen

Parsing with implicit value assignment.  Concrete argument syntax of
attributes, methods etc.
*)

signature ARGS =
sig
  type src
  val src: xstring * Position.T -> Token.T list -> src
  val name_of_src: src -> string * Position.T
  val range_of_src: src -> Position.T
  val unparse_src: src -> string list
  val pretty_src: Proof.context -> src -> Pretty.T
  val check_src: Proof.context -> 'a Name_Space.table -> src -> src * 'a
  val transform_values: morphism -> src -> src
  val init_assignable: src -> src
  val closure: src -> src
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
  val cartouche_source_position: Symbol_Pos.source parser
  val text_source_position: Symbol_Pos.source parser
  val text: string parser
  val name_inner_syntax: string parser
  val name_source_position: Symbol_Pos.source parser
  val name: string parser
  val binding: binding parser
  val alt_name: string parser
  val symbol: string parser
  val liberal_name: string parser
  val var: indexname parser
  val internal_text: string parser
  val internal_typ: typ parser
  val internal_term: term parser
  val internal_fact: thm list parser
  val internal_attribute: (morphism -> attribute) parser
  val named_text: (string -> string) -> string parser
  val named_typ: (string -> typ) -> typ parser
  val named_term: (string -> term) -> term parser
  val named_fact: (string -> thm list) -> thm list parser
  val named_attribute:
    (string * Position.T -> morphism -> attribute) -> (morphism -> attribute) parser
  val typ_abbrev: typ context_parser
  val typ: typ context_parser
  val term: term context_parser
  val term_pattern: term context_parser
  val term_abbrev: term context_parser
  val prop: term context_parser
  val type_name: {proper: bool, strict: bool} -> string context_parser
  val const: {proper: bool, strict: bool} -> string context_parser
  val goal_spec: ((int -> tactic) -> tactic) context_parser
  val attribs: (xstring * Position.T -> string) -> src list parser
  val opt_attribs: (xstring * Position.T -> string) -> src list parser
  val syntax_generic: 'a context_parser -> src -> Context.generic -> 'a * Context.generic
  val syntax: 'a context_parser -> src -> Proof.context -> 'a * Proof.context
end;

structure Args: ARGS =
struct

(** datatype src **)

datatype src =
  Src of
   {name: string * Position.T,
    args: Token.T list,
    output_info: (string * Markup.T) option};

fun src name args = Src {name = name, args = args, output_info = NONE};

fun name_of_src (Src {name, ...}) = name;

fun range_of_src (Src {name = (_, pos), args, ...}) =
  if null args then pos
  else Position.set_range (pos, #2 (Token.range_of args));

fun unparse_src (Src {args, ...}) = map Token.unparse args;

fun pretty_src ctxt src =
  let
    val Src {name = (name, _), args, output_info} = src;
    val prt_name =
      (case output_info of
        NONE => Pretty.str name
      | SOME (_, markup) => Pretty.mark_str (markup, name));
    val prt_thm = Pretty.backquote o Display.pretty_thm ctxt;
    fun prt_arg arg =
      (case Token.get_value arg of
        SOME (Token.Literal markup) =>
          let val x = Token.content_of arg
          in Pretty.mark_str (Token.keyword_markup markup x, x) end
      | SOME (Token.Text s) => Pretty.str (quote s)
      | SOME (Token.Typ T) => Syntax.pretty_typ ctxt T
      | SOME (Token.Term t) => Syntax.pretty_term ctxt t
      | SOME (Token.Fact ths) => Pretty.enclose "(" ")" (Pretty.breaks (map prt_thm ths))
      | _ => Pretty.mark_str (Token.markup arg, Token.unparse arg));
  in Pretty.block (Pretty.breaks (prt_name :: map prt_arg args)) end;


(* check *)

fun check_src ctxt table (Src {name = (xname, pos), args, output_info = _}) =
  let
    val (name, x) = Name_Space.check (Context.Proof ctxt) table (xname, pos);
    val space = Name_Space.space_of_table table;
    val kind = Name_Space.kind_of space;
    val markup = Name_Space.markup space name;
  in (Src {name = (name, pos), args = args, output_info = SOME (kind, markup)}, x) end;


(* values *)

fun map_args f (Src {name, args, output_info}) =
  Src {name = name, args = map f args, output_info = output_info};

fun transform_values phi = map_args (Token.map_value
  (fn Token.Typ T => Token.Typ (Morphism.typ phi T)
    | Token.Term t => Token.Term (Morphism.term phi t)
    | Token.Fact ths => Token.Fact (Morphism.fact phi ths)
    | Token.Attribute att => Token.Attribute (Morphism.transform phi att)
    | tok => tok));

val init_assignable = map_args Token.init_assignable;
val closure = map_args Token.closure;



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
val cartouche_source_position = cartouche >> Token.source_position_of;

val text_token = named || Parse.token (Parse.verbatim || Parse.cartouche);
val text_source_position = text_token >> Token.source_position_of;
val text = text_token >> Token.content_of;

val name_inner_syntax = named >> Token.inner_syntax_of;
val name_source_position = named >> Token.source_position_of;

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

val internal_text = value (fn Token.Text s => s);
val internal_typ = value (fn Token.Typ T => T);
val internal_term = value (fn Token.Term t => t);
val internal_fact = value (fn Token.Fact ths => ths);
val internal_attribute = value (fn Token.Attribute att => att);

fun named_text intern = internal_text || named >> evaluate Token.Text (intern o Token.content_of);
fun named_typ readT = internal_typ || named >> evaluate Token.Typ (readT o Token.inner_syntax_of);
fun named_term read = internal_term || named >> evaluate Token.Term (read o Token.inner_syntax_of);

fun named_fact get = internal_fact || named >> evaluate Token.Fact (get o Token.content_of) ||
  alt_string >> evaluate Token.Fact (get o Token.inner_syntax_of);

fun named_attribute att =
  internal_attribute ||
  named >> evaluate Token.Attribute (fn tok => att (Token.content_of tok, Token.pos_of tok));


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
    val attrib_name = internal_text || (symbolic || named) >> evaluate Token.Text intern;
    val attrib = Parse.position attrib_name -- Parse.!!! Parse.args >> uncurry src;
  in $$$ "[" |-- Parse.!!! (Parse.list attrib --| $$$ "]") end;

fun opt_attribs check = Scan.optional (attribs check) [];



(** syntax wrapper **)

fun syntax_generic scan (Src {name = (name, pos), args = args0, output_info}) context =
  let
    val args1 = map Token.init_assignable args0;
    fun reported_text () =
      if Context_Position.is_visible_generic context then
        ((pos, Markup.operator) :: maps (Token.reports_of_value o Token.closure) args1)
        |> map (fn (p, m) => Position.reported_text p m "")
      else [];
  in
    (case Scan.error (Scan.finite' Token.stopper (Scan.option scan)) (context, args1) of
      (SOME x, (context', [])) =>
        let val _ = Output.report (reported_text ())
        in (x, context') end
    | (_, (_, args2)) =>
        let
          val print_name =
            (case output_info of
              NONE => quote name
            | SOME (kind, markup) => plain_words kind ^ " " ^ quote (Markup.markup markup name));
          val print_args =
            if null args2 then "" else ":\n  " ^ space_implode " " (map Token.print args2);
        in
          error ("Bad arguments for " ^ print_name ^ Position.here pos ^ print_args ^
            Markup.markup_report (implode (reported_text ())))
        end)
  end;

fun syntax scan src = apsnd Context.the_proof o syntax_generic scan src o Context.Proof;

end;
