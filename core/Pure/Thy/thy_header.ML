(*  Title:      Pure/Thy/thy_header.ML
    Author:     Makarius

Static theory header information.
*)

signature THY_HEADER =
sig
  type keywords = (string * Keyword.spec option) list
  type header =
   {name: string * Position.T,
    imports: (string * Position.T) list,
    keywords: keywords}
  val make: string * Position.T -> (string * Position.T) list -> keywords -> header
  val define_keywords: header -> unit
  val declare_keyword: string * Keyword.spec option -> theory -> theory
  val the_keyword: theory -> string -> Keyword.spec option
  val args: header parser
  val read: Position.T -> string -> header
  val read_tokens: Token.T list -> header
end;

structure Thy_Header: THY_HEADER =
struct

type keywords = (string * Keyword.spec option) list;

type header =
 {name: string * Position.T,
  imports: (string * Position.T) list,
  keywords: keywords};

fun make name imports keywords : header =
  {name = name, imports = imports, keywords = keywords};



(** keyword declarations **)

fun define_keywords ({keywords, ...}: header) =
  List.app (Keyword.define o apsnd (Option.map Keyword.spec)) keywords;

fun err_dup name = error ("Duplicate declaration of outer syntax keyword " ^ quote name);

structure Data = Theory_Data
(
  type T = Keyword.spec option Symtab.table;
  val empty = Symtab.empty;
  val extend = I;
  fun merge data : T = Symtab.merge (op =) data handle Symtab.DUP name => err_dup name;
);

fun declare_keyword (name, spec) =
  Data.map (fn data =>
    (Option.map Keyword.spec spec;
      Symtab.update_new (name, spec) data handle Symtab.DUP dup => err_dup dup));

fun the_keyword thy name =
  (case Symtab.lookup (Data.get thy) name of
    SOME spec => spec
  | NONE => error ("Undeclared outer syntax keyword " ^ quote name));



(** concrete syntax **)

(* header keywords *)

val headerN = "header";
val theoryN = "theory";
val importsN = "imports";
val keywordsN = "keywords";
val beginN = "begin";

val header_lexicons =
  pairself (Scan.make_lexicon o map Symbol.explode)
   (["%", "(", ")", ",", "::", ";", "==", "and", beginN, importsN, keywordsN],
    [headerN, theoryN]);


(* header args *)

local

val theory_name = Parse.group (fn () => "theory name") (Parse.position Parse.name);
val theory_xname = Parse.group (fn () => "theory name reference") (Parse.position Parse.xname);

val imports = Parse.$$$ importsN |-- Parse.!!! (Scan.repeat1 theory_xname);

val opt_files =
  Scan.optional (Parse.$$$ "(" |-- Parse.!!! (Parse.list1 Parse.name) --| Parse.$$$ ")") [];

val keyword_spec =
  Parse.group (fn () => "outer syntax keyword specification")
    (Parse.name -- opt_files -- Parse.tags);

val keyword_compl =
  Parse.group (fn () => "outer syntax keyword completion") Parse.name;

val keyword_decl =
  Scan.repeat1 Parse.string --
  Scan.option (Parse.$$$ "::" |-- Parse.!!! keyword_spec) --
  Scan.option (Parse.$$$ "==" |-- Parse.!!! keyword_compl)
  >> (fn ((names, spec), _) => map (rpair spec) names);

val keyword_decls = Parse.and_list1 keyword_decl >> flat;

in

val args =
  theory_name :|-- (fn (name, pos) =>
    (if name = Context.PureN then Scan.succeed [] else imports) --
    Scan.optional (Parse.$$$ keywordsN |-- Parse.!!! keyword_decls) [] --|
    Parse.$$$ beginN >> (fn (imports, keywords) => make (name, pos) imports keywords));

end;


(* read header *)

val header =
  (Parse.command_name headerN -- Parse.tags) |--
    (Parse.!!! (Parse.document_source -- Scan.repeat Parse.semicolon --
      (Parse.command_name theoryN -- Parse.tags) |-- args)) ||
  (Parse.command_name theoryN -- Parse.tags) |-- Parse.!!! args;

fun token_source pos str =
  str
  |> Source.of_string_limited 8000
  |> Symbol.source
  |> Token.source {do_recover = NONE} (K header_lexicons) pos;

fun read_source pos source =
  let val res =
    source
    |> Token.source_proper
    |> Source.source Token.stopper (Scan.single (Scan.error (Parse.!!! header))) NONE
    |> Source.get_single;
  in
    (case res of
      SOME (h, _) => h
    | NONE => error ("Unexpected end of input" ^ Position.here pos))
  end;

fun read pos str = read_source pos (token_source pos str);
fun read_tokens toks = read_source Position.none (Source.of_list toks);

end;
