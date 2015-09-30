(*  Title:      Pure/Thy/thy_header.ML
    Author:     Makarius

Static theory header information.
*)

signature THY_HEADER =
sig
  type keywords = ((string * Position.T) * Keyword.spec option) list
  type header =
   {name: string * Position.T,
    imports: (string * Position.T) list,
    keywords: keywords}
  val make: string * Position.T -> (string * Position.T) list -> keywords -> header
  val theoryN: string
  val bootstrap_keywords: Keyword.keywords
  val add_keywords: keywords -> theory -> theory
  val get_keywords: theory -> Keyword.keywords
  val get_keywords': Proof.context -> Keyword.keywords
  val args: header parser
  val read: Position.T -> string -> header
  val read_tokens: Token.T list -> header
end;

structure Thy_Header: THY_HEADER =
struct

(** keyword declarations **)

(* header *)

type keywords = ((string * Position.T) * Keyword.spec option) list;

type header =
 {name: string * Position.T,
  imports: (string * Position.T) list,
  keywords: keywords};

fun make name imports keywords : header =
  {name = name, imports = imports, keywords = keywords};


(* bootstrap keywords *)

val headerN = "header";
val chapterN = "chapter";
val sectionN = "section";
val subsectionN = "subsection";
val subsubsectionN = "subsubsection";
val textN = "text";
val txtN = "txt";
val text_rawN = "text_raw";

val theoryN = "theory";
val importsN = "imports";
val keywordsN = "keywords";
val beginN = "begin";

val bootstrap_keywords =
  Keyword.empty_keywords
  |> Keyword.add_keywords
    [(("%", @{here}), NONE),
     (("(", @{here}), NONE),
     ((")", @{here}), NONE),
     ((",", @{here}), NONE),
     (("::", @{here}), NONE),
     (("==", @{here}), NONE),
     (("and", @{here}), NONE),
     ((beginN, @{here}), NONE),
     ((importsN, @{here}), NONE),
     ((keywordsN, @{here}), NONE),
     ((headerN, @{here}), SOME ((Keyword.document_heading, []), [])),
     ((chapterN, @{here}), SOME ((Keyword.document_heading, []), [])),
     ((sectionN, @{here}), SOME ((Keyword.document_heading, []), [])),
     ((subsectionN, @{here}), SOME ((Keyword.document_heading, []), [])),
     ((subsubsectionN, @{here}), SOME ((Keyword.document_heading, []), [])),
     ((textN, @{here}), SOME ((Keyword.document_body, []), [])),
     ((txtN, @{here}), SOME ((Keyword.document_body, []), [])),
     ((text_rawN, @{here}), SOME ((Keyword.document_raw, []), [])),
     ((theoryN, @{here}), SOME ((Keyword.thy_begin, []), ["theory"])),
     (("ML_file", @{here}), SOME ((Keyword.thy_load, []), ["ML"]))];


(* theory data *)

structure Data = Theory_Data
(
  type T = Keyword.keywords;
  val empty = bootstrap_keywords;
  val extend = I;
  val merge = Keyword.merge_keywords;
);

val add_keywords = Data.map o Keyword.add_keywords;

val get_keywords = Data.get;
val get_keywords' = get_keywords o Proof_Context.theory_of;



(** concrete syntax **)

(* header args *)

local

fun imports name =
  if name = Context.PureN then Scan.succeed []
  else Parse.$$$ importsN |-- Parse.!!! (Scan.repeat1 (Parse.position Parse.theory_xname));

val opt_files =
  Scan.optional (Parse.$$$ "(" |-- Parse.!!! (Parse.list1 Parse.name) --| Parse.$$$ ")") [];

val keyword_spec =
  Parse.group (fn () => "outer syntax keyword specification")
    (Parse.name -- opt_files -- Parse.tags);

val keyword_compl =
  Parse.group (fn () => "outer syntax keyword completion") Parse.name;

val keyword_decl =
  Scan.repeat1 (Parse.position Parse.string) --
  Scan.option (Parse.$$$ "::" |-- Parse.!!! keyword_spec) --
  Scan.option (Parse.$$$ "==" |-- Parse.!!! keyword_compl)
  >> (fn ((names, spec), _) => map (rpair spec) names);

val keyword_decls = Parse.and_list1 keyword_decl >> flat;

in

val args =
  Parse.position Parse.theory_name :|-- (fn (name, pos) =>
    imports name --
    Scan.optional (Parse.$$$ keywordsN |-- Parse.!!! keyword_decls) [] --|
    Parse.$$$ beginN >> (fn (imports, keywords) => make (name, pos) imports keywords));

end;


(* read header *)

val heading =
  (Parse.command headerN ||
    Parse.command chapterN ||
    Parse.command sectionN ||
    Parse.command subsectionN ||
    Parse.command subsubsectionN ||
    Parse.command textN ||
    Parse.command txtN ||
    Parse.command text_rawN) --
  Parse.tags -- Parse.!!! Parse.document_source;

val header =
  (Scan.repeat heading -- Parse.command theoryN -- Parse.tags) |-- Parse.!!! args;

fun token_source pos str =
  str
  |> Source.of_string_limited 8000
  |> Symbol.source
  |> Token.source_strict bootstrap_keywords pos;

fun read_source pos source =
  let val res =
    source
    |> Token.source_proper
    |> Source.source Token.stopper (Scan.single (Scan.error (Parse.!!! header)))
    |> Source.get_single;
  in
    (case res of
      SOME (h, _) => h
    | NONE => error ("Unexpected end of input" ^ Position.here pos))
  end;

fun read pos str = read_source pos (token_source pos str);
fun read_tokens toks = read_source Position.none (Source.of_list toks);

end;
