(*  Title:      Pure/Thy/latex.ML
    Author:     Markus Wenzel, TU Muenchen

LaTeX presentation elements -- based on outer lexical syntax.
*)

signature LATEX =
sig
  val output_ascii: string -> string
  val output_known_symbols: (string -> bool) * (string -> bool) ->
    Symbol.symbol list -> string
  val output_symbols: Symbol.symbol list -> string
  val output_basic: Token.T -> string
  val output_markup: string -> string -> string
  val output_markup_env: string -> string -> string
  val output_verbatim: string -> string
  val markup_true: string
  val markup_false: string
  val begin_delim: string -> string
  val end_delim: string -> string
  val begin_tag: string -> string
  val end_tag: string -> string
  val tex_trailer: string
  val isabelle_theory: string -> string -> string
  val symbol_source: (string -> bool) * (string -> bool) ->
    string -> Symbol.symbol list -> string
  val theory_entry: string -> string
  val modes: string list
end;

structure Latex: LATEX =
struct

(* literal ASCII *)

val output_ascii =
  translate_string
    (fn " " => "\\ "
      | "\t" => "\\ "
      | "\n" => "\\isanewline\n"
      | s =>
          if exists_string (fn s' => s = s') "#$%^&_{}~\\<>"
          then enclose "{\\char`\\" "}" s else s);


(* symbol output *)

local

val char_table =
  Symtab.make
   [("!", "{\\isacharbang}"),
    ("\"", "{\\isachardoublequote}"),
    ("#", "{\\isacharhash}"),
    ("$", "{\\isachardollar}"),
    ("%", "{\\isacharpercent}"),
    ("&", "{\\isacharampersand}"),
    ("'", "{\\isacharprime}"),
    ("(", "{\\isacharparenleft}"),
    (")", "{\\isacharparenright}"),
    ("*", "{\\isacharasterisk}"),
    ("+", "{\\isacharplus}"),
    (",", "{\\isacharcomma}"),
    ("-", "{\\isacharminus}"),
    (".", "{\\isachardot}"),
    ("/", "{\\isacharslash}"),
    (":", "{\\isacharcolon}"),
    (";", "{\\isacharsemicolon}"),
    ("<", "{\\isacharless}"),
    ("=", "{\\isacharequal}"),
    (">", "{\\isachargreater}"),
    ("?", "{\\isacharquery}"),
    ("@", "{\\isacharat}"),
    ("[", "{\\isacharbrackleft}"),
    ("\\", "{\\isacharbackslash}"),
    ("]", "{\\isacharbrackright}"),
    ("^", "{\\isacharcircum}"),
    ("_", "{\\isacharunderscore}"),
    ("`", "{\\isacharbackquote}"),
    ("{", "{\\isacharbraceleft}"),
    ("|", "{\\isacharbar}"),
    ("}", "{\\isacharbraceright}"),
    ("~", "{\\isachartilde}")];

fun output_chr " " = "\\ "
  | output_chr "\t" = "\\ "
  | output_chr "\n" = "\\isanewline\n"
  | output_chr c =
      (case Symtab.lookup char_table c of
        SOME s => s
      | NONE => if Symbol.is_ascii_digit c then enclose "{\\isadigit{" "}}" c else c);

val output_chrs = translate_string output_chr;

fun output_known_sym (known_sym, known_ctrl) sym =
  (case Symbol.decode sym of
    Symbol.Char s => output_chr s
  | Symbol.UTF8 s => s
  | Symbol.Sym s => if known_sym s then enclose "{\\isasym" "}" s else output_chrs sym
  | Symbol.Ctrl s => if known_ctrl s then enclose "\\isactrl" " " s else output_chrs sym
  | Symbol.Raw s => s
  | Symbol.Malformed s => error (Symbol.malformed_msg s)
  | Symbol.EOF => error "Bad EOF symbol");

in

val output_known_symbols = implode oo (map o output_known_sym);
val output_symbols = output_known_symbols (K true, K true);
val output_syms = output_symbols o Symbol.explode;

val output_syms_antiq =
  (fn Antiquote.Text ss => output_symbols (map Symbol_Pos.symbol ss)
    | Antiquote.Antiq (ss, _) =>
        enclose "%\n\\isaantiq\n" "{}%\n\\endisaantiq\n"
          (output_symbols (map Symbol_Pos.symbol ss)));

end;


(* token output *)

fun output_basic tok =
  let val s = Token.content_of tok in
    if Token.is_kind Token.Comment tok then ""
    else if Token.is_command tok then
      "\\isacommand{" ^ output_syms s ^ "}"
    else if Token.is_kind Token.Keyword tok andalso Symbol.is_ascii_identifier s then
      "\\isakeyword{" ^ output_syms s ^ "}"
    else if Token.is_kind Token.String tok then
      enclose "{\\isachardoublequoteopen}" "{\\isachardoublequoteclose}" (output_syms s)
    else if Token.is_kind Token.Alt_String tok then
      enclose "{\\isacharbackquoteopen}" "{\\isacharbackquoteclose}" (output_syms s)
    else if Token.is_kind Token.Verbatim tok then
      let
        val ants = Antiquote.read (Token.input_of tok);
        val out = implode (map output_syms_antiq ants);
      in enclose "{\\isacharverbatimopen}" "{\\isacharverbatimclose}" out end
    else if Token.is_kind Token.Cartouche tok then
      enclose "{\\isacartoucheopen}" "{\\isacartoucheclose}" (output_syms s)
    else output_syms s
  end;

fun output_markup cmd txt = "%\n\\isamarkup" ^ cmd ^ "{" ^ Symbol.strip_blanks txt ^ "%\n}\n";

fun output_markup_env cmd txt =
  "%\n\\begin{isamarkup" ^ cmd ^ "}%\n" ^
  Symbol.strip_blanks txt ^
  "%\n\\end{isamarkup" ^ cmd ^ "}%\n";

fun output_verbatim txt = "%\n" ^ Symbol.strip_blanks txt ^ "\n";

val markup_true = "\\isamarkuptrue%\n";
val markup_false = "\\isamarkupfalse%\n";

val begin_delim = enclose "%\n\\isadelim" "\n";
val end_delim = enclose "%\n\\endisadelim" "\n";
val begin_tag = enclose "%\n\\isatag" "\n";
fun end_tag tg = enclose "%\n\\endisatag" "\n" tg ^ enclose "{\\isafold" "}%\n" tg;


(* theory presentation *)

val tex_trailer =
  "%%% Local Variables:\n\
  \%%% mode: latex\n\
  \%%% TeX-master: \"root\"\n\
  \%%% End:\n";

fun isabelle_theory name txt =
  "%\n\\begin{isabellebody}%\n\
  \\\setisabellecontext{" ^ output_syms name ^ "}%\n" ^ txt ^
  "\\end{isabellebody}%\n" ^ tex_trailer;

fun symbol_source known name syms =
  isabelle_theory name
    ("\\isamarkupfile{" ^ output_known_symbols known (Symbol.explode name) ^ "}%\n" ^
      output_known_symbols known syms);

fun theory_entry name = "\\input{" ^ name ^ ".tex}\n\n";


(* print mode *)

val latexN = "latex";
val modes = [latexN, Symbol.xsymbolsN];

fun latex_output str =
  let val syms = Symbol.explode str
  in (output_symbols syms, Symbol.length syms) end;

fun latex_markup (s, _) =
  if s = Markup.commandN orelse s = Markup.keyword1N orelse s = Markup.keyword3N
  then ("\\isacommand{", "}")
  else if s = Markup.keyword2N
  then ("\\isakeyword{", "}")
  else Markup.no_output;

fun latex_indent "" _ = ""
  | latex_indent s _ = enclose "\\isaindent{" "}" s;

val _ = Output.add_mode latexN latex_output Symbol.encode_raw;
val _ = Markup.add_mode latexN latex_markup;
val _ = Pretty.add_mode latexN latex_indent;

end;
