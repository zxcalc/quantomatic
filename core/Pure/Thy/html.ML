(*  Title:      Pure/Thy/html.ML
    Author:     Markus Wenzel and Stefan Berghofer, TU Muenchen

HTML presentation elements.
*)

signature HTML =
sig
  type symbols
  val make_symbols: (string * int) list -> symbols
  val no_symbols: symbols
  val present_span: symbols -> Keyword.keywords -> Command_Span.span -> Output.output
  type text = string
  val begin_document: symbols -> string -> text
  val end_document: text
  val begin_session_index: symbols -> string -> Url.T -> (Url.T * string) list -> text
  val theory_entry: symbols -> Url.T * string -> text
  val theory: symbols -> string -> (Url.T option * string) list -> text -> text
end;

structure HTML: HTML =
struct


(* common markup *)

fun span class = ("<span class=" ^ quote (XML.text class) ^ ">", "</span>");

val hidden = span Markup.hiddenN |-> enclose;


(* symbol output *)

abstype symbols = Symbols of string Symtab.table
with

fun make_symbols codes =
  let
    val mapping =
      map (apsnd (fn c => "&#" ^ Value.print_int c ^ ";")) codes @
       [("'", "&#39;"),
        ("\<^bsub>", hidden "&#8664;" ^ "<sub>"),
        ("\<^esub>", hidden "&#8665;" ^ "</sub>"),
        ("\<^bsup>", hidden "&#8663;" ^ "<sup>"),
        ("\<^esup>", hidden "&#8662;" ^ "</sup>")];
  in Symbols (fold Symtab.update mapping Symtab.empty) end;

val no_symbols = make_symbols [];

fun output_sym (Symbols tab) s =
  (case Symtab.lookup tab s of
    SOME x => x
  | NONE => XML.text s);

end;

local

fun output_markup (bg, en) symbols s1 s2 =
  hidden s1 ^ enclose bg en (output_sym symbols s2);

val output_sub = output_markup ("<sub>", "</sub>");
val output_sup = output_markup ("<sup>", "</sup>");
val output_bold = output_markup (span "bold");

fun output_syms _ [] result = implode (rev result)
  | output_syms symbols (s1 :: rest) result =
      let
        val (s2, ss) = (case rest of [] => ("", []) | s2 :: ss => (s2, ss));
        val (s, r) =
          if s1 = "\<^sub>" then (output_sub symbols "&#8681;" s2, ss)
          else if s1 = "\<^sup>" then (output_sup symbols "&#8679;" s2, ss)
          else if s1 = "\<^bold>" then (output_bold symbols "&#10073;" s2, ss)
          else (output_sym symbols s1, rest);
      in output_syms symbols r (s :: result) end;

in

fun output symbols str = output_syms symbols (Symbol.explode str) [];

end;


(* presentation *)

fun present_span symbols keywords =
  let
    fun present_token tok =
      fold_rev (uncurry enclose o span o #1)
        (Token.markups keywords tok) (output symbols (Token.unparse tok));
  in implode o map present_token o Command_Span.content end;



(** HTML markup **)

type text = string;


(* atoms *)

val name = enclose "<span class=\"name\">" "</span>" oo output;
val keyword = enclose "<span class=\"keyword\">" "</span>" oo output;
val command = enclose "<span class=\"command\">" "</span>" oo output;


(* misc *)

fun href_name s txt = "<a href=" ^ quote s ^ ">" ^ txt ^ "</a>";
fun href_path path txt = href_name (Url.implode path) txt;

fun href_opt_path NONE txt = txt
  | href_opt_path (SOME p) txt = href_path p txt;

fun para txt = "\n<p>" ^ txt ^ "</p>\n";


(* document *)

fun begin_document symbols title =
  "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n" ^
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" " ^
  "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n" ^
  "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" ^
  "<head>\n" ^
  "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n" ^
  "<title>" ^ output symbols (title ^ " (" ^ Distribution.version ^ ")") ^ "</title>\n" ^
  "<link media=\"all\" rel=\"stylesheet\" type=\"text/css\" href=\"isabelle.css\"/>\n" ^
  "</head>\n" ^
  "\n" ^
  "<body>\n" ^
  "<div class=\"head\">" ^
  "<h1>" ^ output symbols title ^ "</h1>\n";

val end_document = "\n</div>\n</body>\n</html>\n";


(* session index *)

fun begin_session_index symbols session graph docs =
  begin_document symbols ("Session " ^ output symbols session) ^
  para ("View " ^ href_path graph "theory dependencies" ^
    implode (map (fn (p, name) => "<br/>\nView " ^ href_path p name) docs)) ^
  "\n</div>\n<div class=\"theories\">\n<h2>Theories</h2>\n<ul>\n";

fun theory_entry symbols (p, s) = "<li>" ^ href_path p (output symbols s) ^ "</li>\n";


(* theory *)

fun theory symbols A Bs txt =
  begin_document symbols ("Theory " ^ A) ^ "\n" ^
  command symbols "theory" ^ " " ^ name symbols A ^ "<br/>\n" ^
  keyword symbols "imports" ^ " " ^
    space_implode " " (map (uncurry href_opt_path o apsnd (name symbols)) Bs) ^
  "<br/>\n" ^
  enclose "\n</div>\n<div class=\"source\">\n<pre class=\"source\">" "</pre>\n" txt ^
  end_document;

end;
