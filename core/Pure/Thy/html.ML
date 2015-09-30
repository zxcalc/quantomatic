(*  Title:      Pure/Thy/html.ML
    Author:     Markus Wenzel and Stefan Berghofer, TU Muenchen

HTML presentation elements.
*)

signature HTML =
sig
  val html_mode: ('a -> 'b) -> 'a -> 'b
  type text = string
  val plain: string -> text
  val name: string -> text
  val keyword: string -> text
  val command: string -> text
  val href_name: string -> text -> text
  val href_path: Url.T -> text -> text
  val href_opt_path: Url.T option -> text -> text
  val para: text -> text
  val preform: text -> text
  val verbatim: string -> text
  val begin_document: string -> text
  val end_document: text
  val begin_session_index: string -> Url.T -> (Url.T * string) list -> text
  val theory_entry: Url.T * string -> text
  val theory: string -> (Url.T option * string) list -> text -> text
end;

structure HTML: HTML =
struct


(** HTML print modes **)

(* mode *)

val htmlN = "HTML";
fun html_mode f x = Print_Mode.with_modes [htmlN] f x;


(* common markup *)

fun span class = ("<span class=" ^ quote (XML.text class) ^ ">", "</span>");

val _ = Markup.add_mode htmlN (span o fst);


(* symbol output *)

local
  val hidden = span Markup.hiddenN |-> enclose;

  (* FIXME proper unicode -- produced on Scala side *)
  val html_syms = Symtab.make
   [("", (0, "")),
    ("'", (1, "&#39;")),
    ("\\<exclamdown>", (1, "&iexcl;")),
    ("\\<cent>", (1, "&cent;")),
    ("\\<pounds>", (1, "&pound;")),
    ("\\<currency>", (1, "&curren;")),
    ("\\<yen>", (1, "&yen;")),
    ("\\<bar>", (1, "&brvbar;")),
    ("\\<section>", (1, "&sect;")),
    ("\\<dieresis>", (1, "&uml;")),
    ("\\<copyright>", (1, "&copy;")),
    ("\\<ordfeminine>", (1, "&ordf;")),
    ("\\<guillemotleft>", (1, "&laquo;")),
    ("\\<not>", (1, "&not;")),
    ("\\<hyphen>", (1, "&shy;")),
    ("\\<registered>", (1, "&reg;")),
    ("\\<inverse>", (1, "&macr;")),
    ("\\<degree>", (1, "&deg;")),
    ("\\<plusminus>", (1, "&plusmn;")),
    ("\\<acute>", (1, "&acute;")),
    ("\\<paragraph>", (1, "&para;")),
    ("\\<cdot>", (1, "&middot;")),
    ("\\<cedilla>", (1, "&cedil;")),
    ("\\<ordmasculine>", (1, "&ordm;")),
    ("\\<guillemotright>", (1, "&raquo;")),
    ("\\<onequarter>", (1, "&frac14;")),
    ("\\<onehalf>", (1, "&frac12;")),
    ("\\<threequarters>", (1, "&frac34;")),
    ("\\<questiondown>", (1, "&iquest;")),
    ("\\<times>", (1, "&times;")),
    ("\\<div>", (1, "&divide;")),
    ("\\<circ>", (1, "o")),
    ("\\<Alpha>", (1, "&Alpha;")),
    ("\\<Beta>", (1, "&Beta;")),
    ("\\<Gamma>", (1, "&Gamma;")),
    ("\\<Delta>", (1, "&Delta;")),
    ("\\<Epsilon>", (1, "&Epsilon;")),
    ("\\<Zeta>", (1, "&Zeta;")),
    ("\\<Eta>", (1, "&Eta;")),
    ("\\<Theta>", (1, "&Theta;")),
    ("\\<Iota>", (1, "&Iota;")),
    ("\\<Kappa>", (1, "&Kappa;")),
    ("\\<Lambda>", (1, "&Lambda;")),
    ("\\<Mu>", (1, "&Mu;")),
    ("\\<Nu>", (1, "&Nu;")),
    ("\\<Xi>", (1, "&Xi;")),
    ("\\<Omicron>", (1, "&Omicron;")),
    ("\\<Pi>", (1, "&Pi;")),
    ("\\<Rho>", (1, "&Rho;")),
    ("\\<Sigma>", (1, "&Sigma;")),
    ("\\<Tau>", (1, "&Tau;")),
    ("\\<Upsilon>", (1, "&Upsilon;")),
    ("\\<Phi>", (1, "&Phi;")),
    ("\\<Chi>", (1, "&Chi;")),
    ("\\<Psi>", (1, "&Psi;")),
    ("\\<Omega>", (1, "&Omega;")),
    ("\\<alpha>", (1, "&alpha;")),
    ("\\<beta>", (1, "&beta;")),
    ("\\<gamma>", (1, "&gamma;")),
    ("\\<delta>", (1, "&delta;")),
    ("\\<epsilon>", (1, "&epsilon;")),
    ("\\<zeta>", (1, "&zeta;")),
    ("\\<eta>", (1, "&eta;")),
    ("\\<theta>", (1, "&thetasym;")),
    ("\\<iota>", (1, "&iota;")),
    ("\\<kappa>", (1, "&kappa;")),
    ("\\<lambda>", (1, "&lambda;")),
    ("\\<mu>", (1, "&mu;")),
    ("\\<nu>", (1, "&nu;")),
    ("\\<xi>", (1, "&xi;")),
    ("\\<omicron>", (1, "&omicron;")),
    ("\\<pi>", (1, "&pi;")),
    ("\\<rho>", (1, "&rho;")),
    ("\\<sigma>", (1, "&sigma;")),
    ("\\<tau>", (1, "&tau;")),
    ("\\<upsilon>", (1, "&upsilon;")),
    ("\\<phi>", (1, "&phi;")),
    ("\\<chi>", (1, "&chi;")),
    ("\\<psi>", (1, "&psi;")),
    ("\\<omega>", (1, "&omega;")),
    ("\\<bullet>", (1, "&bull;")),
    ("\\<dots>", (1, "&hellip;")),
    ("\\<wp>", (1, "&weierp;")),
    ("\\<forall>", (1, "&forall;")),
    ("\\<partial>", (1, "&part;")),
    ("\\<exists>", (1, "&exist;")),
    ("\\<emptyset>", (1, "&empty;")),
    ("\\<nabla>", (1, "&nabla;")),
    ("\\<in>", (1, "&isin;")),
    ("\\<notin>", (1, "&notin;")),
    ("\\<Prod>", (1, "&prod;")),
    ("\\<Sum>", (1, "&sum;")),
    ("\\<star>", (1, "&lowast;")),
    ("\\<propto>", (1, "&prop;")),
    ("\\<infinity>", (1, "&infin;")),
    ("\\<angle>", (1, "&ang;")),
    ("\\<and>", (1, "&and;")),
    ("\\<or>", (1, "&or;")),
    ("\\<inter>", (1, "&cap;")),
    ("\\<union>", (1, "&cup;")),
    ("\\<sim>", (1, "&sim;")),
    ("\\<cong>", (1, "&cong;")),
    ("\\<approx>", (1, "&asymp;")),
    ("\\<noteq>", (1, "&ne;")),
    ("\\<equiv>", (1, "&equiv;")),
    ("\\<le>", (1, "&le;")),
    ("\\<ge>", (1, "&ge;")),
    ("\\<subset>", (1, "&sub;")),
    ("\\<supset>", (1, "&sup;")),
    ("\\<subseteq>", (1, "&sube;")),
    ("\\<supseteq>", (1, "&supe;")),
    ("\\<oplus>", (1, "&oplus;")),
    ("\\<otimes>", (1, "&otimes;")),
    ("\\<bottom>", (1, "&perp;")),
    ("\\<lceil>", (1, "&lceil;")),
    ("\\<rceil>", (1, "&rceil;")),
    ("\\<lfloor>", (1, "&lfloor;")),
    ("\\<rfloor>", (1, "&rfloor;")),
    ("\\<langle>", (1, "&lang;")),
    ("\\<rangle>", (1, "&rang;")),
    ("\\<lozenge>", (1, "&loz;")),
    ("\\<spadesuit>", (1, "&spades;")),
    ("\\<clubsuit>", (1, "&clubs;")),
    ("\\<heartsuit>", (1, "&hearts;")),
    ("\\<diamondsuit>", (1, "&diams;")),
    ("\\<lbrakk>", (2, "[|")),
    ("\\<rbrakk>", (2, "|]")),
    ("\\<Longrightarrow>", (3, "==&gt;")),
    ("\\<Rightarrow>", (2, "=&gt;")),
    ("\\<And>", (2, "!!")),
    ("\\<Colon>", (2, "::")),
    ("\\<lparr>", (2, "(|")),
    ("\\<rparr>", (2, "|)),")),
    ("\\<longleftrightarrow>", (3, "&lt;-&gt;")),
    ("\\<longrightarrow>", (3, "--&gt;")),
    ("\\<rightarrow>", (2, "-&gt;")),
    ("\\<open>", (1, "&#8249;")),
    ("\\<close>", (1, "&#8250;")),
    ("\\<newline>", (1, "&#9166;")),
    ("\\<^bsub>", (0, hidden "&#8664;" ^ "<sub>")),
    ("\\<^esub>", (0, hidden "&#8665;" ^ "</sub>")),
    ("\\<^bsup>", (0, hidden "&#8663;" ^ "<sup>")),
    ("\\<^esup>", (0, hidden "&#8662;" ^ "</sup>"))];

  fun output_sym s =
    if Symbol.is_raw s then (1, Symbol.decode_raw s)
    else
      (case Symtab.lookup html_syms s of
        SOME x => x
      | NONE => (size s, XML.text s));

  fun output_markup (bg, en) s1 s2 =
    let val (n, txt) = output_sym s2
    in (n, hidden s1 ^ enclose bg en txt) end;

  val output_sub = output_markup ("<sub>", "</sub>");
  val output_sup = output_markup ("<sup>", "</sup>");
  val output_bold = output_markup (span "bold");

  fun output_syms [] (result, width) = (implode (rev result), width)
    | output_syms (s1 :: rest) (result, width) =
        let
          val (s2, ss) = (case rest of [] => ("", []) | s2 :: ss => (s2, ss));
          val ((w, s), r) =
            if s1 = "\\<^sub>" then (output_sub "&#8681;" s2, ss)
            else if s1 = "\\<^sup>" then (output_sup "&#8679;" s2, ss)
            else if s1 = "\\<^bold>" then (output_bold "&#10073;" s2, ss)
            else (output_sym s1, rest);
        in output_syms r (s :: result, width + w) end;
in

fun output_width str = output_syms (Symbol.explode str) ([], 0);
val output = #1 o output_width;

val _ = Output.add_mode htmlN output_width Symbol.encode_raw;

end;



(** HTML markup **)

type text = string;


(* atoms *)

val plain = output;
val name = enclose "<span class=\"name\">" "</span>" o output;
val keyword = enclose "<span class=\"keyword\">" "</span>" o output;
val command = enclose "<span class=\"command\">" "</span>" o output;


(* misc *)

fun href_name s txt = "<a href=" ^ quote s ^ ">" ^ txt ^ "</a>";
fun href_path path txt = href_name (Url.implode path) txt;

fun href_opt_path NONE txt = txt
  | href_opt_path (SOME p) txt = href_path p txt;

fun para txt = "\n<p>" ^ txt ^ "</p>\n";
fun preform txt = "<pre>" ^ txt ^ "</pre>";
val verbatim = preform o output;


(* document *)

fun begin_document title =
  "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n\
  \<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \
  \\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n\
  \<html xmlns=\"http://www.w3.org/1999/xhtml\">\n\
  \<head>\n\
  \<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n\
  \<title>" ^ plain (title ^ " (" ^ Distribution.version ^ ")") ^ "</title>\n\
  \<link media=\"all\" rel=\"stylesheet\" type=\"text/css\" href=\"isabelle.css\"/>\n\
  \</head>\n\
  \\n\
  \<body>\n\
  \<div class=\"head\">\
  \<h1>" ^ plain title ^ "</h1>\n";

val end_document = "\n</div>\n</body>\n</html>\n";


(* session index *)

fun begin_session_index session graph docs =
  begin_document ("Session " ^ plain session) ^
  para ("View " ^ href_path graph "theory dependencies" ^
    implode (map (fn (p, name) => "<br/>\nView " ^ href_path p name) docs)) ^
  "\n</div>\n<div class=\"theories\">\n<h2>Theories</h2>\n<ul>\n";

fun theory_entry (p, s) = "<li>" ^ href_path p (plain s) ^ "</li>\n";


(* theory *)

fun theory A Bs txt =
  begin_document ("Theory " ^ A) ^ "\n" ^
  command "theory" ^ " " ^ name A ^ "<br/>\n" ^
  keyword "imports" ^ " " ^ space_implode " " (map (uncurry href_opt_path o apsnd name) Bs) ^
  "<br/>\n" ^
  enclose "\n</div>\n<div class=\"source\">\n<pre>" "</pre>\n" txt ^
  end_document;

end;
