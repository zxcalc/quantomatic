(*  Title:      Pure/General/markup.ML
    ID:         $Id: markup.ML,v 1.28 2008/05/23 19:18:47 wenzelm Exp $
    Author:     Makarius

Common markup elements.
*)

signature MARKUP =
sig
  type property = string * string
  type T = string * property list
  val get_string: T -> string -> string option
  val get_int: T -> string -> int option
  val none: T
  val properties: (string * string) list -> T -> T
  val nameN: string
  val groupN: string
  val theory_nameN: string
  val idN: string
  val kindN: string
  val internalK: string
  val property_internal: property
  val lineN: string
  val columnN: string
  val fileN: string
  val position_properties: string list
  val positionN: string val position: T
  val locationN: string val location: T
  val indentN: string
  val blockN: string val block: int -> T
  val widthN: string
  val breakN: string val break: int -> T
  val fbreakN: string val fbreak: T
  val classN: string val class: string -> T
  val tyconN: string val tycon: string -> T
  val fixedN: string val fixed: string -> T
  val constN: string val const: string -> T
  val axiomN: string val axiom: string -> T
  val tfreeN: string val tfree: T
  val tvarN: string val tvar: T
  val freeN: string val free: T
  val skolemN: string val skolem: T
  val boundN: string val bound: T
  val varN: string val var: T
  val numN: string val num: T
  val xnumN: string val xnum: T
  val xstrN: string val xstr: T
  val sortN: string val sort: T
  val typN: string val typ: T
  val termN: string val term: T
  val keywordN: string val keyword: string -> T
  val commandN: string val command: string -> T
  val keyword_declN: string val keyword_decl: string -> T
  val command_declN: string val command_decl: string -> string -> T
  val stringN: string val string: T
  val altstringN: string val altstring: T
  val verbatimN: string val verbatim: T
  val commentN: string val comment: T
  val controlN: string val control: T
  val malformedN: string val malformed: T
  val antiqN: string val antiq: T
  val whitespaceN: string val whitespace: T
  val junkN: string val junk: T
  val commandspanN: string val commandspan: string -> T
  val stateN: string val state: T
  val subgoalN: string val subgoal: T
  val sendbackN: string val sendback: T
  val hiliteN: string val hilite: T
  val default_output: T -> output * output
  val add_mode: string -> (T -> output * output) -> unit
  val output: T -> output * output
  val enclose: T -> output -> output
  val markup: T -> string -> string
end;

structure Markup: MARKUP =
struct

(* basic markup *)

type property = string * string;
type T = string * property list;

val none = ("", []);


fun properties more_props ((elem, props): T) =
  (elem, fold_rev (AList.update (op =)) more_props props);

fun get_string ((_, props): T) prop = AList.lookup (op =) props prop;
fun get_int m prop = (case get_string m prop of NONE => NONE | SOME s => Int.fromString s);

fun markup_elem elem = (elem, (elem, []): T);
fun markup_string elem prop = (elem, fn s => (elem, [(prop, s)]): T);
fun markup_int elem prop = (elem, fn i => (elem, [(prop, Int.toString i)]): T);


(* name *)

val nameN = "name";
val groupN = "group";
val theory_nameN = "theory_name";


(* kind *)

val kindN = "kind";

val internalK = "internal";
val property_internal = (kindN, internalK);


(* position *)

val lineN = "line";
val columnN = "column";
val fileN = "file";
val idN = "id";

val position_properties = [lineN, columnN, fileN, idN];
val (positionN, position) = markup_elem "position";

val (locationN, location) = markup_elem "location";


(* pretty printing *)

val indentN = "indent";
val (blockN, block) = markup_int "block" indentN;

val widthN = "width";
val (breakN, break) = markup_int "break" widthN;

val (fbreakN, fbreak) = markup_elem "fbreak";


(* logical entities *)

val (classN, class) = markup_string "class" nameN;
val (tyconN, tycon) = markup_string "tycon" nameN;
val (fixedN, fixed) = markup_string "fixed" nameN;
val (constN, const) = markup_string "const" nameN;
val (axiomN, axiom) = markup_string "axiom" nameN;


(* inner syntax *)

val (tfreeN, tfree) = markup_elem "tfree";
val (tvarN, tvar) = markup_elem "tvar";
val (freeN, free) = markup_elem "free";
val (skolemN, skolem) = markup_elem "skolem";
val (boundN, bound) = markup_elem "bound";
val (varN, var) = markup_elem "var";
val (numN, num) = markup_elem "num";
val (xnumN, xnum) = markup_elem "xnum";
val (xstrN, xstr) = markup_elem "xstr";

val (sortN, sort) = markup_elem "sort";
val (typN, typ) = markup_elem "typ";
val (termN, term) = markup_elem "term";


(* outer syntax *)

val (keywordN, keyword) = markup_string "keyword" nameN;
val (commandN, command) = markup_string "command" nameN;

val (keyword_declN, keyword_decl) = markup_string "keyword_decl" nameN;

val command_declN = "command_decl";
fun command_decl name kind : T = (command_declN, [(nameN, name), (kindN, kind)]);

val (stringN, string) = markup_elem "string";
val (altstringN, altstring) = markup_elem "altstring";
val (verbatimN, verbatim) = markup_elem "verbatim";
val (commentN, comment) = markup_elem "comment";
val (controlN, control) = markup_elem "control";
val (malformedN, malformed) = markup_elem "malformed";

val (antiqN, antiq) = markup_elem "antiq";

val (whitespaceN, whitespace) = markup_elem "whitespace";
val (junkN, junk) = markup_elem "junk";
val (commandspanN, commandspan) = markup_string "commandspan" nameN;


(* toplevel *)

val (stateN, state) = markup_elem "state";
val (subgoalN, subgoal) = markup_elem "subgoal";
val (sendbackN, sendback) = markup_elem "sendback";
val (hiliteN, hilite) = markup_elem "hilite";


(* print mode operations *)

fun default_output (_: T) = ("", "");

local
  val default = {output = default_output};
  val modes = ref (Symtab.make [("", default)]);
in
  fun add_mode name output = CRITICAL (fn () =>
    change modes (Symtab.update_new (name, {output = output})));
  fun get_mode () =
    the_default default (Library.get_first (Symtab.lookup (! modes)) (print_mode_value ()));
end;

fun output ("", _) = ("", "")
  | output m = #output (get_mode ()) m;

val enclose = output #-> Library.enclose;

fun markup m =
  let val (bg, en) = output m
  in Library.enclose (Output.escape bg) (Output.escape en) end;

end;
