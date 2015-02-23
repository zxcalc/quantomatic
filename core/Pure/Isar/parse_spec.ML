(*  Title:      Pure/Isar/parse_spec.ML
    Author:     Makarius

Parsers for complex specifications.
*)

signature PARSE_SPEC =
sig
  val attribs: Attrib.src list parser
  val opt_attribs: Attrib.src list parser
  val thm_name: string -> Attrib.binding parser
  val opt_thm_name: string -> Attrib.binding parser
  val spec: (Attrib.binding * string) parser
  val specs: (Attrib.binding * string list) parser
  val alt_specs: (Attrib.binding * string) list parser
  val where_alt_specs: (Attrib.binding * string) list parser
  val xthm: (Facts.ref * Attrib.src list) parser
  val xthms1: (Facts.ref * Attrib.src list) list parser
  val name_facts: (Attrib.binding * (Facts.ref * Attrib.src list) list) list parser
  val constdecl: (binding * string option * mixfix) parser
  val constdef: ((binding * string option * mixfix) option * (Attrib.binding * string)) parser
  val includes: (xstring * Position.T) list parser
  val locale_fixes: (binding * string option * mixfix) list parser
  val locale_insts: (string option list * (Attrib.binding * string) list) parser
  val class_expression: string list parser
  val locale_prefix: bool -> (string * bool) parser
  val locale_keyword: string parser
  val locale_expression: bool -> Expression.expression parser
  val context_element: Element.context parser
  val statement: (Attrib.binding * (string * string list) list) list parser
  val general_statement: (Element.context list * Element.statement) parser
  val statement_keyword: string parser
end;

structure Parse_Spec: PARSE_SPEC =
struct

(* theorem specifications *)

val attrib = Parse.position Parse.liberal_name -- Parse.!!! Parse.args >> uncurry Args.src;
val attribs = Parse.$$$ "[" |-- Parse.list attrib --| Parse.$$$ "]";
val opt_attribs = Scan.optional attribs [];

fun thm_name s = Parse.binding -- opt_attribs --| Parse.$$$ s;

fun opt_thm_name s =
  Scan.optional ((Parse.binding -- opt_attribs || attribs >> pair Binding.empty) --| Parse.$$$ s)
    Attrib.empty_binding;

val spec = opt_thm_name ":" -- Parse.prop;
val specs = opt_thm_name ":" -- Scan.repeat1 Parse.prop;

val alt_specs =
  Parse.enum1 "|"
    (spec --| Scan.option (Scan.ahead (Parse.name || Parse.$$$ "[") -- Parse.!!! (Parse.$$$ "|")));

val where_alt_specs = Parse.where_ |-- Parse.!!! alt_specs;

val xthm =
  Parse.$$$ "[" |-- attribs --| Parse.$$$ "]" >> pair (Facts.named "") ||
  (Parse.literal_fact >> Facts.Fact ||
    Parse.position Parse.xname -- Scan.option Attrib.thm_sel >> Facts.Named) -- opt_attribs;

val xthms1 = Scan.repeat1 xthm;

val name_facts = Parse.and_list1 (opt_thm_name "=" -- xthms1);


(* basic constant specifications *)

val constdecl =
  Parse.binding --
    (Parse.where_ >> K (NONE, NoSyn) ||
      Parse.$$$ "::" |-- Parse.!!! ((Parse.typ >> SOME) -- Parse.opt_mixfix' --| Parse.where_) ||
      Scan.ahead (Parse.$$$ "(") |-- Parse.!!! (Parse.mixfix' --| Parse.where_ >> pair NONE))
  >> Parse.triple2;

val constdef = Scan.option constdecl -- (opt_thm_name ":" -- Parse.prop);


(* locale and context elements *)

val includes = Parse.$$$ "includes" |-- Parse.!!! (Scan.repeat1 (Parse.position Parse.xname));

val locale_fixes =
  Parse.and_list1 (Parse.binding -- Scan.option (Parse.$$$ "::" |-- Parse.typ) -- Parse.mixfix
    >> (single o Parse.triple1) ||
  Parse.params >> map (fn (x, y) => (x, y, NoSyn))) >> flat;

val locale_insts =
  Scan.optional
    (Parse.$$$ "[" |-- Parse.!!! (Scan.repeat1 (Parse.maybe Parse.term) --| Parse.$$$ "]")) [] --
  Scan.optional (Parse.where_ |-- Parse.and_list1 (opt_thm_name ":" -- Parse.prop)) [];

local

val loc_element =
  Parse.$$$ "fixes" |-- Parse.!!! locale_fixes >> Element.Fixes ||
  Parse.$$$ "constrains" |--
    Parse.!!! (Parse.and_list1 (Parse.name -- (Parse.$$$ "::" |-- Parse.typ)))
    >> Element.Constrains ||
  Parse.$$$ "assumes" |-- Parse.!!! (Parse.and_list1 (opt_thm_name ":" -- Scan.repeat1 Parse.propp))
    >> Element.Assumes ||
  Parse.$$$ "defines" |-- Parse.!!! (Parse.and_list1 (opt_thm_name ":" -- Parse.propp))
    >> Element.Defines ||
  Parse.$$$ "notes" |-- Parse.!!! (Parse.and_list1 (opt_thm_name "=" -- xthms1))
    >> (curry Element.Notes "");

fun plus1_unless test scan =
  scan ::: Scan.repeat (Parse.$$$ "+" |-- Scan.unless test (Parse.!!! scan));

val instance = Parse.where_ |--
  Parse.and_list1 (Parse.name -- (Parse.$$$ "=" |-- Parse.term)) >> Expression.Named ||
  Scan.repeat1 (Parse.maybe Parse.term) >> Expression.Positional;

in

fun locale_prefix mandatory =
  Scan.optional
    (Parse.name --
      (Parse.$$$ "!" >> K true || Parse.$$$ "?" >> K false || Scan.succeed mandatory) --|
      Parse.$$$ ":")
    ("", false);

val locale_keyword =
  Parse.$$$ "fixes" || Parse.$$$ "constrains" || Parse.$$$ "assumes" ||
  Parse.$$$ "defines" || Parse.$$$ "notes";

val class_expression = plus1_unless locale_keyword Parse.class;

fun locale_expression mandatory =
  let
    val expr2 = Parse.position Parse.xname;
    val expr1 = locale_prefix mandatory -- expr2 --
      Scan.optional instance (Expression.Named []) >> (fn ((p, l), i) => (l, (p, i)));
    val expr0 = plus1_unless locale_keyword expr1;
  in expr0 -- Scan.optional (Parse.$$$ "for" |-- Parse.!!! locale_fixes) [] end;

val context_element = Parse.group (fn () => "context element") loc_element;

end;


(* statements *)

val statement = Parse.and_list1 (opt_thm_name ":" -- Scan.repeat1 Parse.propp);

val obtain_case =
  Parse.parbinding -- (Scan.optional (Parse.simple_fixes --| Parse.where_) [] --
    (Parse.and_list1 (Scan.repeat1 Parse.prop) >> flat));

val general_statement =
  statement >> (fn x => ([], Element.Shows x)) ||
  Scan.repeat context_element --
   (Parse.$$$ "obtains" |-- Parse.!!! (Parse.enum1 "|" obtain_case) >> Element.Obtains ||
    Parse.$$$ "shows" |-- Parse.!!! statement >> Element.Shows);

val statement_keyword = Parse.$$$ "obtains" || Parse.$$$ "shows";

end;
