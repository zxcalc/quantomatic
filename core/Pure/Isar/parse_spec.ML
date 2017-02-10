(*  Title:      Pure/Isar/parse_spec.ML
    Author:     Makarius

Parsers for complex specifications.
*)

signature PARSE_SPEC =
sig
  val thm_name: string -> Attrib.binding parser
  val opt_thm_name: string -> Attrib.binding parser
  val name_facts: (Attrib.binding * (Facts.ref * Token.src list) list) list parser
  val simple_spec: (Attrib.binding * string) parser
  val simple_specs: (Attrib.binding * string list) parser
  val if_assumes: string list parser
  val multi_specs: Specification.multi_specs_cmd parser
  val where_multi_specs: Specification.multi_specs_cmd parser
  val specification:
    ((binding * string option * mixfix) list * Specification.multi_specs_cmd) parser
  val constdecl: (binding * string option * mixfix) parser
  val includes: (xstring * Position.T) list parser
  val locale_fixes: (binding * string option * mixfix) list parser
  val locale_insts: (string option list * (Attrib.binding * string) list) parser
  val class_expression: string list parser
  val locale_prefix: (string * bool) parser
  val locale_keyword: string parser
  val locale_expression: Expression.expression parser
  val context_element: Element.context parser
  val statement': (string * string list) list list parser
  val if_statement': (string * string list) list list parser
  val statement: (Attrib.binding * (string * string list) list) list parser
  val if_statement: (Attrib.binding * (string * string list) list) list parser
  val cond_statement: (bool * (Attrib.binding * (string * string list) list) list) parser
  val obtains: Element.obtains parser
  val long_statement: (Element.context list * Element.statement) parser
  val long_statement_keyword: string parser
  val overloaded: bool parser
end;

structure Parse_Spec: PARSE_SPEC =
struct

(* simple specifications *)

fun thm_name s = Parse.binding -- Parse.opt_attribs --| Parse.$$$ s;

fun opt_thm_name s =
  Scan.optional
    ((Parse.binding -- Parse.opt_attribs || Parse.attribs >> pair Binding.empty) --| Parse.$$$ s)
    Binding.empty_atts;

val simple_spec = opt_thm_name ":" -- Parse.prop;
val simple_specs = opt_thm_name ":" -- Scan.repeat1 Parse.prop;

val name_facts = Parse.and_list1 (opt_thm_name "=" -- Parse.thms1);


(* structured specifications *)

val if_assumes =
  Scan.optional (Parse.$$$ "if" |-- Parse.!!! (Parse.and_list1 (Scan.repeat1 Parse.prop) >> flat))
    [];

val multi_specs =
  Parse.enum1 "|"
    ((opt_thm_name ":" -- Parse.prop -- if_assumes -- Parse.for_fixes >> Scan.triple1) --|
      Scan.option (Scan.ahead (Parse.name || Parse.$$$ "[") -- Parse.!!! (Parse.$$$ "|")));

val where_multi_specs = Parse.where_ |-- Parse.!!! multi_specs;

val specification = Parse.vars -- where_multi_specs;


(* basic constant specifications *)

val constdecl =
  Parse.binding --
    (Parse.where_ >> K (NONE, NoSyn) ||
      Parse.$$$ "::" |-- Parse.!!! ((Parse.typ >> SOME) -- Parse.opt_mixfix' --| Parse.where_) ||
      Scan.ahead (Parse.$$$ "(") |-- Parse.!!! (Parse.mixfix' --| Parse.where_ >> pair NONE))
  >> Scan.triple2;


(* locale and context elements *)

val includes = Parse.$$$ "includes" |-- Parse.!!! (Scan.repeat1 (Parse.position Parse.name));

val locale_fixes =
  Parse.and_list1 (Parse.binding -- Scan.option (Parse.$$$ "::" |-- Parse.typ) -- Parse.mixfix
    >> (single o Scan.triple1) ||
  Parse.params) >> flat;

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
  Parse.$$$ "notes" |-- Parse.!!! (Parse.and_list1 (opt_thm_name "=" -- Parse.thms1))
    >> (curry Element.Notes "");

fun plus1_unless test scan =
  scan ::: Scan.repeat (Parse.$$$ "+" |-- Scan.unless test (Parse.!!! scan));

val instance = Parse.where_ |--
  Parse.and_list1 (Parse.name -- (Parse.$$$ "=" |-- Parse.term)) >> Expression.Named ||
  Scan.repeat1 (Parse.maybe Parse.term) >> Expression.Positional;

in

val locale_prefix =
  Scan.optional
    (Parse.name -- (Scan.option (Parse.$$$ "?") >> is_none) --| Parse.$$$ ":")
    ("", false);

val locale_keyword =
  Parse.$$$ "fixes" || Parse.$$$ "constrains" || Parse.$$$ "assumes" ||
  Parse.$$$ "defines" || Parse.$$$ "notes";

val class_expression = plus1_unless locale_keyword Parse.class;

val locale_expression =
  let
    val expr2 = Parse.position Parse.name;
    val expr1 =
      locale_prefix -- expr2 --
      Scan.optional instance (Expression.Named []) >> (fn ((p, l), i) => (l, (p, i)));
    val expr0 = plus1_unless locale_keyword expr1;
  in expr0 -- Scan.optional (Parse.$$$ "for" |-- Parse.!!! locale_fixes) [] end;

val context_element = Parse.group (fn () => "context element") loc_element;

end;


(* statements *)

val statement' = Parse.and_list1 (Scan.repeat1 Parse.propp);
val if_statement' = Scan.optional (Parse.$$$ "if" |-- Parse.!!! statement') [];

val statement = Parse.and_list1 (opt_thm_name ":" -- Scan.repeat1 Parse.propp);
val if_statement = Scan.optional (Parse.$$$ "if" |-- Parse.!!! statement) [];

val cond_statement =
  Parse.$$$ "if" |-- Parse.!!! statement >> pair true ||
  Parse.$$$ "when" |-- Parse.!!! statement >> pair false ||
  Scan.succeed (true, []);

val obtain_case =
  Parse.parbinding --
    (Scan.optional (Parse.and_list1 Parse.vars --| Parse.where_ >> flat) [] --
      (Parse.and_list1 (Scan.repeat1 Parse.prop) >> flat));

val obtains = Parse.enum1 "|" obtain_case;

val long_statement =
  Scan.repeat context_element --
   (Parse.$$$ "obtains" |-- Parse.!!! obtains >> Element.Obtains ||
    Parse.$$$ "shows" |-- Parse.!!! statement >> Element.Shows);

val long_statement_keyword =
  locale_keyword || Parse.$$$ "obtains" || Parse.$$$ "shows";


(* options *)

val overloaded =
  Scan.optional (Parse.$$$ "(" -- Parse.$$$ "overloaded" -- Parse.$$$ ")" >> K true) false;

end;
