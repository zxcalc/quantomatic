(*  Title:      Pure/Syntax/syntax.ML
    Author:     Tobias Nipkow and Markus Wenzel, TU Muenchen

Standard Isabelle syntax, based on arbitrary context-free grammars
(specified by mixfix declarations).
*)

signature SYNTAX =
sig
  type operations
  val install_operations: operations -> unit
  val root: string Config.T
  val ambiguity_warning_raw: Config.raw
  val ambiguity_warning: bool Config.T
  val ambiguity_limit_raw: Config.raw
  val ambiguity_limit: int Config.T
  val encode_input: Input.source -> XML.tree
  val implode_input: Input.source -> string
  val read_input_pos: string -> Position.T
  val read_input: string -> Input.source
  val parse_input: Proof.context -> (XML.tree list -> 'a) ->
    (bool -> Markup.T) -> (Symbol_Pos.T list * Position.T -> 'a) -> string -> 'a
  val parse_sort: Proof.context -> string -> sort
  val parse_typ: Proof.context -> string -> typ
  val parse_term: Proof.context -> string -> term
  val parse_prop: Proof.context -> string -> term
  val unparse_sort: Proof.context -> sort -> Pretty.T
  val unparse_classrel: Proof.context -> class list -> Pretty.T
  val unparse_arity: Proof.context -> arity -> Pretty.T
  val unparse_typ: Proof.context -> typ -> Pretty.T
  val unparse_term: Proof.context -> term -> Pretty.T
  val check_sort: Proof.context -> sort -> sort
  val check_typ: Proof.context -> typ -> typ
  val check_term: Proof.context -> term -> term
  val check_prop: Proof.context -> term -> term
  val check_typs: Proof.context -> typ list -> typ list
  val check_terms: Proof.context -> term list -> term list
  val check_props: Proof.context -> term list -> term list
  val uncheck_sort: Proof.context -> sort -> sort
  val uncheck_arity: Proof.context -> arity -> arity
  val uncheck_classrel: Proof.context -> class list -> class list
  val uncheck_typs: Proof.context -> typ list -> typ list
  val uncheck_terms: Proof.context -> term list -> term list
  val read_sort: Proof.context -> string -> sort
  val read_typ: Proof.context -> string -> typ
  val read_term: Proof.context -> string -> term
  val read_prop: Proof.context -> string -> term
  val read_typs: Proof.context -> string list -> typ list
  val read_terms: Proof.context -> string list -> term list
  val read_props: Proof.context -> string list -> term list
  val read_sort_global: theory -> string -> sort
  val read_typ_global: theory -> string -> typ
  val read_term_global: theory -> string -> term
  val read_prop_global: theory -> string -> term
  val pretty_term: Proof.context -> term -> Pretty.T
  val pretty_typ: Proof.context -> typ -> Pretty.T
  val pretty_sort: Proof.context -> sort -> Pretty.T
  val pretty_classrel: Proof.context -> class list -> Pretty.T
  val pretty_arity: Proof.context -> arity -> Pretty.T
  val string_of_term: Proof.context -> term -> string
  val string_of_typ: Proof.context -> typ -> string
  val string_of_sort: Proof.context -> sort -> string
  val string_of_classrel: Proof.context -> class list -> string
  val string_of_arity: Proof.context -> arity -> string
  val is_pretty_global: Proof.context -> bool
  val set_pretty_global: bool -> Proof.context -> Proof.context
  val init_pretty_global: theory -> Proof.context
  val init_pretty: Context.pretty -> Proof.context
  val pretty_term_global: theory -> term -> Pretty.T
  val pretty_typ_global: theory -> typ -> Pretty.T
  val pretty_sort_global: theory -> sort -> Pretty.T
  val string_of_term_global: theory -> term -> string
  val string_of_typ_global: theory -> typ -> string
  val string_of_sort_global: theory -> sort -> string
  type syntax
  val eq_syntax: syntax * syntax -> bool
  val force_syntax: syntax -> unit
  val lookup_const: syntax -> string -> string option
  val is_keyword: syntax -> string -> bool
  val tokenize: syntax -> bool -> Symbol_Pos.T list -> Lexicon.token list
  val parse: syntax -> string -> Lexicon.token list -> Parser.parsetree list
  val parse_ast_translation: syntax -> string -> (Proof.context -> Ast.ast list -> Ast.ast) option
  val parse_rules: syntax -> string -> (Ast.ast * Ast.ast) list
  val parse_translation: syntax -> string -> (Proof.context -> term list -> term) option
  val print_translation: syntax -> string ->
    Proof.context -> typ -> term list -> term  (*exception Match*)
  val print_rules: syntax -> string -> (Ast.ast * Ast.ast) list
  val print_ast_translation: syntax -> string ->
    Proof.context -> Ast.ast list -> Ast.ast  (*exception Match*)
  val prtabs: syntax -> Printer.prtabs
  type mode
  val mode_default: mode
  val mode_input: mode
  val empty_syntax: syntax
  val merge_syntax: syntax * syntax -> syntax
  val print_gram: syntax -> unit
  val print_trans: syntax -> unit
  val print_syntax: syntax -> unit
  val guess_infix: syntax -> string -> mixfix option
  datatype 'a trrule =
    Parse_Rule of 'a * 'a |
    Print_Rule of 'a * 'a |
    Parse_Print_Rule of 'a * 'a
  val map_trrule: ('a -> 'b) -> 'a trrule -> 'b trrule
  val update_trfuns:
    (string * ((Proof.context -> Ast.ast list -> Ast.ast) * stamp)) list *
    (string * ((Proof.context -> term list -> term) * stamp)) list *
    (string * ((Proof.context -> typ -> term list -> term) * stamp)) list *
    (string * ((Proof.context -> Ast.ast list -> Ast.ast) * stamp)) list -> syntax -> syntax
  val update_type_gram: bool -> mode -> (string * typ * mixfix) list -> syntax -> syntax
  val update_const_gram: bool -> string list ->
    mode -> (string * typ * mixfix) list -> syntax -> syntax
  val update_trrules: Ast.ast trrule list -> syntax -> syntax
  val remove_trrules: Ast.ast trrule list -> syntax -> syntax
  val const: string -> term
  val free: string -> term
  val var: indexname -> term
end;

structure Syntax: SYNTAX =
struct


(** inner syntax operations **)

(* back-patched operations *)

type operations =
 {parse_sort: Proof.context -> string -> sort,
  parse_typ: Proof.context -> string -> typ,
  parse_term: Proof.context -> string -> term,
  parse_prop: Proof.context -> string -> term,
  unparse_sort: Proof.context -> sort -> Pretty.T,
  unparse_typ: Proof.context -> typ -> Pretty.T,
  unparse_term: Proof.context -> term -> Pretty.T,
  check_typs: Proof.context -> typ list -> typ list,
  check_terms: Proof.context -> term list -> term list,
  check_props: Proof.context -> term list -> term list,
  uncheck_typs: Proof.context -> typ list -> typ list,
  uncheck_terms: Proof.context -> term list -> term list};

val operations: operations Single_Assignment.var = Single_Assignment.var "Syntax.operations";
fun install_operations ops = Single_Assignment.assign operations ops;

fun operation which ctxt x =
  (case Single_Assignment.peek operations of
    NONE => raise Fail "Inner syntax operations not installed"
  | SOME ops => which ops ctxt x);


(* configuration options *)

val root = Config.string (Config.declare ("syntax_root", @{here}) (K (Config.String "any")));

val ambiguity_warning_raw =
  Config.declare ("syntax_ambiguity_warning", @{here}) (fn _ => Config.Bool true);
val ambiguity_warning = Config.bool ambiguity_warning_raw;

val ambiguity_limit_raw =
  Config.declare ("syntax_ambiguity_limit", @{here}) (fn _ => Config.Int 10);
val ambiguity_limit = Config.int ambiguity_limit_raw;


(* formal input *)

fun encode_input source =
  let
    val delimited = Input.is_delimited source;
    val pos = Input.pos_of source;
    val text = Input.text_of source;
  in XML.Elem (Markup.input delimited (Position.properties_of pos), [XML.Text text]) end;

val implode_input = encode_input #> YXML.string_of;

local

fun input_range (XML.Elem ((name, props), _)) =
      if name = Markup.inputN
      then (Markup.is_delimited props, Position.range_of_properties props)
      else (false, Position.no_range)
  | input_range (XML.Text _) = (false, Position.no_range);

fun input_source tree =
  let
    val text = XML.content_of [tree];
    val (delimited, range) = input_range tree;
  in Input.source delimited text range end;

in

fun read_input_pos str = #1 (#2 (input_range (YXML.parse str handle Fail msg => error msg)));

fun read_input str = input_source (YXML.parse str handle Fail msg => error msg);

fun parse_input ctxt decode markup parse str =
  let
    fun parse_tree tree =
      let
        val source = input_source tree;
        val syms = Input.source_explode source;
        val pos = Input.pos_of source;
        val _ = Context_Position.report ctxt pos (markup (Input.is_delimited source));
      in parse (syms, pos) end;
  in
    (case YXML.parse_body str handle Fail msg => error msg of
      body as [tree as XML.Elem ((name, _), _)] =>
        if name = Markup.inputN then parse_tree tree else decode body
    | [tree as XML.Text _] => parse_tree tree
    | body => decode body)
  end;

end;


(* (un)parsing *)

val parse_sort = operation #parse_sort;
val parse_typ = operation #parse_typ;
val parse_term = operation #parse_term;
val parse_prop = operation #parse_prop;
val unparse_sort = operation #unparse_sort;
val unparse_typ = operation #unparse_typ;
val unparse_term = operation #unparse_term;


(* (un)checking *)

val check_typs = operation #check_typs;
val check_terms = operation #check_terms;
val check_props = operation #check_props;

val check_typ = singleton o check_typs;
val check_term = singleton o check_terms;
val check_prop = singleton o check_props;

val uncheck_typs = operation #uncheck_typs;
val uncheck_terms = operation #uncheck_terms;


(* derived operations for algebra of sorts *)

local

fun map_sort f S =
  (case f (TFree ("", S)) of
    TFree ("", S') => S'
  | _ => raise TYPE ("map_sort", [TFree ("", S)], []));

in

val check_sort = map_sort o check_typ;
val uncheck_sort = map_sort o singleton o uncheck_typs;

end;


val uncheck_classrel = map o singleton o uncheck_sort;

fun unparse_classrel ctxt cs = Pretty.block (flat
  (separate [Pretty.str " <", Pretty.brk 1] (map (single o unparse_sort ctxt o single) cs)));

fun uncheck_arity ctxt (a, Ss, S) =
  let
    val T = Type (a, replicate (length Ss) dummyT);
    val a' =
      (case singleton (uncheck_typs ctxt) T of
        Type (a', _) => a'
      | T => raise TYPE ("uncheck_arity", [T], []));
    val Ss' = map (uncheck_sort ctxt) Ss;
    val S' = uncheck_sort ctxt S;
  in (a', Ss', S') end;

fun unparse_arity ctxt (a, Ss, S) =
  let
    val prtT = unparse_typ ctxt (Type (a, []));
    val dom =
      if null Ss then []
      else [Pretty.list "(" ")" (map (unparse_sort ctxt) Ss), Pretty.brk 1];
  in Pretty.block ([prtT, Pretty.str " ::", Pretty.brk 1] @ dom @ [unparse_sort ctxt S]) end;


(* read = parse + check *)

fun read_sort ctxt = parse_sort ctxt #> check_sort ctxt;

fun read_typs ctxt =
  grouped 10 Par_List.map_independent (parse_typ ctxt) #> check_typs ctxt;

fun read_terms ctxt =
  grouped 10 Par_List.map_independent (parse_term ctxt) #> check_terms ctxt;

fun read_props ctxt =
  grouped 10 Par_List.map_independent (parse_prop ctxt) #> check_props ctxt;

val read_typ = singleton o read_typs;
val read_term = singleton o read_terms;
val read_prop = singleton o read_props;

val read_sort_global = read_sort o Proof_Context.init_global;
val read_typ_global = read_typ o Proof_Context.init_global;
val read_term_global = read_term o Proof_Context.init_global;
val read_prop_global = read_prop o Proof_Context.init_global;


(* pretty = uncheck + unparse *)

fun pretty_term ctxt = singleton (uncheck_terms ctxt) #> unparse_term ctxt;
fun pretty_typ ctxt = singleton (uncheck_typs ctxt) #> unparse_typ ctxt;
fun pretty_sort ctxt = uncheck_sort ctxt #> unparse_sort ctxt;
fun pretty_classrel ctxt = uncheck_classrel ctxt #> unparse_classrel ctxt;
fun pretty_arity ctxt = uncheck_arity ctxt #> unparse_arity ctxt;

val string_of_term = Pretty.string_of oo pretty_term;
val string_of_typ = Pretty.string_of oo pretty_typ;
val string_of_sort = Pretty.string_of oo pretty_sort;
val string_of_classrel = Pretty.string_of oo pretty_classrel;
val string_of_arity = Pretty.string_of oo pretty_arity;


(* global pretty printing *)

val pretty_global =
  Config.bool (Config.declare ("Syntax.pretty_global", @{here}) (K (Config.Bool false)));
fun is_pretty_global ctxt = Config.get ctxt pretty_global;
val set_pretty_global = Config.put pretty_global;
val init_pretty_global = set_pretty_global true o Proof_Context.init_global;
val init_pretty = Context.pretty_context init_pretty_global;

val pretty_term_global = pretty_term o init_pretty_global;
val pretty_typ_global = pretty_typ o init_pretty_global;
val pretty_sort_global = pretty_sort o init_pretty_global;

val string_of_term_global = string_of_term o init_pretty_global;
val string_of_typ_global = string_of_typ o init_pretty_global;
val string_of_sort_global = string_of_sort o init_pretty_global;



(** tables of translation functions **)

(* parse (ast) translations *)

fun err_dup_trfun name c =
  error ("More than one " ^ name ^ " for " ^ quote c);

fun lookup_tr tab c = Option.map fst (Symtab.lookup tab c);

fun remove_trtab trfuns = fold (Symtab.remove Syntax_Ext.eq_trfun) trfuns;

fun update_trtab name trfuns tab = fold Symtab.update_new trfuns (remove_trtab trfuns tab)
  handle Symtab.DUP c => err_dup_trfun name c;

fun merge_trtabs name tab1 tab2 = Symtab.merge Syntax_Ext.eq_trfun (tab1, tab2)
  handle Symtab.DUP c => err_dup_trfun name c;


(* print (ast) translations *)

fun apply_tr' tab c ctxt T args =
  let
    val fns = map fst (Symtab.lookup_list tab c);
    fun app_first [] = raise Match
      | app_first (f :: fs) = f ctxt T args handle Match => app_first fs;
  in app_first fns end;

fun apply_ast_tr' tab c ctxt args =
  let
    val fns = map fst (Symtab.lookup_list tab c);
    fun app_first [] = raise Match
      | app_first (f :: fs) = f ctxt args handle Match => app_first fs;
  in app_first fns end;

fun update_tr'tab trfuns = fold_rev (Symtab.update_list Syntax_Ext.eq_trfun) trfuns;
fun remove_tr'tab trfuns = fold (Symtab.remove_list Syntax_Ext.eq_trfun) trfuns;
fun merge_tr'tabs tab1 tab2 = Symtab.merge_list Syntax_Ext.eq_trfun (tab1, tab2);



(** tables of translation rules **)

type ruletab = (Ast.ast * Ast.ast) list Symtab.table;

fun dest_ruletab tab = maps snd (Symtab.dest tab);

val update_ruletab = fold_rev (fn r => Symtab.update_list (op =) (Ast.head_of_rule r, r));
val remove_ruletab = fold (fn r => Symtab.remove_list (op =) (Ast.head_of_rule r, r));
fun merge_ruletabs tab1 tab2 = Symtab.merge_list (op =) (tab1, tab2);



(** datatype syntax **)

datatype syntax =
  Syntax of {
    input: Syntax_Ext.xprod list,
    lexicon: Scan.lexicon,
    gram: Parser.gram lazy,
    consts: string Symtab.table,
    prmodes: string list,
    parse_ast_trtab: ((Proof.context -> Ast.ast list -> Ast.ast) * stamp) Symtab.table,
    parse_ruletab: ruletab,
    parse_trtab: ((Proof.context -> term list -> term) * stamp) Symtab.table,
    print_trtab: ((Proof.context -> typ -> term list -> term) * stamp) list Symtab.table,
    print_ruletab: ruletab,
    print_ast_trtab: ((Proof.context -> Ast.ast list -> Ast.ast) * stamp) list Symtab.table,
    prtabs: Printer.prtabs} * stamp;

fun eq_syntax (Syntax (_, s1), Syntax (_, s2)) = s1 = s2;

fun force_syntax (Syntax ({gram, ...}, _)) = ignore (Lazy.force gram);

fun lookup_const (Syntax ({consts, ...}, _)) = Symtab.lookup consts;
fun is_keyword (Syntax ({lexicon, ...}, _)) = Scan.is_literal lexicon o Symbol.explode;
fun tokenize (Syntax ({lexicon, ...}, _)) = Lexicon.tokenize lexicon;
fun parse (Syntax ({gram, ...}, _)) = Parser.parse (Lazy.force gram);

fun parse_ast_translation (Syntax ({parse_ast_trtab, ...}, _)) = lookup_tr parse_ast_trtab;
fun parse_translation (Syntax ({parse_trtab, ...}, _)) = lookup_tr parse_trtab;
fun parse_rules (Syntax ({parse_ruletab, ...}, _)) = Symtab.lookup_list parse_ruletab;
fun print_translation (Syntax ({print_trtab, ...}, _)) = apply_tr' print_trtab;
fun print_rules (Syntax ({print_ruletab, ...}, _)) = Symtab.lookup_list print_ruletab;
fun print_ast_translation (Syntax ({print_ast_trtab, ...}, _)) = apply_ast_tr' print_ast_trtab;

fun prtabs (Syntax ({prtabs, ...}, _)) = prtabs;

type mode = string * bool;
val mode_default = ("", true);
val mode_input = (Print_Mode.input, true);


(* empty_syntax *)

val empty_syntax = Syntax
  ({input = [],
    lexicon = Scan.empty_lexicon,
    gram = Lazy.value Parser.empty_gram,
    consts = Symtab.empty,
    prmodes = [],
    parse_ast_trtab = Symtab.empty,
    parse_ruletab = Symtab.empty,
    parse_trtab = Symtab.empty,
    print_trtab = Symtab.empty,
    print_ruletab = Symtab.empty,
    print_ast_trtab = Symtab.empty,
    prtabs = Printer.empty_prtabs}, stamp ());


(* update_syntax *)

fun update_const (c, b) tab =
  if c = "" orelse (b = "" andalso (Lexicon.is_marked c orelse Symtab.defined tab c))
  then tab
  else Symtab.update (c, b) tab;

fun update_syntax (mode, inout) syn_ext (Syntax (tabs, _)) =
  let
    val {input, lexicon, gram, consts = consts1, prmodes, parse_ast_trtab, parse_ruletab,
      parse_trtab, print_trtab, print_ruletab, print_ast_trtab, prtabs} = tabs;
    val Syntax_Ext.Syn_Ext {xprods, consts = consts2, parse_ast_translation,
      parse_rules, parse_translation, print_translation, print_rules,
      print_ast_translation} = syn_ext;
    val new_xprods =
      if inout then distinct (op =) (filter_out (member (op =) input) xprods) else [];
    fun if_inout xs = if inout then xs else [];
  in
    Syntax
    ({input = new_xprods @ input,
      lexicon = fold Scan.extend_lexicon (Syntax_Ext.delims_of new_xprods) lexicon,
      gram = Lazy.value (Parser.extend_gram new_xprods (Lazy.force gram)),
      consts = fold update_const consts2 consts1,
      prmodes = insert (op =) mode prmodes,
      parse_ast_trtab =
        update_trtab "parse ast translation" (if_inout parse_ast_translation) parse_ast_trtab,
      parse_ruletab = update_ruletab (if_inout parse_rules) parse_ruletab,
      parse_trtab = update_trtab "parse translation" (if_inout parse_translation) parse_trtab,
      print_trtab = update_tr'tab print_translation print_trtab,
      print_ruletab = update_ruletab print_rules print_ruletab,
      print_ast_trtab = update_tr'tab print_ast_translation print_ast_trtab,
      prtabs = Printer.update_prtabs mode xprods prtabs}, stamp ())
  end;


(* remove_syntax *)

fun remove_syntax (mode, inout) syn_ext (Syntax (tabs, _)) =
  let
    val Syntax_Ext.Syn_Ext {xprods, consts = _, parse_ast_translation, parse_rules,
      parse_translation, print_translation, print_rules, print_ast_translation} = syn_ext;
    val {input, lexicon, gram, consts, prmodes, parse_ast_trtab, parse_ruletab,
      parse_trtab, print_trtab, print_ruletab, print_ast_trtab, prtabs} = tabs;
    val input' = if inout then subtract (op =) xprods input else input;
    val changed = length input <> length input';
    fun if_inout xs = if inout then xs else [];
  in
    Syntax
    ({input = input',
      lexicon = if changed then Scan.make_lexicon (Syntax_Ext.delims_of input') else lexicon,
      gram = if changed then Lazy.value (Parser.make_gram input') else gram,
      consts = consts,
      prmodes = prmodes,
      parse_ast_trtab = remove_trtab (if_inout parse_ast_translation) parse_ast_trtab,
      parse_ruletab = remove_ruletab (if_inout parse_rules) parse_ruletab,
      parse_trtab = remove_trtab (if_inout parse_translation) parse_trtab,
      print_trtab = remove_tr'tab print_translation print_trtab,
      print_ruletab = remove_ruletab print_rules print_ruletab,
      print_ast_trtab = remove_tr'tab print_ast_translation print_ast_trtab,
      prtabs = Printer.remove_prtabs mode xprods prtabs}, stamp ())
  end;


(* merge_syntax *)

fun merge_syntax (Syntax (tabs1, _), Syntax (tabs2, _)) =
  let
    val {input = input1, lexicon = lexicon1, gram = gram1, consts = consts1,
      prmodes = prmodes1, parse_ast_trtab = parse_ast_trtab1, parse_ruletab = parse_ruletab1,
      parse_trtab = parse_trtab1, print_trtab = print_trtab1, print_ruletab = print_ruletab1,
      print_ast_trtab = print_ast_trtab1, prtabs = prtabs1} = tabs1;

    val {input = input2, lexicon = lexicon2, gram = gram2, consts = consts2,
      prmodes = prmodes2, parse_ast_trtab = parse_ast_trtab2, parse_ruletab = parse_ruletab2,
      parse_trtab = parse_trtab2, print_trtab = print_trtab2, print_ruletab = print_ruletab2,
      print_ast_trtab = print_ast_trtab2, prtabs = prtabs2} = tabs2;

    val (input', gram') =
      (case subtract (op =) input1 input2 of
        [] => (input1, gram1)
      | new_xprods2 =>
          if subset (op =) (input1, input2) then (input2, gram2)
          else
            let
              val input' = new_xprods2 @ input1;
              val gram' = Lazy.lazy (fn () => Parser.make_gram input');
            in (input', gram') end);
  in
    Syntax
    ({input = input',
      lexicon = Scan.merge_lexicons (lexicon1, lexicon2),
      gram = gram',
      consts = Symtab.merge (K true) (consts1, consts2),
      prmodes = Library.merge (op =) (prmodes1, prmodes2),
      parse_ast_trtab =
        merge_trtabs "parse ast translation" parse_ast_trtab1 parse_ast_trtab2,
      parse_ruletab = merge_ruletabs parse_ruletab1 parse_ruletab2,
      parse_trtab = merge_trtabs "parse translation" parse_trtab1 parse_trtab2,
      print_trtab = merge_tr'tabs print_trtab1 print_trtab2,
      print_ruletab = merge_ruletabs print_ruletab1 print_ruletab2,
      print_ast_trtab = merge_tr'tabs print_ast_trtab1 print_ast_trtab2,
      prtabs = Printer.merge_prtabs prtabs1 prtabs2}, stamp ())
  end;



(** print syntax **)

local

fun pretty_strs_qs name strs =
  Pretty.strs (name :: map quote (sort_strings strs));

fun pretty_gram (Syntax (tabs, _)) =
  let
    val {lexicon, prmodes, gram, ...} = tabs;
    val prmodes' = sort_strings (filter_out (fn s => s = "") prmodes);
  in
    [pretty_strs_qs "lexicon:" (Scan.dest_lexicon lexicon),
      Pretty.big_list "prods:" (Parser.pretty_gram (Lazy.force gram)),
      pretty_strs_qs "print modes:" prmodes']
  end;

fun pretty_trans (Syntax (tabs, _)) =
  let
    fun pretty_tab name tab =
      pretty_strs_qs name (sort_strings (Symtab.keys tab));

    fun pretty_ruletab name tab =
      Pretty.big_list name (map (Pretty.item o single o Ast.pretty_rule) (dest_ruletab tab));

    val {consts, parse_ast_trtab, parse_ruletab, parse_trtab, print_trtab,
      print_ruletab, print_ast_trtab, ...} = tabs;
  in
   [pretty_tab "consts:" consts,
    pretty_tab "parse_ast_translation:" parse_ast_trtab,
    pretty_ruletab "parse_rules:" parse_ruletab,
    pretty_tab "parse_translation:" parse_trtab,
    pretty_tab "print_translation:" print_trtab,
    pretty_ruletab "print_rules:" print_ruletab,
    pretty_tab "print_ast_translation:" print_ast_trtab]
  end;

in

fun print_gram syn = Pretty.writeln_chunks (pretty_gram syn);
fun print_trans syn = Pretty.writeln_chunks (pretty_trans syn);
fun print_syntax syn = Pretty.writeln_chunks (pretty_gram syn @ pretty_trans syn);

end;


(* reconstructing infixes -- educated guessing *)

fun guess_infix (Syntax ({gram, ...}, _)) c =
  (case Parser.guess_infix_lr (Lazy.force gram) c of
    SOME (s, l, r, j) => SOME
     (if l then Mixfix.Infixl (s, j)
      else if r then Mixfix.Infixr (s, j)
      else Mixfix.Infix (s, j))
  | NONE => NONE);



(** prepare translation rules **)

(* rules *)

datatype 'a trrule =
  Parse_Rule of 'a * 'a |
  Print_Rule of 'a * 'a |
  Parse_Print_Rule of 'a * 'a;

fun map_trrule f (Parse_Rule (x, y)) = Parse_Rule (f x, f y)
  | map_trrule f (Print_Rule (x, y)) = Print_Rule (f x, f y)
  | map_trrule f (Parse_Print_Rule (x, y)) = Parse_Print_Rule (f x, f y);

fun parse_rule (Parse_Rule pats) = SOME pats
  | parse_rule (Print_Rule _) = NONE
  | parse_rule (Parse_Print_Rule pats) = SOME pats;

fun print_rule (Parse_Rule _) = NONE
  | print_rule (Print_Rule pats) = SOME (swap pats)
  | print_rule (Parse_Print_Rule pats) = SOME (swap pats);


(* check_rules *)

local

fun check_rule rule =
  (case Ast.rule_error rule of
    SOME msg =>
      error ("Error in syntax translation rule: " ^ msg ^ "\n" ^
        Pretty.string_of (Ast.pretty_rule rule))
  | NONE => rule);

in

fun check_rules rules =
 (map check_rule (map_filter parse_rule rules),
  map check_rule (map_filter print_rule rules));

end;



(** modify syntax **)

val update_trfuns = update_syntax mode_default o Syntax_Ext.syn_ext_trfuns;

fun update_type_gram add prmode decls =
  (if add then update_syntax else remove_syntax) prmode (Mixfix.syn_ext_types decls);

fun update_const_gram add logical_types prmode decls =
  (if add then update_syntax else remove_syntax) prmode (Mixfix.syn_ext_consts logical_types decls);

val update_trrules = update_syntax mode_default o Syntax_Ext.syn_ext_rules o check_rules;
val remove_trrules = remove_syntax mode_default o Syntax_Ext.syn_ext_rules o check_rules;


open Lexicon.Syntax;

end;

