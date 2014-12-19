(*  Title:      Pure/Syntax/printer.ML
    Author:     Tobias Nipkow and Markus Wenzel, TU Muenchen

Pretty printing of asts, terms, types and print (ast) translation.
*)

signature BASIC_PRINTER =
sig
  val show_brackets: bool Config.T
  val show_types: bool Config.T
  val show_sorts: bool Config.T
  val show_markup: bool Config.T
  val show_structs: bool Config.T
  val show_question_marks: bool Config.T
  val pretty_priority: int Config.T
end;

signature PRINTER =
sig
  include BASIC_PRINTER
  val show_brackets_raw: Config.raw
  val show_types_raw: Config.raw
  val show_sorts_raw: Config.raw
  val show_markup_default: bool Unsynchronized.ref
  val show_markup_raw: Config.raw
  val show_structs_raw: Config.raw
  val show_question_marks_raw: Config.raw
  val show_type_emphasis: bool Config.T
  val type_emphasis: Proof.context -> typ -> bool
  type prtabs
  val empty_prtabs: prtabs
  val update_prtabs: string -> Syntax_Ext.xprod list -> prtabs -> prtabs
  val remove_prtabs: string -> Syntax_Ext.xprod list -> prtabs -> prtabs
  val merge_prtabs: prtabs -> prtabs -> prtabs
  val pretty_term_ast: bool -> Proof.context -> prtabs ->
    (string -> Proof.context -> Ast.ast list -> Ast.ast) ->
    (string -> Ast.ast list -> Pretty.T option) ->
    (string -> Markup.T list * string) ->
    Ast.ast -> Pretty.T list
  val pretty_typ_ast: Proof.context -> prtabs ->
    (string -> Proof.context -> Ast.ast list -> Ast.ast) ->
    (string -> Ast.ast list -> Pretty.T option) ->
    (string -> Markup.T list * string) -> Ast.ast -> Pretty.T list
end;

structure Printer: PRINTER =
struct

(** options for printing **)

val show_brackets_raw = Config.declare_option ("show_brackets", @{here});
val show_brackets = Config.bool show_brackets_raw;

val show_types_raw = Config.declare_option ("show_types", @{here});
val show_types = Config.bool show_types_raw;

val show_sorts_raw = Config.declare_option ("show_sorts", @{here});
val show_sorts = Config.bool show_sorts_raw;

val show_markup_default = Unsynchronized.ref false;
val show_markup_raw =
  Config.declare ("show_markup", @{here}) (fn _ => Config.Bool (! show_markup_default));
val show_markup = Config.bool show_markup_raw;

val show_structs_raw =
  Config.declare ("show_structs", @{here}) (fn _ => Config.Bool false);
val show_structs = Config.bool show_structs_raw;

val show_question_marks_raw = Config.declare_option ("show_question_marks", @{here});
val show_question_marks = Config.bool show_question_marks_raw;

val show_type_emphasis =
  Config.bool (Config.declare ("show_type_emphasis", @{here}) (fn _ => Config.Bool true));

fun type_emphasis ctxt T =
  T <> dummyT andalso
    (Config.get ctxt show_types orelse Config.get ctxt show_markup orelse
      Config.get ctxt show_type_emphasis andalso not (can dest_Type T));



(** type prtabs **)

datatype symb =
  Arg of int |
  TypArg of int |
  String of bool * string |
  Break of int |
  Block of int * symb list;

type prtabs = (string * ((symb list * int * int) list) Symtab.table) list;

fun mode_tab prtabs mode = the_default Symtab.empty (AList.lookup (op =) prtabs mode);
fun mode_tabs prtabs modes = map_filter (AList.lookup (op =) prtabs) (modes @ [""]);


(* xprod_to_fmt *)

fun xprod_to_fmt (Syntax_Ext.XProd (_, _, "", _)) = NONE
  | xprod_to_fmt (Syntax_Ext.XProd (_, xsymbs, const, pri)) =
      let
        fun arg (s, p) =
          (if s = "type" then TypArg else Arg)
          (if Lexicon.is_terminal s then 1000 else p);

        fun xsyms_to_syms (Syntax_Ext.Delim s :: xsyms) =
              apfst (cons (String (not (exists Symbol.is_block_ctrl (Symbol.explode s)), s)))
                (xsyms_to_syms xsyms)
          | xsyms_to_syms (Syntax_Ext.Argument s_p :: xsyms) =
              apfst (cons (arg s_p)) (xsyms_to_syms xsyms)
          | xsyms_to_syms (Syntax_Ext.Space s :: xsyms) =
              apfst (cons (String (false, s))) (xsyms_to_syms xsyms)
          | xsyms_to_syms (Syntax_Ext.Bg i :: xsyms) =
              let
                val (bsyms, xsyms') = xsyms_to_syms xsyms;
                val (syms, xsyms'') = xsyms_to_syms xsyms';
              in
                (Block (i, bsyms) :: syms, xsyms'')
              end
          | xsyms_to_syms (Syntax_Ext.Brk i :: xsyms) =
              apfst (cons (Break i)) (xsyms_to_syms xsyms)
          | xsyms_to_syms (Syntax_Ext.En :: xsyms) = ([], xsyms)
          | xsyms_to_syms [] = ([], []);

        fun nargs (Arg _ :: syms) = nargs syms + 1
          | nargs (TypArg _ :: syms) = nargs syms + 1
          | nargs (String _ :: syms) = nargs syms
          | nargs (Break _ :: syms) = nargs syms
          | nargs (Block (_, bsyms) :: syms) = nargs syms + nargs bsyms
          | nargs [] = 0;
      in
        (case xsyms_to_syms xsymbs of
          (symbs, []) => SOME (const, (symbs, nargs symbs, pri))
        | _ => raise Fail "Unbalanced pretty-printing blocks")
      end;


(* empty, extend, merge prtabs *)

val empty_prtabs = [];

fun update_prtabs mode xprods prtabs =
  let
    val fmts = map_filter xprod_to_fmt xprods;
    val tab' = fold (Symtab.update_list (op =)) fmts (mode_tab prtabs mode);
  in AList.update (op =) (mode, tab') prtabs end;

fun remove_prtabs mode xprods prtabs =
  let
    val tab = mode_tab prtabs mode;
    val fmts = map_filter (fn xprod as Syntax_Ext.XProd (_, _, c, _) =>
      if null (Symtab.lookup_list tab c) then NONE
      else xprod_to_fmt xprod) xprods;
    val tab' = fold (Symtab.remove_list (op =)) fmts tab;
  in AList.update (op =) (mode, tab') prtabs end;

fun merge_prtabs prtabs1 prtabs2 =
  let
    val modes = distinct (op =) (map fst (prtabs1 @ prtabs2));
    fun merge m = (m, Symtab.merge_list (op =) (mode_tab prtabs1 m, mode_tab prtabs2 m));
  in map merge modes end;



(** pretty term or typ asts **)

fun is_chain [Block (_, pr)] = is_chain pr
  | is_chain [Arg _] = true
  | is_chain _  = false;

val pretty_priority =
  Config.int (Config.declare ("Syntax.pretty_priority", @{here}) (K (Config.Int 0)));

fun pretty type_mode curried ctxt tabs trf markup_trans markup_extern ast0 =
  let
    val show_brackets = Config.get ctxt show_brackets;

    (*default applications: prefix / postfix*)
    val appT =
      if type_mode then Syntax_Trans.tappl_ast_tr'
      else if curried then Syntax_Trans.applC_ast_tr'
      else Syntax_Trans.appl_ast_tr';

    fun synT _ ([], args) = ([], args)
      | synT m (Arg p :: symbs, t :: args) =
          let val (Ts, args') = synT m (symbs, args);
          in (astT (t, p) @ Ts, args') end
      | synT m (TypArg p :: symbs, t :: args) =
          let
            val (Ts, args') = synT m (symbs, args);
          in
            if type_mode then (astT (t, p) @ Ts, args')
            else
              (pretty true curried (Config.put pretty_priority p ctxt)
                tabs trf markup_trans markup_extern t @ Ts, args')
          end
      | synT m (String (do_mark, s) :: symbs, args) =
          let
            val (Ts, args') = synT m (symbs, args);
            val T =
              if do_mark
              then Pretty.marks_str (m @ [Lexicon.literal_markup s], s)
              else Pretty.str s;
          in (T :: Ts, args') end
      | synT m (Block (i, bsymbs) :: symbs, args) =
          let
            val (bTs, args') = synT m (bsymbs, args);
            val (Ts, args'') = synT m (symbs, args');
            val T =
              if i < 0 then Pretty.unbreakable (Pretty.block bTs)
              else Pretty.blk (i, bTs);
          in (T :: Ts, args'') end
      | synT m (Break i :: symbs, args) =
          let
            val (Ts, args') = synT m (symbs, args);
            val T = if i < 0 then Pretty.fbrk else Pretty.brk i;
          in (T :: Ts, args') end

    and parT m (pr, args, p, p': int) = #1 (synT m
          (if p > p' orelse (show_brackets andalso p' <> 1000 andalso not (is_chain pr))
           then [Block (1, String (false, "(") :: pr @ [String (false, ")")])]
           else pr, args))

    and atomT a = Pretty.marks_str (markup_extern a)

    and prefixT (_, a, [], _) = [atomT a]
      | prefixT (c, _, args, p) = astT (appT (c, args), p)

    and splitT 0 ([x], ys) = (x, ys)
      | splitT 0 (rev_xs, ys) = (Ast.Appl (rev rev_xs), ys)
      | splitT n (rev_xs, y :: ys) = splitT (n - 1) (y :: rev_xs, ys)

    and combT (tup as (c, a, args, p)) =
      let
        val nargs = length args;

        (*find matching table entry, or print as prefix / postfix*)
        fun prnt ([], []) = prefixT tup
          | prnt ([], tb :: tbs) = prnt (Symtab.lookup_list tb a, tbs)
          | prnt ((pr, n, p') :: prnps, tbs) =
              if nargs = n then parT (#1 (markup_extern a)) (pr, args, p, p')
              else if nargs > n andalso not type_mode then
                astT (appT (splitT n ([c], args)), p)
              else prnt (prnps, tbs);
      in
        (case markup_trans a args of
          SOME prt => [prt]
        | NONE => astT (trf a ctxt args, p) handle Match => prnt ([], tabs))
      end

    and astT (c as Ast.Constant a, p) = combT (c, a, [], p)
      | astT (ast as Ast.Variable _, _) = [Ast.pretty_ast ast]
      | astT (Ast.Appl ((c as Ast.Constant a) :: (args as _ :: _)), p) = combT (c, a, args, p)
      | astT (Ast.Appl (f :: (args as _ :: _)), p) = astT (appT (f, args), p)
      | astT (ast as Ast.Appl _, _) = raise Ast.AST ("pretty: malformed ast", [ast]);
  in astT (ast0, Config.get ctxt pretty_priority) end;


(* pretty_term_ast *)

fun pretty_term_ast curried ctxt prtabs trf markup_trans extern ast =
  pretty false curried ctxt (mode_tabs prtabs (print_mode_value ())) trf markup_trans extern ast;


(* pretty_typ_ast *)

fun pretty_typ_ast ctxt prtabs trf markup_trans extern ast =
  pretty true false ctxt (mode_tabs prtabs (print_mode_value ())) trf markup_trans extern ast;

end;

structure Basic_Printer: BASIC_PRINTER = Printer;
open Basic_Printer;

