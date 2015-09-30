(*  Title:      Pure/Syntax/syntax_ext.ML
    Author:     Markus Wenzel and Carsten Clasohm, TU Muenchen

Syntax extension.
*)

signature SYNTAX_EXT =
sig
  val dddot_indexname: indexname
  datatype mfix = Mfix of string * typ * string * int list * int
  val err_in_mfix: string -> mfix -> 'a
  val typ_to_nonterm: typ -> string
  datatype xsymb =
    Delim of string |
    Argument of string * int |
    Space of string |
    Bg of int | Brk of int | En
  datatype xprod = XProd of string * xsymb list * string * int
  val chain_pri: int
  val delims_of: xprod list -> string list list
  datatype syn_ext =
    Syn_Ext of {
      xprods: xprod list,
      consts: (string * string) list,
      parse_ast_translation: (string * ((Proof.context -> Ast.ast list -> Ast.ast) * stamp)) list,
      parse_rules: (Ast.ast * Ast.ast) list,
      parse_translation: (string * ((Proof.context -> term list -> term) * stamp)) list,
      print_translation: (string * ((Proof.context -> typ -> term list -> term) * stamp)) list,
      print_rules: (Ast.ast * Ast.ast) list,
      print_ast_translation: (string * ((Proof.context -> Ast.ast list -> Ast.ast) * stamp)) list}
  val mfix_delims: string -> string list
  val mfix_args: string -> int
  val escape: string -> string
  val syn_ext': string list -> mfix list ->
    (string * string) list -> (string * ((Proof.context -> Ast.ast list -> Ast.ast) * stamp)) list *
    (string * ((Proof.context -> term list -> term) * stamp)) list *
    (string * ((Proof.context -> typ -> term list -> term) * stamp)) list *
    (string * ((Proof.context -> Ast.ast list -> Ast.ast) * stamp)) list ->
    (Ast.ast * Ast.ast) list * (Ast.ast * Ast.ast) list -> syn_ext
  val syn_ext: mfix list -> (string * string) list ->
    (string * ((Proof.context -> Ast.ast list -> Ast.ast) * stamp)) list *
    (string * ((Proof.context -> term list -> term) * stamp)) list *
    (string * ((Proof.context -> typ -> term list -> term) * stamp)) list *
    (string * ((Proof.context -> Ast.ast list -> Ast.ast) * stamp)) list ->
    (Ast.ast * Ast.ast) list * (Ast.ast * Ast.ast) list -> syn_ext
  val syn_ext_rules: (Ast.ast * Ast.ast) list * (Ast.ast * Ast.ast) list -> syn_ext
  val syn_ext_trfuns:
    (string * ((Proof.context -> Ast.ast list -> Ast.ast) * stamp)) list *
    (string * ((Proof.context -> term list -> term) * stamp)) list *
    (string * ((Proof.context -> typ -> term list -> term) * stamp)) list *
    (string * ((Proof.context -> Ast.ast list -> Ast.ast) * stamp)) list -> syn_ext
  val stamp_trfun: stamp -> string * 'a -> string * ('a * stamp)
  val mk_trfun: string * 'a -> string * ('a * stamp)
  val eq_trfun: ('a * stamp) * ('a * stamp) -> bool
end;

structure Syntax_Ext: SYNTAX_EXT =
struct


(** misc definitions **)

val dddot_indexname = ("dddot", 0);


(** datatype xprod **)

(*Delim s: delimiter s
  Argument (s, p): nonterminal s requiring priority >= p, or valued token
  Space s: some white space for printing
  Bg, Brk, En: blocks and breaks for pretty printing*)

datatype xsymb =
  Delim of string |
  Argument of string * int |
  Space of string |
  Bg of int | Brk of int | En;

fun is_delim (Delim _) = true
  | is_delim _ = false;

fun is_terminal (Delim _) = true
  | is_terminal (Argument (s, _)) = Lexicon.is_terminal s
  | is_terminal _ = false;

fun is_argument (Argument _) = true
  | is_argument _ = false;

fun is_index (Argument ("index", _)) = true
  | is_index _ = false;

val index = Argument ("index", 1000);


(*XProd (lhs, syms, c, p):
    lhs: name of nonterminal on the lhs of the production
    syms: list of symbols on the rhs of the production
    c: head of parse tree
    p: priority of this production*)

datatype xprod = XProd of string * xsymb list * string * int;

val chain_pri = ~1;   (*dummy for chain productions*)

fun delims_of xprods =
  fold (fn XProd (_, xsymbs, _, _) =>
    fold (fn Delim s => insert (op =) s | _ => I) xsymbs) xprods []
  |> map Symbol.explode;



(** datatype mfix **)

(*Mfix (sy, ty, c, ps, p):
    sy: rhs of production as symbolic string
    ty: type description of production
    c: head of parse tree
    ps: priorities of arguments in sy
    p: priority of production*)

datatype mfix = Mfix of string * typ * string * int list * int;

fun err_in_mfix msg (Mfix (sy, _, const, _, _)) =
  cat_error msg ("in mixfix annotation " ^ quote sy ^ " for " ^ quote const);


(* typ_to_nonterm *)

fun typ_to_nt _ (Type (c, _)) = c
  | typ_to_nt default _ = default;

(*get nonterminal for rhs*)
val typ_to_nonterm = typ_to_nt "any";

(*get nonterminal for lhs*)
val typ_to_nonterm1 = typ_to_nt "logic";


(* read mixfix annotations *)

local

val is_meta = member (op =) ["(", ")", "/", "_", "\\<index>"];

val scan_delim_char =
  $$ "'" |-- Scan.one ((not o Symbol.is_blank) andf Symbol.not_eof) ||
  Scan.one ((not o is_meta) andf (not o Symbol.is_blank) andf Symbol.not_eof);

fun read_int ["0", "0"] = ~1
  | read_int cs = #1 (Library.read_int cs);

val scan_sym =
  $$ "_" >> K (Argument ("", 0)) ||
  $$ "\\<index>" >> K index ||
  $$ "(" |-- Scan.many Symbol.is_digit >> (Bg o read_int) ||
  $$ ")" >> K En ||
  $$ "/" -- $$ "/" >> K (Brk ~1) ||
  $$ "/" |-- Scan.many Symbol.is_blank >> (Brk o length) ||
  Scan.many1 Symbol.is_blank >> (Space o implode) ||
  Scan.repeat1 scan_delim_char >> (Delim o implode);

val scan_symb =
  scan_sym >> SOME ||
  $$ "'" -- Scan.one Symbol.is_blank >> K NONE;

val scan_symbs = Scan.repeat scan_symb --| Scan.ahead (~$$ "'");
val read_symbs = map_filter I o the o Scan.read Symbol.stopper scan_symbs;

fun unique_index xsymbs =
  if length (filter is_index xsymbs) <= 1 then xsymbs
  else error "Duplicate index arguments (\\<index>)";

in

val read_mfix = unique_index o read_symbs o Symbol.explode;

fun mfix_delims sy = fold_rev (fn Delim s => cons s | _ => I) (read_mfix sy) [];
val mfix_args = length o filter is_argument o read_mfix;

val escape = implode o map (fn s => if is_meta s then "'" ^ s else s) o Symbol.explode;

end;


(* mfix_to_xprod *)

fun mfix_to_xprod logical_types (mfix as Mfix (sy, typ, const, pris, pri)) =
  let
    val is_logtype = member (op =) logical_types;

    fun check_pri p =
      if p >= 0 andalso p <= 1000 then ()
      else err_in_mfix ("Precedence out of range: " ^ string_of_int p) mfix;

    fun blocks_ok [] 0 = true
      | blocks_ok [] _ = false
      | blocks_ok (Bg _ :: syms) n = blocks_ok syms (n + 1)
      | blocks_ok (En :: _) 0 = false
      | blocks_ok (En :: syms) n = blocks_ok syms (n - 1)
      | blocks_ok (_ :: syms) n = blocks_ok syms n;

    fun check_blocks syms =
      if blocks_ok syms 0 then ()
      else err_in_mfix "Unbalanced block parentheses" mfix;


    val cons_fst = apfst o cons;

    fun add_args [] ty [] = ([], typ_to_nonterm1 ty)
      | add_args [] _ _ = err_in_mfix "Too many precedences" mfix
      | add_args ((arg as Argument ("index", _)) :: syms) ty ps =
          cons_fst arg (add_args syms ty ps)
      | add_args (Argument _ :: syms) (Type ("fun", [ty, tys])) [] =
          cons_fst (Argument (typ_to_nonterm ty, 0)) (add_args syms tys [])
      | add_args (Argument _ :: syms) (Type ("fun", [ty, tys])) (p :: ps) =
          cons_fst (Argument (typ_to_nonterm ty, p)) (add_args syms tys ps)
      | add_args (Argument _ :: _) _ _ =
          err_in_mfix "More arguments than in corresponding type" mfix
      | add_args (sym :: syms) ty ps = cons_fst sym (add_args syms ty ps);

    fun rem_pri (Argument (s, _)) = Argument (s, chain_pri)
      | rem_pri sym = sym;

    fun logify_types (a as (Argument (s, p))) =
          if s <> "prop" andalso is_logtype s then Argument ("logic", p) else a
      | logify_types a = a;


    val raw_symbs = read_mfix sy handle ERROR msg => err_in_mfix msg mfix;
    val args = filter (fn Argument _ => true | _ => false) raw_symbs;
    val (const', typ', syntax_consts, parse_rules) =
      if not (exists is_index args) then (const, typ, NONE, NONE)
      else
        let
          val indexed_const =
            if const <> "" then const ^ "_indexed"
            else err_in_mfix "Missing constant name for indexed syntax" mfix;
          val rangeT = Term.range_type typ handle Match =>
            err_in_mfix "Missing structure argument for indexed syntax" mfix;

          val xs = map Ast.Variable (Name.invent Name.context "xa" (length args - 1));
          val (xs1, xs2) = chop (find_index is_index args) xs;
          val i = Ast.Variable "i";
          val lhs = Ast.mk_appl (Ast.Constant indexed_const)
            (xs1 @ [Ast.mk_appl (Ast.Constant "_index") [i]] @ xs2);
          val rhs = Ast.mk_appl (Ast.Constant const) (i :: xs);
        in (indexed_const, rangeT, SOME (indexed_const, const), SOME (lhs, rhs)) end;

    val (symbs, lhs) = add_args raw_symbs typ' pris;

    val copy_prod =
      (lhs = "prop" orelse lhs = "logic")
        andalso const <> ""
        andalso not (null symbs)
        andalso not (exists is_delim symbs);
    val lhs' =
      if copy_prod orelse lhs = "prop" andalso symbs = [Argument ("prop'", 0)] then lhs
      else if lhs = "prop" then "prop'"
      else if is_logtype lhs then "logic"
      else lhs;
    val symbs' = map logify_types symbs;
    val xprod = XProd (lhs', symbs', const', pri);

    val _ = (List.app check_pri pris; check_pri pri; check_blocks symbs');
    val xprod' =
      if Lexicon.is_terminal lhs' then err_in_mfix ("Illegal lhs: " ^ lhs') mfix
      else if const <> "" then xprod
      else if length (filter is_argument symbs') <> 1 then
        err_in_mfix "Copy production must have exactly one argument" mfix
      else if exists is_terminal symbs' then xprod
      else XProd (lhs', map rem_pri symbs', "", chain_pri);

  in (xprod', syntax_consts, parse_rules) end;



(** datatype syn_ext **)

datatype syn_ext =
  Syn_Ext of {
    xprods: xprod list,
    consts: (string * string) list,
    parse_ast_translation: (string * ((Proof.context -> Ast.ast list -> Ast.ast) * stamp)) list,
    parse_rules: (Ast.ast * Ast.ast) list,
    parse_translation: (string * ((Proof.context -> term list -> term) * stamp)) list,
    print_translation: (string * ((Proof.context -> typ -> term list -> term) * stamp)) list,
    print_rules: (Ast.ast * Ast.ast) list,
    print_ast_translation: (string * ((Proof.context -> Ast.ast list -> Ast.ast) * stamp)) list};


(* syn_ext *)

fun syn_ext' logical_types mfixes consts trfuns (parse_rules, print_rules) =
  let
    val (parse_ast_translation, parse_translation, print_translation,
      print_ast_translation) = trfuns;

    val xprod_results = map (mfix_to_xprod logical_types) mfixes;
    val xprods = map #1 xprod_results;
    val consts' = map_filter #2 xprod_results;
    val parse_rules' = rev (map_filter #3 xprod_results);
    val mfix_consts = map (fn Mfix x => (#3 x, "")) mfixes @ map (fn XProd x => (#3 x, "")) xprods;
  in
    Syn_Ext {
      xprods = xprods,
      consts = mfix_consts @ consts' @ consts,
      parse_ast_translation = parse_ast_translation,
      parse_rules = parse_rules' @ parse_rules,
      parse_translation = parse_translation,
      print_translation = print_translation,
      print_rules = map swap parse_rules' @ print_rules,
      print_ast_translation = print_ast_translation}
  end;


val syn_ext = syn_ext' [];

fun syn_ext_rules rules = syn_ext [] [] ([], [], [], []) rules;
fun syn_ext_trfuns trfuns = syn_ext [] [] trfuns ([], []);

fun stamp_trfun s (c, f) = (c, (f, s));
fun mk_trfun tr = stamp_trfun (stamp ()) tr;
fun eq_trfun ((_, s1: stamp), (_, s2)) = s1 = s2;

end;
