(*  Title:      Pure/Syntax/ast.ML
    Author:     Markus Wenzel, TU Muenchen

Abstract syntax trees, translation rules, matching and normalization of asts.
*)

signature AST =
sig
  datatype ast =
    Constant of string |
    Variable of string |
    Appl of ast list
  val mk_appl: ast -> ast list -> ast
  exception AST of string * ast list
  val pretty_ast: ast -> Pretty.T
  val pretty_rule: ast * ast -> Pretty.T
  val strip_positions: ast -> ast
  val head_of_rule: ast * ast -> string
  val rule_error: ast * ast -> string option
  val fold_ast: string -> ast list -> ast
  val fold_ast_p: string -> ast list * ast -> ast
  val unfold_ast: string -> ast -> ast list
  val unfold_ast_p: string -> ast -> ast list * ast
  val trace_raw: Config.raw
  val trace: bool Config.T
  val stats_raw: Config.raw
  val stats: bool Config.T
  val normalize: Proof.context -> (string -> (ast * ast) list) -> ast -> ast
end;

structure Ast: AST =
struct

(** abstract syntax trees **)

(*asts come in two flavours:
   - ordinary asts representing terms and typs: Variables are (often) treated
     like Constants;
   - patterns used as lhs and rhs in rules: Variables are placeholders for
     proper asts*)

datatype ast =
  Constant of string |    (*"not", "_abs", "fun"*)
  Variable of string |    (*x, ?x, 'a, ?'a*)
  Appl of ast list;       (*(f x y z), ("fun" 'a 'b), ("_abs" x t)*)

(*the list of subasts of an Appl node has to contain at least 2 elements, i.e.
  there are no empty asts or nullary applications; use mk_appl for convenience*)
fun mk_appl f [] = f
  | mk_appl f args = Appl (f :: args);

(*exception for system errors involving asts*)
exception AST of string * ast list;



(** print asts in a LISP-like style **)

fun pretty_ast (Constant a) = Pretty.quote (Pretty.str a)
  | pretty_ast (Variable x) =
      (case Term_Position.decode x of
        SOME pos => Term_Position.pretty pos
      | NONE => Pretty.str x)
  | pretty_ast (Appl asts) = Pretty.enclose "(" ")" (Pretty.breaks (map pretty_ast asts));

fun pretty_rule (lhs, rhs) =
  Pretty.block [pretty_ast lhs, Pretty.str "  ->", Pretty.brk 2, pretty_ast rhs];


(* strip_positions *)

fun strip_positions (Appl ((t as Constant c) :: u :: (v as Variable x) :: asts)) =
      if member (op =) Term_Position.markers c andalso is_some (Term_Position.decode x)
      then mk_appl (strip_positions u) (map strip_positions asts)
      else Appl (map strip_positions (t :: u :: v :: asts))
  | strip_positions (Appl asts) = Appl (map strip_positions asts)
  | strip_positions ast = ast;


(* head_of_ast and head_of_rule *)

fun head_of_ast (Constant a) = a
  | head_of_ast (Appl (Constant a :: _)) = a
  | head_of_ast _ = "";

fun head_of_rule (lhs, _) = head_of_ast lhs;



(** check translation rules **)

fun rule_error (lhs, rhs) =
  let
    fun add_vars (Constant _) = I
      | add_vars (Variable x) = cons x
      | add_vars (Appl asts) = fold add_vars asts;

    val lvars = add_vars lhs [];
    val rvars = add_vars rhs [];
  in
    if has_duplicates (op =) lvars then SOME "duplicate vars in lhs"
    else if not (subset (op =) (rvars, lvars)) then SOME "rhs contains extra variables"
    else NONE
  end;



(** ast translation utilities **)

(* fold asts *)

fun fold_ast _ [] = raise Match
  | fold_ast _ [y] = y
  | fold_ast c (x :: xs) = Appl [Constant c, x, fold_ast c xs];

fun fold_ast_p c = uncurry (fold_rev (fn x => fn xs => Appl [Constant c, x, xs]));


(* unfold asts *)

fun unfold_ast c (y as Appl [Constant c', x, xs]) =
      if c = c' then x :: unfold_ast c xs else [y]
  | unfold_ast _ y = [y];

fun unfold_ast_p c (y as Appl [Constant c', x, xs]) =
      if c = c' then apfst (cons x) (unfold_ast_p c xs)
      else ([], y)
  | unfold_ast_p _ y = ([], y);



(** normalization of asts **)

(* match *)

fun match ast pat =
  let
    exception NO_MATCH;

    fun mtch (Constant a) (Constant b) env =
          if a = b then env else raise NO_MATCH
      | mtch (Variable a) (Constant b) env =
          if a = b then env else raise NO_MATCH
      | mtch ast (Variable x) env = Symtab.update (x, ast) env
      | mtch (Appl asts) (Appl pats) env = mtch_lst asts pats env
      | mtch _ _ _ = raise NO_MATCH
    and mtch_lst (ast :: asts) (pat :: pats) env =
          mtch_lst asts pats (mtch ast pat env)
      | mtch_lst [] [] env = env
      | mtch_lst _ _ _ = raise NO_MATCH;

    val (head, args) =
      (case (ast, pat) of
        (Appl asts, Appl pats) =>
          let val a = length asts and p = length pats in
            if a > p then (Appl (take p asts), drop p asts)
            else (ast, [])
          end
      | _ => (ast, []));
  in
    SOME (mtch head pat Symtab.empty, args) handle NO_MATCH => NONE
  end;


(* normalize *)

val trace_raw = Config.declare ("syntax_ast_trace", @{here}) (fn _ => Config.Bool false);
val trace = Config.bool trace_raw;

val stats_raw = Config.declare ("syntax_ast_stats", @{here}) (fn _ => Config.Bool false);
val stats = Config.bool stats_raw;

fun message head body =
  Pretty.string_of (Pretty.block [Pretty.str head, Pretty.brk 1, body]);

(*the normalizer works yoyo-like: top-down, bottom-up, top-down, ...*)
fun normalize ctxt get_rules pre_ast =
  let
    val trace = Config.get ctxt trace;
    val stats = Config.get ctxt stats;

    val passes = Unsynchronized.ref 0;
    val failed_matches = Unsynchronized.ref 0;
    val changes = Unsynchronized.ref 0;

    fun subst _ (ast as Constant _) = ast
      | subst env (Variable x) = the (Symtab.lookup env x)
      | subst env (Appl asts) = Appl (map (subst env) asts);

    fun try_rules ((lhs, rhs) :: pats) ast =
          (case match ast lhs of
            SOME (env, args) =>
              (Unsynchronized.inc changes; SOME (mk_appl (subst env rhs) args))
          | NONE => (Unsynchronized.inc failed_matches; try_rules pats ast))
      | try_rules [] _ = NONE;
    val try_headless_rules = try_rules (get_rules "");

    fun try ast a =
      (case try_rules (get_rules a) ast of
        NONE => try_headless_rules ast
      | some => some);

    fun rewrite (ast as Constant a) = try ast a
      | rewrite (ast as Variable a) = try ast a
      | rewrite (ast as Appl (Constant a :: _)) = try ast a
      | rewrite (ast as Appl (Variable a :: _)) = try ast a
      | rewrite ast = try_headless_rules ast;

    fun rewrote old_ast new_ast =
      if trace then tracing (message "rewrote:" (pretty_rule (old_ast, new_ast)))
      else ();

    fun norm_root ast =
      (case rewrite ast of
        SOME new_ast => (rewrote ast new_ast; norm_root new_ast)
      | NONE => ast);

    fun norm ast =
      (case norm_root ast of
        Appl sub_asts =>
          let
            val old_changes = ! changes;
            val new_ast = Appl (map norm sub_asts);
          in
            if old_changes = ! changes then new_ast else norm_root new_ast
          end
      | atomic_ast => atomic_ast);

    fun normal ast =
      let
        val old_changes = ! changes;
        val new_ast = norm ast;
      in
        Unsynchronized.inc passes;
        if old_changes = ! changes then new_ast else normal new_ast
      end;


    val _ = if trace then tracing (message "pre:" (pretty_ast pre_ast)) else ();
    val post_ast = normal pre_ast;
    val _ =
      if trace orelse stats then
        tracing (message "post:" (pretty_ast post_ast) ^ "\nnormalize: " ^
          string_of_int (! passes) ^ " passes, " ^
          string_of_int (! changes) ^ " changes, " ^
          string_of_int (! failed_matches) ^ " matches failed")
      else ();
  in post_ast end;

end;
