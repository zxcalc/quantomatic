(*  Title:      Pure/Syntax/syntax_trans.ML
    Author:     Tobias Nipkow and Markus Wenzel, TU Muenchen

Syntax translation functions.
*)

signature BASIC_SYNTAX_TRANS =
sig
  val eta_contract: bool Config.T
end

signature SYNTAX_TRANS =
sig
  include BASIC_SYNTAX_TRANS
  val bracketsN: string
  val no_bracketsN: string
  val no_brackets: unit -> bool
  val type_bracketsN: string
  val no_type_bracketsN: string
  val no_type_brackets: unit -> bool
  val abs_tr: term list -> term
  val mk_binder_tr: string * string -> string * (Proof.context -> term list -> term)
  val antiquote_tr: string -> term -> term
  val quote_tr: string -> term -> term
  val quote_antiquote_tr: string -> string -> string ->
    string * (Proof.context -> term list -> term)
  val non_typed_tr': (Proof.context -> term list -> term) ->
    Proof.context -> typ -> term list -> term
  val tappl_ast_tr': Ast.ast * Ast.ast list -> Ast.ast
  val appl_ast_tr': Ast.ast * Ast.ast list -> Ast.ast
  val applC_ast_tr': Ast.ast * Ast.ast list -> Ast.ast
  val eta_contract_raw: Config.raw
  val mark_bound_abs: string * typ -> term
  val mark_bound_body: string * typ -> term
  val bound_vars: (string * typ) list -> term -> term
  val abs_tr': Proof.context -> term -> term
  val atomic_abs_tr': string * typ * term -> term * term
  val const_abs_tr': term -> term
  val mk_binder_tr': string * string -> string * (Proof.context -> term list -> term)
  val preserve_binder_abs_tr': string -> string -> string * (Proof.context -> term list -> term)
  val preserve_binder_abs2_tr': string -> string -> string * (Proof.context -> term list -> term)
  val variant_abs: string * typ * term -> string * term
  val variant_abs': string * typ * term -> string * term
  val dependent_tr': string * string -> term list -> term
  val antiquote_tr': string -> term -> term
  val quote_tr': string -> term -> term
  val quote_antiquote_tr': string -> string -> string ->
    string * (Proof.context -> term list -> term)
  val update_name_tr': term -> term
  val pure_parse_ast_translation: (string * (Proof.context -> Ast.ast list -> Ast.ast)) list
  val pure_parse_translation: (string * (Proof.context -> term list -> term)) list
  val pure_print_ast_translation: (string * (Proof.context -> Ast.ast list -> Ast.ast)) list
  val struct_tr: string list -> string * (Proof.context -> term list -> term)
  val struct_ast_tr': string list -> string * (Proof.context -> Ast.ast list -> Ast.ast)
end;

structure Syntax_Trans: SYNTAX_TRANS =
struct

structure Syntax = Lexicon.Syntax;


(* print mode *)

val bracketsN = "brackets";
val no_bracketsN = "no_brackets";

fun no_brackets () =
  find_first (fn mode => mode = bracketsN orelse mode = no_bracketsN)
    (print_mode_value ()) = SOME no_bracketsN;

val type_bracketsN = "type_brackets";
val no_type_bracketsN = "no_type_brackets";

fun no_type_brackets () =
  find_first (fn mode => mode = type_bracketsN orelse mode = no_type_bracketsN)
    (print_mode_value ()) <> SOME type_bracketsN;



(** parse (ast) translations **)

(* strip_positions *)

fun strip_positions_ast_tr [ast] = Ast.strip_positions ast
  | strip_positions_ast_tr asts = raise Ast.AST ("strip_positions_ast_tr", asts);


(* constify *)

fun constify_ast_tr [Ast.Appl [c as Ast.Constant "_constrain", ast1, ast2]] =
      Ast.Appl [c, constify_ast_tr [ast1], ast2]
  | constify_ast_tr [Ast.Variable c] = Ast.Constant c
  | constify_ast_tr asts = raise Ast.AST ("constify_ast_tr", asts);


(* type syntax *)

fun tapp_ast_tr [ty, c] = Ast.Appl [c, ty]
  | tapp_ast_tr asts = raise Ast.AST ("tapp_ast_tr", asts);

fun tappl_ast_tr [ty, tys, c] = Ast.mk_appl c (ty :: Ast.unfold_ast "_types" tys)
  | tappl_ast_tr asts = raise Ast.AST ("tappl_ast_tr", asts);

fun bracket_ast_tr [dom, cod] = Ast.fold_ast_p "\\<^type>fun" (Ast.unfold_ast "_types" dom, cod)
  | bracket_ast_tr asts = raise Ast.AST ("bracket_ast_tr", asts);


(* application *)

fun appl_ast_tr [f, args] = Ast.Appl (f :: Ast.unfold_ast "_args" args)
  | appl_ast_tr asts = raise Ast.AST ("appl_ast_tr", asts);

fun applC_ast_tr [f, args] = Ast.Appl (f :: Ast.unfold_ast "_cargs" args)
  | applC_ast_tr asts = raise Ast.AST ("applC_ast_tr", asts);


(* abstraction *)

fun idtyp_ast_tr [x, ty] = Ast.Appl [Ast.Constant "_constrain", x, ty]
  | idtyp_ast_tr asts = raise Ast.AST ("idtyp_ast_tr", asts);

fun lambda_ast_tr [pats, body] = Ast.fold_ast_p "_abs" (Ast.unfold_ast "_pttrns" pats, body)
  | lambda_ast_tr asts = raise Ast.AST ("lambda_ast_tr", asts);

fun absfree_proper (x, T) t =
  if Name.is_internal x
  then error ("Illegal internal variable in abstraction: " ^ quote x)
  else absfree (x, T) t;

fun abs_tr [Free x, t] = absfree_proper x t
  | abs_tr [Const ("_idtdummy", T), t] = absdummy T t
  | abs_tr [Const ("_constrain", _) $ x $ tT, t] =
      Syntax.const "_constrainAbs" $ abs_tr [x, t] $ tT
  | abs_tr ts = raise TERM ("abs_tr", ts);


(* binder *)

fun mk_binder_tr (syn, name) =
  let
    fun err ts = raise TERM ("binder_tr: " ^ syn, ts)
    fun binder_tr [Const ("_idts", _) $ idt $ idts, t] = binder_tr [idt, binder_tr [idts, t]]
      | binder_tr [x, t] =
          let val abs = abs_tr [x, t] handle TERM _ => err [x, t]
          in Syntax.const name $ abs end
      | binder_tr ts = err ts;
  in (syn, fn _ => binder_tr) end;


(* type propositions *)

fun mk_type ty =
  Syntax.const "_constrain" $
    Syntax.const "\\<^const>Pure.type" $ (Syntax.const "\\<^type>itself" $ ty);

fun ofclass_tr [ty, cls] = cls $ mk_type ty
  | ofclass_tr ts = raise TERM ("ofclass_tr", ts);

fun sort_constraint_tr [ty] = Syntax.const "\\<^const>Pure.sort_constraint" $ mk_type ty
  | sort_constraint_tr ts = raise TERM ("sort_constraint_tr", ts);


(* meta propositions *)

fun aprop_tr [t] = Syntax.const "_constrain" $ t $ Syntax.const "\\<^type>prop"
  | aprop_tr ts = raise TERM ("aprop_tr", ts);


(* meta implication *)

fun bigimpl_ast_tr (asts as [asms, concl]) =
      let val prems =
        (case Ast.unfold_ast_p "_asms" asms of
          (asms', Ast.Appl [Ast.Constant "_asm", asm']) => asms' @ [asm']
        | _ => raise Ast.AST ("bigimpl_ast_tr", asts))
      in Ast.fold_ast_p "\\<^const>Pure.imp" (prems, concl) end
  | bigimpl_ast_tr asts = raise Ast.AST ("bigimpl_ast_tr", asts);


(* type/term reflection *)

fun type_tr [ty] = mk_type ty
  | type_tr ts = raise TERM ("type_tr", ts);


(* dddot *)

fun dddot_tr ts = Term.list_comb (Syntax.var Syntax_Ext.dddot_indexname, ts);


(* quote / antiquote *)

fun antiquote_tr name =
  let
    fun tr i ((t as Const (c, _)) $ u) =
          if c = name then tr i u $ Bound i
          else tr i t $ tr i u
      | tr i (t $ u) = tr i t $ tr i u
      | tr i (Abs (x, T, t)) = Abs (x, T, tr (i + 1) t)
      | tr _ a = a;
  in tr 0 end;

fun quote_tr name t = Abs ("s", dummyT, antiquote_tr name (Term.incr_boundvars 1 t));

fun quote_antiquote_tr quoteN antiquoteN name =
  let
    fun tr [t] = Syntax.const name $ quote_tr antiquoteN t
      | tr ts = raise TERM ("quote_tr", ts);
  in (quoteN, fn _ => tr) end;


(* corresponding updates *)

fun update_name_tr (Free (x, T) :: ts) = list_comb (Free (suffix "_update" x, T), ts)
  | update_name_tr (Const (x, T) :: ts) = list_comb (Const (suffix "_update" x, T), ts)
  | update_name_tr (((c as Const ("_constrain", _)) $ t $ ty) :: ts) =
      if Term_Position.is_position ty then list_comb (c $ update_name_tr [t] $ ty, ts)
      else
        list_comb (c $ update_name_tr [t] $
          (Lexicon.fun_type $
            (Lexicon.fun_type $ Lexicon.dummy_type $ ty) $ Lexicon.dummy_type), ts)
  | update_name_tr ts = raise TERM ("update_name_tr", ts);


(* indexed syntax *)

fun indexdefault_ast_tr [] =
      Ast.Appl [Ast.Constant "_index",
        Ast.Appl [Ast.Constant "_struct", Ast.Constant "_indexdefault"]]
  | indexdefault_ast_tr asts = raise Ast.AST ("indexdefault_ast_tr", asts);

fun indexvar_ast_tr [] = Ast.Appl [Ast.Constant "_index", Ast.Variable "some_index"]
  | indexvar_ast_tr asts = raise Ast.AST ("indexvar_ast_tr", asts);

fun struct_ast_tr [Ast.Appl [Ast.Constant "_index", ast]] = ast
  | struct_ast_tr asts = Ast.mk_appl (Ast.Constant "_struct") asts;

fun index_tr [t] = t
  | index_tr ts = raise TERM ("index_tr", ts);

fun struct_tr structs =
  ("_struct", fn _ =>
    (fn [Const ("_indexdefault", _)] =>
        (case structs of
          x :: _ => Syntax.const (Lexicon.mark_fixed x)
        | _ => error "Illegal reference to implicit structure")
      | ts => raise TERM ("struct_tr", ts)));



(** print (ast) translations **)

(* types *)

fun non_typed_tr' f ctxt _ ts = f ctxt ts;


(* type syntax *)

fun tappl_ast_tr' (f, []) = raise Ast.AST ("tappl_ast_tr'", [f])
  | tappl_ast_tr' (f, [ty]) = Ast.Appl [Ast.Constant "_tapp", ty, f]
  | tappl_ast_tr' (f, ty :: tys) =
      Ast.Appl [Ast.Constant "_tappl", ty, Ast.fold_ast "_types" tys, f];

fun fun_ast_tr' asts =
  if no_brackets () orelse no_type_brackets () then raise Match
  else
    (case Ast.unfold_ast_p "\\<^type>fun" (Ast.Appl (Ast.Constant "\\<^type>fun" :: asts)) of
      (dom as _ :: _ :: _, cod)
        => Ast.Appl [Ast.Constant "_bracket", Ast.fold_ast "_types" dom, cod]
    | _ => raise Match);


(* application *)

fun appl_ast_tr' (f, []) = raise Ast.AST ("appl_ast_tr'", [f])
  | appl_ast_tr' (f, args) = Ast.Appl [Ast.Constant "_appl", f, Ast.fold_ast "_args" args];

fun applC_ast_tr' (f, []) = raise Ast.AST ("applC_ast_tr'", [f])
  | applC_ast_tr' (f, args) = Ast.Appl [Ast.Constant "_applC", f, Ast.fold_ast "_cargs" args];


(* partial eta-contraction before printing *)

fun eta_abs (Abs (a, T, t)) =
      (case eta_abs t of
        t' as Const ("_aprop", _) $ _ => Abs (a, T, t')
      | t' as f $ u =>
          (case eta_abs u of
            Bound 0 =>
              if Term.is_dependent f then Abs (a, T, t')
              else incr_boundvars ~1 f
          | _ => Abs (a, T, t'))
      | t' => Abs (a, T, t'))
  | eta_abs t = t;

val eta_contract_raw = Config.declare_option ("eta_contract", @{here});
val eta_contract = Config.bool eta_contract_raw;

fun eta_contr ctxt tm =
  if Config.get ctxt eta_contract then eta_abs tm else tm;


(* abstraction *)

fun mark_bound_abs (x, T) = Const ("_bound", T --> T) $ Free (x, T);
fun mark_bound_body (x, T) = Const ("_bound", dummyT) $ Free (x, T);

fun bound_vars vars body =
  subst_bounds (map mark_bound_abs (Term.rename_wrt_term body vars), body);

fun strip_abss vars_of body_of tm =
  let
    val vars = vars_of tm;
    val body = body_of tm;
    val rev_new_vars = Term.rename_wrt_term body vars;
    fun subst (x, T) b =
      if Name.is_internal x andalso not (Term.is_dependent b)
      then (Const ("_idtdummy", T), incr_boundvars ~1 b)
      else (mark_bound_abs (x, T), Term.subst_bound (mark_bound_body (x, T), b));
    val (rev_vars', body') = fold_map subst rev_new_vars body;
  in (rev rev_vars', body') end;


fun abs_tr' ctxt tm =
  uncurry (fold_rev (fn x => fn t => Syntax.const "_abs" $ x $ t))
    (strip_abss strip_abs_vars strip_abs_body (eta_contr ctxt tm));

fun atomic_abs_tr' (x, T, t) =
  let val [xT] = Term.rename_wrt_term t [(x, T)]
  in (mark_bound_abs xT, subst_bound (mark_bound_body xT, t)) end;

fun abs_ast_tr' asts =
  (case Ast.unfold_ast_p "_abs" (Ast.Appl (Ast.Constant "_abs" :: asts)) of
    ([], _) => raise Ast.AST ("abs_ast_tr'", asts)
  | (xs, body) => Ast.Appl [Ast.Constant "_lambda", Ast.fold_ast "_pttrns" xs, body]);

fun const_abs_tr' t =
  (case eta_abs t of
    Abs (_, _, t') =>
      if Term.is_dependent t' then raise Match
      else incr_boundvars ~1 t'
  | _ => raise Match);


(* binders *)

fun mk_binder_tr' (name, syn) =
  let
    fun mk_idts [] = raise Match    (*abort translation*)
      | mk_idts [idt] = idt
      | mk_idts (idt :: idts) = Syntax.const "_idts" $ idt $ mk_idts idts;

    fun tr' t =
      let
        val (xs, bd) = strip_abss (strip_qnt_vars name) (strip_qnt_body name) t;
      in Syntax.const syn $ mk_idts xs $ bd end;

    fun binder_tr' (t :: ts) = Term.list_comb (tr' (Syntax.const name $ t), ts)
      | binder_tr' [] = raise Match;
  in (name, fn _ => binder_tr') end;

fun preserve_binder_abs_tr' name syn = (name, fn _ => fn Abs abs :: ts =>
  let val (x, t) = atomic_abs_tr' abs
  in list_comb (Syntax.const syn $ x $ t, ts) end);

fun preserve_binder_abs2_tr' name syn = (name, fn _ => fn A :: Abs abs :: ts =>
  let val (x, t) = atomic_abs_tr' abs
  in list_comb (Syntax.const syn $ x $ A $ t, ts) end);


(* idtyp constraints *)

fun idtyp_ast_tr' a [Ast.Appl [Ast.Constant "_constrain", x, ty], xs] =
      Ast.Appl [Ast.Constant a, Ast.Appl [Ast.Constant "_idtyp", x, ty], xs]
  | idtyp_ast_tr' _ _ = raise Match;


(* meta implication *)

fun impl_ast_tr' asts =
  if no_brackets () then raise Match
  else
    (case Ast.unfold_ast_p "\\<^const>Pure.imp"
        (Ast.Appl (Ast.Constant "\\<^const>Pure.imp" :: asts)) of
      (prems as _ :: _ :: _, concl) =>
        let
          val (asms, asm) = split_last prems;
          val asms' = Ast.fold_ast_p "_asms" (asms, Ast.Appl [Ast.Constant "_asm", asm]);
        in Ast.Appl [Ast.Constant "_bigimpl", asms', concl] end
    | _ => raise Match);


(* dependent / nondependent quantifiers *)

fun var_abs mark (x, T, b) =
  let val (x', _) = Name.variant x (Term.declare_term_names b Name.context)
  in (x', subst_bound (mark (x', T), b)) end;

val variant_abs = var_abs Free;
val variant_abs' = var_abs mark_bound_abs;

fun dependent_tr' (q, r) (A :: Abs (x, T, B) :: ts) =
      if Term.is_dependent B then
        let val (x', B') = variant_abs' (x, dummyT, B);
        in Term.list_comb (Syntax.const q $ mark_bound_abs (x', T) $ A $ B', ts) end
      else Term.list_comb (Syntax.const r $ A $ incr_boundvars ~1 B, ts)
  | dependent_tr' _ _ = raise Match;


(* quote / antiquote *)

fun antiquote_tr' name =
  let
    fun tr' i (t $ u) =
          if u aconv Bound i then Syntax.const name $ tr' i t
          else tr' i t $ tr' i u
      | tr' i (Abs (x, T, t)) = Abs (x, T, tr' (i + 1) t)
      | tr' i a = if a aconv Bound i then raise Match else a;
  in tr' 0 end;

fun quote_tr' name (Abs (_, _, t)) = Term.incr_boundvars ~1 (antiquote_tr' name t)
  | quote_tr' _ _ = raise Match;

fun quote_antiquote_tr' quoteN antiquoteN name =
  let
    fun tr' (t :: ts) = Term.list_comb (Syntax.const quoteN $ quote_tr' antiquoteN t, ts)
      | tr' _ = raise Match;
  in (name, fn _ => tr') end;


(* corresponding updates *)

local

fun upd_type (Type ("fun", [Type ("fun", [_, T]), _])) = T
  | upd_type _ = dummyT;

fun upd_tr' (x_upd, T) =
  (case try (unsuffix "_update") x_upd of
    SOME x => (x, upd_type T)
  | NONE => raise Match);

in

fun update_name_tr' (Free x) = Free (upd_tr' x)
  | update_name_tr' ((c as Const ("_free", _)) $ Free x) = c $ Free (upd_tr' x)
  | update_name_tr' (Const x) = Const (upd_tr' x)
  | update_name_tr' _ = raise Match;

end;


(* indexed syntax *)

fun index_ast_tr' [Ast.Appl [Ast.Constant "_struct", ast]] = ast
  | index_ast_tr' _ = raise Match;

fun struct_ast_tr' structs =
  ("_struct", fn _ =>
    (fn [Ast.Constant "_indexdefault"] =>
        (case structs of
          x :: _ => Ast.Appl [Ast.Constant "_free", Ast.Variable x]
        | _ => raise Match)
      | _ => raise Match));



(** Pure translations **)

val pure_parse_ast_translation =
 [("_strip_positions", fn _ => strip_positions_ast_tr),
  ("_constify", fn _ => constify_ast_tr),
  ("_tapp", fn _ => tapp_ast_tr),
  ("_tappl", fn _ => tappl_ast_tr),
  ("_bracket", fn _ => bracket_ast_tr),
  ("_appl", fn _ => appl_ast_tr),
  ("_applC", fn _ => applC_ast_tr),
  ("_lambda", fn _ => lambda_ast_tr),
  ("_idtyp", fn _ => idtyp_ast_tr),
  ("_bigimpl", fn _ => bigimpl_ast_tr),
  ("_indexdefault", fn _ => indexdefault_ast_tr),
  ("_indexvar", fn _ => indexvar_ast_tr),
  ("_struct", fn _ => struct_ast_tr)];

val pure_parse_translation =
 [("_abs", fn _ => abs_tr),
  ("_aprop", fn _ => aprop_tr),
  ("_ofclass", fn _ => ofclass_tr),
  ("_sort_constraint", fn _ => sort_constraint_tr),
  ("_TYPE", fn _ => type_tr),
  ("_DDDOT", fn _ => dddot_tr),
  ("_update_name", fn _ => update_name_tr),
  ("_index", fn _ => index_tr)];

val pure_print_ast_translation =
 [("\\<^type>fun", fn _ => fun_ast_tr'),
  ("_abs", fn _ => abs_ast_tr'),
  ("_idts", fn _ => idtyp_ast_tr' "_idts"),
  ("_pttrns", fn _ => idtyp_ast_tr' "_pttrns"),
  ("\\<^const>Pure.imp", fn _ => impl_ast_tr'),
  ("_index", fn _ => index_ast_tr')];

end;

structure Basic_Syntax_Trans: BASIC_SYNTAX_TRANS = Syntax_Trans;
open Basic_Syntax_Trans;
