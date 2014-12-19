(*  Title:      Pure/sign.ML
    Author:     Lawrence C Paulson and Markus Wenzel

Logical signature content: naming conventions, concrete syntax, type
signature, polymorphic constants.
*)

signature SIGN =
sig
  val change_begin: theory -> theory
  val change_end: theory -> theory
  val change_end_local: Proof.context -> Proof.context
  val change_check: theory -> theory
  val syn_of: theory -> Syntax.syntax
  val tsig_of: theory -> Type.tsig
  val classes_of: theory -> Sorts.algebra
  val all_classes: theory -> class list
  val super_classes: theory -> class -> class list
  val minimize_sort: theory -> sort -> sort
  val complete_sort: theory -> sort -> sort
  val set_defsort: sort -> theory -> theory
  val defaultS: theory -> sort
  val subsort: theory -> sort * sort -> bool
  val of_sort: theory -> typ * sort -> bool
  val inter_sort: theory -> sort * sort -> sort
  val witness_sorts: theory -> (typ * sort) list -> sort list -> (typ * sort) list
  val is_logtype: theory -> string -> bool
  val typ_instance: theory -> typ * typ -> bool
  val typ_equiv: theory -> typ * typ -> bool
  val typ_match: theory -> typ * typ -> Type.tyenv -> Type.tyenv
  val typ_unify: theory -> typ * typ -> Type.tyenv * int -> Type.tyenv * int
  val consts_of: theory -> Consts.T
  val the_const_constraint: theory -> string -> typ
  val const_type: theory -> string -> typ option
  val the_const_type: theory -> string -> typ
  val declared_tyname: theory -> string -> bool
  val declared_const: theory -> string -> bool
  val naming_of: theory -> Name_Space.naming
  val map_naming: (Name_Space.naming -> Name_Space.naming) -> theory -> theory
  val restore_naming: theory -> theory -> theory
  val inherit_naming: theory -> Proof.context -> Context.generic
  val full_name: theory -> binding -> string
  val full_name_path: theory -> string -> binding -> string
  val full_bname: theory -> bstring -> string
  val full_bname_path: theory -> string -> bstring -> string
  val const_monomorphic: theory -> string -> bool
  val const_typargs: theory -> string * typ -> typ list
  val const_instance: theory -> string * typ list -> typ
  val mk_const: theory -> string * typ list -> term
  val class_space: theory -> Name_Space.T
  val type_space: theory -> Name_Space.T
  val const_space: theory -> Name_Space.T
  val intern_class: theory -> xstring -> string
  val intern_type: theory -> xstring -> string
  val intern_const: theory -> xstring -> string
  val class_alias: binding -> class -> theory -> theory
  val type_alias: binding -> string -> theory -> theory
  val const_alias: binding -> string -> theory -> theory
  val arity_number: theory -> string -> int
  val arity_sorts: theory -> string -> sort -> sort list
  val certify_class: theory -> class -> class
  val certify_sort: theory -> sort -> sort
  val certify_typ: theory -> typ -> typ
  val certify_typ_mode: Type.mode -> theory -> typ -> typ
  val certify': bool -> Context.pretty -> bool -> Consts.T -> theory -> term -> term * typ * int
  val certify_term: theory -> term -> term * typ * int
  val cert_term: theory -> term -> term
  val cert_prop: theory -> term -> term
  val no_frees: Proof.context -> term -> term
  val no_vars: Proof.context -> term -> term
  val add_type: Proof.context -> binding * int * mixfix -> theory -> theory
  val add_types_global: (binding * int * mixfix) list -> theory -> theory
  val add_nonterminals: Proof.context -> binding list -> theory -> theory
  val add_nonterminals_global: binding list -> theory -> theory
  val add_type_abbrev: Proof.context -> binding * string list * typ -> theory -> theory
  val add_syntax: Syntax.mode -> (string * typ * mixfix) list -> theory -> theory
  val add_syntax_cmd: Syntax.mode -> (string * string * mixfix) list -> theory -> theory
  val del_syntax: Syntax.mode -> (string * typ * mixfix) list -> theory -> theory
  val del_syntax_cmd: Syntax.mode -> (string * string * mixfix) list -> theory -> theory
  val type_notation: bool -> Syntax.mode -> (typ * mixfix) list -> theory -> theory
  val notation: bool -> Syntax.mode -> (term * mixfix) list -> theory -> theory
  val declare_const: Proof.context -> (binding * typ) * mixfix -> theory -> term * theory
  val declare_const_global: (binding * typ) * mixfix -> theory -> term * theory
  val add_consts: (binding * typ * mixfix) list -> theory -> theory
  val add_consts_cmd: (binding * string * mixfix) list -> theory -> theory
  val add_abbrev: string -> binding * term -> theory -> (term * term) * theory
  val revert_abbrev: string -> string -> theory -> theory
  val add_const_constraint: string * typ option -> theory -> theory
  val primitive_class: binding * class list -> theory -> theory
  val primitive_classrel: class * class -> theory -> theory
  val primitive_arity: arity -> theory -> theory
  val parse_ast_translation:
    (string * (Proof.context -> Ast.ast list -> Ast.ast)) list -> theory -> theory
  val parse_translation:
    (string * (Proof.context -> term list -> term)) list -> theory -> theory
  val print_translation:
    (string * (Proof.context -> term list -> term)) list -> theory -> theory
  val typed_print_translation:
    (string * (Proof.context -> typ -> term list -> term)) list -> theory -> theory
  val print_ast_translation:
    (string * (Proof.context -> Ast.ast list -> Ast.ast)) list -> theory -> theory
  val add_trrules: Ast.ast Syntax.trrule list -> theory -> theory
  val del_trrules: Ast.ast Syntax.trrule list -> theory -> theory
  val new_group: theory -> theory
  val reset_group: theory -> theory
  val add_path: string -> theory -> theory
  val root_path: theory -> theory
  val parent_path: theory -> theory
  val mandatory_path: string -> theory -> theory
  val qualified_path: bool -> binding -> theory -> theory
  val local_path: theory -> theory
  val hide_class: bool -> string -> theory -> theory
  val hide_type: bool -> string -> theory -> theory
  val hide_const: bool -> string -> theory -> theory
end

structure Sign: SIGN =
struct

(** datatype sign **)

datatype sign = Sign of
 {syn: Syntax.syntax,           (*concrete syntax for terms, types, sorts*)
  tsig: Type.tsig,              (*order-sorted signature of types*)
  consts: Consts.T};            (*polymorphic constants*)

fun make_sign (syn, tsig, consts) = Sign {syn = syn, tsig = tsig, consts = consts};

structure Data = Theory_Data_PP
(
  type T = sign;
  fun extend (Sign {syn, tsig, consts, ...}) = make_sign (syn, tsig, consts);

  val empty = make_sign (Syntax.empty_syntax, Type.empty_tsig, Consts.empty);

  fun merge pp (sign1, sign2) =
    let
      val Sign {syn = syn1, tsig = tsig1, consts = consts1} = sign1;
      val Sign {syn = syn2, tsig = tsig2, consts = consts2} = sign2;

      val syn = Syntax.merge_syntax (syn1, syn2);
      val tsig = Type.merge_tsig pp (tsig1, tsig2);
      val consts = Consts.merge (consts1, consts2);
    in make_sign (syn, tsig, consts) end;
);

fun rep_sg thy = Data.get thy |> (fn Sign args => args);

fun map_sign f = Data.map (fn Sign {syn, tsig, consts} => make_sign (f (syn, tsig, consts)));

fun map_syn f = map_sign (fn (syn, tsig, consts) => (f syn, tsig, consts));
fun map_tsig f = map_sign (fn (syn, tsig, consts) => (syn, f tsig, consts));
fun map_consts f = map_sign (fn (syn, tsig, consts) => (syn, tsig, f consts));


(* linear change discipline *)

fun change_base begin = map_sign (fn (syn, tsig, consts) =>
  (syn, Type.change_base begin tsig, Consts.change_base begin consts));

val change_begin = change_base true;
val change_end = change_base false;

fun change_end_local ctxt =
  Context.raw_transfer (change_end (Proof_Context.theory_of ctxt)) ctxt;

fun change_check thy =
  if can change_end thy
  then raise Fail "Unfinished linear change of theory content" else thy;


(* syntax *)

val syn_of = #syn o rep_sg;


(* type signature *)

val tsig_of = #tsig o rep_sg;

val classes_of = #2 o #classes o Type.rep_tsig o tsig_of;
val all_classes = Sorts.all_classes o classes_of;
val super_classes = Sorts.super_classes o classes_of;
val minimize_sort = Sorts.minimize_sort o classes_of;
val complete_sort = Sorts.complete_sort o classes_of;

val set_defsort = map_tsig o Type.set_defsort;
val defaultS = Type.defaultS o tsig_of;
val subsort = Type.subsort o tsig_of;
val of_sort = Type.of_sort o tsig_of;
val inter_sort = Type.inter_sort o tsig_of;
val witness_sorts = Type.witness_sorts o tsig_of;
val is_logtype = member (op =) o Type.logical_types o tsig_of;

val typ_instance = Type.typ_instance o tsig_of;
fun typ_equiv thy (T, U) = typ_instance thy (T, U) andalso typ_instance thy (U, T);
val typ_match = Type.typ_match o tsig_of;
val typ_unify = Type.unify o tsig_of;


(* polymorphic constants *)

val consts_of = #consts o rep_sg;
val the_const_constraint = Consts.the_constraint o consts_of;
val the_const_type = #2 oo (Consts.the_const o consts_of);
val const_type = try o the_const_type;
val const_monomorphic = Consts.is_monomorphic o consts_of;
val const_typargs = Consts.typargs o consts_of;
val const_instance = Consts.instance o consts_of;

fun mk_const thy (c, Ts) = Const (c, const_instance thy (c, Ts));

fun declared_tyname ctxt c = can (Type.the_decl (tsig_of ctxt)) (c, Position.none);
val declared_const = can o the_const_constraint;


(* naming *)

val naming_of = Name_Space.naming_of o Context.Theory;
val map_naming = Context.theory_map o Name_Space.map_naming;
val restore_naming = map_naming o K o naming_of;
fun inherit_naming thy = Name_Space.map_naming (K (naming_of thy)) o Context.Proof;

val full_name = Name_Space.full_name o naming_of;
fun full_name_path thy path = Name_Space.full_name (Name_Space.add_path path (naming_of thy));

fun full_bname thy = Name_Space.full_name (naming_of thy) o Binding.name;
fun full_bname_path thy path = full_name_path thy path o Binding.name;



(** name spaces **)

val class_space = Type.class_space o tsig_of;
val type_space = Type.type_space o tsig_of;
val const_space = Consts.space_of o consts_of;

val intern_class = Name_Space.intern o class_space;
val intern_type = Name_Space.intern o type_space;
val intern_const = Name_Space.intern o const_space;

fun class_alias b c thy = map_tsig (Type.class_alias (naming_of thy) b c) thy;
fun type_alias b c thy = map_tsig (Type.type_alias (naming_of thy) b c) thy;
fun const_alias b c thy = map_consts (Consts.alias (naming_of thy) b c) thy;



(** certify entities **)    (*exception TYPE*)

(* certify wrt. type signature *)

val arity_number = Type.arity_number o tsig_of;
fun arity_sorts thy = Type.arity_sorts (Context.pretty_global thy) (tsig_of thy);

val certify_class = Type.cert_class o tsig_of;
val certify_sort = Type.cert_sort o tsig_of;
val certify_typ = Type.cert_typ o tsig_of;
fun certify_typ_mode mode = Type.cert_typ_mode mode o tsig_of;


(* certify term/prop *)

local

fun type_check pp tm =
  let
    fun err_appl bs t T u U =
      let
        val xs = map Free bs;           (*we do not rename here*)
        val t' = subst_bounds (xs, t);
        val u' = subst_bounds (xs, u);
        val msg = Type.appl_error (Syntax.init_pretty pp) t' T u' U;
      in raise TYPE (msg, [T, U], [t', u']) end;

    fun typ_of (_, Const (_, T)) = T
      | typ_of (_, Free  (_, T)) = T
      | typ_of (_, Var (_, T)) = T
      | typ_of (bs, Bound i) = snd (nth bs i handle General.Subscript =>
          raise TYPE ("Loose bound variable: B." ^ string_of_int i, [], [Bound i]))
      | typ_of (bs, Abs (x, T, body)) = T --> typ_of ((x, T) :: bs, body)
      | typ_of (bs, t $ u) =
          let val T = typ_of (bs, t) and U = typ_of (bs, u) in
            (case T of
              Type ("fun", [T1, T2]) =>
                if T1 = U then T2 else err_appl bs t T u U
            | _ => err_appl bs t T u U)
          end;
  in typ_of ([], tm) end;

fun err msg = raise TYPE (msg, [], []);

fun check_vars (t $ u) = (check_vars t; check_vars u)
  | check_vars (Abs (_, _, t)) = check_vars t
  | check_vars (Free (x, _)) =
      if Long_Name.is_qualified x then err ("Malformed variable: " ^ quote x) else ()
  | check_vars (Var (xi as (_, i), _)) =
      if i < 0 then err ("Malformed variable: " ^ quote (Term.string_of_vname xi)) else ()
  | check_vars _ = ();

in

fun certify' prop pp do_expand consts thy tm =
  let
    val _ = check_vars tm;
    val tm' = Term.map_types (certify_typ thy) tm;
    val T = type_check pp tm';
    val _ = if prop andalso T <> propT then err "Term not of type prop" else ();
    val tm'' = Consts.certify pp (tsig_of thy) do_expand consts tm';
  in (if tm = tm'' then tm else tm'', T, Term.maxidx_of_term tm'') end;

fun certify_term thy = certify' false (Context.pretty_global thy) true (consts_of thy) thy;
fun cert_term_abbrev thy =
  #1 o certify' false (Context.pretty_global thy) false (consts_of thy) thy;
val cert_term = #1 oo certify_term;
fun cert_prop thy = #1 o certify' true (Context.pretty_global thy) true (consts_of thy) thy;

end;


(* specifications *)

fun no_variables kind add addT mk mkT ctxt tm =
  (case (add tm [], addT tm []) of
    ([], []) => tm
  | (frees, tfrees) => error (Pretty.string_of (Pretty.block
      (Pretty.str ("Illegal " ^ kind ^ " variable(s) in term:") :: Pretty.brk 1 ::
       Pretty.commas
        (map (Syntax.pretty_term ctxt o mk) frees @ map (Syntax.pretty_typ ctxt o mkT) tfrees)))));

val no_frees = no_variables "free" Term.add_frees Term.add_tfrees Free TFree;
val no_vars = no_variables "schematic" Term.add_vars Term.add_tvars Var TVar;



(** signature extension functions **)  (*exception ERROR/TYPE*)

(* add type constructors *)

fun add_type ctxt (b, n, mx) thy = thy |> map_sign (fn (syn, tsig, consts) =>
  let
    val type_syntax = (Lexicon.mark_type (full_name thy b), Mixfix.make_type n, mx);
    val syn' = Syntax.update_type_gram true Syntax.mode_default [type_syntax] syn;
    val tsig' = Type.add_type (inherit_naming thy ctxt) (b, n) tsig;
  in (syn', tsig', consts) end);

fun add_types_global types thy =
  fold (add_type (Syntax.init_pretty_global thy)) types thy;


(* add nonterminals *)

fun add_nonterminals ctxt ns thy = thy |> map_sign (fn (syn, tsig, consts) =>
  (syn, fold (Type.add_nonterminal (inherit_naming thy ctxt)) ns tsig, consts));

fun add_nonterminals_global ns thy =
  add_nonterminals (Syntax.init_pretty_global thy) ns thy;


(* add type abbreviations *)

fun add_type_abbrev ctxt abbr thy = thy |> map_sign (fn (syn, tsig, consts) =>
  (syn, Type.add_abbrev (inherit_naming thy ctxt) abbr tsig, consts));


(* modify syntax *)

fun gen_syntax change_gram parse_typ mode args thy =
  let
    val ctxt = Type.set_mode Type.mode_syntax (Proof_Context.init_global thy);
    fun prep (c, T, mx) = (c, certify_typ_mode Type.mode_syntax thy (parse_typ ctxt T), mx)
      handle ERROR msg => cat_error msg ("in syntax declaration " ^ quote c);
  in thy |> map_syn (change_gram (is_logtype thy) mode (map prep args)) end;

fun gen_add_syntax x = gen_syntax (Syntax.update_const_gram true) x;

val add_syntax = gen_add_syntax (K I);
val add_syntax_cmd = gen_add_syntax Syntax.read_typ;
val del_syntax = gen_syntax (Syntax.update_const_gram false) (K I);
val del_syntax_cmd = gen_syntax (Syntax.update_const_gram false) Syntax.read_typ;

fun type_notation add mode args =
  let
    fun type_syntax (Type (c, args), mx) =
          SOME (Lexicon.mark_type c, Mixfix.make_type (length args), mx)
      | type_syntax _ = NONE;
  in map_syn (Syntax.update_type_gram add mode (map_filter type_syntax args)) end;

fun notation add mode args thy =
  let
    fun const_syntax (Const (c, _), mx) =
          (case try (Consts.type_scheme (consts_of thy)) c of
            SOME T => SOME (Lexicon.mark_const c, T, mx)
          | NONE => NONE)
      | const_syntax _ = NONE;
  in gen_syntax (Syntax.update_const_gram add) (K I) mode (map_filter const_syntax args) thy end;


(* add constants *)

local

fun gen_add_consts prep_typ ctxt raw_args thy =
  let
    val prepT = Type.no_tvars o Term.no_dummyT o certify_typ thy o prep_typ ctxt;
    fun prep (b, raw_T, mx) =
      let
        val c = full_name thy b;
        val T = (prepT raw_T handle TYPE (msg, _, _) => error msg) handle ERROR msg =>
          cat_error msg ("in declaration of constant " ^ Binding.print b);
        val T' = Logic.varifyT_global T;
      in ((b, T'), (Lexicon.mark_const c, T', mx), Const (c, T)) end;
    val args = map prep raw_args;
  in
    thy
    |> map_consts (fold (Consts.declare (inherit_naming thy ctxt) o #1) args)
    |> add_syntax Syntax.mode_default (map #2 args)
    |> pair (map #3 args)
  end;

in

fun add_consts args thy =
  #2 (gen_add_consts (K I) (Proof_Context.init_global thy) args thy);

fun add_consts_cmd args thy =
  #2 (gen_add_consts Syntax.read_typ (Proof_Context.init_global thy) args thy);

fun declare_const ctxt ((b, T), mx) = yield_singleton (gen_add_consts (K I) ctxt) (b, T, mx);
fun declare_const_global arg thy = declare_const (Proof_Context.init_global thy) arg thy;

end;


(* abbreviations *)

fun add_abbrev mode (b, raw_t) thy =  (* FIXME proper ctxt (?) *)
  let
    val ctxt = Syntax.init_pretty_global thy;
    val prep_tm = no_frees ctxt o Term.no_dummy_patterns o cert_term_abbrev thy;
    val t = (prep_tm raw_t handle TYPE (msg, _, _) => error msg | TERM (msg, _) => error msg)
      handle ERROR msg => cat_error msg ("in constant abbreviation " ^ Binding.print b);
    val (res, consts') = consts_of thy
      |> Consts.abbreviate (inherit_naming thy ctxt) (tsig_of thy) mode (b, t);
  in (res, thy |> map_consts (K consts')) end;

fun revert_abbrev mode c = map_consts (Consts.revert_abbrev mode c);


(* add constraints *)

fun add_const_constraint (c, opt_T) thy =
  let
    fun prepT raw_T =
      let val T = Logic.varifyT_global (Type.no_tvars (Term.no_dummyT (certify_typ thy raw_T)))
      in cert_term thy (Const (c, T)); T end
      handle TYPE (msg, _, _) => error msg;
  in thy |> map_consts (Consts.constrain (c, Option.map prepT opt_T)) end;


(* primitive classes and arities *)

fun primitive_class (bclass, classes) thy =
  thy
  |> map_sign (fn (syn, tsig, consts) =>
      let val tsig' = Type.add_class (Context.Theory thy) (bclass, classes) tsig;
      in (syn, tsig', consts) end)
  |> add_consts [(Binding.map_name Logic.const_of_class bclass, Term.a_itselfT --> propT, NoSyn)];

fun primitive_classrel arg thy =
  thy |> map_tsig (Type.add_classrel (Context.pretty_global thy) arg);

fun primitive_arity arg thy =
  thy |> map_tsig (Type.add_arity (Context.pretty_global thy) arg);


(* add translation functions *)

local

fun mk trs = map Syntax_Ext.mk_trfun trs;

in

fun parse_ast_translation atrs = map_syn (Syntax.update_trfuns (mk atrs, [], [], []));
fun parse_translation trs = map_syn (Syntax.update_trfuns ([], mk trs, [], []));
fun print_translation tr's =
  map_syn (Syntax.update_trfuns ([], [], mk (map (apsnd Syntax_Trans.non_typed_tr') tr's), []));
fun typed_print_translation tr's = map_syn (Syntax.update_trfuns ([], [], mk tr's, []));
fun print_ast_translation atr's = map_syn (Syntax.update_trfuns ([], [], [], mk atr's));

end;


(* translation rules *)

val add_trrules = map_syn o Syntax.update_trrules;
val del_trrules = map_syn o Syntax.remove_trrules;


(* naming *)

val new_group = map_naming Name_Space.new_group;
val reset_group = map_naming Name_Space.reset_group;

val add_path = map_naming o Name_Space.add_path;
val root_path = map_naming Name_Space.root_path;
val parent_path = map_naming Name_Space.parent_path;
val mandatory_path = map_naming o Name_Space.mandatory_path;
val qualified_path = map_naming oo Name_Space.qualified_path;

fun local_path thy = thy |> root_path |> add_path (Context.theory_name thy);


(* hide names *)

val hide_class = map_tsig oo Type.hide_class;
val hide_type = map_tsig oo Type.hide_type;
val hide_const = map_consts oo Consts.hide;

end;
