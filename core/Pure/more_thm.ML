(*  Title:      Pure/more_thm.ML
    Author:     Makarius

Further operations on type ctyp/cterm/thm, outside the inference kernel.
*)

infix aconvc;

signature BASIC_THM =
sig
  include BASIC_THM
  val show_consts: bool Config.T
  val show_hyps: bool Config.T
  val show_tags: bool Config.T
  structure Ctermtab: TABLE
  structure Thmtab: TABLE
  val aconvc: cterm * cterm -> bool
  type attribute = Context.generic * thm -> Context.generic option * thm option
end;

signature THM =
sig
  include THM
  structure Ctermtab: TABLE
  structure Thmtab: TABLE
  val eq_ctyp: ctyp * ctyp -> bool
  val aconvc: cterm * cterm -> bool
  val add_tvars: thm -> ctyp list -> ctyp list
  val add_frees: thm -> cterm list -> cterm list
  val add_vars: thm -> cterm list -> cterm list
  val all_name: Proof.context -> string * cterm -> cterm -> cterm
  val all: Proof.context -> cterm -> cterm -> cterm
  val mk_binop: cterm -> cterm -> cterm -> cterm
  val dest_binop: cterm -> cterm * cterm
  val dest_implies: cterm -> cterm * cterm
  val dest_equals: cterm -> cterm * cterm
  val dest_equals_lhs: cterm -> cterm
  val dest_equals_rhs: cterm -> cterm
  val lhs_of: thm -> cterm
  val rhs_of: thm -> cterm
  val thm_ord: thm * thm -> order
  val cterm_cache: (cterm -> 'a) -> cterm -> 'a
  val thm_cache: (thm -> 'a) -> thm -> 'a
  val is_reflexive: thm -> bool
  val eq_thm: thm * thm -> bool
  val eq_thm_prop: thm * thm -> bool
  val eq_thm_strict: thm * thm -> bool
  val equiv_thm: theory -> thm * thm -> bool
  val class_triv: theory -> class -> thm
  val of_sort: ctyp * sort -> thm list
  val is_dummy: thm -> bool
  val plain_prop_of: thm -> term
  val add_thm: thm -> thm list -> thm list
  val del_thm: thm -> thm list -> thm list
  val merge_thms: thm list * thm list -> thm list
  val full_rules: thm Item_Net.T
  val intro_rules: thm Item_Net.T
  val elim_rules: thm Item_Net.T
  val declare_hyps: cterm -> Proof.context -> Proof.context
  val assume_hyps: cterm -> Proof.context -> thm * Proof.context
  val unchecked_hyps: Proof.context -> Proof.context
  val restore_hyps: Proof.context -> Proof.context -> Proof.context
  val undeclared_hyps: Context.generic -> thm -> term list
  val check_hyps: Context.generic -> thm -> thm
  val declare_term_sorts: term -> Proof.context -> Proof.context
  val extra_shyps': Proof.context -> thm -> sort list
  val check_shyps: Proof.context -> thm -> thm
  val weaken_sorts': Proof.context -> cterm -> cterm
  val elim_implies: thm -> thm -> thm
  val forall_intr_name: string * cterm -> thm -> thm
  val forall_elim_var: int -> thm -> thm
  val forall_elim_vars: int -> thm -> thm
  val instantiate': ctyp option list -> cterm option list -> thm -> thm
  val forall_intr_frees: thm -> thm
  val unvarify_global: theory -> thm -> thm
  val unvarify_axiom: theory -> string -> thm
  val close_derivation: thm -> thm
  val rename_params_rule: string list * int -> thm -> thm
  val rename_boundvars: term -> term -> thm -> thm
  val add_axiom: Proof.context -> binding * term -> theory -> (string * thm) * theory
  val add_axiom_global: binding * term -> theory -> (string * thm) * theory
  val add_def: Defs.context -> bool -> bool -> binding * term -> theory -> (string * thm) * theory
  val add_def_global: bool -> bool -> binding * term -> theory -> (string * thm) * theory
  type attribute = Context.generic * thm -> Context.generic option * thm option
  type binding = binding * attribute list
  val tag_rule: string * string -> thm -> thm
  val untag_rule: string -> thm -> thm
  val is_free_dummy: thm -> bool
  val tag_free_dummy: thm -> thm
  val def_name: string -> string
  val def_name_optional: string -> string -> string
  val def_binding: Binding.binding -> Binding.binding
  val def_binding_optional: Binding.binding -> Binding.binding -> Binding.binding
  val make_def_binding: bool -> Binding.binding -> Binding.binding
  val has_name_hint: thm -> bool
  val get_name_hint: thm -> string
  val put_name_hint: string -> thm -> thm
  val theoremK: string
  val legacy_get_kind: thm -> string
  val kind_rule: string -> thm -> thm
  val rule_attribute: thm list -> (Context.generic -> thm -> thm) -> attribute
  val declaration_attribute: (thm -> Context.generic -> Context.generic) -> attribute
  val mixed_attribute: (Context.generic * thm -> Context.generic * thm) -> attribute
  val apply_attribute: attribute -> thm -> Context.generic -> thm * Context.generic
  val attribute_declaration: attribute -> thm -> Context.generic -> Context.generic
  val theory_attributes: attribute list -> thm -> theory -> thm * theory
  val proof_attributes: attribute list -> thm -> Proof.context -> thm * Proof.context
  val no_attributes: 'a -> 'a * 'b list
  val simple_fact: 'a -> ('a * 'b list) list
  val tag: string * string -> attribute
  val untag: string -> attribute
  val kind: string -> attribute
  val register_proofs: thm list -> theory -> theory
  val join_theory_proofs: theory -> unit
  val show_consts_raw: Config.raw
  val show_consts: bool Config.T
  val show_hyps_raw: Config.raw
  val show_hyps: bool Config.T
  val show_tags_raw: Config.raw
  val show_tags: bool Config.T
  val pretty_flexpair: Proof.context -> term * term -> Pretty.T
  val pretty_thm_raw: Proof.context -> {quote: bool, show_hyps: bool} -> thm -> Pretty.T
  val pretty_thm: Proof.context -> thm -> Pretty.T
  val pretty_thm_item: Proof.context -> thm -> Pretty.T
  val pretty_thm_global: theory -> thm -> Pretty.T
  val string_of_thm: Proof.context -> thm -> string
  val string_of_thm_global: theory -> thm -> string
end;

structure Thm: THM =
struct

(** basic operations **)

(* collecting ctyps and cterms *)

val eq_ctyp = op = o apply2 Thm.typ_of;
val op aconvc = op aconv o apply2 Thm.term_of;

val add_tvars = Thm.fold_atomic_ctyps (fn a => is_TVar (Thm.typ_of a) ? insert eq_ctyp a);
val add_frees = Thm.fold_atomic_cterms (fn a => is_Free (Thm.term_of a) ? insert (op aconvc) a);
val add_vars = Thm.fold_atomic_cterms (fn a => is_Var (Thm.term_of a) ? insert (op aconvc) a);


(* cterm constructors and destructors *)

fun all_name ctxt (x, t) A =
  let
    val T = Thm.typ_of_cterm t;
    val all_const = Thm.cterm_of ctxt (Const ("Pure.all", (T --> propT) --> propT));
  in Thm.apply all_const (Thm.lambda_name (x, t) A) end;

fun all ctxt t A = all_name ctxt ("", t) A;

fun mk_binop c a b = Thm.apply (Thm.apply c a) b;
fun dest_binop ct = (Thm.dest_arg1 ct, Thm.dest_arg ct);

fun dest_implies ct =
  (case Thm.term_of ct of
    Const ("Pure.imp", _) $ _ $ _ => dest_binop ct
  | _ => raise TERM ("dest_implies", [Thm.term_of ct]));

fun dest_equals ct =
  (case Thm.term_of ct of
    Const ("Pure.eq", _) $ _ $ _ => dest_binop ct
  | _ => raise TERM ("dest_equals", [Thm.term_of ct]));

fun dest_equals_lhs ct =
  (case Thm.term_of ct of
    Const ("Pure.eq", _) $ _ $ _ => Thm.dest_arg1 ct
  | _ => raise TERM ("dest_equals_lhs", [Thm.term_of ct]));

fun dest_equals_rhs ct =
  (case Thm.term_of ct of
    Const ("Pure.eq", _) $ _ $ _ => Thm.dest_arg ct
  | _ => raise TERM ("dest_equals_rhs", [Thm.term_of ct]));

val lhs_of = dest_equals_lhs o Thm.cprop_of;
val rhs_of = dest_equals_rhs o Thm.cprop_of;


(* thm order: ignores theory context! *)

fun thm_ord ths =
  (case Term_Ord.fast_term_ord (apply2 Thm.prop_of ths) of
    EQUAL =>
      (case
        list_ord (prod_ord Term_Ord.fast_term_ord Term_Ord.fast_term_ord)
          (apply2 Thm.tpairs_of ths)
       of
        EQUAL =>
          (case list_ord Term_Ord.fast_term_ord (apply2 Thm.hyps_of ths) of
            EQUAL => list_ord Term_Ord.sort_ord (apply2 Thm.shyps_of ths)
          | ord => ord)
      | ord => ord)
  | ord => ord);


(* tables and caches *)

structure Ctermtab = Table(type key = cterm val ord = Term_Ord.fast_term_ord o apply2 Thm.term_of);
structure Thmtab = Table(type key = thm val ord = thm_ord);

fun cterm_cache f = Cache.create Ctermtab.empty Ctermtab.lookup Ctermtab.update f;
fun thm_cache f = Cache.create Thmtab.empty Thmtab.lookup Thmtab.update f;


(* equality *)

fun is_reflexive th = op aconv (Logic.dest_equals (Thm.prop_of th))
  handle TERM _ => false;

val eq_thm = is_equal o thm_ord;

val eq_thm_prop = op aconv o apply2 Thm.full_prop_of;

fun eq_thm_strict ths =
  eq_thm ths andalso
  Context.eq_thy_id (apply2 Thm.theory_id_of_thm ths) andalso
  op = (apply2 Thm.maxidx_of ths) andalso
  op = (apply2 Thm.get_tags ths);


(* pattern equivalence *)

fun equiv_thm thy ths =
  Pattern.equiv thy (apply2 (Thm.full_prop_of o Thm.transfer thy) ths);


(* type classes and sorts *)

fun class_triv thy c =
  Thm.of_class (Thm.global_ctyp_of thy (TVar ((Name.aT, 0), [c])), c);

fun of_sort (T, S) = map (fn c => Thm.of_class (T, c)) S;


(* misc operations *)

fun is_dummy thm =
  (case try Logic.dest_term (Thm.concl_of thm) of
    NONE => false
  | SOME t => Term.is_dummy_pattern (Term.head_of t));

fun plain_prop_of raw_thm =
  let
    val thm = Thm.strip_shyps raw_thm;
    fun err msg = raise THM ("plain_prop_of: " ^ msg, 0, [thm]);
  in
    if not (null (Thm.hyps_of thm)) then
      err "theorem may not contain hypotheses"
    else if not (null (Thm.extra_shyps thm)) then
      err "theorem may not contain sort hypotheses"
    else if not (null (Thm.tpairs_of thm)) then
      err "theorem may not contain flex-flex pairs"
    else Thm.prop_of thm
  end;


(* collections of theorems in canonical order *)

val add_thm = update eq_thm_prop;
val del_thm = remove eq_thm_prop;
val merge_thms = merge eq_thm_prop;

val full_rules = Item_Net.init eq_thm_prop (single o Thm.full_prop_of);
val intro_rules = Item_Net.init eq_thm_prop (single o Thm.concl_of);
val elim_rules = Item_Net.init eq_thm_prop (single o Thm.major_prem_of);



(** declared hyps and sort hyps **)

structure Hyps = Proof_Data
(
  type T = {checked_hyps: bool, hyps: Termtab.set, shyps: sort Ord_List.T};
  fun init _ : T = {checked_hyps = true, hyps = Termtab.empty, shyps = []};
);

fun map_hyps f = Hyps.map (fn {checked_hyps, hyps, shyps} =>
  let val (checked_hyps', hyps', shyps') = f (checked_hyps, hyps, shyps)
  in {checked_hyps = checked_hyps', hyps = hyps', shyps = shyps'} end);


(* hyps *)

fun declare_hyps raw_ct ctxt = ctxt |> map_hyps (fn (checked_hyps, hyps, shyps) =>
  let
    val ct = Thm.transfer_cterm (Proof_Context.theory_of ctxt) raw_ct;
    val hyps' = Termtab.update (Thm.term_of ct, ()) hyps;
  in (checked_hyps, hyps', shyps) end);

fun assume_hyps ct ctxt = (Thm.assume ct, declare_hyps ct ctxt);

val unchecked_hyps = map_hyps (fn (_, hyps, shyps) => (false, hyps, shyps));

fun restore_hyps ctxt =
  map_hyps (fn (_, hyps, shyps) => (#checked_hyps (Hyps.get ctxt), hyps, shyps));

fun undeclared_hyps context th =
  Thm.hyps_of th
  |> filter_out
    (case context of
      Context.Theory _ => K false
    | Context.Proof ctxt =>
        (case Hyps.get ctxt of
          {checked_hyps = false, ...} => K true
        | {hyps, ...} => Termtab.defined hyps));

fun check_hyps context th =
  (case undeclared_hyps context th of
    [] => th
  | undeclared =>
      error (Pretty.string_of (Pretty.big_list "Undeclared hyps:"
        (map (Pretty.item o single o Syntax.pretty_term (Syntax.init_pretty context)) undeclared))));


(* shyps *)

fun declare_term_sorts t =
  map_hyps (fn (checked_hyps, hyps, shyps) =>
    (checked_hyps, hyps, Sorts.insert_term t shyps));

fun extra_shyps' ctxt th =
  Sorts.subtract (#shyps (Hyps.get ctxt)) (Thm.extra_shyps th);

fun check_shyps ctxt raw_th =
  let
    val th = Thm.strip_shyps raw_th;
    val extra_shyps = extra_shyps' ctxt th;
  in
    if null extra_shyps then th
    else error (Pretty.string_of (Pretty.block (Pretty.str "Pending sort hypotheses:" ::
      Pretty.brk 1 :: Pretty.commas (map (Syntax.pretty_sort ctxt) extra_shyps))))
  end;

val weaken_sorts' = Thm.weaken_sorts o #shyps o Hyps.get;



(** basic derived rules **)

(*Elimination of implication
  A    A ==> B
  ------------
        B
*)
fun elim_implies thA thAB = Thm.implies_elim thAB thA;


(* forall_intr_name *)

fun forall_intr_name (a, x) th =
  let
    val th' = Thm.forall_intr x th;
    val prop' = (case Thm.prop_of th' of all $ Abs (_, T, b) => all $ Abs (a, T, b));
  in Thm.renamed_prop prop' th' end;


(* forall_elim_var(s) *)

local

fun dest_all ct =
  (case Thm.term_of ct of
    Const ("Pure.all", _) $ Abs (a, _, _) =>
      let val (x, ct') = Thm.dest_abs NONE (Thm.dest_arg ct)
      in SOME ((a, Thm.ctyp_of_cterm x), ct') end
  | _ => NONE);

fun dest_all_list ct =
  (case dest_all ct of
    NONE => []
  | SOME (v, ct') => v :: dest_all_list ct');

fun forall_elim_vars_list vars i th =
  let
    val used =
      (Thm.fold_terms o Term.fold_aterms)
        (fn Var ((x, j), _) => if i = j then insert (op =) x else I | _ => I) th [];
    val vars' = (Name.variant_list used (map #1 vars), vars)
      |> ListPair.map (fn (x, (_, T)) => Thm.var ((x, i), T));
  in fold Thm.forall_elim vars' th end;

in

fun forall_elim_vars i th =
  forall_elim_vars_list (dest_all_list (Thm.cprop_of th)) i th;

fun forall_elim_var i th =
  let
    val vars =
      (case dest_all (Thm.cprop_of th) of
        SOME (v, _) => [v]
      | NONE => raise THM ("forall_elim_var", i, [th]));
  in forall_elim_vars_list vars i th end;

end;


(* instantiate by left-to-right occurrence of variables *)

fun instantiate' cTs cts thm =
  let
    fun err msg =
      raise TYPE ("instantiate': " ^ msg,
        map_filter (Option.map Thm.typ_of) cTs,
        map_filter (Option.map Thm.term_of) cts);

    fun zip_vars xs ys =
      zip_options xs ys handle ListPair.UnequalLengths =>
        err "more instantiations than variables in thm";

    val thm' =
      Thm.instantiate ((zip_vars (rev (Thm.fold_terms Term.add_tvars thm [])) cTs), []) thm;
    val thm'' =
      Thm.instantiate ([], zip_vars (rev (Thm.fold_terms Term.add_vars thm' [])) cts) thm';
  in thm'' end;


(* forall_intr_frees: generalization over all suitable Free variables *)

fun forall_intr_frees th =
  let
    val fixed =
      fold Term.add_frees (Thm.terms_of_tpairs (Thm.tpairs_of th) @ Thm.hyps_of th) [];
    val frees =
      Thm.fold_atomic_cterms (fn a =>
        (case Thm.term_of a of
          Free v => not (member (op =) fixed v) ? insert (op aconvc) a
        | _ => I)) th [];
  in fold Thm.forall_intr frees th end;


(* unvarify_global: global schematic variables *)

fun unvarify_global thy th =
  let
    val prop = Thm.full_prop_of th;
    val _ = map Logic.unvarify_global (prop :: Thm.hyps_of th)
      handle TERM (msg, _) => raise THM (msg, 0, [th]);

    val instT = rev (Term.add_tvars prop []) |> map (fn v as ((a, _), S) => (v, TFree (a, S)));
    val inst = rev (Term.add_vars prop []) |> map (fn ((a, i), T) =>
      let val T' = Term_Subst.instantiateT instT T
      in (((a, i), T'), Thm.global_cterm_of thy (Free ((a, T')))) end);
  in Thm.instantiate (map (apsnd (Thm.global_ctyp_of thy)) instT, inst) th end;

fun unvarify_axiom thy = unvarify_global thy o Thm.axiom thy;


(* close_derivation *)

fun close_derivation thm =
  if Thm.derivation_name thm = "" then Thm.name_derivation "" thm
  else thm;


(* user renaming of parameters in a subgoal *)

(*The names, if distinct, are used for the innermost parameters of subgoal i;
  preceding parameters may be renamed to make all parameters distinct.*)
fun rename_params_rule (names, i) st =
  let
    val (_, Bs, Bi, C) = Thm.dest_state (st, i);
    val params = map #1 (Logic.strip_params Bi);
    val short = length params - length names;
    val names' =
      if short < 0 then error "More names than parameters in subgoal!"
      else Name.variant_list names (take short params) @ names;
    val free_names = Term.fold_aterms (fn Free (x, _) => insert (op =) x | _ => I) Bi [];
    val Bi' = Logic.list_rename_params names' Bi;
  in
    (case duplicates (op =) names of
      a :: _ => (warning ("Can't rename.  Bound variables not distinct: " ^ a); st)
    | [] =>
      (case inter (op =) names free_names of
        a :: _ => (warning ("Can't rename.  Bound/Free variable clash: " ^ a); st)
      | [] => Thm.renamed_prop (Logic.list_implies (Bs @ [Bi'], C)) st))
  end;


(* preservation of bound variable names *)

fun rename_boundvars pat obj th =
  (case Term.rename_abs pat obj (Thm.prop_of th) of
    NONE => th
  | SOME prop' => Thm.renamed_prop prop' th);



(** specification primitives **)

(* rules *)

fun stripped_sorts thy t =
  let
    val tfrees = rev (Term.add_tfrees t []);
    val tfrees' = map (fn a => (a, [])) (Name.invent Name.context Name.aT (length tfrees));
    val recover =
      map2 (fn (a', S') => fn (a, S) => (((a', 0), S'), Thm.global_ctyp_of thy (TVar ((a, 0), S))))
        tfrees' tfrees;
    val strip = map (apply2 TFree) (tfrees ~~ tfrees');
    val t' = Term.map_types (Term.map_atyps (perhaps (AList.lookup (op =) strip))) t;
  in (strip, recover, t') end;

fun add_axiom ctxt (b, prop) thy =
  let
    val _ = Sign.no_vars ctxt prop;
    val (strip, recover, prop') = stripped_sorts thy prop;
    val constraints = map (fn (TFree (_, S), T) => (T, S)) strip;
    val of_sorts = maps (fn (T as TFree (_, S), _) => of_sort (Thm.ctyp_of ctxt T, S)) strip;

    val thy' = thy
      |> Theory.add_axiom ctxt (b, Logic.list_implies (maps Logic.mk_of_sort constraints, prop'));
    val axm_name = Sign.full_name thy' b;
    val axm' = Thm.axiom thy' axm_name;
    val thm =
      Thm.instantiate (recover, []) axm'
      |> unvarify_global thy'
      |> fold elim_implies of_sorts;
  in ((axm_name, thm), thy') end;

fun add_axiom_global arg thy = add_axiom (Syntax.init_pretty_global thy) arg thy;

fun add_def (context as (ctxt, _)) unchecked overloaded (b, prop) thy =
  let
    val _ = Sign.no_vars ctxt prop;
    val prems = map (Thm.cterm_of ctxt) (Logic.strip_imp_prems prop);
    val (_, recover, concl') = stripped_sorts thy (Logic.strip_imp_concl prop);

    val thy' = Theory.add_def context unchecked overloaded (b, concl') thy;
    val axm_name = Sign.full_name thy' b;
    val axm' = Thm.axiom thy' axm_name;
    val thm =
      Thm.instantiate (recover, []) axm'
      |> unvarify_global thy'
      |> fold_rev Thm.implies_intr prems;
  in ((axm_name, thm), thy') end;

fun add_def_global unchecked overloaded arg thy =
  add_def (Defs.global_context thy) unchecked overloaded arg thy;



(*** theorem tags ***)

(* add / delete tags *)

fun tag_rule tg = Thm.map_tags (insert (op =) tg);
fun untag_rule s = Thm.map_tags (filter_out (fn (s', _) => s = s'));


(* free dummy thm -- for abstract closure *)

val free_dummyN = "free_dummy";
fun is_free_dummy thm = Properties.defined (Thm.get_tags thm) free_dummyN;
val tag_free_dummy = tag_rule (free_dummyN, "");


(* def_name *)

fun def_name c = c ^ "_def";

fun def_name_optional c "" = def_name c
  | def_name_optional _ name = name;

val def_binding = Binding.map_name def_name #> Binding.reset_pos;
fun def_binding_optional b name = if Binding.is_empty name then def_binding b else name;
fun make_def_binding cond b = if cond then def_binding b else Binding.empty;


(* unofficial theorem names *)

fun the_name_hint thm = the (AList.lookup (op =) (Thm.get_tags thm) Markup.nameN);

val has_name_hint = can the_name_hint;
val get_name_hint = the_default "??.unknown" o try the_name_hint;

fun put_name_hint name = untag_rule Markup.nameN #> tag_rule (Markup.nameN, name);


(* theorem kinds *)

val theoremK = "theorem";

fun legacy_get_kind thm = the_default "" (Properties.get (Thm.get_tags thm) Markup.kindN);

fun kind_rule k = tag_rule (Markup.kindN, k) o untag_rule Markup.kindN;



(** attributes **)

(*attributes subsume any kind of rules or context modifiers*)
type attribute = Context.generic * thm -> Context.generic option * thm option;

type binding = binding * attribute list;

fun rule_attribute ths f (x, th) =
  (NONE,
    (case find_first is_free_dummy (th :: ths) of
      SOME th' => SOME th'
    | NONE => SOME (f x th)));

fun declaration_attribute f (x, th) =
  (if is_free_dummy th then NONE else SOME (f th x), NONE);

fun mixed_attribute f (x, th) =
  let val (x', th') = f (x, th) in (SOME x', SOME th') end;

fun apply_attribute (att: attribute) th x =
  let val (x', th') = att (x, check_hyps x (Thm.transfer (Context.theory_of x) th))
  in (the_default th th', the_default x x') end;

fun attribute_declaration att th x = #2 (apply_attribute att th x);

fun apply_attributes mk dest =
  let
    fun app [] th x = (th, x)
      | app (att :: atts) th x = apply_attribute att th (mk x) ||> dest |-> app atts;
  in app end;

val theory_attributes = apply_attributes Context.Theory Context.the_theory;
val proof_attributes = apply_attributes Context.Proof Context.the_proof;

fun no_attributes x = (x, []);
fun simple_fact x = [(x, [])];

fun tag tg = rule_attribute [] (K (tag_rule tg));
fun untag s = rule_attribute [] (K (untag_rule s));
fun kind k = rule_attribute [] (K (k <> "" ? kind_rule k));


(* forked proofs *)

structure Proofs = Theory_Data
(
  type T = thm list;
  val empty = [];
  fun extend _ = empty;
  fun merge _ = empty;
);

fun register_proofs more_thms =
  Proofs.map (fold (cons o Thm.trim_context) more_thms);

fun join_theory_proofs thy =
  Thm.join_proofs (map (Thm.transfer thy) (rev (Proofs.get thy)));



(** print theorems **)

(* options *)

val show_consts_raw = Config.declare_option ("show_consts", @{here});
val show_consts = Config.bool show_consts_raw;

val show_hyps_raw = Config.declare ("show_hyps", @{here}) (fn _ => Config.Bool false);
val show_hyps = Config.bool show_hyps_raw;

val show_tags_raw = Config.declare ("show_tags", @{here}) (fn _ => Config.Bool false);
val show_tags = Config.bool show_tags_raw;


(* pretty_thm etc. *)

fun pretty_tag (name, arg) = Pretty.strs [name, quote arg];
val pretty_tags = Pretty.list "[" "]" o map pretty_tag;

fun pretty_flexpair ctxt (t, u) = Pretty.block
  [Syntax.pretty_term ctxt t, Pretty.str " =?=", Pretty.brk 1, Syntax.pretty_term ctxt u];

fun pretty_thm_raw ctxt {quote, show_hyps = show_hyps'} raw_th =
  let
    val show_tags = Config.get ctxt show_tags;
    val show_hyps = Config.get ctxt show_hyps;

    val th = raw_th
      |> perhaps (try (Thm.transfer (Proof_Context.theory_of ctxt)))
      |> perhaps (try Thm.strip_shyps);

    val hyps = if show_hyps then Thm.hyps_of th else undeclared_hyps (Context.Proof ctxt) th;
    val extra_shyps = extra_shyps' ctxt th;
    val tags = Thm.get_tags th;
    val tpairs = Thm.tpairs_of th;

    val q = if quote then Pretty.quote else I;
    val prt_term = q o Syntax.pretty_term ctxt;


    val hlen = length extra_shyps + length hyps + length tpairs;
    val hsymbs =
      if hlen = 0 then []
      else if show_hyps orelse show_hyps' then
        [Pretty.brk 2, Pretty.list "[" "]"
          (map (q o pretty_flexpair ctxt) tpairs @ map prt_term hyps @
           map (Syntax.pretty_sort ctxt) extra_shyps)]
      else [Pretty.brk 2, Pretty.str ("[" ^ replicate_string hlen "." ^ "]")];
    val tsymbs =
      if null tags orelse not show_tags then []
      else [Pretty.brk 1, pretty_tags tags];
  in Pretty.block (prt_term (Thm.prop_of th) :: (hsymbs @ tsymbs)) end;

fun pretty_thm ctxt = pretty_thm_raw ctxt {quote = false, show_hyps = true};
fun pretty_thm_item ctxt th = Pretty.item [pretty_thm ctxt th];

fun pretty_thm_global thy =
  pretty_thm_raw (Syntax.init_pretty_global thy) {quote = false, show_hyps = false};

val string_of_thm = Pretty.string_of oo pretty_thm;
val string_of_thm_global = Pretty.string_of oo pretty_thm_global;


open Thm;

end;

structure Basic_Thm: BASIC_THM = Thm;
open Basic_Thm;
