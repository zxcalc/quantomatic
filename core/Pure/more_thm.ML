(*  Title:      Pure/more_thm.ML
    Author:     Makarius

Further operations on type ctyp/cterm/thm, outside the inference kernel.
*)

infix aconvc;

signature BASIC_THM =
sig
  include BASIC_THM
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
  val aconvc: cterm * cterm -> bool
  val add_cterm_frees: cterm -> cterm list -> cterm list
  val all_name: string * cterm -> cterm -> cterm
  val all: cterm -> cterm -> cterm
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
  val equiv_thm: thm * thm -> bool
  val class_triv: theory -> class -> thm
  val of_sort: ctyp * sort -> thm list
  val check_shyps: sort list -> thm -> thm
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
  val elim_implies: thm -> thm -> thm
  val forall_elim_var: int -> thm -> thm
  val forall_elim_vars: int -> thm -> thm
  val certify_inst: theory ->
    ((indexname * sort) * typ) list * ((indexname * typ) * term) list ->
    (ctyp * ctyp) list * (cterm * cterm) list
  val certify_instantiate:
    ((indexname * sort) * typ) list * ((indexname * typ) * term) list -> thm -> thm
  val forall_intr_frees: thm -> thm
  val unvarify_global: thm -> thm
  val close_derivation: thm -> thm
  val add_axiom: Proof.context -> binding * term -> theory -> (string * thm) * theory
  val add_axiom_global: binding * term -> theory -> (string * thm) * theory
  val add_def: Proof.context -> bool -> bool -> binding * term -> theory -> (string * thm) * theory
  val add_def_global: bool -> bool -> binding * term -> theory -> (string * thm) * theory
  type attribute = Context.generic * thm -> Context.generic option * thm option
  type binding = binding * attribute list
  val empty_binding: binding
  val rule_attribute: (Context.generic -> thm -> thm) -> attribute
  val declaration_attribute: (thm -> Context.generic -> Context.generic) -> attribute
  val mixed_attribute: (Context.generic * thm -> Context.generic * thm) -> attribute
  val apply_attribute: attribute -> thm -> Context.generic -> thm * Context.generic
  val attribute_declaration: attribute -> thm -> Context.generic -> Context.generic
  val theory_attributes: attribute list -> thm -> theory -> thm * theory
  val proof_attributes: attribute list -> thm -> Proof.context -> thm * Proof.context
  val no_attributes: 'a -> 'a * 'b list
  val simple_fact: 'a -> ('a * 'b list) list
  val tag_rule: string * string -> thm -> thm
  val untag_rule: string -> thm -> thm
  val tag: string * string -> attribute
  val untag: string -> attribute
  val def_name: string -> string
  val def_name_optional: string -> string -> string
  val def_binding: Binding.binding -> Binding.binding
  val def_binding_optional: Binding.binding -> Binding.binding -> Binding.binding
  val has_name_hint: thm -> bool
  val get_name_hint: thm -> string
  val put_name_hint: string -> thm -> thm
  val theoremK: string
  val lemmaK: string
  val corollaryK: string
  val legacy_get_kind: thm -> string
  val kind_rule: string -> thm -> thm
  val kind: string -> attribute
  val register_proofs: thm list -> theory -> theory
  val join_theory_proofs: theory -> unit
end;

structure Thm: THM =
struct

(** basic operations **)

(* collecting cterms *)

val op aconvc = op aconv o pairself Thm.term_of;

fun add_cterm_frees ct =
  let
    val cert = Thm.cterm_of (Thm.theory_of_cterm ct);
    val t = Thm.term_of ct;
  in Term.fold_aterms (fn v as Free _ => insert (op aconvc) (cert v) | _ => I) t end;


(* cterm constructors and destructors *)

fun all_name (x, t) A =
  let
    val cert = Thm.cterm_of (Thm.theory_of_cterm t);
    val T = #T (Thm.rep_cterm t);
  in Thm.apply (cert (Const ("Pure.all", (T --> propT) --> propT))) (Thm.lambda_name (x, t) A) end;

fun all t A = all_name ("", t) A;

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

fun thm_ord (th1, th2) =
  let
    val {shyps = shyps1, hyps = hyps1, tpairs = tpairs1, prop = prop1, ...} = Thm.rep_thm th1;
    val {shyps = shyps2, hyps = hyps2, tpairs = tpairs2, prop = prop2, ...} = Thm.rep_thm th2;
  in
    (case Term_Ord.fast_term_ord (prop1, prop2) of
      EQUAL =>
        (case list_ord (prod_ord Term_Ord.fast_term_ord Term_Ord.fast_term_ord) (tpairs1, tpairs2) of
          EQUAL =>
            (case list_ord Term_Ord.fast_term_ord (hyps1, hyps2) of
              EQUAL => list_ord Term_Ord.sort_ord (shyps1, shyps2)
            | ord => ord)
        | ord => ord)
    | ord => ord)
  end;


(* tables and caches *)

structure Ctermtab = Table(type key = cterm val ord = Term_Ord.fast_term_ord o pairself Thm.term_of);
structure Thmtab = Table(type key = thm val ord = thm_ord);

fun cterm_cache f = Cache.create Ctermtab.empty Ctermtab.lookup Ctermtab.update f;
fun thm_cache f = Cache.create Thmtab.empty Thmtab.lookup Thmtab.update f;


(* equality *)

fun is_reflexive th = op aconv (Logic.dest_equals (Thm.prop_of th))
  handle TERM _ => false;

val eq_thm = is_equal o thm_ord;

val eq_thm_prop = op aconv o pairself Thm.full_prop_of;

fun eq_thm_strict ths =
  eq_thm ths andalso
    let val (rep1, rep2) = pairself Thm.rep_thm ths in
      Theory.eq_thy (#thy rep1, #thy rep2) andalso
      #maxidx rep1 = #maxidx rep2 andalso
      #tags rep1 = #tags rep2
    end;


(* pattern equivalence *)

fun equiv_thm ths =
  Pattern.equiv (Theory.merge (pairself Thm.theory_of_thm ths)) (pairself Thm.full_prop_of ths);


(* type classes and sorts *)

fun class_triv thy c =
  Thm.of_class (Thm.ctyp_of thy (TVar ((Name.aT, 0), [c])), c);

fun of_sort (T, S) = map (fn c => Thm.of_class (T, c)) S;

fun check_shyps sorts raw_th =
  let
    val th = Thm.strip_shyps raw_th;
    val prt_sort = Syntax.pretty_sort_global (Thm.theory_of_thm th);
    val pending = Sorts.subtract sorts (Thm.extra_shyps th);
  in
    if null pending then th
    else error (Pretty.string_of (Pretty.block (Pretty.str "Pending sort hypotheses:" ::
      Pretty.brk 1 :: Pretty.commas (map prt_sort pending))))
  end;


(* misc operations *)

fun is_dummy thm =
  (case try Logic.dest_term (Thm.concl_of thm) of
    NONE => false
  | SOME t => Term.is_dummy_pattern t);

fun plain_prop_of raw_thm =
  let
    val thm = Thm.strip_shyps raw_thm;
    fun err msg = raise THM ("plain_prop_of: " ^ msg, 0, [thm]);
    val {hyps, prop, tpairs, ...} = Thm.rep_thm thm;
  in
    if not (null hyps) then
      err "theorem may not contain hypotheses"
    else if not (null (Thm.extra_shyps thm)) then
      err "theorem may not contain sort hypotheses"
    else if not (null tpairs) then
      err "theorem may not contain flex-flex pairs"
    else prop
  end;


(* collections of theorems in canonical order *)

val add_thm = update eq_thm_prop;
val del_thm = remove eq_thm_prop;
val merge_thms = merge eq_thm_prop;

val full_rules = Item_Net.init eq_thm_prop (single o Thm.full_prop_of);
val intro_rules = Item_Net.init eq_thm_prop (single o Thm.concl_of);
val elim_rules = Item_Net.init eq_thm_prop (single o Thm.major_prem_of);



(** declared hyps **)

structure Hyps = Proof_Data
(
  type T = Termtab.set * bool;
  fun init _ : T = (Termtab.empty, true);
);

fun declare_hyps ct ctxt =
  if Theory.subthy (theory_of_cterm ct, Proof_Context.theory_of ctxt) then
    (Hyps.map o apfst) (Termtab.update (term_of ct, ())) ctxt
  else raise CTERM ("assume_hyps: bad background theory", [ct]);

fun assume_hyps ct ctxt = (Thm.assume ct, declare_hyps ct ctxt);

val unchecked_hyps = (Hyps.map o apsnd) (K false);
fun restore_hyps ctxt = (Hyps.map o apsnd) (K (#2 (Hyps.get ctxt)));

fun undeclared_hyps context th =
  Thm.hyps_of th
  |> filter_out
    (case context of
      Context.Theory _ => K false
    | Context.Proof ctxt =>
        (case Hyps.get ctxt of
          (_, false) => K true
        | (hyps, _) => Termtab.defined hyps));

fun check_hyps context th =
  (case undeclared_hyps context th of
    [] => th
  | undeclared =>
      let
        val ctxt = Context.cases Syntax.init_pretty_global I context;
      in
        error (Pretty.string_of (Pretty.big_list "Undeclared hyps:"
          (map (Pretty.item o single o Syntax.pretty_term ctxt) undeclared)))
      end);



(** basic derived rules **)

(*Elimination of implication
  A    A ==> B
  ------------
        B
*)
fun elim_implies thA thAB = Thm.implies_elim thAB thA;


(* forall_elim_var(s) *)

local

fun forall_elim_vars_aux strip_vars i th =
  let
    val thy = Thm.theory_of_thm th;
    val {tpairs, prop, ...} = Thm.rep_thm th;
    val add_used = Term.fold_aterms
      (fn Var ((x, j), _) => if i = j then insert (op =) x else I | _ => I);
    val used = fold (fn (t, u) => add_used t o add_used u) tpairs (add_used prop []);
    val vars = strip_vars prop;
    val cvars = (Name.variant_list used (map #1 vars), vars)
      |> ListPair.map (fn (x, (_, T)) => Thm.cterm_of thy (Var ((x, i), T)));
  in fold Thm.forall_elim cvars th end;

in

val forall_elim_vars = forall_elim_vars_aux Term.strip_all_vars;

fun forall_elim_var i th =
  forall_elim_vars_aux
    (fn Const ("Pure.all", _) $ Abs (a, T, _) => [(a, T)]
      | _ => raise THM ("forall_elim_vars", i, [th])) i th;

end;


(* certify_instantiate *)

fun certify_inst thy (instT, inst) =
  (map (fn (v, T) => (Thm.ctyp_of thy (TVar v), Thm.ctyp_of thy T)) instT,
    map (fn (v, t) => (Thm.cterm_of thy (Var v), Thm.cterm_of thy t)) inst);

fun certify_instantiate insts th =
  Thm.instantiate (certify_inst (Thm.theory_of_thm th) insts) th;


(* forall_intr_frees: generalization over all suitable Free variables *)

fun forall_intr_frees th =
  let
    val thy = Thm.theory_of_thm th;
    val {prop, hyps, tpairs, ...} = Thm.rep_thm th;
    val fixed = fold Term.add_frees (Thm.terms_of_tpairs tpairs @ hyps) [];
    val frees = Term.fold_aterms (fn Free v =>
      if member (op =) fixed v then I else insert (op =) v | _ => I) prop [];
  in fold (Thm.forall_intr o Thm.cterm_of thy o Free) frees th end;


(* unvarify_global: global schematic variables *)

fun unvarify_global th =
  let
    val prop = Thm.full_prop_of th;
    val _ = map Logic.unvarify_global (prop :: Thm.hyps_of th)
      handle TERM (msg, _) => raise THM (msg, 0, [th]);

    val instT = rev (Term.add_tvars prop []) |> map (fn v as ((a, _), S) => (v, TFree (a, S)));
    val inst = rev (Term.add_vars prop []) |> map (fn ((a, i), T) =>
      let val T' = Term_Subst.instantiateT instT T
      in (((a, i), T'), Free ((a, T'))) end);
  in certify_instantiate (instT, inst) th end;


(* close_derivation *)

fun close_derivation thm =
  if Thm.derivation_name thm = "" then Thm.name_derivation "" thm
  else thm;



(** specification primitives **)

(* rules *)

fun stripped_sorts thy t =
  let
    val tfrees = rev (map TFree (Term.add_tfrees t []));
    val tfrees' = map (fn a => TFree (a, [])) (Name.invent Name.context Name.aT (length tfrees));
    val strip = tfrees ~~ tfrees';
    val recover = map (pairself (Thm.ctyp_of thy o Logic.varifyT_global) o swap) strip;
    val t' = Term.map_types (Term.map_atyps (perhaps (AList.lookup (op =) strip))) t;
  in (strip, recover, t') end;

fun add_axiom ctxt (b, prop) thy =
  let
    val _ = Sign.no_vars ctxt prop;
    val (strip, recover, prop') = stripped_sorts thy prop;
    val constraints = map (fn (TFree (_, S), T) => (T, S)) strip;
    val of_sorts = maps (fn (T as TFree (_, S), _) => of_sort (Thm.ctyp_of thy T, S)) strip;

    val thy' = thy
      |> Theory.add_axiom ctxt (b, Logic.list_implies (maps Logic.mk_of_sort constraints, prop'));
    val axm_name = Sign.full_name thy' b;
    val axm' = Thm.axiom thy' axm_name;
    val thm =
      Thm.instantiate (recover, []) axm'
      |> unvarify_global
      |> fold elim_implies of_sorts;
  in ((axm_name, thm), thy') end;

fun add_axiom_global arg thy = add_axiom (Syntax.init_pretty_global thy) arg thy;

fun add_def ctxt unchecked overloaded (b, prop) thy =
  let
    val _ = Sign.no_vars ctxt prop;
    val prems = map (Thm.cterm_of thy) (Logic.strip_imp_prems prop);
    val (_, recover, concl') = stripped_sorts thy (Logic.strip_imp_concl prop);

    val thy' = Theory.add_def ctxt unchecked overloaded (b, concl') thy;
    val axm_name = Sign.full_name thy' b;
    val axm' = Thm.axiom thy' axm_name;
    val thm =
      Thm.instantiate (recover, []) axm'
      |> unvarify_global
      |> fold_rev Thm.implies_intr prems;
  in ((axm_name, thm), thy') end;

fun add_def_global unchecked overloaded arg thy =
  add_def (Syntax.init_pretty_global thy) unchecked overloaded arg thy;



(** attributes **)

(*attributes subsume any kind of rules or context modifiers*)
type attribute = Context.generic * thm -> Context.generic option * thm option;

type binding = binding * attribute list;
val empty_binding: binding = (Binding.empty, []);

fun rule_attribute f (x, th) = (NONE, SOME (f x th));
fun declaration_attribute f (x, th) = (SOME (f th x), NONE);
fun mixed_attribute f (x, th) = let val (x', th') = f (x, th) in (SOME x', SOME th') end;

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



(*** theorem tags ***)

(* add / delete tags *)

fun tag_rule tg = Thm.map_tags (insert (op =) tg);
fun untag_rule s = Thm.map_tags (filter_out (fn (s', _) => s = s'));

fun tag tg = rule_attribute (K (tag_rule tg));
fun untag s = rule_attribute (K (untag_rule s));


(* def_name *)

fun def_name c = c ^ "_def";

fun def_name_optional c "" = def_name c
  | def_name_optional _ name = name;

val def_binding = Binding.map_name def_name;

fun def_binding_optional b name =
  if Binding.is_empty name then def_binding b else name;


(* unofficial theorem names *)

fun the_name_hint thm = the (AList.lookup (op =) (Thm.get_tags thm) Markup.nameN);

val has_name_hint = can the_name_hint;
val get_name_hint = the_default "??.unknown" o try the_name_hint;

fun put_name_hint name = untag_rule Markup.nameN #> tag_rule (Markup.nameN, name);


(* theorem kinds *)

val theoremK = "theorem";
val lemmaK = "lemma";
val corollaryK = "corollary";

fun legacy_get_kind thm = the_default "" (Properties.get (Thm.get_tags thm) Markup.kindN);

fun kind_rule k = tag_rule (Markup.kindN, k) o untag_rule Markup.kindN;
fun kind k = rule_attribute (K (k <> "" ? kind_rule k));


(* forked proofs *)

structure Proofs = Theory_Data
(
  type T = thm list;
  val empty = [];
  fun extend _ = empty;
  fun merge _ = empty;
);

fun register_proofs more_thms = Proofs.map (fn thms => fold cons more_thms thms);
val join_theory_proofs = Thm.join_proofs o rev o Proofs.get;


open Thm;

end;

structure Basic_Thm: BASIC_THM = Thm;
open Basic_Thm;

