(*  Title:      Pure/drule.ML
    Author:     Lawrence C Paulson, Cambridge University Computer Laboratory

Derived rules and other operations on theorems.
*)

infix 0 RS RSN RL RLN MRS OF COMP INCR_COMP COMP_INCR;

signature BASIC_DRULE =
sig
  val mk_implies: cterm * cterm -> cterm
  val list_implies: cterm list * cterm -> cterm
  val strip_imp_prems: cterm -> cterm list
  val strip_imp_concl: cterm -> cterm
  val cprems_of: thm -> cterm list
  val forall_intr_list: cterm list -> thm -> thm
  val forall_intr_vars: thm -> thm
  val forall_elim_list: cterm list -> thm -> thm
  val lift_all: Proof.context -> cterm -> thm -> thm
  val implies_elim_list: thm -> thm list -> thm
  val implies_intr_list: cterm list -> thm -> thm
  val instantiate_normalize: ((indexname * sort) * ctyp) list * ((indexname * typ) * cterm) list ->
    thm -> thm
  val infer_instantiate_types: Proof.context -> ((indexname * typ) * cterm) list -> thm -> thm
  val infer_instantiate: Proof.context -> (indexname * cterm) list -> thm -> thm
  val infer_instantiate': Proof.context -> cterm option list -> thm -> thm
  val zero_var_indexes_list: thm list -> thm list
  val zero_var_indexes: thm -> thm
  val implies_intr_hyps: thm -> thm
  val rotate_prems: int -> thm -> thm
  val rearrange_prems: int list -> thm -> thm
  val RSN: thm * (int * thm) -> thm
  val RS: thm * thm -> thm
  val RLN: thm list * (int * thm list) -> thm list
  val RL: thm list * thm list -> thm list
  val MRS: thm list * thm -> thm
  val OF: thm * thm list -> thm
  val COMP: thm * thm -> thm
  val INCR_COMP: thm * thm -> thm
  val COMP_INCR: thm * thm -> thm
  val size_of_thm: thm -> int
  val reflexive_thm: thm
  val symmetric_thm: thm
  val transitive_thm: thm
  val extensional: thm -> thm
  val asm_rl: thm
  val cut_rl: thm
  val revcut_rl: thm
  val thin_rl: thm
end;

signature DRULE =
sig
  include BASIC_DRULE
  val outer_params: term -> (string * typ) list
  val generalize: string list * string list -> thm -> thm
  val list_comb: cterm * cterm list -> cterm
  val strip_comb: cterm -> cterm * cterm list
  val strip_type: ctyp -> ctyp list * ctyp
  val beta_conv: cterm -> cterm -> cterm
  val flexflex_unique: Proof.context option -> thm -> thm
  val export_without_context: thm -> thm
  val export_without_context_open: thm -> thm
  val store_thm: binding -> thm -> thm
  val store_standard_thm: binding -> thm -> thm
  val store_thm_open: binding -> thm -> thm
  val store_standard_thm_open: binding -> thm -> thm
  val multi_resolve: Proof.context option -> thm list -> thm -> thm Seq.seq
  val multi_resolves: Proof.context option -> thm list -> thm list -> thm Seq.seq
  val compose: thm * int * thm -> thm
  val equals_cong: thm
  val imp_cong: thm
  val swap_prems_eq: thm
  val imp_cong_rule: thm -> thm -> thm
  val arg_cong_rule: cterm -> thm -> thm
  val binop_cong_rule: cterm -> thm -> thm -> thm
  val fun_cong_rule: thm -> cterm -> thm
  val beta_eta_conversion: cterm -> thm
  val eta_contraction_rule: thm -> thm
  val norm_hhf_eq: thm
  val norm_hhf_eqs: thm list
  val is_norm_hhf: term -> bool
  val norm_hhf: theory -> term -> term
  val norm_hhf_cterm: Proof.context -> cterm -> cterm
  val protect: cterm -> cterm
  val protectI: thm
  val protectD: thm
  val protect_cong: thm
  val implies_intr_protected: cterm list -> thm -> thm
  val termI: thm
  val mk_term: cterm -> thm
  val dest_term: thm -> cterm
  val cterm_rule: (thm -> thm) -> cterm -> cterm
  val cterm_add_frees: cterm -> cterm list -> cterm list
  val cterm_add_vars: cterm -> cterm list -> cterm list
  val dummy_thm: thm
  val free_dummy_thm: thm
  val is_sort_constraint: term -> bool
  val sort_constraintI: thm
  val sort_constraint_eq: thm
  val with_subgoal: int -> (thm -> thm) -> thm -> thm
  val comp_no_flatten: thm * int -> int -> thm -> thm
  val rename_bvars: (string * string) list -> thm -> thm
  val rename_bvars': string option list -> thm -> thm
  val incr_indexes: thm -> thm -> thm
  val incr_indexes2: thm -> thm -> thm -> thm
  val triv_forall_equality: thm
  val distinct_prems_rl: thm
  val equal_intr_rule: thm
  val equal_elim_rule1: thm
  val equal_elim_rule2: thm
  val remdups_rl: thm
  val abs_def: thm -> thm
end;

structure Drule: DRULE =
struct


(** some cterm->cterm operations: faster than calling cterm_of! **)

(* A1==>...An==>B  goes to  [A1,...,An], where B is not an implication *)
fun strip_imp_prems ct =
  let val (cA, cB) = Thm.dest_implies ct
  in cA :: strip_imp_prems cB end
  handle TERM _ => [];

(* A1==>...An==>B  goes to B, where B is not an implication *)
fun strip_imp_concl ct =
  (case Thm.term_of ct of
    Const ("Pure.imp", _) $ _ $ _ => strip_imp_concl (Thm.dest_arg ct)
  | _ => ct);

(*The premises of a theorem, as a cterm list*)
val cprems_of = strip_imp_prems o Thm.cprop_of;

fun certify t = Thm.global_cterm_of (Context.the_global_context ()) t;

val implies = certify Logic.implies;
fun mk_implies (A, B) = Thm.apply (Thm.apply implies A) B;

(*cterm version of list_implies: [A1,...,An], B  goes to [|A1;==>;An|]==>B *)
fun list_implies([], B) = B
  | list_implies(A::AS, B) = mk_implies (A, list_implies(AS,B));

(*cterm version of list_comb: maps  (f, [t1,...,tn])  to  f(t1,...,tn) *)
fun list_comb (f, []) = f
  | list_comb (f, t::ts) = list_comb (Thm.apply f t, ts);

(*cterm version of strip_comb: maps  f(t1,...,tn)  to  (f, [t1,...,tn]) *)
fun strip_comb ct =
  let
    fun stripc (p as (ct, cts)) =
      let val (ct1, ct2) = Thm.dest_comb ct
      in stripc (ct1, ct2 :: cts) end handle CTERM _ => p
  in stripc (ct, []) end;

(* cterm version of strip_type: maps  [T1,...,Tn]--->T  to   ([T1,T2,...,Tn], T) *)
fun strip_type cT = (case Thm.typ_of cT of
    Type ("fun", _) =>
      let
        val [cT1, cT2] = Thm.dest_ctyp cT;
        val (cTs, cT') = strip_type cT2
      in (cT1 :: cTs, cT') end
  | _ => ([], cT));

(*Beta-conversion for cterms, where x is an abstraction. Simply returns the rhs
  of the meta-equality returned by the beta_conversion rule.*)
fun beta_conv x y =
  Thm.dest_arg (Thm.cprop_of (Thm.beta_conversion false (Thm.apply x y)));



(** Standardization of rules **)

(*Generalization over a list of variables*)
val forall_intr_list = fold_rev Thm.forall_intr;

(*Generalization over Vars -- canonical order*)
fun forall_intr_vars th = fold Thm.forall_intr (Thm.add_vars th []) th;

fun outer_params t =
  let val vs = Term.strip_all_vars t
  in Name.variant_list [] (map (Name.clean o #1) vs) ~~ map #2 vs end;

(*lift vars wrt. outermost goal parameters
  -- reverses the effect of gen_all modulo higher-order unification*)
fun lift_all ctxt raw_goal raw_th =
  let
    val thy = Proof_Context.theory_of ctxt;
    val goal = Thm.transfer_cterm thy raw_goal;
    val th = Thm.transfer thy raw_th;

    val maxidx = Thm.maxidx_of th;
    val ps = outer_params (Thm.term_of goal)
      |> map (fn (x, T) => Var ((x, maxidx + 1), Logic.incr_tvar (maxidx + 1) T));
    val Ts = map Term.fastype_of ps;
    val inst =
      Thm.fold_terms Term.add_vars th []
      |> map (fn (xi, T) => ((xi, T), Thm.cterm_of ctxt (Term.list_comb (Var (xi, Ts ---> T), ps))));
  in
    th
    |> Thm.instantiate ([], inst)
    |> fold_rev (Thm.forall_intr o Thm.cterm_of ctxt) ps
  end;

(*direct generalization*)
fun generalize names th = Thm.generalize names (Thm.maxidx_of th + 1) th;

(*specialization over a list of cterms*)
val forall_elim_list = fold Thm.forall_elim;

(*maps A1,...,An |- B  to  [| A1;...;An |] ==> B*)
val implies_intr_list = fold_rev Thm.implies_intr;

(*maps [| A1;...;An |] ==> B and [A1,...,An]  to  B*)
fun implies_elim_list impth ths = fold Thm.elim_implies ths impth;

(*Reset Var indexes to zero, renaming to preserve distinctness*)
fun zero_var_indexes_list [] = []
  | zero_var_indexes_list ths =
      let
        val (instT, inst) = Term_Subst.zero_var_indexes_inst (map Thm.full_prop_of ths);

        val tvars = fold Thm.add_tvars ths [];
        fun the_tvar v = the (find_first (fn cT => v = dest_TVar (Thm.typ_of cT)) tvars);
        val instT' = map (fn (v, TVar (b, _)) => (v, Thm.rename_tvar b (the_tvar v))) instT;

        val vars = fold (Thm.add_vars o Thm.instantiate (instT', [])) ths [];
        fun the_var v = the (find_first (fn ct => v = dest_Var (Thm.term_of ct)) vars);
        val inst' = map (fn (v, Var (b, _)) => (v, Thm.var (b, Thm.ctyp_of_cterm (the_var v)))) inst;
      in map (Thm.adjust_maxidx_thm ~1 o Thm.instantiate (instT', inst')) ths end;

val zero_var_indexes = singleton zero_var_indexes_list;


(** Standard form of object-rule: no hypotheses, flexflex constraints,
    Frees, or outer quantifiers; all generality expressed by Vars of index 0.**)

(*Discharge all hypotheses.*)
fun implies_intr_hyps th = fold Thm.implies_intr (Thm.chyps_of th) th;

(*Squash a theorem's flexflex constraints provided it can be done uniquely.
  This step can lose information.*)
fun flexflex_unique opt_ctxt th =
  if null (Thm.tpairs_of th) then th
  else
    (case distinct Thm.eq_thm (Seq.list_of (Thm.flexflex_rule opt_ctxt th)) of
      [th] => th
    | [] => raise THM ("flexflex_unique: impossible constraints", 0, [th])
    | _ => raise THM ("flexflex_unique: multiple unifiers", 0, [th]));


(* old-style export without context *)

val export_without_context_open =
  implies_intr_hyps
  #> Thm.forall_intr_frees
  #> `Thm.maxidx_of
  #-> (fn maxidx =>
    Thm.forall_elim_vars (maxidx + 1)
    #> Thm.strip_shyps
    #> zero_var_indexes
    #> Thm.varifyT_global);

val export_without_context =
  flexflex_unique NONE
  #> export_without_context_open
  #> Thm.close_derivation;


(*Rotates a rule's premises to the left by k*)
fun rotate_prems 0 = I
  | rotate_prems k = Thm.permute_prems 0 k;

fun with_subgoal i f = rotate_prems (i - 1) #> f #> rotate_prems (1 - i);

(*Permute prems, where the i-th position in the argument list (counting from 0)
  gives the position within the original thm to be transferred to position i.
  Any remaining trailing positions are left unchanged.*)
val rearrange_prems =
  let
    fun rearr new [] thm = thm
      | rearr new (p :: ps) thm =
          rearr (new + 1)
            (map (fn q => if new <= q andalso q < p then q + 1 else q) ps)
            (Thm.permute_prems (new + 1) (new - p) (Thm.permute_prems new (p - new) thm))
  in rearr 0 end;


(*Resolution: multiple arguments, multiple results*)
local
  fun res opt_ctxt th i rule =
    Thm.biresolution opt_ctxt false [(false, th)] i rule handle THM _ => Seq.empty;

  fun multi_res _ _ [] rule = Seq.single rule
    | multi_res opt_ctxt i (th :: ths) rule =
        Seq.maps (res opt_ctxt th i) (multi_res opt_ctxt (i + 1) ths rule);
in
  fun multi_resolve opt_ctxt = multi_res opt_ctxt 1;
  fun multi_resolves opt_ctxt facts rules =
    Seq.maps (multi_resolve opt_ctxt facts) (Seq.of_list rules);
end;

(*Resolution: exactly one resolvent must be produced*)
fun tha RSN (i, thb) =
  (case Seq.chop 2 (Thm.biresolution NONE false [(false, tha)] i thb) of
    ([th], _) => th
  | ([], _) => raise THM ("RSN: no unifiers", i, [tha, thb])
  | _ => raise THM ("RSN: multiple unifiers", i, [tha, thb]));

(*Resolution: P==>Q, Q==>R gives P==>R*)
fun tha RS thb = tha RSN (1,thb);

(*For joining lists of rules*)
fun thas RLN (i, thbs) =
  let
    val resolve = Thm.biresolution NONE false (map (pair false) thas) i
    fun resb thb = Seq.list_of (resolve thb) handle THM _ => []
  in maps resb thbs end;

fun thas RL thbs = thas RLN (1, thbs);

(*Isar-style multi-resolution*)
fun bottom_rl OF rls =
  (case Seq.chop 2 (multi_resolve NONE rls bottom_rl) of
    ([th], _) => th
  | ([], _) => raise THM ("OF: no unifiers", 0, bottom_rl :: rls)
  | _ => raise THM ("OF: multiple unifiers", 0, bottom_rl :: rls));

(*Resolve a list of rules against bottom_rl from right to left;
  makes proof trees*)
fun rls MRS bottom_rl = bottom_rl OF rls;

(*compose Q and [...,Qi,Q(i+1),...]==>R to [...,Q(i+1),...]==>R
  with no lifting or renaming!  Q may contain ==> or meta-quants
  ALWAYS deletes premise i *)
fun compose (tha, i, thb) =
  Thm.bicompose NONE {flatten = true, match = false, incremented = false} (false, tha, 0) i thb
  |> Seq.list_of |> distinct Thm.eq_thm
  |> (fn [th] => th | _ => raise THM ("compose: unique result expected", i, [tha, thb]));


(** theorem equality **)

(*Useful "distance" function for BEST_FIRST*)
val size_of_thm = size_of_term o Thm.full_prop_of;



(*** Meta-Rewriting Rules ***)

val read_prop = certify o Simple_Syntax.read_prop;

fun store_thm name th =
  Context.>>> (Context.map_theory_result (Global_Theory.store_thm (name, th)));

fun store_thm_open name th =
  Context.>>> (Context.map_theory_result (Global_Theory.store_thm_open (name, th)));

fun store_standard_thm name th = store_thm name (export_without_context th);
fun store_standard_thm_open name th = store_thm_open name (export_without_context_open th);

val reflexive_thm =
  let val cx = certify (Var(("x",0),TVar(("'a",0),[])))
  in store_standard_thm_open (Binding.make ("reflexive", @{here})) (Thm.reflexive cx) end;

val symmetric_thm =
  let
    val xy = read_prop "x::'a == y::'a";
    val thm = Thm.implies_intr xy (Thm.symmetric (Thm.assume xy));
  in store_standard_thm_open (Binding.make ("symmetric", @{here})) thm end;

val transitive_thm =
  let
    val xy = read_prop "x::'a == y::'a";
    val yz = read_prop "y::'a == z::'a";
    val xythm = Thm.assume xy;
    val yzthm = Thm.assume yz;
    val thm = Thm.implies_intr yz (Thm.transitive xythm yzthm);
  in store_standard_thm_open (Binding.make ("transitive", @{here})) thm end;

fun extensional eq =
  let val eq' =
    Thm.abstract_rule "x" (Thm.dest_arg (fst (Thm.dest_equals (Thm.cprop_of eq)))) eq
  in Thm.equal_elim (Thm.eta_conversion (Thm.cprop_of eq')) eq' end;

val equals_cong =
  store_standard_thm_open (Binding.make ("equals_cong", @{here}))
    (Thm.reflexive (read_prop "x::'a == y::'a"));

val imp_cong =
  let
    val ABC = read_prop "A ==> B::prop == C::prop"
    val AB = read_prop "A ==> B"
    val AC = read_prop "A ==> C"
    val A = read_prop "A"
  in
    store_standard_thm_open (Binding.make ("imp_cong", @{here}))
      (Thm.implies_intr ABC (Thm.equal_intr
        (Thm.implies_intr AB (Thm.implies_intr A
          (Thm.equal_elim (Thm.implies_elim (Thm.assume ABC) (Thm.assume A))
            (Thm.implies_elim (Thm.assume AB) (Thm.assume A)))))
        (Thm.implies_intr AC (Thm.implies_intr A
          (Thm.equal_elim (Thm.symmetric (Thm.implies_elim (Thm.assume ABC) (Thm.assume A)))
            (Thm.implies_elim (Thm.assume AC) (Thm.assume A)))))))
  end;

val swap_prems_eq =
  let
    val ABC = read_prop "A ==> B ==> C"
    val BAC = read_prop "B ==> A ==> C"
    val A = read_prop "A"
    val B = read_prop "B"
  in
    store_standard_thm_open (Binding.make ("swap_prems_eq", @{here}))
      (Thm.equal_intr
        (Thm.implies_intr ABC (Thm.implies_intr B (Thm.implies_intr A
          (Thm.implies_elim (Thm.implies_elim (Thm.assume ABC) (Thm.assume A)) (Thm.assume B)))))
        (Thm.implies_intr BAC (Thm.implies_intr A (Thm.implies_intr B
          (Thm.implies_elim (Thm.implies_elim (Thm.assume BAC) (Thm.assume B)) (Thm.assume A))))))
  end;

val imp_cong_rule = Thm.combination o Thm.combination (Thm.reflexive implies);

fun arg_cong_rule ct th = Thm.combination (Thm.reflexive ct) th;    (*AP_TERM in LCF/HOL*)
fun fun_cong_rule th ct = Thm.combination th (Thm.reflexive ct);    (*AP_THM in LCF/HOL*)
fun binop_cong_rule ct th1 th2 = Thm.combination (arg_cong_rule ct th1) th2;

fun beta_eta_conversion ct =
  let val thm = Thm.beta_conversion true ct
  in Thm.transitive thm (Thm.eta_conversion (Thm.rhs_of thm)) end;

(*Contract all eta-redexes in the theorem, lest they give rise to needless abstractions*)
fun eta_contraction_rule th =
  Thm.equal_elim (Thm.eta_conversion (Thm.cprop_of th)) th;


(* abs_def *)

(*
   f ?x1 ... ?xn == u
  --------------------
   f == %x1 ... xn. u
*)

local

fun contract_lhs th =
  Thm.transitive (Thm.symmetric (beta_eta_conversion (#1 (Thm.dest_equals (Thm.cprop_of th))))) th;

fun var_args ct =
  (case try Thm.dest_comb ct of
    SOME (f, arg) =>
      (case Thm.term_of arg of
        Var ((x, _), _) => update (eq_snd (op aconvc)) (x, arg) (var_args f)
      | _ => [])
  | NONE => []);

in

fun abs_def th =
  let
    val th' = contract_lhs th;
    val args = var_args (Thm.lhs_of th');
  in contract_lhs (fold (uncurry Thm.abstract_rule) args th') end;

end;



(*** Some useful meta-theorems ***)

(*The rule V/V, obtains assumption solving for eresolve_tac*)
val asm_rl =
  store_standard_thm_open (Binding.make ("asm_rl", @{here}))
    (Thm.trivial (read_prop "?psi"));

(*Meta-level cut rule: [| V==>W; V |] ==> W *)
val cut_rl =
  store_standard_thm_open (Binding.make ("cut_rl", @{here}))
    (Thm.trivial (read_prop "?psi ==> ?theta"));

(*Generalized elim rule for one conclusion; cut_rl with reversed premises:
     [| PROP V;  PROP V ==> PROP W |] ==> PROP W *)
val revcut_rl =
  let
    val V = read_prop "V";
    val VW = read_prop "V ==> W";
  in
    store_standard_thm_open (Binding.make ("revcut_rl", @{here}))
      (Thm.implies_intr V
        (Thm.implies_intr VW (Thm.implies_elim (Thm.assume VW) (Thm.assume V))))
  end;

(*for deleting an unwanted assumption*)
val thin_rl =
  let
    val V = read_prop "V";
    val W = read_prop "W";
    val thm = Thm.implies_intr V (Thm.implies_intr W (Thm.assume W));
  in store_standard_thm_open (Binding.make ("thin_rl", @{here})) thm end;

(* (!!x. PROP ?V) == PROP ?V       Allows removal of redundant parameters*)
val triv_forall_equality =
  let
    val V = read_prop "V";
    val QV = read_prop "!!x::'a. V";
    val x = certify (Free ("x", Term.aT []));
  in
    store_standard_thm_open (Binding.make ("triv_forall_equality", @{here}))
      (Thm.equal_intr (Thm.implies_intr QV (Thm.forall_elim x (Thm.assume QV)))
        (Thm.implies_intr V (Thm.forall_intr x (Thm.assume V))))
  end;

(* (PROP ?Phi ==> PROP ?Phi ==> PROP ?Psi) ==>
   (PROP ?Phi ==> PROP ?Psi)
*)
val distinct_prems_rl =
  let
    val AAB = read_prop "Phi ==> Phi ==> Psi";
    val A = read_prop "Phi";
  in
    store_standard_thm_open (Binding.make ("distinct_prems_rl", @{here}))
      (implies_intr_list [AAB, A]
        (implies_elim_list (Thm.assume AAB) [Thm.assume A, Thm.assume A]))
  end;

(* [| PROP ?phi ==> PROP ?psi; PROP ?psi ==> PROP ?phi |]
   ==> PROP ?phi == PROP ?psi
   Introduction rule for == as a meta-theorem.
*)
val equal_intr_rule =
  let
    val PQ = read_prop "phi ==> psi";
    val QP = read_prop "psi ==> phi";
  in
    store_standard_thm_open (Binding.make ("equal_intr_rule", @{here}))
      (Thm.implies_intr PQ
        (Thm.implies_intr QP (Thm.equal_intr (Thm.assume PQ) (Thm.assume QP))))
  end;

(* PROP ?phi == PROP ?psi ==> PROP ?phi ==> PROP ?psi *)
val equal_elim_rule1 =
  let
    val eq = read_prop "phi::prop == psi::prop";
    val P = read_prop "phi";
  in
    store_standard_thm_open (Binding.make ("equal_elim_rule1", @{here}))
      (Thm.equal_elim (Thm.assume eq) (Thm.assume P) |> implies_intr_list [eq, P])
  end;

(* PROP ?psi == PROP ?phi ==> PROP ?phi ==> PROP ?psi *)
val equal_elim_rule2 =
  store_standard_thm_open (Binding.make ("equal_elim_rule2", @{here}))
    (symmetric_thm RS equal_elim_rule1);

(* PROP ?phi ==> PROP ?phi ==> PROP ?psi ==> PROP ?psi *)
val remdups_rl =
  let
    val P = read_prop "phi";
    val Q = read_prop "psi";
    val thm = implies_intr_list [P, P, Q] (Thm.assume Q);
  in store_standard_thm_open (Binding.make ("remdups_rl", @{here})) thm end;



(** embedded terms and types **)

local
  val A = certify (Free ("A", propT));
  val axiom = Thm.unvarify_axiom (Context.the_global_context ());
  val prop_def = axiom "Pure.prop_def";
  val term_def = axiom "Pure.term_def";
  val sort_constraint_def = axiom "Pure.sort_constraint_def";
  val C = Thm.lhs_of sort_constraint_def;
  val T = Thm.dest_arg C;
  val CA = mk_implies (C, A);
in

(* protect *)

val protect = Thm.apply (certify Logic.protectC);

val protectI =
  store_standard_thm (Binding.concealed (Binding.make ("protectI", @{here})))
    (Thm.equal_elim (Thm.symmetric prop_def) (Thm.assume A));

val protectD =
  store_standard_thm (Binding.concealed (Binding.make ("protectD", @{here})))
    (Thm.equal_elim prop_def (Thm.assume (protect A)));

val protect_cong =
  store_standard_thm_open (Binding.make ("protect_cong", @{here}))
    (Thm.reflexive (protect A));

fun implies_intr_protected asms th =
  let val asms' = map protect asms in
    implies_elim_list
      (implies_intr_list asms th)
      (map (fn asm' => Thm.assume asm' RS protectD) asms')
    |> implies_intr_list asms'
  end;


(* term *)

val termI =
  store_standard_thm (Binding.concealed (Binding.make ("termI", @{here})))
    (Thm.equal_elim (Thm.symmetric term_def) (Thm.forall_intr A (Thm.trivial A)));

fun mk_term ct =
  let
    val cT = Thm.ctyp_of_cterm ct;
    val T = Thm.typ_of cT;
  in Thm.instantiate ([((("'a", 0), []), cT)], [((("x", 0), T), ct)]) termI end;

fun dest_term th =
  let val cprop = strip_imp_concl (Thm.cprop_of th) in
    if can Logic.dest_term (Thm.term_of cprop) then
      Thm.dest_arg cprop
    else raise THM ("dest_term", 0, [th])
  end;

fun cterm_rule f = dest_term o f o mk_term;

val cterm_add_frees = Thm.add_frees o mk_term;
val cterm_add_vars = Thm.add_vars o mk_term;

val dummy_thm = mk_term (certify Term.dummy_prop);
val free_dummy_thm = Thm.tag_free_dummy dummy_thm;


(* sort_constraint *)

fun is_sort_constraint (Const ("Pure.sort_constraint", _) $ Const ("Pure.type", _)) = true
  | is_sort_constraint _ = false;

val sort_constraintI =
  store_standard_thm (Binding.concealed (Binding.make ("sort_constraintI", @{here})))
    (Thm.equal_elim (Thm.symmetric sort_constraint_def) (mk_term T));

val sort_constraint_eq =
  store_standard_thm (Binding.concealed (Binding.make ("sort_constraint_eq", @{here})))
    (Thm.equal_intr
      (Thm.implies_intr CA (Thm.implies_elim (Thm.assume CA)
        (Thm.unvarify_global (Context.the_global_context ()) sort_constraintI)))
      (implies_intr_list [A, C] (Thm.assume A)));

end;


(* HHF normalization *)

(* (PROP ?phi ==> (!!x. PROP ?psi x)) == (!!x. PROP ?phi ==> PROP ?psi x) *)
val norm_hhf_eq =
  let
    val aT = TFree ("'a", []);
    val x = Free ("x", aT);
    val phi = Free ("phi", propT);
    val psi = Free ("psi", aT --> propT);

    val cx = certify x;
    val cphi = certify phi;
    val lhs = certify (Logic.mk_implies (phi, Logic.all x (psi $ x)));
    val rhs = certify (Logic.all x (Logic.mk_implies (phi, psi $ x)));
  in
    Thm.equal_intr
      (Thm.implies_elim (Thm.assume lhs) (Thm.assume cphi)
        |> Thm.forall_elim cx
        |> Thm.implies_intr cphi
        |> Thm.forall_intr cx
        |> Thm.implies_intr lhs)
      (Thm.implies_elim
          (Thm.assume rhs |> Thm.forall_elim cx) (Thm.assume cphi)
        |> Thm.forall_intr cx
        |> Thm.implies_intr cphi
        |> Thm.implies_intr rhs)
    |> store_standard_thm_open (Binding.make ("norm_hhf_eq", @{here}))
  end;

val norm_hhf_prop = Logic.dest_equals (Thm.prop_of norm_hhf_eq);
val norm_hhf_eqs = [norm_hhf_eq, sort_constraint_eq];

fun is_norm_hhf (Const ("Pure.sort_constraint", _)) = false
  | is_norm_hhf (Const ("Pure.imp", _) $ _ $ (Const ("Pure.all", _) $ _)) = false
  | is_norm_hhf (Abs _ $ _) = false
  | is_norm_hhf (t $ u) = is_norm_hhf t andalso is_norm_hhf u
  | is_norm_hhf (Abs (_, _, t)) = is_norm_hhf t
  | is_norm_hhf _ = true;

fun norm_hhf thy t =
  if is_norm_hhf t then t
  else Pattern.rewrite_term thy [norm_hhf_prop] [] t;

fun norm_hhf_cterm ctxt raw_ct =
  let
    val thy = Proof_Context.theory_of ctxt;
    val ct = Thm.transfer_cterm thy raw_ct;
    val t = Thm.term_of ct;
  in if is_norm_hhf t then ct else Thm.cterm_of ctxt (norm_hhf thy t) end;


(* var indexes *)

fun incr_indexes th = Thm.incr_indexes (Thm.maxidx_of th + 1);

fun incr_indexes2 th1 th2 =
  Thm.incr_indexes (Int.max (Thm.maxidx_of th1, Thm.maxidx_of th2) + 1);

local

(*compose Q and [Q1,Q2,...,Qk]==>R to [Q2,...,Qk]==>R getting unique result*)
fun comp incremented th1 th2 =
  Thm.bicompose NONE {flatten = true, match = false, incremented = incremented}
    (false, th1, 0) 1 th2
  |> Seq.list_of |> distinct Thm.eq_thm
  |> (fn [th] => th | _ => raise THM ("COMP", 1, [th1, th2]));

in

fun th1 COMP th2 = comp false th1 th2;
fun th1 INCR_COMP th2 = comp true (incr_indexes th2 th1) th2;
fun th1 COMP_INCR th2 = comp true th1 (incr_indexes th1 th2);

end;

fun comp_no_flatten (th, n) i rule =
  (case distinct Thm.eq_thm (Seq.list_of
      (Thm.bicompose NONE {flatten = false, match = false, incremented = true}
        (false, th, n) i (incr_indexes th rule))) of
    [th'] => th'
  | [] => raise THM ("comp_no_flatten", i, [th, rule])
  | _ => raise THM ("comp_no_flatten: unique result expected", i, [th, rule]));



(** variations on Thm.instantiate **)

fun instantiate_normalize instpair th =
  Thm.adjust_maxidx_thm ~1 (Thm.instantiate instpair th COMP_INCR asm_rl);

(*instantiation with type-inference for variables*)
fun infer_instantiate_types _ [] th = th
  | infer_instantiate_types ctxt args raw_th =
      let
        val thy = Proof_Context.theory_of ctxt;
        val th = Thm.transfer thy raw_th;

        fun infer ((xi, T), cu) (tyenv, maxidx) =
          let
            val _ = Thm.ctyp_of ctxt T;
            val _ = Thm.transfer_cterm thy cu;
            val U = Thm.typ_of_cterm cu;
            val maxidx' = maxidx
              |> Integer.max (#2 xi)
              |> Term.maxidx_typ T
              |> Integer.max (Thm.maxidx_of_cterm cu);
            val (tyenv', maxidx'') = Sign.typ_unify thy (T, U) (tyenv, maxidx')
              handle Type.TUNIFY =>
                let
                  val t = Var (xi, T);
                  val u = Thm.term_of cu;
                in
                  raise THM ("infer_instantiate_types: type " ^
                    Syntax.string_of_typ ctxt (Envir.norm_type tyenv T) ^ " of variable " ^
                    Syntax.string_of_term ctxt (Term.map_types (Envir.norm_type tyenv) t) ^
                    "\ncannot be unified with type " ^
                    Syntax.string_of_typ ctxt (Envir.norm_type tyenv U) ^ " of term " ^
                    Syntax.string_of_term ctxt (Term.map_types (Envir.norm_type tyenv) u),
                    0, [th])
                end;
          in (tyenv', maxidx'') end;

        val (tyenv, _) = fold infer args (Vartab.empty, 0);
        val instT =
          Vartab.fold (fn (xi, (S, T)) =>
            cons ((xi, S), Thm.ctyp_of ctxt (Envir.norm_type tyenv T))) tyenv [];
        val inst = args |> map (fn ((xi, T), cu) =>
          ((xi, Envir.norm_type tyenv T),
            Thm.instantiate_cterm (instT, []) (Thm.transfer_cterm thy cu)));
      in instantiate_normalize (instT, inst) th end
      handle CTERM (msg, _) => raise THM (msg, 0, [raw_th])
        | TERM (msg, _) => raise THM (msg, 0, [raw_th])
        | TYPE (msg, _, _) => raise THM (msg, 0, [raw_th]);

fun infer_instantiate _ [] th = th
  | infer_instantiate ctxt args th =
      let
        val vars = Term.add_vars (Thm.full_prop_of th) [];
        val dups = duplicates (eq_fst op =) vars;
        val _ = null dups orelse
          raise THM ("infer_instantiate: inconsistent types for variables " ^
            commas_quote (map (Syntax.string_of_term (Config.put show_types true ctxt) o Var) dups),
            0, [th]);
        val args' = args |> map_filter (fn (xi, cu) =>
          AList.lookup (op =) vars xi |> Option.map (fn T => ((xi, T), cu)));
      in infer_instantiate_types ctxt args' th end;

fun infer_instantiate' ctxt args th =
  let
    val vars = rev (Term.add_vars (Thm.full_prop_of th) []);
    val args' = zip_options vars args
      handle ListPair.UnequalLengths =>
        raise THM ("infer_instantiate': more instantiations than variables in thm", 0, [th]);
  in infer_instantiate_types ctxt args' th end;



(** renaming of bound variables **)

(* replace bound variables x_i in thm by y_i *)
(* where vs = [(x_1, y_1), ..., (x_n, y_n)]  *)

fun rename_bvars [] thm = thm
  | rename_bvars vs thm =
      let
        fun rename (Abs (x, T, t)) = Abs (AList.lookup (op =) vs x |> the_default x, T, rename t)
          | rename (t $ u) = rename t $ rename u
          | rename a = a;
      in Thm.renamed_prop (rename (Thm.prop_of thm)) thm end;


(* renaming in left-to-right order *)

fun rename_bvars' xs thm =
  let
    fun rename [] t = ([], t)
      | rename (x' :: xs) (Abs (x, T, t)) =
          let val (xs', t') = rename xs t
          in (xs', Abs (the_default x x', T, t')) end
      | rename xs (t $ u) =
          let
            val (xs', t') = rename xs t;
            val (xs'', u') = rename xs' u;
          in (xs'', t' $ u') end
      | rename xs a = (xs, a);
  in
    (case rename xs (Thm.prop_of thm) of
      ([], prop') => Thm.renamed_prop prop' thm
    | _ => error "More names than abstractions in theorem")
  end;

end;

structure Basic_Drule: BASIC_DRULE = Drule;
open Basic_Drule;
