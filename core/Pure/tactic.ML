(*  Title:      Pure/tactic.ML
    Author:     Lawrence C Paulson, Cambridge University Computer Laboratory

Fundamental tactics.
*)

signature BASIC_TACTIC =
sig
  val trace_goalno_tac: (int -> tactic) -> int -> tactic
  val rule_by_tactic: Proof.context -> tactic -> thm -> thm
  val assume_tac: Proof.context -> int -> tactic
  val eq_assume_tac: int -> tactic
  val compose_tac: Proof.context -> (bool * thm * int) -> int -> tactic
  val make_elim: thm -> thm
  val biresolve0_tac: (bool * thm) list -> int -> tactic
  val biresolve_tac: Proof.context -> (bool * thm) list -> int -> tactic
  val resolve0_tac: thm list -> int -> tactic
  val resolve_tac: Proof.context -> thm list -> int -> tactic
  val eresolve0_tac: thm list -> int -> tactic
  val eresolve_tac: Proof.context -> thm list -> int -> tactic
  val forward0_tac: thm list -> int -> tactic
  val forward_tac: Proof.context -> thm list -> int -> tactic
  val dresolve0_tac: thm list -> int -> tactic
  val dresolve_tac: Proof.context -> thm list -> int -> tactic
  val atac: int -> tactic
  val rtac: thm -> int -> tactic
  val dtac: thm -> int -> tactic
  val etac: thm -> int -> tactic
  val ftac: thm -> int -> tactic
  val ares_tac: thm list -> int -> tactic
  val solve_tac: Proof.context -> thm list -> int -> tactic
  val bimatch_tac: Proof.context -> (bool * thm) list -> int -> tactic
  val match_tac: Proof.context -> thm list -> int -> tactic
  val ematch_tac: Proof.context -> thm list -> int -> tactic
  val dmatch_tac: Proof.context -> thm list -> int -> tactic
  val flexflex_tac: Proof.context -> tactic
  val distinct_subgoal_tac: int -> tactic
  val distinct_subgoals_tac: tactic
  val cut_tac: thm -> int -> tactic
  val cut_rules_tac: thm list -> int -> tactic
  val cut_facts_tac: thm list -> int -> tactic
  val filter_thms: (term * term -> bool) -> int * term * thm list -> thm list
  val biresolution_from_nets_tac: Proof.context ->
    ('a list -> (bool * thm) list) -> bool -> 'a Net.net * 'a Net.net -> int -> tactic
  val biresolve_from_nets_tac: Proof.context ->
    (int * (bool * thm)) Net.net * (int * (bool * thm)) Net.net -> int -> tactic
  val bimatch_from_nets_tac: Proof.context ->
    (int * (bool * thm)) Net.net * (int * (bool * thm)) Net.net -> int -> tactic
  val filt_resolve_from_net_tac: Proof.context -> int -> (int * thm) Net.net -> int -> tactic
  val resolve_from_net_tac: Proof.context -> (int * thm) Net.net -> int -> tactic
  val match_from_net_tac: Proof.context -> (int * thm) Net.net -> int -> tactic
  val subgoals_of_brl: bool * thm -> int
  val lessb: (bool * thm) * (bool * thm) -> bool
  val rename_tac: string list -> int -> tactic
  val rotate_tac: int -> int -> tactic
  val defer_tac: int -> tactic
  val prefer_tac: int -> tactic
  val filter_prems_tac: Proof.context -> (term -> bool) -> int -> tactic
end;

signature TACTIC =
sig
  include BASIC_TACTIC
  val insert_tagged_brl: 'a * (bool * thm) ->
    ('a * (bool * thm)) Net.net * ('a * (bool * thm)) Net.net ->
      ('a * (bool * thm)) Net.net * ('a * (bool * thm)) Net.net
  val delete_tagged_brl: bool * thm ->
    ('a * (bool * thm)) Net.net * ('a * (bool * thm)) Net.net ->
      ('a * (bool * thm)) Net.net * ('a * (bool * thm)) Net.net
  val eq_kbrl: ('a * (bool * thm)) * ('a * (bool * thm)) -> bool
  val build_net: thm list -> (int * thm) Net.net
end;

structure Tactic: TACTIC =
struct

(*Discover which goal is chosen:  SOMEGOAL(trace_goalno_tac tac) *)
fun trace_goalno_tac tac i st =
    case Seq.pull(tac i st) of
        NONE    => Seq.empty
      | seqcell => (tracing ("Subgoal " ^ string_of_int i ^ " selected");
                         Seq.make(fn()=> seqcell));

(*Makes a rule by applying a tactic to an existing rule*)
fun rule_by_tactic ctxt tac rl =
  let
    val thy = Proof_Context.theory_of ctxt;
    val ctxt' = Variable.declare_thm rl ctxt;
    val ((_, [st]), ctxt'') = Variable.import true [Thm.transfer thy rl] ctxt';
  in
    (case Seq.pull (tac st) of
      NONE => raise THM ("rule_by_tactic", 0, [rl])
    | SOME (st', _) => zero_var_indexes (singleton (Variable.export ctxt'' ctxt') st'))
  end;


(*** Basic tactics ***)

(*** The following fail if the goal number is out of range:
     thus (REPEAT (resolve_tac rules i)) stops once subgoal i disappears. *)

(*Solve subgoal i by assumption*)
fun assume_tac ctxt i = PRIMSEQ (Thm.assumption (SOME ctxt) i);
fun atac i = PRIMSEQ (Thm.assumption NONE i);

(*Solve subgoal i by assumption, using no unification*)
fun eq_assume_tac i = PRIMITIVE (Thm.eq_assumption i);


(** Resolution/matching tactics **)

(*The composition rule/state: no lifting or var renaming.
  The arg = (bires_flg, orule, m);  see Thm.bicompose for explanation.*)
fun compose_tac ctxt arg i =
  PRIMSEQ (Thm.bicompose (SOME ctxt) {flatten = true, match = false, incremented = false} arg i);

(*Converts a "destruct" rule like P&Q==>P to an "elimination" rule
  like [| P&Q; P==>R |] ==> R *)
fun make_elim rl = zero_var_indexes (rl RS revcut_rl);

(*Attack subgoal i by resolution, using flags to indicate elimination rules*)
fun biresolve0_tac brules i = PRIMSEQ (Thm.biresolution NONE false brules i);
fun biresolve_tac ctxt brules i = PRIMSEQ (Thm.biresolution (SOME ctxt) false brules i);

(*Resolution: the simple case, works for introduction rules*)
fun resolve0_tac rules = biresolve0_tac (map (pair false) rules);
fun resolve_tac ctxt rules = biresolve_tac ctxt (map (pair false) rules);

(*Resolution with elimination rules only*)
fun eresolve0_tac rules = biresolve0_tac (map (pair true) rules);
fun eresolve_tac ctxt rules = biresolve_tac ctxt (map (pair true) rules);

(*Forward reasoning using destruction rules.*)
fun forward0_tac rls = resolve0_tac (map make_elim rls) THEN' atac;
fun forward_tac ctxt rls = resolve_tac ctxt (map make_elim rls) THEN' atac;

(*Like forward_tac, but deletes the assumption after use.*)
fun dresolve0_tac rls = eresolve0_tac (map make_elim rls);
fun dresolve_tac ctxt rls = eresolve_tac ctxt (map make_elim rls);

(*Shorthand versions: for resolution with a single theorem*)
fun rtac rl =  resolve0_tac [rl];
fun dtac rl = dresolve0_tac [rl];
fun etac rl = eresolve0_tac [rl];
fun ftac rl =  forward0_tac [rl];

(*Use an assumption or some rules ... A popular combination!*)
fun ares_tac rules = atac  ORELSE'  resolve0_tac rules;

fun solve_tac ctxt rules = resolve_tac ctxt rules THEN_ALL_NEW assume_tac ctxt;

(*Matching tactics -- as above, but forbid updating of state*)
fun bimatch_tac ctxt brules i = PRIMSEQ (Thm.biresolution (SOME ctxt) true brules i);
fun match_tac ctxt rules = bimatch_tac ctxt (map (pair false) rules);
fun ematch_tac ctxt rules = bimatch_tac ctxt (map (pair true) rules);
fun dmatch_tac ctxt rls = ematch_tac ctxt (map make_elim rls);

(*Smash all flex-flex disagreement pairs in the proof state.*)
fun flexflex_tac ctxt = PRIMSEQ (Thm.flexflex_rule (SOME ctxt));

(*Remove duplicate subgoals.*)
val permute_tac = PRIMITIVE oo Thm.permute_prems;
fun distinct_tac (i, k) =
  permute_tac 0 (i - 1) THEN
  permute_tac 1 (k - 1) THEN
  PRIMITIVE (fn st => Drule.comp_no_flatten (st, 0) 1 Drule.distinct_prems_rl) THEN
  permute_tac 1 (1 - k) THEN
  permute_tac 0 (1 - i);

fun distinct_subgoal_tac i st =
  (case drop (i - 1) (Thm.prems_of st) of
    [] => no_tac st
  | A :: Bs =>
      st |> EVERY (fold (fn (B, k) =>
        if A aconv B then cons (distinct_tac (i, k)) else I) (Bs ~~ (1 upto length Bs)) []));

fun distinct_subgoals_tac state =
  let
    val goals = Thm.prems_of state;
    val dups = distinct (eq_fst (op aconv)) (goals ~~ (1 upto length goals));
  in EVERY (rev (map (distinct_subgoal_tac o snd) dups)) state end;


(*** Applications of cut_rl ***)

(*The conclusion of the rule gets assumed in subgoal i,
  while subgoal i+1,... are the premises of the rule.*)
fun cut_tac rule i = resolve0_tac [cut_rl] i THEN resolve0_tac [rule] (i + 1);

(*"Cut" a list of rules into the goal.  Their premises will become new
  subgoals.*)
fun cut_rules_tac ths i = EVERY (map (fn th => cut_tac th i) ths);

(*As above, but inserts only facts (unconditional theorems);
  generates no additional subgoals. *)
fun cut_facts_tac ths = cut_rules_tac (filter Thm.no_prems ths);


(**** Indexing and filtering of theorems ****)

(*Returns the list of potentially resolvable theorems for the goal "prem",
        using the predicate  could(subgoal,concl).
  Resulting list is no longer than "limit"*)
fun filter_thms could (limit, prem, ths) =
  let val pb = Logic.strip_assums_concl prem;   (*delete assumptions*)
      fun filtr (limit, []) = []
        | filtr (limit, th::ths) =
            if limit=0 then  []
            else if could(pb, Thm.concl_of th)  then th :: filtr(limit-1, ths)
            else filtr(limit,ths)
  in  filtr(limit,ths)  end;


(*** biresolution and resolution using nets ***)

(** To preserve the order of the rules, tag them with increasing integers **)

(*insert one tagged brl into the pair of nets*)
fun insert_tagged_brl (kbrl as (k, (eres, th))) (inet, enet) =
  if eres then
    (case try Thm.major_prem_of th of
      SOME prem => (inet, Net.insert_term (K false) (prem, kbrl) enet)
    | NONE => error "insert_tagged_brl: elimination rule with no premises")
  else (Net.insert_term (K false) (Thm.concl_of th, kbrl) inet, enet);

(*delete one kbrl from the pair of nets*)
fun eq_kbrl ((_, (_, th)), (_, (_, th'))) = Thm.eq_thm_prop (th, th')

fun delete_tagged_brl (brl as (eres, th)) (inet, enet) =
  (if eres then
    (case try Thm.major_prem_of th of
      SOME prem => (inet, Net.delete_term eq_kbrl (prem, ((), brl)) enet)
    | NONE => (inet, enet))  (*no major premise: ignore*)
  else (Net.delete_term eq_kbrl (Thm.concl_of th, ((), brl)) inet, enet))
  handle Net.DELETE => (inet,enet);


(*biresolution using a pair of nets rather than rules.
    function "order" must sort and possibly filter the list of brls.
    boolean "match" indicates matching or unification.*)
fun biresolution_from_nets_tac ctxt order match (inet, enet) =
  SUBGOAL
    (fn (prem, i) =>
      let
        val hyps = Logic.strip_assums_hyp prem;
        val concl = Logic.strip_assums_concl prem;
        val kbrls = Net.unify_term inet concl @ maps (Net.unify_term enet) hyps;
      in PRIMSEQ (Thm.biresolution (SOME ctxt) match (order kbrls) i) end);

(*versions taking pre-built nets.  No filtering of brls*)
fun biresolve_from_nets_tac ctxt = biresolution_from_nets_tac ctxt order_list false;
fun bimatch_from_nets_tac ctxt = biresolution_from_nets_tac ctxt order_list true;


(*** Simpler version for resolve_tac -- only one net, and no hyps ***)

(*insert one tagged rl into the net*)
fun insert_krl (krl as (k,th)) =
  Net.insert_term (K false) (Thm.concl_of th, krl);

(*build a net of rules for resolution*)
fun build_net rls =
  fold_rev insert_krl (tag_list 1 rls) Net.empty;

(*resolution using a net rather than rules; pred supports filt_resolve_tac*)
fun filt_resolution_from_net_tac ctxt match pred net =
  SUBGOAL (fn (prem, i) =>
    let val krls = Net.unify_term net (Logic.strip_assums_concl prem) in
      if pred krls then
        PRIMSEQ (Thm.biresolution (SOME ctxt) match (map (pair false) (order_list krls)) i)
      else no_tac
    end);

(*Resolve the subgoal using the rules (making a net) unless too flexible,
   which means more than maxr rules are unifiable.      *)
fun filt_resolve_from_net_tac ctxt maxr net =
  let fun pred krls = length krls <= maxr
  in filt_resolution_from_net_tac ctxt false pred net end;

(*versions taking pre-built nets*)
fun resolve_from_net_tac ctxt = filt_resolution_from_net_tac ctxt false (K true);
fun match_from_net_tac ctxt = filt_resolution_from_net_tac ctxt true (K true);


(*** For Natural Deduction using (bires_flg, rule) pairs ***)

(*The number of new subgoals produced by the brule*)
fun subgoals_of_brl (true, rule) = Thm.nprems_of rule - 1
  | subgoals_of_brl (false, rule) = Thm.nprems_of rule;

(*Less-than test: for sorting to minimize number of new subgoals*)
fun lessb (brl1,brl2) = subgoals_of_brl brl1 < subgoals_of_brl brl2;


(*Renaming of parameters in a subgoal*)
fun rename_tac xs i =
  case find_first (not o Symbol_Pos.is_identifier) xs of
      SOME x => error ("Not an identifier: " ^ x)
    | NONE => PRIMITIVE (Thm.rename_params_rule (xs, i));

(*rotate_tac n i: rotate the assumptions of subgoal i by n positions, from
  right to left if n is positive, and from left to right if n is negative.*)
fun rotate_tac 0 i = all_tac
  | rotate_tac k i = PRIMITIVE (Thm.rotate_rule k i);

(*Rotate the given subgoal to be the last.*)
fun defer_tac i = PRIMITIVE (Thm.permute_prems (i - 1) 1);

(*Rotate the given subgoal to be the first.*)
fun prefer_tac i = PRIMITIVE (Thm.permute_prems (i - 1) 1 #> Thm.permute_prems 0 ~1);

(*Remove premises that do not satisfy pred; fails if all prems satisfy pred.*)
fun filter_prems_tac ctxt pred =
  let
    fun Then NONE tac = SOME tac
      | Then (SOME tac) tac' = SOME (tac THEN' tac');
    fun thins H (tac, n) =
      if pred H then (tac, n + 1)
      else (Then tac (rotate_tac n THEN' eresolve_tac ctxt [thin_rl]), 0);
  in
    SUBGOAL (fn (goal, i) =>
      let val Hs = Logic.strip_assums_hyp goal in
        (case fst (fold thins Hs (NONE, 0)) of
          NONE => no_tac
        | SOME tac => tac i)
      end)
  end;

end;

structure Basic_Tactic: BASIC_TACTIC = Tactic;
open Basic_Tactic;
