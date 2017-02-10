(*  Title:      Pure/Isar/rule_cases.ML
    Author:     Markus Wenzel, TU Muenchen

Annotations and local contexts of rules.
*)

signature RULE_CASES =
sig
  datatype T = Case of
   {fixes: (binding * typ) list,
    assumes: (string * term list) list,
    binds: (indexname * term option) list,
    cases: (string * T) list}
  type cases = (string * T option) list
  val case_conclN: string
  val case_hypsN: string
  val case_premsN: string
  val strip_params: term -> (string * typ) list
  type info = ((string * string list) * string list) list
  val make_common: Proof.context -> term -> info -> cases
  val make_nested: Proof.context -> term -> term -> info -> cases
  val apply: term list -> T -> T
  val consume: Proof.context -> thm list -> thm list -> ('a * int) * thm ->
    (('a * (int * thm list)) * thm) Seq.seq
  val get_consumes: thm -> int
  val put_consumes: int option -> thm -> thm
  val add_consumes: int -> thm -> thm
  val default_consumes: int -> thm -> thm
  val consumes: int -> attribute
  val get_constraints: thm -> int
  val constraints: int -> attribute
  val name: string list -> thm -> thm
  val case_names: string list -> attribute
  val cases_hyp_names: string list -> string list list -> attribute
  val case_conclusion: string * string list -> attribute
  val is_inner_rule: thm -> bool
  val inner_rule: attribute
  val is_cases_open: thm -> bool
  val cases_open: attribute
  val internalize_params: thm -> thm
  val save: thm -> thm -> thm
  val get: thm -> info * int
  val rename_params: string list list -> thm -> thm
  val params: string list list -> attribute
  val mutual_rule: Proof.context -> thm list -> (int list * thm) option
  val strict_mutual_rule: Proof.context -> thm list -> int list * thm
end;

structure Rule_Cases: RULE_CASES =
struct

(** cases **)

datatype T = Case of
 {fixes: (binding * typ) list,
  assumes: (string * term list) list,
  binds: (indexname * term option) list,
  cases: (string * T) list};

type cases = (string * T option) list;

val case_conclN = "case";
val case_hypsN = "hyps";
val case_premsN = "prems";

val strip_params = map (apfst (perhaps (try Name.dest_skolem))) o Logic.strip_params;

type info = ((string * string list) * string list) list;

local

fun app us t = Term.betapplys (t, us);

fun dest_binops cs tm =
  let
    val n = length cs;
    fun dest 0 _ = []
      | dest 1 t = [t]
      | dest k (_ $ t $ u) = t :: dest (k - 1) u
      | dest _ _ = raise TERM ("Expected " ^ string_of_int n ^ " binop arguments", [tm]);
  in cs ~~ dest n tm end;

fun extract_fixes NONE prop = (strip_params prop, [])
  | extract_fixes (SOME outline) prop =
      chop (length (Logic.strip_params outline)) (strip_params prop);

fun extract_assumes _ NONE prop = ([("", Logic.strip_assums_hyp prop)], [])
  | extract_assumes hyp_names (SOME outline) prop =
      let
        val (hyps, prems) =
          chop (length (Logic.strip_assums_hyp outline)) (Logic.strip_assums_hyp prop)
        fun extract ((hyp_name, hyp) :: rest) = (hyp_name, hyp :: map snd rest);
        val (hyps1, hyps2) = chop (length hyp_names) hyps;
        val pairs1 = if null hyps1 then [] else hyp_names ~~ hyps1;
        val pairs = pairs1 @ map (pair case_hypsN) hyps2;
        val named_hyps = map extract (partition_eq (eq_fst op =) pairs);
      in (named_hyps, [(case_premsN, prems)]) end;

fun bindings args = map (apfst Binding.name) args;

fun extract_case ctxt (case_outline, raw_prop) hyp_names concls =
  let
    val props = Logic.dest_conjunctions (Drule.norm_hhf (Proof_Context.theory_of ctxt) raw_prop);
    val len = length props;
    val nested = is_some case_outline andalso len > 1;

    fun extract prop =
      let
        val (fixes1, fixes2) = extract_fixes case_outline prop;
        val abs_fixes = fold_rev Term.abs (fixes1 @ fixes2);
        fun abs_fixes1 t =
          if not nested then abs_fixes t
          else
            fold_rev Term.abs fixes1
              (app (map (Term.dummy_pattern o #2) fixes2) (fold_rev Term.abs fixes2 t));
        val (assumes1, assumes2) =
          extract_assumes hyp_names case_outline prop
          |> apply2 (map (apsnd (maps Logic.dest_conjunctions)));

        val concl = Object_Logic.drop_judgment ctxt (Logic.strip_assums_concl prop);
        val binds =
          (case_conclN, concl) :: dest_binops concls concl
          |> map (fn (x, t) => ((x, 0), SOME (abs_fixes t)));
      in
       ((fixes1, map (apsnd (map abs_fixes1)) assumes1),
        ((fixes2, map (apsnd (map abs_fixes)) assumes2), binds))
      end;

    val cases = map extract props;

    fun common_case ((fixes1, assumes1), ((fixes2, assumes2), binds)) =
      Case {fixes = bindings (fixes1 @ fixes2),
        assumes = assumes1 @ assumes2, binds = binds, cases = []};
    fun inner_case (_, ((fixes2, assumes2), binds)) =
      Case {fixes = bindings fixes2, assumes = assumes2, binds = binds, cases = []};
    fun nested_case ((fixes1, assumes1), _) =
      Case {fixes = bindings fixes1, assumes = assumes1, binds = [],
        cases = map string_of_int (1 upto len) ~~ map inner_case cases};
  in
    if len = 0 then NONE
    else if len = 1 then SOME (common_case (hd cases))
    else if is_none case_outline orelse length (distinct (op =) (map fst cases)) > 1 then NONE
    else SOME (nested_case (hd cases))
  end;

fun make ctxt rule_struct prop cases =
  let
    val n = length cases;
    val nprems = Logic.count_prems prop;
    fun add_case ((name, hyp_names), concls) (cs, i) =
      ((case try (fn () =>
          (Option.map (curry Logic.nth_prem i) rule_struct, Logic.nth_prem (i, prop))) () of
        NONE => (name, NONE)
      | SOME p => (name, extract_case ctxt p hyp_names concls)) :: cs, i - 1);
  in fold_rev add_case (drop (Int.max (n - nprems, 0)) cases) ([], n) |> #1 end;

in

fun make_common ctxt = make ctxt NONE;
fun make_nested ctxt rule_struct = make ctxt (SOME rule_struct);

fun apply args =
  let
    fun appl (Case {fixes, assumes, binds, cases}) =
      let
        val assumes' = map (apsnd (map (app args))) assumes;
        val binds' = map (apsnd (Option.map (app args))) binds;
        val cases' = map (apsnd appl) cases;
      in Case {fixes = fixes, assumes = assumes', binds = binds', cases = cases'} end;
  in appl end;

end;



(** annotations and operations on rules **)

(* consume facts *)

local

fun unfold_prems ctxt n defs th =
  if null defs then th
  else Conv.fconv_rule (Conv.prems_conv n (Raw_Simplifier.rewrite ctxt true defs)) th;

fun unfold_prems_concls ctxt defs th =
  if null defs orelse not (can Logic.dest_conjunction (Thm.concl_of th)) then th
  else
    Conv.fconv_rule
      (Conv.concl_conv ~1 (Conjunction.convs
        (Conv.prems_conv ~1 (Raw_Simplifier.rewrite ctxt true defs)))) th;

in

fun consume ctxt defs facts ((a, n), th) =
  let val m = Int.min (length facts, n) in
    th
    |> unfold_prems ctxt n defs
    |> unfold_prems_concls ctxt defs
    |> Drule.multi_resolve (SOME ctxt) (take m facts)
    |> Seq.map (pair (a, (n - m, drop m facts)))
  end;

end;

val consumes_tagN = "consumes";

fun lookup_consumes th =
  (case AList.lookup (op =) (Thm.get_tags th) consumes_tagN of
    NONE => NONE
  | SOME s =>
      (case Lexicon.read_nat s of
        SOME n => SOME n
      | _ => raise THM ("Malformed 'consumes' tag of theorem", 0, [th])));

fun get_consumes th = the_default 0 (lookup_consumes th);

fun put_consumes NONE th = th
  | put_consumes (SOME n) th = th
      |> Thm.untag_rule consumes_tagN
      |> Thm.tag_rule (consumes_tagN, string_of_int (if n < 0 then Thm.nprems_of th + n else n));

fun add_consumes k th = put_consumes (SOME (k + get_consumes th)) th;

fun default_consumes n th =
  if is_some (lookup_consumes th) then th else put_consumes (SOME n) th;

val save_consumes = put_consumes o lookup_consumes;

fun consumes n = Thm.mixed_attribute (apsnd (put_consumes (SOME n)));


(* equality constraints *)

val constraints_tagN = "constraints";

fun lookup_constraints th =
  (case AList.lookup (op =) (Thm.get_tags th) constraints_tagN of
    NONE => NONE
  | SOME s =>
      (case Lexicon.read_nat s of
        SOME n => SOME n
      | _ => raise THM ("Malformed 'constraints' tag of theorem", 0, [th])));

fun get_constraints th = the_default 0 (lookup_constraints th);

fun put_constraints NONE th = th
  | put_constraints (SOME n) th = th
      |> Thm.untag_rule constraints_tagN
      |> Thm.tag_rule (constraints_tagN, string_of_int (if n < 0 then 0 else n));

val save_constraints = put_constraints o lookup_constraints;

fun constraints n = Thm.mixed_attribute (apsnd (put_constraints (SOME n)));


(* case names *)

val implode_args = space_implode ";";
val explode_args = space_explode ";";

val case_names_tagN = "case_names";

fun add_case_names NONE = I
  | add_case_names (SOME names) =
      Thm.untag_rule case_names_tagN
      #> Thm.tag_rule (case_names_tagN, implode_args names);

fun lookup_case_names th =
  AList.lookup (op =) (Thm.get_tags th) case_names_tagN
  |> Option.map explode_args;

val save_case_names = add_case_names o lookup_case_names;
val name = add_case_names o SOME;
fun case_names cs = Thm.mixed_attribute (apsnd (name cs));


(* hyp names *)

val implode_hyps = implode_args o map (suffix "," o space_implode ",");
val explode_hyps = map (space_explode "," o unsuffix ",") o explode_args;

val cases_hyp_names_tagN = "cases_hyp_names";

fun add_cases_hyp_names NONE = I
  | add_cases_hyp_names (SOME namess) =
      Thm.untag_rule cases_hyp_names_tagN
      #> Thm.tag_rule (cases_hyp_names_tagN, implode_hyps namess);

fun lookup_cases_hyp_names th =
  AList.lookup (op =) (Thm.get_tags th) cases_hyp_names_tagN
  |> Option.map explode_hyps;

val save_cases_hyp_names = add_cases_hyp_names o lookup_cases_hyp_names;
fun cases_hyp_names cs hs =
  Thm.mixed_attribute (apsnd (name cs #> add_cases_hyp_names (SOME hs)));


(* case conclusions *)

val case_concl_tagN = "case_conclusion";

fun get_case_concl name (a, b) =
  if a = case_concl_tagN then
    (case explode_args b of
      c :: cs => if c = name then SOME cs else NONE
    | [] => NONE)
  else NONE;

fun add_case_concl (name, cs) = Thm.map_tags (fn tags =>
  filter_out (is_some o get_case_concl name) tags @
    [(case_concl_tagN, implode_args (name :: cs))]);

fun get_case_concls th name =
  these (get_first (get_case_concl name) (Thm.get_tags th));

fun save_case_concls th =
  let val concls = Thm.get_tags th |> map_filter
    (fn (a, b) =>
      if a = case_concl_tagN
      then (case explode_args b of c :: cs => SOME (c, cs) | _ => NONE)
      else NONE)
  in fold add_case_concl concls end;

fun case_conclusion concl = Thm.mixed_attribute (apsnd (add_case_concl concl));


(* inner rule *)

val inner_rule_tagN = "inner_rule";

fun is_inner_rule th =
  AList.defined (op =) (Thm.get_tags th) inner_rule_tagN;

fun put_inner_rule inner =
  Thm.untag_rule inner_rule_tagN
  #> inner ? Thm.tag_rule (inner_rule_tagN, "");

val save_inner_rule = put_inner_rule o is_inner_rule;

val inner_rule = Thm.mixed_attribute (apsnd (put_inner_rule true));


(* parameter names *)

val cases_open_tagN = "cases_open";

fun is_cases_open th =
  AList.defined (op =) (Thm.get_tags th) cases_open_tagN;

fun put_cases_open cases_open =
  Thm.untag_rule cases_open_tagN
  #> cases_open ? Thm.tag_rule (cases_open_tagN, "");

val save_cases_open = put_cases_open o is_cases_open;

val cases_open = Thm.mixed_attribute (apsnd (put_cases_open true));

fun internalize_params rule =
  if is_cases_open rule then rule
  else
    let
      fun rename prem =
        let val xs =
          map (Name.internal o Name.clean o fst) (Logic.strip_params prem)
        in Logic.list_rename_params xs prem end;
      fun rename_prems prop =
        let val (As, C) = Logic.strip_horn prop
        in Logic.list_implies (map rename As, C) end;
    in Thm.renamed_prop (rename_prems (Thm.prop_of rule)) rule end;



(** case declarations **)

(* access hints *)

fun save th =
  save_consumes th #>
  save_constraints th #>
  save_case_names th #>
  save_cases_hyp_names th #>
  save_case_concls th #>
  save_inner_rule th #>
  save_cases_open th;

fun get th =
  let
    val n = get_consumes th;
    val cases =
      (case lookup_case_names th of
        NONE => map (rpair [] o Library.string_of_int) (1 upto (Thm.nprems_of th - n))
      | SOME names => map (fn name => (name, get_case_concls th name)) names);
    val cases_hyps =
      (case lookup_cases_hyp_names th of
        NONE => replicate (length cases) []
      | SOME names => names);
    fun regroup (name, concls) hyps = ((name, hyps), concls);
  in (map2 regroup cases cases_hyps, n) end;


(* params *)

fun rename_params xss th =
  th
  |> fold_index (fn (i, xs) => Thm.rename_params_rule (xs, i + 1)) xss
  |> save th;

fun params xss = Thm.rule_attribute [] (K (rename_params xss));



(** mutual_rule **)

local

fun equal_cterms ts us =
  is_equal (list_ord (Term_Ord.fast_term_ord o apply2 Thm.term_of) (ts, us));

fun prep_rule n th =
  let
    val th' = Thm.permute_prems 0 n th;
    val prems = take (Thm.nprems_of th' - n) (Drule.cprems_of th');
    val th'' = Drule.implies_elim_list th' (map Thm.assume prems);
  in (prems, (n, th'')) end;

in

fun mutual_rule _ [] = NONE
  | mutual_rule _ [th] = SOME ([0], th)
  | mutual_rule ctxt (ths as th :: _) =
      let
        val ((_, ths'), ctxt') = Variable.import true ths ctxt;
        val rules as (prems, _) :: _ = map (prep_rule (get_consumes th)) ths';
        val (ns, rls) = split_list (map #2 rules);
      in
        if not (forall (equal_cterms prems o #1) rules) then NONE
        else
          SOME (ns,
            rls
            |> Conjunction.intr_balanced
            |> Drule.implies_intr_list prems
            |> singleton (Variable.export ctxt' ctxt)
            |> save th
            |> put_consumes (SOME 0))
      end;

end;

fun strict_mutual_rule ctxt ths =
  (case mutual_rule ctxt ths of
    NONE => error "Failed to join given rules into one mutual rule"
  | SOME res => res);

end;
