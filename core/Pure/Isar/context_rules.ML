(*  Title:      Pure/Isar/context_rules.ML
    Author:     Stefan Berghofer and Markus Wenzel, TU Muenchen

Declarations of intro/elim/dest rules in Pure (see also
Provers/classical.ML for a more specialized version of the same idea).
*)

signature CONTEXT_RULES =
sig
  type netpair = ((int * int) * (bool * thm)) Net.net * ((int * int) * (bool * thm)) Net.net
  val netpair_bang: Proof.context -> netpair
  val netpair: Proof.context -> netpair
  val orderlist: ((int * int) * 'a) list -> 'a list
  val find_rules_netpair: bool -> thm list -> term -> netpair -> thm list
  val find_rules: bool -> thm list -> term -> Proof.context -> thm list list
  val print_rules: Proof.context -> unit
  val addSWrapper: (Proof.context -> (int -> tactic) -> int -> tactic) -> theory -> theory
  val addWrapper: (Proof.context -> (int -> tactic) -> int -> tactic) -> theory -> theory
  val Swrap: Proof.context -> (int -> tactic) -> int -> tactic
  val wrap: Proof.context -> (int -> tactic) -> int -> tactic
  val intro_bang: int option -> attribute
  val elim_bang: int option -> attribute
  val dest_bang: int option -> attribute
  val intro: int option -> attribute
  val elim: int option -> attribute
  val dest: int option -> attribute
  val intro_query: int option -> attribute
  val elim_query: int option -> attribute
  val dest_query: int option -> attribute
  val rule_del: attribute
  val add: (int option -> attribute) -> (int option -> attribute) -> (int option -> attribute) ->
    attribute context_parser
end;

structure Context_Rules: CONTEXT_RULES =
struct


(** rule declaration contexts **)

(* rule kinds *)

val intro_bangK = (0, false);
val elim_bangK = (0, true);
val introK = (1, false);
val elimK = (1, true);
val intro_queryK = (2, false);
val elim_queryK = (2, true);

val kind_names =
 [(intro_bangK, "safe introduction rules (intro!)"),
  (elim_bangK, "safe elimination rules (elim!)"),
  (introK, "introduction rules (intro)"),
  (elimK, "elimination rules (elim)"),
  (intro_queryK, "extra introduction rules (intro?)"),
  (elim_queryK, "extra elimination rules (elim?)")];

val rule_kinds = map #1 kind_names;
val rule_indexes = distinct (op =) (map #1 rule_kinds);


(* context data *)

type netpair = ((int * int) * (bool * thm)) Net.net * ((int * int) * (bool * thm)) Net.net;
val empty_netpairs: netpair list = replicate (length rule_indexes) (Net.empty, Net.empty);

datatype rules = Rules of
 {next: int,
  rules: (int * ((int * bool) * thm)) list,
  netpairs: netpair list,
  wrappers:
    ((Proof.context -> (int -> tactic) -> int -> tactic) * stamp) list *
    ((Proof.context -> (int -> tactic) -> int -> tactic) * stamp) list};

fun make_rules next rules netpairs wrappers =
  Rules {next = next, rules = rules, netpairs = netpairs, wrappers = wrappers};

fun add_rule (i, b) opt_w th (Rules {next, rules, netpairs, wrappers}) =
  let val w = (case opt_w of SOME w => w | NONE => Tactic.subgoals_of_brl (b, th)) in
    make_rules (next - 1) ((w, ((i, b), th)) :: rules)
      (nth_map i (Tactic.insert_tagged_brl ((w, next), (b, th))) netpairs) wrappers
  end;

fun del_rule0 th (rs as Rules {next, rules, netpairs, wrappers}) =
  let
    fun eq_th (_, (_, th')) = Thm.eq_thm_prop (th, th');
    fun del b netpair = Tactic.delete_tagged_brl (b, th) netpair handle Net.DELETE => netpair;
  in
    if not (exists eq_th rules) then rs
    else make_rules next (filter_out eq_th rules) (map (del false o del true) netpairs) wrappers
  end;

fun del_rule th = del_rule0 th o del_rule0 (Tactic.make_elim th);

structure Rules = Generic_Data
(
  type T = rules;
  val empty = make_rules ~1 [] empty_netpairs ([], []);
  val extend = I;
  fun merge
    (Rules {rules = rules1, wrappers = (ws1, ws1'), ...},
      Rules {rules = rules2, wrappers = (ws2, ws2'), ...}) =
    let
      val wrappers =
        (Library.merge (eq_snd (op =)) (ws1, ws2), Library.merge (eq_snd (op =)) (ws1', ws2'));
      val rules = Library.merge (fn ((_, (k1, th1)), (_, (k2, th2))) =>
          k1 = k2 andalso Thm.eq_thm_prop (th1, th2)) (rules1, rules2);
      val next = ~ (length rules);
      val netpairs = fold (fn (n, (w, ((i, b), th))) =>
          nth_map i (Tactic.insert_tagged_brl ((w, n), (b, th))))
        (next upto ~1 ~~ rules) empty_netpairs;
    in make_rules (next - 1) rules netpairs wrappers end;
);

fun print_rules ctxt =
  let
    val Rules {rules, ...} = Rules.get (Context.Proof ctxt);
    fun prt_kind (i, b) =
      Pretty.big_list ((the o AList.lookup (op =) kind_names) (i, b) ^ ":")
        (map_filter (fn (_, (k, th)) =>
            if k = (i, b) then SOME (Display.pretty_thm_item ctxt th) else NONE)
          (sort (int_ord o apply2 fst) rules));
  in Pretty.writeln_chunks (map prt_kind rule_kinds) end;


(* access data *)

fun netpairs ctxt = let val Rules {netpairs, ...} = Rules.get (Context.Proof ctxt) in netpairs end;
val netpair_bang = hd o netpairs;
val netpair = hd o tl o netpairs;


(* retrieving rules *)

fun untaglist [] = []
  | untaglist [(_ : int * int, x)] = [x]
  | untaglist ((k, x) :: (rest as (k', _) :: _)) =
      if k = k' then untaglist rest
      else x :: untaglist rest;

fun orderlist brls =
  untaglist (sort (prod_ord int_ord int_ord o apply2 fst) brls);

fun orderlist_no_weight brls =
  untaglist (sort (int_ord o apply2 (snd o fst)) brls);

fun may_unify weighted t net =
  map snd ((if weighted then orderlist else orderlist_no_weight) (Net.unify_term net t));

fun find_erules _ [] = K []
  | find_erules w (fact :: _) = may_unify w (Logic.strip_assums_concl (Thm.prop_of fact));

fun find_irules w goal = may_unify w (Logic.strip_assums_concl goal);

fun find_rules_netpair weighted facts goal (inet, enet) =
  find_erules weighted facts enet @ find_irules weighted goal inet;

fun find_rules weighted facts goals =
  map (find_rules_netpair weighted facts goals) o netpairs;


(* wrappers *)

fun gen_add_wrapper upd w =
  Context.theory_map (Rules.map (fn Rules {next, rules, netpairs, wrappers} =>
    make_rules next rules netpairs (upd (fn ws => (w, stamp ()) :: ws) wrappers)));

val addSWrapper = gen_add_wrapper Library.apfst;
val addWrapper = gen_add_wrapper Library.apsnd;


fun gen_wrap which ctxt =
  let val Rules {wrappers, ...} = Rules.get (Context.Proof ctxt)
  in fold_rev (fn (w, _) => w ctxt) (which wrappers) end;

val Swrap = gen_wrap #1;
val wrap = gen_wrap #2;



(** attributes **)

(* add and del rules *)


val rule_del = Thm.declaration_attribute (fn th => Rules.map (del_rule th));

fun rule_add k view opt_w =
  Thm.declaration_attribute (fn th => Rules.map (add_rule k opt_w (view th) o del_rule th));

val intro_bang  = rule_add intro_bangK I;
val elim_bang   = rule_add elim_bangK I;
val dest_bang   = rule_add elim_bangK Tactic.make_elim;
val intro       = rule_add introK I;
val elim        = rule_add elimK I;
val dest        = rule_add elimK Tactic.make_elim;
val intro_query = rule_add intro_queryK I;
val elim_query  = rule_add elim_queryK I;
val dest_query  = rule_add elim_queryK Tactic.make_elim;

val _ = Theory.setup
  (snd o Global_Theory.add_thms [((Binding.empty, Drule.equal_intr_rule), [intro_query NONE])]);


(* concrete syntax *)

fun add a b c x =
  (Scan.lift ((Args.bang >> K a || Args.query >> K c || Scan.succeed b) --
    Scan.option Parse.nat) >> (fn (f, n) => f n)) x;

val _ = Theory.setup
 (Attrib.setup @{binding intro} (add intro_bang intro intro_query)
    "declaration of introduction rule" #>
  Attrib.setup @{binding elim} (add elim_bang elim elim_query)
    "declaration of elimination rule" #>
  Attrib.setup @{binding dest} (add dest_bang dest dest_query)
    "declaration of destruction rule" #>
  Attrib.setup @{binding rule} (Scan.lift Args.del >> K rule_del)
    "remove declaration of intro/elim/dest rule");

end;
