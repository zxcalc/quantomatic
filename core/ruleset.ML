signature RULESET =
sig
    include SSTR_NAMES
    structure Rule : RULE;
    type T = ((Rule.T * bool) Rule.Name.NTab.T)
    (* a ruleset, keeping track of what rules are active, etc. *)

    (* (T * bool) is a ruleset and a flag for "active" *)
    val get_active_rules : (T * bool) NTab.T -> ((Rule.Name.name * Rule.T) list)
    val get_active_rulesets : (T * bool) NTab.T -> name list
    val get_all_rulesets : (T * bool) NTab.T -> name list
    
    (*val get_rules_in_ruleset : T NTab.T -> name -> (Rule.Name.name * Rule.T * bool) list*)
    val apply_rules_in : (Rule.Name.name * Rule.T) list -> Rule.RGGraph.BBox.NSet.T -> RGGraph.T -> (Rule.Name.name * Rule.T) Seq.seq
    val apply_a_rule_in : (Rule.Name.name * Rule.T) list -> NSet.T ->
                          RGGraph.T -> (Rule.Name.name * Rule.T) Seq.seq

    val activate_rule : Rule.Name.name -> T -> T
    val deactivate_rule : Rule.Name.name -> T -> T
    val delete_rule : Rule.Name.name -> T -> T

    val lookup_rule : T -> Rule.Name.name -> Rule.T option
    val list_of_rules : T -> Rule.Name.name list
end

(* at some point, a ruleset should be a richer structure,
   but this will do for now. Its just a rule list and a flag
   for "active". *)
structure DB_RuleSet =
struct

open SStrName
val default_name = mk "new-ruleset-1"
structure Rule = Rule


(* at some point, a ruleset should be a richer structure,
   but this will do for now. Its just a rule list and a flag
   for "active". 
   The rule list has a boolean to show if  the rule is active *)
type T = ((Rule.T * bool) Rule.Name.NTab.T)

fun active_rule_filter rules = List.map 
                                 (fn (n,(r,_)) => (n,r))
                                 (List.filter 
                                    (fn (_,(_,active)) => active) rules);


fun onoff_rule onoff name rules =
  rules |> Rule.Name.NTab.map_entry (fn (r,_) => (r,onoff)) name

    
(* these all raise UNDEF on failure *)
fun activate_rule name rules = onoff_rule true name rules
fun deactivate_rule name rules = onoff_rule false name rules
fun delete_rule name rules = rules |> Rule.Name.NTab.delete name


(* apply rules to whole graph *)
fun apply_rules rules g = 
    Seq.append 
      (Seq.map (fn r => (mk "spider", r)) 
               (Spider.mk_max_spider_rule g))
      (Seq.maps
         (fn (n,r) => Seq.map (fn r2 => (n,r2)) (Rule.rule_matches r g))
         (Seq.of_list rules));

(* apply rules to select vertices (vset) *)
fun apply_rules_in rules vset g = 
    Seq.append 
      (Seq.map (fn r => (mk "spider", r)) 
               (Spider.mk_max_spider_rule_within vset g))
      (Seq.maps
         (fn (n,r) => Seq.map (fn r2 => (n,r2)) 
                              (Rule.rule_matches_within r vset g))
         (Seq.of_list rules));

(* try to match and apply one rule *)
fun apply_a_rule_in rules vset g = 
    case Seq.pull (apply_rules_in rules vset g) 
     of NONE => Seq.empty
      | SOME (h,_) => Seq.single h


(* get a list of all the rules in activated rulesets *)
fun get_active_rules thys =
    NTab.fold (fn (_, (rules,active)) =>
               fn l => if active then
                         (active_rule_filter (Rule.Name.NTab.list_of rules)) @ l
                       else l) thys []

(* get a list of all the activated rulesets *)    
fun get_active_rulesets rsets =
    NTab.fold (fn (nm, (_,active)) =>
               fn l => if active then nm::l
                       else l) rsets []

val get_all_rulesets = NTab.keys

(*fun get_rules_in_ruleset rsets rset_name = fst (NTab.get rsets rset_name)*)

fun lookup_rule rset rule_name = case (NTab.lookup rset rule_name) of
                                   SOME (a,b) => SOME a | NONE => NONE
                                   
val list_of_rules = Rule.Name.NTab.keys

end



structure RuleSet = DB_RuleSet : RULESET

