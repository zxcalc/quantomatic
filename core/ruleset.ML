signature RULESET =
sig
    include SSTR_NAMES
    structure Rule : RULE;
    type T
    val get_active_rules : T NTab.T -> ((Rule.Name.name * Rule.T) list)
    val get_active_rulesets : T NTab.T -> name list
    val get_all_rulesets : T NTab.T -> name list
    val get_rules_in_ruleset : T NTab.T -> name -> (Rule.Name.name * Rule.T * bool) list
    val apply_rules_in : (Rule.Name.name * Rule.T) list -> BBox.NSet.T ->
                         RGGraph.T -> (Rule.Name.name * Rule.T) Seq.seq
    val apply_a_rule_in : (Rule.Name.name * Rule.T) list -> NSet.T ->
                          RGGraph.T -> (Rule.Name.name * Rule.T) Seq.seq

    val activate_rule :  ((Rule.Name.name * Rule.T * bool) list) -> Rule.Name.name 
                         -> ((Rule.Name.name * Rule.T * bool) list)
    val deactivate_rule :  ((Rule.Name.name * Rule.T * bool) list) -> Rule.Name.name 
                         -> ((Rule.Name.name * Rule.T * bool) list)
    val delete_rule :   ((Rule.Name.name * Rule.T * bool) list) -> Rule.Name.name 
                         -> ((Rule.Name.name * Rule.T * bool) list)

    (* TODO : this should be a Rule name type rather than string *)
    val lookup_rule : T -> Rule.Name.name -> Rule.T option
end

(* at some point, a ruleset should be a richer structure,
   but this will do for now. Its just a rule list and a flag
   for "active". *)
structure DB_RuleSet =
struct

open SStrName
val default_name = mk "New Ruleset"
structure Rule = Rule


(* at some point, a ruleset should be a richer structure,
   but this will do for now. Its just a rule list and a flag
   for "active". 
   The rule list has a boolean to show if  the rule is active *)
type T = ((Rule.Name.name * Rule.T * bool) list) * bool

fun active_rule_filter rules = List.map 
                                 (fn (n,r,_) => (n,r))
                                 (List.filter 
                                    (fn (_,_,active) => active) rules);

(* assumes that there is only one rule with the same name in each ruleset *)
fun onoff_rule onoff rules name = 
    let fun f (name, done, []) = List.rev done
          | f (name, done, (n,r,b) :: rest) = 
            if name_eq (n,name) 
            then List.revAppend(done, ((n,r,onoff) :: rest))
            else f (name, ((n,r,b) :: done), rest)
    in
      f (name, [], rules)
    end
        
fun activate_rule rules name = onoff_rule true rules name
fun deactivate_rule rules name = onoff_rule false rules name

fun delete_rule rules name = filter (fn (n,_,_) => name_eq (name,n)) rules

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
               fn l => if active then (active_rule_filter rules) @ l
                       else l) thys []

(* get a list of all the activated rulesets *)    
fun get_active_rulesets thys =
    NTab.fold (fn (nm, (_,active)) =>
               fn l => if active then nm::l
                       else l) thys []

val get_all_rulesets = NTab.keys

fun get_rules_in_ruleset thys thy_name = fst (NTab.get thys thy_name)

fun lookup_rule thy rule_name = 
    case List.find (fn (k,_,_) => (Rule.Name.name_eq(k, rule_name))) (#1 thy)
     of SOME (_,v,_) => SOME v
      | NONE => NONE

end



structure RuleSet = DB_RuleSet : RULESET

