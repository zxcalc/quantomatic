signature THEORY =
sig
    include SSTR_NAMES
    structure Rule : RULE;
    type T
    val get_active_rules : T NTab.T -> ((string * Rule.T) list)
    val get_active_theories : T NTab.T -> name list
    val get_all_theories : T NTab.T -> name list
    val get_rules_in_theory : T NTab.T -> name -> (string * Rule.T) list
    val apply_rules_in : (string * Rule.T) list -> Rule.RGGraph.BBox.NSet.T ->
                         RGGraph.T -> (string * Rule.T) Seq.seq
    val apply_a_rule_in : (string * Rule.T) list -> NSet.T ->
                          RGGraph.T -> (string * Rule.T) Seq.seq
    (* TODO : this should be a Rule name type rather than string *)
    val lookup_rule : T -> string -> Rule.T option
end

(* at some point, a theory should be a richer structure,
   but this will do for now. Its just a rule list and a flag
   for "active". *)
structure DB_Theory =
struct

open SStrName
val default_name = mk "Ta"
structure Rule = Rule

(* at some point, a theory should be a richer structure,
   but this will do for now. Its just a rule list and a flag
   for "active". *)
type T = ((string * Rule.T) list) * bool


(* apply rules to whole graph *)
fun apply_rules rules g = 
    Seq.append 
      (Seq.map (fn r => ("spider", r)) 
               (Spider.mk_max_spider_rule g))
      (Seq.maps
         (fn (n,r) => Seq.map (fn r2 => (n,r2)) (Rule.rule_matches r g))
         (Seq.of_list rules));

(* apply rules to select vertices (vset) *)
fun apply_rules_in rules vset g = 
    Seq.append 
      (Seq.map (fn r => ("spider", r)) 
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


(* get a list of all the rules in activated theories *)
fun get_active_rules thys =
    NTab.fold (fn (_, (rules,active)) =>
               fn l => if active then rules @ l
                       else l) thys []

(* get a list of all the activated theories *)    
fun get_active_theories thys =
    NTab.fold (fn (nm, (_,active)) =>
               fn l => if active then nm::l
                       else l) thys []

val get_all_theories = NTab.keys

fun get_rules_in_theory thys thy_name = fst (NTab.get thys thy_name)

fun lookup_rule thy rule_name = case List.find (fn (k,_) => (k=rule_name)) (#1 thy)
                              of SOME (_,v) => SOME v
                               | NONE => NONE

end



structure Theory = DB_Theory : THEORY

