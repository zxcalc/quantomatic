(* *)

signature RULESET_SHARING =
sig
  structure SubsetName : NAMES_SHARING
  structure RuleName : NAMES_SHARING
  structure Rule : RULE_SHARING
  structure InBRel : NAME_BINREL_SHARING
  sharing InBRel.Dom = RuleName
  sharing InBRel.Cod = SubsetName
  type T
end


signature RULESET =
sig
    structure Rule : BASIC_RULE;
    structure SubsetName : SSTR_NAMES; (* name for a subset of rules *)
    structure RuleName : SSTR_NAMES; (* names of rules *)
    structure InBRel : NAME_BINREL; (* name for a subset of rules *)

    type T (* a set of named rules, with subsets that can be activated *)

    val empty : T

    (* sharing structure *)
    structure Sharing : RULESET_SHARING
      sharing Sharing.SubsetName = SubsetName.Sharing
      sharing Sharing.RuleName = RuleName.Sharing
      sharing Sharing.Rule = Rule.Sharing
      sharing Sharing.InBRel = InBRel.Sharing
      sharing type Sharing.T = T 
    
    val get_allrules : T -> Rule.T RuleName.NTab.T 
    val get_active : T -> RuleName.NSet.T 
    val get_brel : T -> InBRel.T
    
    exception no_such_rule_exp of RuleName.name;

    val applicable_rules : T -> Rule.Graph.T 
          -> ((RuleName.name * Rule.T) * Rule.Graph.T) Seq.seq

    val is_activate : T -> RuleName.name -> bool
    val rule_exists : T -> RuleName.name -> bool

    val activate_rule : RuleName.name -> T -> T
    val deactivate_rule : RuleName.name -> T -> T
    val delete_rule : RuleName.name -> T -> T

    val activate_ruleset : SubsetName.name -> T -> T
    val deactivate_ruleset : SubsetName.name -> T -> T
    val delete_ruleset : SubsetName.name -> T -> T

    val add_rule : RuleName.name * Rule.T -> T -> RuleName.name * T
    val add_rule_to_subset : RuleName.name -> SubsetName.name -> T -> T
    val del_rule_from_subset : RuleName.name -> SubsetName.name -> T -> T

    val lookup_rule : T -> RuleName.name -> Rule.T option
    val get_rule_names_list : T -> RuleName.name list


end


(* at some point, a ruleset should be a richer structure,
   but this will do for now. Its just a rule list and a flag
   for "active". *)
structure DB_RGRuleSet =
struct

structure Rule = RGRule
structure SubsetName : SSTR_NAMES = 
  struct open SStrName; val default_name = mk "new-ruleset-1"; end;
structure InBRel = SStrBRel;
structure RuleName = RuleName; (* FIXME? functorise *)

structure G = Rule.Graph;
structure BBox = G.BBox;
structure V = G.OVertex;
structure E = G.Edge;

exception no_such_rule_exp of RuleName.name;
exception duplicate_rule_exp of RuleName.name;

(* at some point, a ruleset should be a richer structure,
   but this will do for now. Its just a rule list and a flag
   for "active". 
   The rule list has a boolean to show if  the rule is active *)
datatype T = 
  RuleSet of
    { allrules : Rule.T RuleName.NTab.T,
      brel : InBRel.T,
      active : RuleName.NSet.T };

val empty = RuleSet {
      allrules = RuleName.NTab.empty,
      brel = InBRel.empty,
      active = RuleName.NSet.empty
    };

fun get_allrules (RuleSet rep) = #allrules rep;
fun get_active (RuleSet rep) = #active rep;
fun get_brel (RuleSet rep) = #brel rep;

fun update_allrules f (RuleSet rep) = 
    RuleSet {allrules = f (#allrules rep), active = #active rep, 
             brel = #brel rep };
fun update_active f (RuleSet rep) =
    RuleSet {allrules = #allrules rep, active = f (#active rep), 
             brel = #brel rep };
fun update_brel f (RuleSet rep) = 
    RuleSet {allrules = #allrules rep, active = #active rep, 
             brel = f (#brel rep) };

val set_allrules = update_allrules o K;
val set_active = update_active o K;
val set_brel = update_brel o K;


structure Sharing = struct
  structure SubsetName = SubsetName.Sharing
  structure Rule = Rule.Sharing
  structure RuleName = RuleName.Sharing
  structure InBRel = InBRel.Sharing 
  type T = T;
end

fun rule_exists rset rname = 
    RuleName.NTab.contains (get_allrules rset) rname; 

fun is_activate rset rname =  
    RuleName.NSet.contains (get_active rset) rname; 

fun lookup_rule rset rule_name = 
    RuleName.NTab.lookup (get_allrules rset) rule_name; 

fun get_rule rset rule_name =
    RuleName.NTab.get (get_allrules rset) rule_name; 
    
fun get_rule_names_list rset = RuleName.NTab.keys (get_allrules rset);
fun get_subset_names_list rset = InBRel.get_codset (get_brel rset);

fun get_subset rset subsetname = InBRel.codf (get_brel rset) subsetname;

(* *)
fun set_rule_activation activate name rset =
    if rule_exists rset name then 
      rset |> update_active ((if activate then RuleName.NSet.add
                             else RuleName.NSet.delete) name)
    else raise no_such_rule_exp name;

(* these all raise UNDEF on failure *)
val activate_rule = set_rule_activation true;
val deactivate_rule = set_rule_activation false;
fun delete_rule name rset = 
    rset |> update_active (RuleName.NSet.delete name)
         |> update_brel (InBRel.del_dom name)
         |> update_allrules (RuleName.NTab.delete name);


fun activate_ruleset subsetname rset = 
    RuleName.NSet.fold activate_rule (get_subset rset subsetname) rset;
fun deactivate_ruleset subsetname rset = 
    RuleName.NSet.fold deactivate_rule (get_subset rset subsetname) rset;
fun delete_ruleset subsetname rset = 
    RuleName.NSet.fold delete_rule (get_subset rset subsetname) rset;

fun add_rule (name, rule) rset = 
    let val rset = delete_rule name rset (* avoid renaming, clober names *)
        val (name',allrules') = RuleName.NTab.add (name,rule) (get_allrules rset) 
    in (name', rset |> set_allrules allrules') end;

fun add_rule_to_subset rname subsetname rset =
    if rule_exists rset rname then 
       rset |> update_brel (InBRel.add1 rname subsetname)
    else raise no_such_rule_exp rname;

fun del_rule_from_subset rname subsetname rset =
    if rule_exists rset rname then 
       rset |> update_brel (InBRel.del1 rname subsetname)
    else raise no_such_rule_exp rname;


fun rule_matches r g =
    let
      val lhs = Rule.get_lhs r;
      val gs = [lhs, g]
      val names = 
        (fold G.add_to_vnames gs V.NSet.empty,
         fold G.add_to_enames gs E.NSet.empty,
         fold G.add_to_bbnames gs BBox.NSet.empty)
    in SimpleMatchSearch.match names lhs g end;
      
fun rule_matches_within vset r g = 
    let 
      val lhs = Rule.get_lhs r;
      val gs = [lhs, g]
      val names = 
        (fold G.add_to_vnames gs V.NSet.empty,
         fold G.add_to_enames gs E.NSet.empty,
         fold G.add_to_bbnames gs BBox.NSet.empty)
    in SimpleMatchSearch.match names lhs 
        (G.matched_subgraph_within_vset vset g) 
    end;

(* instantiate rule with a match *)
fun instantiate_rule m r = 
    Rule.mk (Match.inst_pat_graph m (Rule.get_lhs r), 
             Match.inst_replacement_graph m (Rule.get_rhs r));

(* apply rules to whole graph *)
(* IMPROVE: provide a single name-space for all rules, and thus avoid 
   multiple renamings. *)
fun applicable_rules' matcher rset g = 
    RuleName.NSet.fold 
    (fn n =>
        let val r = get_rule rset n
        in Seq.append 
          (Seq.map 
            (fn m => 
             ((n,instantiate_rule m r),
              GraphSubst.rewrite g (Rule.get_lhs r) m (Rule.get_rhs r))) 
             (matcher r g))
         end)
     (get_active rset)
     Seq.empty;

val applicable_rules = applicable_rules' rule_matches;
fun applicable_rules_in rset vset = 
    applicable_rules' (rule_matches_within vset) rset;

(* apply just first rule that can be applied *)
fun apply_first_rule rules g = 
    case Seq.pull (applicable_rules rules g) 
     of NONE => NONE
      | SOME (h,_) => SOME h;
fun apply_first_rule_in rules vset g = 
    case Seq.pull (applicable_rules_in rules vset g) 
     of NONE => NONE
      | SOME (h,_) => SOME h;

end



structure RGRuleSet = DB_RGRuleSet : RULESET

