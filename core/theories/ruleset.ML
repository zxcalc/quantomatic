(* global name structure for tags *)
structure TagName : SSTR_NAMES = 
    struct open SStrName; val default_name = mk "new-ruleset-1"; end;

(* binary relation between tags and rule names *)
structure TagRel : NAME_BINREL = SStrBRel;

(* sharing types for RuleSets *)
signature RULESET_SHARING =
sig
  structure Rule : RULE_SHARING
  type T
end

(* *)
signature RULESET =
sig
  structure Rule : BASIC_RULE;

  type T (* a set of named rules, which can be taged, and which have 
            an active subset *)

  val empty : T

  (* sharing structure *)
  structure Sharing : RULESET_SHARING
    sharing Sharing.Rule = Rule.Sharing
    sharing type Sharing.T = T

  exception no_such_rule_exp of RuleName.name;

  (* basic getting of info about rulesets *)
  val get_allrules : T -> Rule.T RuleName.NTab.T 
  val get_all_rule_names_list : T -> RuleName.name list
  val get_tags : T -> TagName.NSet.T 
  val get_rules_in_tag : T -> TagName.name -> RuleName.NSet.T
  val get_active : T -> RuleName.NSet.T 
  val get_tagrel : T -> TagRel.T
  val get_rule : T -> RuleName.name -> Rule.T 
  val lookup_rule : T -> RuleName.name -> Rule.T option

  val set_brel : TagRel.T -> T -> T 
  val set_allrules : Rule.T RuleName.NTab.T -> T -> T 
  val set_active : RuleName.NSet.T -> T -> T 

  val is_activate : T -> RuleName.name -> bool
  val rule_exists : T -> RuleName.name -> bool
  val tag_exists : T -> TagName.name -> bool

  (* manipulating the ruleset *)
  val merge : T -> T -> T (* rules in first replace those in second with same name *)

  val add_fresh_rule : RuleName.name * Rule.T -> T -> RuleName.name * T (* fresh name for rule *)
  val update_rule : RuleName.name * Rule.T -> T -> T (* replaces/inserts *)
  val tag_rule : RuleName.name -> TagName.name -> T -> T
  val untag_rule : RuleName.name -> TagName.name -> T -> T
  
  val activate_rule : RuleName.name -> T -> T
  val deactivate_rule : RuleName.name -> T -> T
  val delete_rule : RuleName.name -> T -> T

  val activate_tag : TagName.name -> T -> T
  val deactivate_tag : TagName.name -> T -> T
  val delete_tag : TagName.name -> T -> T

  val delete_rules_in_tag : TagName.name -> T -> T

end; (* signature *)


(* at some point, a ruleset should be a richer structure,
   but this will do for now. Its just a rule list and a flag
   for "active". *)
structure DB_RGRuleSet =
struct
  
  structure Rule = RGRule
  
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
        brel : TagRel.T,
        active : RuleName.NSet.T };
  
  val empty = RuleSet {
        allrules = RuleName.NTab.empty,
        brel = TagRel.empty,
        active = RuleName.NSet.empty
      };
  
  fun get_allrules (RuleSet rep) = #allrules rep;
  fun get_active (RuleSet rep) = #active rep;
  fun get_tagrel (RuleSet rep) = #brel rep;
  
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
    structure Rule = Rule.Sharing
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
      
  fun get_all_rule_names_list rset = RuleName.NTab.keys (get_allrules rset);
  fun get_tags rset = TagRel.get_codset (get_tagrel rset);
  
  fun get_rules_in_tag rset tagname = TagRel.codf (get_tagrel rset) tagname;
  
  fun tag_exists rset tagname = 
      TagName.NSet.contains (TagRel.get_codset (get_tagrel rset)) tagname;
  
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
           |> update_brel (TagRel.del_dom name)
           |> update_allrules (RuleName.NTab.delete name);
  
  
  fun activate_tag tagname rset = 
      RuleName.NSet.fold activate_rule (get_rules_in_tag rset tagname) rset;
  fun deactivate_tag tagname rset = 
      RuleName.NSet.fold deactivate_rule (get_rules_in_tag rset tagname) rset;
  fun delete_tag tagname rset = 
      RuleName.NSet.fold delete_rule (get_rules_in_tag rset tagname) rset;
  
  
  fun update_rule (name, rule) rset = 
      rset |> update_allrules (RuleName.NTab.update (name,rule));
  
  fun add_fresh_rule (name, rule) rset = 
      let val rset = delete_rule name rset (* avoid renaming, clober names *)
          val (name',allrules') = RuleName.NTab.add (name,rule) (get_allrules rset) 
      in (name', rset |> set_allrules allrules') end;
  
  (* add all rules from rset1 to rset2, replaces elements in rset2 on duplicates, 
     unions tags contents. *)
  fun merge rset1 rset2 = 
      rset2 
        |> update_allrules (fn rtab2 => RuleName.NTab.merge_joint 
                                          (K true) rtab2 (get_allrules rset1))
        |> update_brel (TagRel.union_merge (get_tagrel rset1));
  
  fun tag_rule rname tagname rset =
      if rule_exists rset rname then 
         rset |> update_brel (TagRel.add1 rname tagname)
      else raise no_such_rule_exp rname;
  
  fun untag_rule rname tagname rset =
      if rule_exists rset rname then 
         rset |> update_brel (TagRel.del1 rname tagname)
      else raise no_such_rule_exp rname;
  
  fun delete_rules_in_tag tagname rset = 
      rset |> RuleName.NSet.fold delete_rule (get_rules_in_tag rset tagname);

end; (* structure DB_RGRuleSet *)


structure RGRuleSet = DB_RGRuleSet : RULESET

