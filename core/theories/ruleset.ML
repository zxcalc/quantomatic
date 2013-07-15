(* sharing types for Rulesets *)
signature RULESET_SHARING =
sig
  structure Rule : OGRAPH_RULE_SHARING
  type T
end

(* *)
signature RULESET =
sig
  structure Rule : OGRAPH_RULE

  type T (* a set of named rules, which can be taged, and which have 
            an active subset *)

  val empty : T

  (* sharing structure *)
  structure Sharing : RULESET_SHARING
    sharing Sharing.Rule = Rule.Sharing
    sharing type Sharing.T = T

  exception no_such_rule_exp of R.name
  exception rname_already_exists_exp of R.name

  (* basic getting of info about rulesets *)
  val get_allrules : T -> Rule.T RTab.T 
  val get_all_rule_names_list : T -> R.name list
  val get_tags : T -> TagName.NSet.T 
  val get_rules_in_tag : T -> TagName.name -> R.NSet.T
  val get_active : T -> R.NSet.T 
  val get_tagrel : T -> RTagRel.T
  val get_rule : T -> R.name -> Rule.T 
  val get_rule_opt : T -> R.name -> Rule.T option

  val set_brel : RTagRel.T -> T -> T 
  val set_allrules : Rule.T RTab.T -> T -> T 
  val set_active : R.NSet.T -> T -> T 

  val is_activate : T -> R.name -> bool
  val rule_exists : T -> R.name -> bool
  val tag_exists : T -> TagName.name -> bool

  (* manipulating the ruleset *)
  val merge : T -> T -> T (* rules in first replace those in second with same name *)

  val add_fresh_rule : Rule.T -> T -> R.name * T (* fresh name for rule *)
  val update_rule : R.name * Rule.T -> T -> T (* replaces/inserts *)
  val tag_rule : R.name -> TagName.name -> T -> T
  val untag_rule : R.name -> TagName.name -> T -> T
  
  val activate_rule : R.name -> T -> T
  val deactivate_rule : R.name -> T -> T
  val delete_rule : R.name -> T -> T
  val rename_rule : R.name -> R.name -> T -> T
  val rename_rules : RSub.T -> T -> T

  val activate_tag : TagName.name -> T -> T
  val deactivate_tag : TagName.name -> T -> T
  val delete_tag : TagName.name -> T -> T

  val delete_rules_in_tag : TagName.name -> T -> T

end (* signature *)

signature BANG_GRAPH_RULESET =
sig
  include RULESET
  structure BangGraphRule : BANG_GRAPH_RULE
  sharing BangGraphRule.Sharing = Rule.Sharing
end (* signature *)


(* a Ruleset is map from rule names to rule, 
   a subset of active rulenames, and a binary relation between rulenames 
   and tagnames. *)
functor Ruleset(Rule : OGRAPH_RULE) : RULESET =
struct
  
  structure Rule = Rule
  structure G = Rule.Graph
  
  exception no_such_rule_exp of R.name
  exception rname_already_exists_exp of R.name
  
  (* at some point, a ruleset should be a richer structure,
     but this will do for now. Its just a rule list and a flag
     for "active". 
     The rule list has a boolean to show if  the rule is active *)
  datatype T = 
    Ruleset of
      { allrules : Rule.T RTab.T,
        brel : RTagRel.T,
        active : R.NSet.T }
  
  val empty = Ruleset {
        allrules = RTab.empty,
        brel = RTagRel.empty,
        active = R.NSet.empty
      }
  
  fun get_allrules (Ruleset rep) = #allrules rep
  fun get_active (Ruleset rep) = #active rep
  fun get_tagrel (Ruleset rep) = #brel rep
  
  fun update_allrules f (Ruleset rep) = 
      Ruleset {allrules = f (#allrules rep), active = #active rep, 
               brel = #brel rep }
  fun update_active f (Ruleset rep) =
      Ruleset {allrules = #allrules rep, active = f (#active rep), 
               brel = #brel rep }
  fun update_brel f (Ruleset rep) = 
      Ruleset {allrules = #allrules rep, active = #active rep, 
               brel = f (#brel rep) }
  
  val set_allrules = update_allrules o K
  val set_active = update_active o K
  val set_brel = update_brel o K
  
  
  structure Sharing = struct
    structure Rule = Rule.Sharing
    type T = T
  end
  
  fun rule_exists rset rname = 
      RTab.dom_contains (get_allrules rset) rname 
  
  fun is_activate rset rname =  
      R.NSet.contains (get_active rset) rname 
  
  fun get_rule_opt rset rule_name = 
      RTab.get_opt (get_allrules rset) rule_name 
  
  fun get_rule rset rule_name =
      RTab.get (get_allrules rset) rule_name 
      
  fun get_all_rule_names_list rset = R.NSet.list_of (RTab.get_dom_set (get_allrules rset))
  fun get_tags rset = RTagRel.get_cod_set (get_tagrel rset)
  
  fun get_rules_in_tag rset tagname = RTagRel.inv_img (get_tagrel rset) tagname
  
  fun tag_exists rset tagname = 
      TagName.NSet.contains (RTagRel.get_cod_set (get_tagrel rset)) tagname
  
  (* *)
  fun set_rule_activation activate name rset =
      if rule_exists rset name then 
        rset |> update_active ((if activate then R.NSet.add
                               else R.NSet.delete) name)
      else raise no_such_rule_exp name
  
  (* these all raise UNDEF on failure *)
  val activate_rule = set_rule_activation true
  val deactivate_rule = set_rule_activation false
  fun delete_rule name rset = 
      rset |> update_active (R.NSet.delete name)
           |> update_brel (RTagRel.delete name)
           |> update_allrules (RTab.delete name)


  exception invalid_state_exp of unit

fun rename_rules rsub rset = let
  val rrnm = rsub |> RSub.extend_fixed (RTab.get_dom_set (get_allrules rset))
  val rrnmi = RSub.inverse_of rrnm
in rset |> update_allrules (fn m => RTab.compose (m, rrnmi))
        |> update_brel (fn m => RTagRel.compose (m, rrnmi))
        |> update_active (RSub.img_of_set rrnm)
end

fun rename_rule old new = rename_rules (RSub.empty |> RSub.add (old,new))

(*  fun rename_rule old new rset = 
      let
        val rules = (get_allrules rset)
        val rule = case RTab.lookup rules old
                     of NONE => raise no_such_rule_exp old
                      | SOME r => r
        val rules2 =
            case (rules |> RTab.delete old
                        |> RTab.try_ins (new,rule))
              of NONE => raise rname_already_exists_exp new
               | SOME r => r
        val brel = case RTagRel.try_rename1_dom old new (get_tagrel rset)
                     of NONE => raise invalid_state_exp ()
                      | SOME b => b
        val active =
            case (R.NSet.try_rename1 old new (get_active rset))
              of NONE => raise invalid_state_exp ()
               | SOME r => r
      in
        rset |> set_active active
             |> set_brel brel
             |> set_allrules rules2
      end*)
  
  
  fun activate_tag tagname rset = 
      R.NSet.fold activate_rule (get_rules_in_tag rset tagname) rset
  fun deactivate_tag tagname rset = 
      R.NSet.fold deactivate_rule (get_rules_in_tag rset tagname) rset
  fun delete_tag tagname rset = 
      R.NSet.fold delete_rule (get_rules_in_tag rset tagname) rset
  
  
  fun update_rule (name, rule) rset = 
      rset |> update_allrules (RTab.set (name,rule))
  

  fun add_fresh_rule rule rset = 
    let
      val (name,allrules) = RTab.store rule (get_allrules rset) 
    in (name, rset |> set_allrules allrules)
    end
  
  
  (* add all rules from rset1 to rset2, replaces elements in rset2 on duplicates, 
     unions tags contents and unions active set. *)
  fun merge rset1 rset2 = 
      rset2 
        |> update_allrules (RTab.fold RTab.set (get_allrules rset1))
        |> update_brel (RTagRel.fold RTagRel.add (get_tagrel rset1))
        |> update_active (R.NSet.union_merge (get_active rset1))
  
  fun tag_rule rname tagname rset =
      if rule_exists rset rname then 
         rset |> update_brel (RTagRel.add (rname, tagname))
      else raise no_such_rule_exp rname
  
  fun untag_rule rname tagname rset =
      if rule_exists rset rname then 
         rset |> update_brel (RTagRel.unmap (rname, tagname))
      else raise no_such_rule_exp rname
  
  fun delete_rules_in_tag tagname rset = 
      rset |> R.NSet.fold delete_rule (get_rules_in_tag rset tagname)

end (* functor Ruleset *)

functor BangGraphRuleset(Rule : BANG_GRAPH_RULE) : BANG_GRAPH_RULESET =
struct
  structure BangGraphRule = Rule
  structure RS = Ruleset(Rule)
  open RS
end


