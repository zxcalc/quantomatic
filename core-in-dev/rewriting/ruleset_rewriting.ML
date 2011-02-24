signature RULESET_REWRITES =
sig
  structure RuleSet : RULESET
  structure Rule : RULE 
  sharing RuleSet.Rule.Sharing = Rule.Sharing
  structure Match : MATCH
  sharing Match.Sharing.G = Rule.Sharing.Graph;

  (* matching *)
  val applicable_rules : RuleSet.T -> Rule.Graph.T 
        -> ((RuleName.name * Rule.T) (* name, rule, rewritten graph *) 
            * Rule.Graph.T) Seq.seq 
  val applicable_rules_in : RuleSet.T -> Rule.Graph.T -> Rule.Graph.Vertex.NSet.T  
        -> ((RuleName.name * Rule.T) * Rule.Graph.T) Seq.seq;

  (* get just first rewrite *)
  val apply_first_rule : RuleSet.T -> Rule.Graph.T 
        -> ((RuleName.name * Rule.T) (* name, rule, rewritten graph *) 
            * Rule.Graph.T) option 
  val apply_first_rule_in : RuleSet.T -> Rule.Graph.T -> Rule.Graph.Vertex.NSet.T  
        -> ((RuleName.name * Rule.T) * Rule.Graph.T) option;

  (* low-level *)
  val applicable_rules' : RuleSet.T (* background ruleset *)
      -> (Rule.T -> Rule.Graph.T -> Match.T Seq.seq) (* matcher *)
      -> RuleName.NSet.T (* subset of rules to use *) 
      -> Rule.Graph.T (* graph to look for matches of rules in *)
      -> ((RuleName.name * Rule.T) (* name, rule, rewritten graph *) 
            * Rule.Graph.T) Seq.seq 
 
  (* check how a single rule matches *)
  val rule_matches : Rule.T -> Rule.Graph.T -> Match.T Seq.seq
  
  (* funny order is because we create a rule-matcher and use it in applicable_rules' *)
  val rule_matches_within : Rule.Graph.Vertex.NSet.T -> Rule.T -> Rule.Graph.T  
      -> Match.T Seq.seq
end;

(* *)
structure RulesetRewrites : RULESET_REWRITES =
struct
  structure Rule = RGRule;
  structure Match = RGMatch;
  structure RuleSet = RGRuleSet;
  structure MatchSearch = RGSimpleMatchSearch;
  structure GraphSubst = RGGraphSubst;

  structure G = Rule.Graph;
  structure V = G.Vertex;
  structure E = G.Edge;
  structure BBox = G.BBox;

  fun rule_matches r g =
      let
        val lhs = Rule.get_lhs r;
        val gs = [lhs, g]
        val names = 
          (fold G.add_to_vnames gs V.NSet.empty,
           fold G.add_to_enames gs E.NSet.empty,
           fold G.add_to_bbnames gs BBox.NSet.empty)
      in MatchSearch.match names lhs g end;
        
  fun rule_matches_within vset r g  = 
      let 
        val lhs = Rule.get_lhs r;
        val gs = [lhs, g]
        val names = 
          (fold G.add_to_vnames gs V.NSet.empty,
           fold G.add_to_enames gs E.NSet.empty,
           fold G.add_to_bbnames gs BBox.NSet.empty)
      in MatchSearch.match names lhs 
          (G.matched_subgraph_within_vset vset g) 
      end;
  
  (* instantiate rule with a match *)
  fun instantiate_rule m r = 
      Rule.mk (Match.inst_pat_graph m (Rule.get_lhs r), 
               Match.inst_replacement_graph m (Rule.get_rhs r));
  
  (* apply rules to whole graph *)
  (* IMPROVE: provide a single name-space for all rules, and thus avoid 
     multiple renamings. *)
  fun applicable_rules' rset matcher rnset g = 
      RuleName.NSet.fold 
      (fn n =>
          let val r = RuleSet.get_rule rset n
          in Seq.append 
            (Seq.map 
              (fn m => 
               ((n,instantiate_rule m r),
                GraphSubst.rewrite g (Rule.get_lhs r) m (Rule.get_rhs r))) 
               (matcher r g))
           end)
       rnset
       Seq.empty;
  
  fun applicable_rules rset g = applicable_rules' rset rule_matches (RuleSet.get_active rset) g;

  fun applicable_rules_in rset g vset = 
      applicable_rules' rset (rule_matches_within vset) (RuleSet.get_active rset) g;
  
  (* apply just first rule that can be applied *)
  fun apply_first_rule rset g = 
      case Seq.pull (applicable_rules rset g) 
       of NONE => NONE
        | SOME (h,_) => SOME h;

  fun apply_first_rule_in rset vset g = 
      case Seq.pull (applicable_rules_in rset vset g) 
       of NONE => NONE
        | SOME (h,_) => SOME h;

end; (* *)


