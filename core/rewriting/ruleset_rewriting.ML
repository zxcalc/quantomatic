signature RULESET_REWRITES_SHARING =
sig
  structure Ruleset : RULESET_SHARING
  structure Match : MATCH_SHARING
  sharing Match.G = Ruleset.Rule.Graph;
end;

signature RULESET_REWRITES =
sig
  structure Ruleset : RULESET
  structure Rule : RULE 
  sharing Ruleset.Rule.Sharing = Rule.Sharing
  structure Match : MATCH
  sharing Match.Sharing.G = Rule.Sharing.Graph;

  structure Sharing : RULESET_REWRITES_SHARING
  sharing Sharing.Ruleset = Ruleset.Sharing;
  sharing Sharing.Match = Match.Sharing;

  (* matching *)
  val apply : Ruleset.T -> Rule.Graph.T 
        -> ((R.name * Rule.T) (* name, rule, rewritten graph *) 
            * Rule.Graph.T) Seq.seq 
  val apply_in : Ruleset.T -> Rule.Graph.T -> V.NSet.T  
        -> ((R.name * Rule.T) * Rule.Graph.T) Seq.seq;

  (* get just first rewrite *)
  val apply_first : Ruleset.T -> Rule.Graph.T 
        -> ((R.name * Rule.T) (* name, rule, rewritten graph *) 
            * Rule.Graph.T) option 
  val apply_first_in : Ruleset.T -> Rule.Graph.T -> V.NSet.T  
        -> ((R.name * Rule.T) * Rule.Graph.T) option;

  (* low-level *)
  val applicable_rules' : Ruleset.T (* background ruleset *)
      -> (Rule.T -> Rule.Graph.T -> Rule.T * Match.T Seq.seq) (* matcher *)
      -> R.NSet.T (* subset of rules to use *) 
      -> Rule.Graph.T (* graph to look for matches of rules in *)
      -> ((R.name * Rule.T) (* name, rule, rewritten graph *) 
            * Rule.Graph.T) Seq.seq 
 
  (* to see what a rule looks like once data is matched and bang-boxes 
     expanded. *)
  val instantiate_rule : Match.T -> Rule.T -> Rule.T; 

  (* check how a single rule matches (rule is renamed) *)
  val rule_matches : Rule.T -> Rule.Graph.T -> (Rule.T * Match.T Seq.seq)
  
  (* funny order is because we create a rule-matcher and use it in applicable_rules' (rule is renamed) *)
  val rule_matches_within : V.NSet.T -> Rule.T -> Rule.Graph.T  
      -> (Rule.T * Match.T Seq.seq)
end;

(* *)
functor RulesetRewrites(
  structure Ruleset : RULESET
  structure RuleMatchSearch : RULE_MATCH_SEARCH
  sharing type RuleMatchSearch.Rule.T = Ruleset.Rule.T
  structure GraphSubst : GRAPH_SUBST
  sharing RuleMatchSearch.Match.Sharing.G = Ruleset.Rule.Sharing.Graph;
  sharing type GraphSubst.graph = Ruleset.Rule.Graph.T
  sharing type GraphSubst.match = RuleMatchSearch.Match.T
) : RULESET_REWRITES =
struct
  structure Match = RuleMatchSearch.Match;
  structure Ruleset = Ruleset;
  structure Rule = Ruleset.Rule;
  (* *)
  structure RuleMatchSearch = RuleMatchSearch;
  structure GraphSubst = GraphSubst;

  structure G = Rule.Graph;

  fun rule_matches r g =
      let
        val rnms = 
            (X.Rnm.mk_from_avoids (G.get_xnames g),
             V.Rnm.mk_from_avoids (G.get_vnames g),
             E.Rnm.mk_from_avoids (G.get_enames g),
             B.Rnm.mk_from_avoids (G.get_bboxes g))
        val (rnm,r) = Rule.rename rnms r;
        val lhs = Rule.get_lhs r;
        val gs = [lhs, g, Rule.get_rhs r]
        val names = 
          (fold G.add_to_vnames gs V.NSet.empty,
           fold G.add_to_enames gs E.NSet.empty,
           fold G.add_to_bboxes gs B.NSet.empty)
      in (r,RuleMatchSearch.match names r g) end;

  (* TODO: no need to totally rename-apart the rule: matching removes the tgt 
     of the match: it is independent of the rule. Instead, we should be
     providing a way to rename-apart the rule and match to correspond to a given
     target (so that we can display a nice picture where they look the same). Is
     this true even for internal variables names? They need to be renamed to
     avoid new variables being captured. *)
  fun rule_matches_within vset r g  = 
      let 
        val rnms = 
            (X.Rnm.mk_from_avoids (G.get_xnames g),
             V.Rnm.mk_from_avoids (G.get_vnames g),
             E.Rnm.mk_from_avoids (G.get_enames g),
             B.Rnm.mk_from_avoids (G.get_bboxes g))
        val (rnm,r) = Rule.rename rnms r;
        val lhs = Rule.get_lhs r;
        val gs = [lhs, g, Rule.get_rhs r]
        val names = 
          (fold G.add_to_vnames gs V.NSet.empty,
           fold G.add_to_enames gs E.NSet.empty,
           fold G.add_to_bboxes gs B.NSet.empty)
      in (r, RuleMatchSearch.match names r 
              (G.matched_subgraph_within_vset vset g)) 
      end;
  
  (* instantiate rule with a match *)
  fun instantiate_rule m r = 
      Rule.mk (Match.inst_pat_graph m (Rule.get_lhs r), 
               Match.inst_replacement_graph m (Rule.get_rhs r));
  
  (* apply rules to whole graph *)
  (* IMPROVE: provide a single name-space for all rules, and thus avoid 
     multiple renamings. *)
  fun applicable_rules' rset matcher rnset g = 
      R.NSet.fold 
      (fn n =>
          let val (r,matches) = matcher (Ruleset.get_rule rset n) g 
          in Seq.append 
            (Seq.map 
              (fn m => 
               ((n,instantiate_rule m r),
                GraphSubst.do_rewrite g (Rule.get_lhs r) m (Rule.get_rhs r)))
               matches)
           end)
       rnset
       Seq.empty;
  
  fun apply rset g = 
      applicable_rules' rset rule_matches 
       (Ruleset.get_active rset) g;

  fun apply_in rset g vset = 
      applicable_rules' rset (rule_matches_within vset) 
        (Ruleset.get_active rset) g;
  
  (* apply just first rule that can be applied *)
  fun apply_first rset g = 
      case Seq.pull (apply rset g) 
       of NONE => NONE
        | SOME (h,_) => SOME h;

  fun apply_first_in rset vset g = 
      case Seq.pull (apply_in rset vset g) 
       of NONE => NONE
        | SOME (h,_) => SOME h;


  structure Sharing : RULESET_REWRITES_SHARING
  = struct
    structure Ruleset = Ruleset.Sharing;
    structure Match = Match.Sharing;
  end


end; (* *)

