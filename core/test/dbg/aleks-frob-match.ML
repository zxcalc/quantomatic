
% cd quantomatic/cosy
% make
% rlwrap poly --use console.ML
> val RULE r = get_rule rs "ghz_fr";
> GHZW_Enum.rule_matches_graph r (GHZW_Theory.Rule.get_lhs r);
Exception- bbox_match_exp () raised

If you comment out "raise bbox_match_exp ()" and replace it with
"raise Match.match_exp ()", then rule_matches_graph just returns
false. Btw, rule_matches_graph is defined as:

fun rule_matches_graph r g = let
   val lhs = Ruleset.Rule.get_lhs r
   val gs = [lhs, g]
   fun add_to_vnames = V.NSet.union_merge o Graph.get_vnames;
   fun add_to_enames = E.NSet.union_merge o Graph.get_enames;
   fun add_to_bboxes = B.NSet.union_merge o Graph.get_bboxes;
   val names =
     (fold add_to_vnames  gs V.NSet.empty,
      fold add_to_enames  gs E.NSet.empty,
      fold add_to_bboxes gs B.NSet.empty)
   val matches = MatchSearch.match names lhs g
 in case Seq.pull matches of NONE=>false | _=>true
 end
