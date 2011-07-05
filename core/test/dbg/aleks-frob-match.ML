
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
   val names =
     (fold Graph.add_to_vnames  gs Graph.Vertex.NSet.empty,
      fold Graph.add_to_enames  gs Graph.Edge.NSet.empty,
      fold Graph.add_to_bbnames gs Graph.BBox.NSet.empty)
   val matches = MatchSearch.match names lhs g
 in case Seq.pull matches of NONE=>false | _=>true
 end
