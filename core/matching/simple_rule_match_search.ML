functor SimpleRuleMatchSearch(
  structure MatchSearch : BASIC_MATCH_SEARCH
  structure Rule : RULE
  sharing Rule.Sharing.Graph = MatchSearch.Match.Sharing.G
  ) : RULE_MATCH_SEARCH
  = struct

  structure Rule = Rule;
  structure Match = MatchSearch.Match;
  
  fun match names rule tgt = 
    let val pat = (Rule.get_lhs rule)
    in MatchSearch.match names pat tgt end

  structure Sharing : RULE_MATCH_SEARCH_SHARING =
  struct
    structure Match = MatchSearch.Match.Sharing;
    structure Rule = Rule.Sharing;
  end;
end;