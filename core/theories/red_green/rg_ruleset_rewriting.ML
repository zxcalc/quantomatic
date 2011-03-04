structure DB_RGMatch = Match(RGGraph);
structure RGMatch : MATCH = DB_RGMatch;

structure DB_RGGraphSubst = GraphSubstFun(RGMatch);
structure RGGraphSubst : GRAPH_SUBST = DB_RGGraphSubst;

structure DB_RGSimpleMatchSearch = SimpleMatchSearch(RGMatch);
structure RGSimpleMatchSearch : SIMPLE_MATCH_SEARCH = DB_RGSimpleMatchSearch;

structure DB_RGRulesetRewrites = RulesetRewrites(
  structure Ruleset = RGRuleset;
  structure MatchSearch = RGSimpleMatchSearch;
  structure GraphSubst = RGGraphSubst;
) 
structure RGRulesetRewrites : RULESET_REWRITES = DB_RGRulesetRewrites;

