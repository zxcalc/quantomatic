functor Test_Rewriting(Graph : BANG_GRAPH) =
struct  
  structure DB_Rule = Rule(Graph);
  structure Rule : RULE = DB_Rule;

  structure DB_Ruleset = Ruleset(Rule);  
  structure Ruleset = DB_Ruleset : RULESET

  structure DB_Match = Match(Graph);
  structure Match : MATCH = DB_Match;

  structure DB_GraphSubst = GraphSubstFun(Match);
  structure GraphSubst : GRAPH_SUBST = DB_GraphSubst;

  structure DB_SimpleMatchSearch = SimpleMatchSearch(Match);
  structure SimpleMatchSearch : SIMPLE_MATCH_SEARCH = DB_SimpleMatchSearch;

  structure DB_RulesetRewrites = RulesetRewrites(
    structure Ruleset = Ruleset;
    structure MatchSearch = SimpleMatchSearch;
    structure GraphSubst = GraphSubst;
  );
  structure RulesetRewrites : RULESET_REWRITES = DB_RulesetRewrites;
end;

structure Test_RG_Rewriting = Test_Rewriting(Test_RGGraph);
