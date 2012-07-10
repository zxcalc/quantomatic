(* test construction of Ruleset *)
structure DB_Test_RulesetRewriterOld = RulesetRewriterOld(
  structure Ruleset = Test_Ruleset;
  structure RuleMatchSearch = Test_SimpleRuleMatchSearch;
  structure GraphSubst = Test_GraphSubst;
);
structure DB_Test_RulesetRewriter = RulesetRewriter(
  structure Ruleset = Test_Ruleset;
  structure MatchSearch = Test_GreedyMatcher;
  structure GraphSubst = Test_BangGraphSubst;
);
structure Test_RulesetRewriter : RULESET_REWRITER = DB_Test_RulesetRewriter;
