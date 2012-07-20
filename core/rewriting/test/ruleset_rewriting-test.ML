(* test construction of Ruleset *)
structure DB_Test_RulesetRewriter = RulesetRewriter(
  structure Ruleset = Test_Ruleset;
  structure MatchSearch = Test_GreedyMatcher;
  structure GraphSubst = Test_BangGraphSubst;
);
structure Test_RulesetRewriter : RULESET_REWRITER = DB_Test_RulesetRewriter;
(* set to 4 for really detailed messages *)
Test_RulesetRewriter.Log.level_ref := 2;
