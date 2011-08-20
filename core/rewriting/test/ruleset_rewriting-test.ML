(* test construction of Ruleset *)
structure DB_Test_RulesetRewrites = RulesetRewrites(
  structure Ruleset = Test_Ruleset;
  structure RuleMatchSearch = Test_SimpleRuleMatchSearch;
  structure GraphSubst = Test_GraphSubst;
);
structure Test_RulesetRewrites : RULESET_REWRITES = DB_Test_RulesetRewrites;
