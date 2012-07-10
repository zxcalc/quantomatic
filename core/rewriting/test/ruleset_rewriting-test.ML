(* test construction of Ruleset *)
structure DB_Test_RulesetRewriter = RulesetRewriterOld(
  structure Ruleset = Test_Ruleset;
  structure RuleMatchSearch = Test_SimpleRuleMatchSearch;
  structure GraphSubst = Test_GraphSubst;
);
structure Test_RulesetRewriter : RULESET_REWRITER = DB_Test_RulesetRewriter;
