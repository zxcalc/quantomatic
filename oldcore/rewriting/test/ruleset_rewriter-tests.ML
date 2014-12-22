structure Test_RulesetRewriter : RULESET_REWRITER = RulesetRewriter(
  structure Ruleset = Test_BG_Ruleset;
  structure Rewriter = Test_BangGraphRewriter;
);
(* set to 4 for really detailed messages *)
(* Test_RulesetRewriter.Log.level_ref := 2; *)

(* TODO: tests for rewriting against rulesets *)

