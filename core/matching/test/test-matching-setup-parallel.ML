
structure Test_InnerMatcherParallel =
  SimpleInnerMatchSearchParallel(structure MatchState = Test_MatchState);
structure Test_BGInnerMatcherParallel =
  SimpleInnerMatchSearchParallel(structure MatchState = Test_BGMatchState);
structure Test_ConcreteMatcherParallel =
  ConcreteMatchSearchParallel(structure InnerMatchSearch = Test_InnerMatcherParallel);
structure Test_GreedyMatcherParallel =
  GreedyMatchSearchParallel(
    structure InnerMatchSearch = Test_BGInnerMatcher
    structure BGMatchState = Test_BGMatchState
  );

(* set to 4 for really detailed messages *)
Test_InnerMatcher.Log.level_ref := 2;
Test_BGInnerMatcher.Log.level_ref := 2;
Test_ConcreteMatcher.Log.level_ref := 2;
Test_GreedyMatcher.Log.level_ref := 2;

