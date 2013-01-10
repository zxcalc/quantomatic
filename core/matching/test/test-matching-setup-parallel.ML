
structure MJB_Test_InnerMatcher_Parallel =
  MJB_SimpleInnerMatchSearchParallel(structure MatchState = Test_MatchState);
structure MJB_Test_BGInnerMatcher_Parallel =
  MJB_SimpleInnerMatchSearchParallel(structure MatchState = Test_BGMatchState);
structure MJB_Test_ConcreteMatcher_Parallel =
  MJB_ConcreteMatchSearch(structure InnerMatchSearch = Test_InnerMatcher);
structure MJB_Test_GreedyMatcher_Parallel =
  MJB_GreedyMatchSearchParallel(
    structure InnerMatchSearch = Test_BGInnerMatcher
    structure BGMatchState = Test_BGMatchState
  );

(* set to 4 for really detailed messages *)
Test_InnerMatcher.Log.level_ref := 2;
Test_BGInnerMatcher.Log.level_ref := 2;
Test_ConcreteMatcher.Log.level_ref := 2;
Test_GreedyMatcher.Log.level_ref := 2;

