structure Test_GreedyMatcher =
  GreedyMatchSearch(Test_Bang_Graph);

(* set to 4 for really detailed messages *)
(*
Test_ConcreteMatcher.Log.level_ref := 2;
Test_GreedyMatcher.Log.level_ref := 2;
*)

local
  structure Greedy_Tester = Test_Bang_Match_Tools(Test_GreedyMatcher)
in
  structure Greedy_Matcher_SG_Tests = SG_To_SG_Matching_Tests(Greedy_Tester)
  structure Greedy_Matcher_BG_SG_Tests = BG_To_SG_Matching_Tests(Greedy_Tester)
end

