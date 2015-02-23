structure Test_ConcreteMatcher =
  ConcreteMatchSearch(Test_OGraph);

(* set to 4 for really detailed messages *)
(*
Test_ConcreteMatcher.Log.level_ref := 2;
*)

structure Concrete_Matcher_Tests =
  SG_To_SG_Matching_Tests(Test_Match_Tools(Test_ConcreteMatcher))

