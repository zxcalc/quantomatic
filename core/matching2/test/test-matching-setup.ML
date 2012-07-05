structure Test_MatchState = OGraphMatchState(structure G = Test_MkG.G)
structure Test_InnerMatcher = SimpleInnerMatchSearch(structure MatchState = Test_MatchState)
structure Test_ConcreteMatcher = ConcreteMatchSearch(structure InnerMatchSearch = Test_InnerMatcher)

structure Test_MatchUtil =
struct
  fun assert_n_match n mseq () = let val nm = length (Seq.list_of mseq)
  in if nm = n then I
     else raise ERROR (
      "expected " ^
      Int.toString n ^
      " matching(s), got " ^
      Int.toString nm)
  end
end

