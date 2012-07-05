structure Test_MatchState =
  OGraphMatchState(structure G = Test_MkG.G)
structure Test_BGMatchState =
  BangGraphMatchState(structure G = Test_MkG.G)
structure Test_InnerMatcher =
  SimpleInnerMatchSearch(structure MatchState = Test_MatchState)
structure Test_BGInnerMatcher =
  SimpleInnerMatchSearch(structure MatchState = Test_BGMatchState)
structure Test_ConcreteMatcher =
  ConcreteMatchSearch(structure InnerMatchSearch = Test_InnerMatcher)
structure Test_GreedyMatcher =
  GreedyMatchSearch(
    structure InnerMatchSearch = Test_BGInnerMatcher
    structure BGMatchState = Test_BGMatchState
  )

structure Test_MatchUtil =
struct
  fun assert_n_match n mseq () = let
    val mlist = Seq.list_of mseq
    val nm = length mlist
  in if nm = n then I
     else
       (List.map Test_MatchState.print mlist;
        raise ERROR ("expected " ^ Int.toString n ^
                     " matching(s), got " ^ Int.toString nm))
  end
end

structure Test_BGMatchUtil =
struct
  fun assert_n_match n mseq () = let
    val mlist = Seq.list_of mseq
    val nm = length mlist
  in if nm = n then I
     else
       (List.map Test_BGMatchState.print mlist;
        raise ERROR ("expected " ^ Int.toString n ^
                     " matching(s), got " ^ Int.toString nm))
  end
end

