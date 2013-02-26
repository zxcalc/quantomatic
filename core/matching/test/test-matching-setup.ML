structure Test_MatchState =
  OGraphMatchState(
    structure G = Test_RG_MkG.G
    type data = unit
    val empty_data = ()
  );
structure Test_BGMatchState =
  BangGraphMatchState(
    structure G = Test_RG_MkG.G
    type data = unit
    val empty_data = ()
  );
structure Test_InnerMatcher =
  SimpleInnerMatchSearch(structure MatchState = Test_MatchState);
structure Test_BGInnerMatcher =
  SimpleInnerMatchSearch(structure MatchState = Test_BGMatchState);
structure Test_ConcreteMatcher =
  ConcreteMatchSearch(structure InnerMatchSearch = Test_InnerMatcher);
structure Test_GreedyMatcher =
  GreedyMatchSearch(
    structure InnerMatchSearch = Test_BGInnerMatcher
    structure BGMatchState = Test_BGMatchState
  );
(* set to 4 for really detailed messages *)
(*
Test_InnerMatcher.Log.level_ref := 2;
Test_BGInnerMatcher.Log.level_ref := 2;
Test_ConcreteMatcher.Log.level_ref := 2;
Test_GreedyMatcher.Log.level_ref := 2;
*)

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
end;

structure Test_BGMatchUtil =
struct
  structure G = Test_RG_MkG.G;
  structure Rule = Rule(G);

  fun assert_n_match n mseq () = let
    val mlist = Seq.list_of mseq
    val nm = length mlist
  in if nm = n then I
     else
       (List.map Test_BGMatchState.print mlist;
        raise ERROR ("expected " ^ Int.toString n ^
                     " matching(s), got " ^ Int.toString nm))
  end

  fun assert_boundary_coherent (g1,g2) = let
    (* easiest just to try to make a rule out of it *)
    val _ = Rule.mk (g1,g2)
  in
    (g1,g2)
  end
end;

