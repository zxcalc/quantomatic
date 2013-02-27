structure Test_MatchState =
  OGraphMatchState(
    structure G = Test_OGraph
    type data = unit
    val empty_data = ()
  );
structure Test_InnerMatcher =
  SimpleInnerMatchSearch(structure MatchState = Test_MatchState);
structure Test_ConcreteMatcher =
  ConcreteMatchSearch(structure InnerMatchSearch = Test_InnerMatcher);

structure Test_BGMatchState =
  BangGraphMatchState(
    structure G = Test_Bang_Graph
    type data = unit
    val empty_data = ()
  );
structure Test_BGInnerMatcher =
  SimpleInnerMatchSearch(structure MatchState = Test_BGMatchState);
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

structure Test_Match_Tools =
struct
  structure GTools = Test_OGraph_Tools(Test_OGraph);
  open GTools;
  structure MS = Test_MatchState;

  fun assert_n_match n mseq () = let
    val mlist = Seq.list_of mseq
    val nm = length mlist
  in if nm = n then I
     else
       (List.map MS.print mlist;
        raise ERROR ("expected " ^ Int.toString n ^
                     " matching(s), got " ^ Int.toString nm))
  end
end;

structure Test_Bang_Match_Tools =
struct
  structure GTools = Test_Bang_Graph_Tools;
  open GTools;
  structure MS = Test_BGMatchState;

  fun assert_n_match n mseq () = let
    val mlist = Seq.list_of mseq
    val nm = length mlist
  in if nm = n then I
     else
       (List.map MS.print mlist;
        raise ERROR ("expected " ^ Int.toString n ^
                     " matching(s), got " ^ Int.toString nm))
  end

  local
    structure Rule = Rule(G);
  in
    fun assert_boundary_coherent (g1,g2) = let
      (* easiest just to try to make a rule out of it *)
      val _ = Rule.mk (g1,g2)
    in
      (g1,g2)
    end
  end
end;

