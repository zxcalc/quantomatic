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

functor Test_Match_Tools(
  Matcher : MATCH_SEARCH where type G.EData.data = Test_Graph_Data.edata
                           and type G.OVData.IData.data = Test_Graph_Data.vdata
) =
struct
  structure GTools = Test_OGraph_Tools(Matcher.G);
  open GTools;
  structure M = Matcher;
  structure MS = M.MatchState;

  fun assert_n_match n mseq () = let
    val mlist = Seq.list_of mseq
    val nm = length mlist
  in if nm = n then mlist
     else
       (List.map MS.print mlist;
        raise ERROR ("expected " ^ Int.toString n ^
                     " matching(s), got " ^ Int.toString nm))
  end;

  fun find_n_matches n (pat,tgt) =
    assert_n_match n (M.match pat tgt) ();
end;

functor Test_Bang_Match_Tools(
  BGMatcher : BG_MATCH_SEARCH where type G.EData.data = Test_Graph_Data.edata
                                and type G.OVData.IData.data = Test_Graph_Data.vdata
) =
struct
  structure MTools = Test_Match_Tools(BGMatcher);
  open MTools;
  structure Tools = Test_Bang_Graph_Tools(BGMatcher.BG);
  open Tools;
  structure M = BGMatcher;
  structure MS = M.BGMatchState;

  fun assert_n_warnings n ms = let
    val ws = MS.get_warnings ms
  in
    Testing.assert ((Int.toString n)^" warnings") (length ws = n)
  end
end;

