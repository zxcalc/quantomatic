(* Test match, subst, and simple match search construction *)
structure DB_Test_Match = Match(Test_Graph);
structure Test_Match : MATCH = DB_Test_Match;

structure DB_Test_GraphSubst = GraphSubstFun(Test_Match);
structure Test_GraphSubst : GRAPH_SUBST = DB_Test_GraphSubst;

structure DB_Test_SimpleMatchSearch = SimpleMatchSearch(Test_Match);
structure Test_SimpleMatchSearch : SIMPLE_MATCH_SEARCH = 
  DB_Test_SimpleMatchSearch;


