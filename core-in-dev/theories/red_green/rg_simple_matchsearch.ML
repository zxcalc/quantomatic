
structure DB_Match = MatchFun(RGGraph);
structure Match : MATCH = DB_Match;

structure DB_SimpleMatchSearch = 
  SimpleMatchSearchFun(structure BangGraph = RGGraph 
                       and Match = Match);
structure SimpleMatchSearch : SIMPLE_MATCH_SEARCH = DB_SimpleMatchSearch;
