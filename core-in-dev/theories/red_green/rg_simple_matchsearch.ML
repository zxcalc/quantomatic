
structure DB_RGMatch = MatchFun(RGGraph);
structure RGMatch : MATCH = DB_RGMatch;

structure DB_RGSimpleMatchSearch = 
  SimpleMatchSearchFun(structure BangGraph = RGGraph 
                       and Match = RGMatch);
structure RGSimpleMatchSearch : SIMPLE_MATCH_SEARCH = DB_RGSimpleMatchSearch;
