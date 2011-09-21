structure DB_Test_GraphSubst = GraphSubstFun(Test_Match);
structure Test_GraphSubst : GRAPH_SUBST = DB_Test_GraphSubst;

(* Test match, subst, and simple match search construction *)
structure DB_Test_SimpleMatchSearch = SimpleMatchSearch(Test_Match);
structure Test_SimpleMatchSearch : BASIC_MATCH_SEARCH = 
  DB_Test_SimpleMatchSearch;

structure DB_Test_SimpleRuleMatchSearch = SimpleRuleMatchSearch(
  structure MatchSearch = Test_SimpleMatchSearch
  structure Rule = Test_Rule);
structure Test_SimpleRuleMatchSearch : RULE_MATCH_SEARCH = 
  DB_Test_SimpleRuleMatchSearch;


