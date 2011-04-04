(* test constrcution of rule and ruleset *)
  structure DB_Test_Rule = Rule(Test_Graph);
  structure Test_Rule : RULE = DB_Test_Rule;

  structure DB_Test_Ruleset = Ruleset(Test_Rule);  
  structure Test_Ruleset = DB_Test_Ruleset : RULESET

