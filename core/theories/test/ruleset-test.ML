(* test constrcution of rule and ruleset *)
structure DB_Test_Ruleset = Ruleset(Test_Rule);  
structure Test_Ruleset = DB_Test_Ruleset : RULESET

