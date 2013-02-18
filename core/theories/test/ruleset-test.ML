(* test constrcution of rule and ruleset *)
structure DB_Test_RG_Ruleset = Ruleset(Test_RG_Rule);  
structure Test_RG_Ruleset = DB_Test_RG_Ruleset : RULESET

