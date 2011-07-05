open Test_MkG;
structure Rule = Test_Rule;
structure Ruleset = Test_Ruleset;
structure IO = Test_IO_Interface_v2;
structure RulesetRewrites = Test_RulesetRewrites;
structure Match = RulesetRewrites.Match;
structure Ruleset = Test_Ruleset;
structure MatchSearch = Test_SimpleMatchSearch;
structure GraphSubst = Test_GraphSubst;

(* 
val rset = "rulesets/basic_v2.rules" 
              |> XMLReader.read_from_file
              |> IO.Input.Ruleset.input;
*)

val rset = "rulesets/basic2_v2.rules" 
              |> XMLReader.read_from_file
              |> IO.Input.Ruleset.input;

val g = "graphs/d2-ladder_rw1.graph"
              |> XMLReader.read_from_file
              |> IO.Input.Graph.input;
              
val seq = Seq.list_of (RulesetRewrites.apply rset g);
