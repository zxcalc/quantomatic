open Test_MkG;
structure Rule = Test_Rule;
structure Ruleset = Test_Ruleset;
structure IO = Test_IO_Interface_v2;
structure RulesetRewrites = Test_RulesetRewrites;
structure Match = RulesetRewrites.Match;
structure Ruleset = Test_Ruleset;
structure MatchSearch = Test_SimpleMatchSearch;
structure GraphSubst = Test_GraphSubst;

val ruleset = "symtest.rules" 
              |> XMLReader.read_from_file
              |> IO.Input.Ruleset.input;

val g = "triangle_failing_graph.graph" 
              |> XMLReader.read_from_file
              |> IO.Input.Graph.input;

val results = RulesetRewrites.apply ruleset g;

(* 
fun list_of_rule_matches r g = 
    let val (r', matches) = RulesetRewrites.rule_matches r g;
    in (r', Seq.list_of matches) end;

val _ = Testing.test "rule1:" (fn () => let
  
  in () end) ();


val _ = Testing.assert_no_failed_tests();
val _ = "Regression test for bug passed!";
*)

