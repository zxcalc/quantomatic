(* cd "test/dbg/2011-08-13-matvey-rewrite/"; *)

open Test_RG_MkG;
structure Rule = Test_RG_Rule;
structure Ruleset = Test_RG_Ruleset;
structure IO = Test_IO_Interface_v2;
structure RulesetRewriter = Test_RG_RulesetRewriter;
structure Match = RulesetRewriter.Match;
structure Ruleset = Test_RG_Ruleset;
structure MatchSearch = Test_SimpleMatchSearch;
structure GraphSubst = Test_GraphSubst;

val rset = "symtest.rules" 
            |> XMLReader.read_from_file
            |> IO.Input.Ruleset.input;

val g = "triangle_failing_graph.graph" 
            |> XMLReader.read_from_file
            |> IO.Input.Graph.input;

(* val results = RulesetRewriter.apply ruleset g; *)
val r = Ruleset.get_rule rset (RuleName.mk "redtriangle");

fun list_of_rule_matches r g = 
    let val (r', matches) = RulesetRewriter.rule_matches r g;
    in (r', Seq.list_of matches) end;

val (r',l) = list_of_rule_matches r g;
val m = hd l;
Rule.print r';
G.print g;
Match.print m;

val rhs = Rule.get_rhs r';
G.print rhs;

raise ERROR "stop";

val _ = Testing.test "rule1:" (fn () => let
  
  in () end) ();


val _ = Testing.assert_no_failed_tests();
val _ = "Regression test for bug passed!";
*)

