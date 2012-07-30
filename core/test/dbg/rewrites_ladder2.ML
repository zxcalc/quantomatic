open Test_MkG;
structure Rule = Test_Rule;
structure Ruleset = Test_Ruleset;
structure IO = Test_IO_Interface_v2;
structure RulesetRewriter = Test_RulesetRewriter;
structure Match = RulesetRewriter.Match;
structure Ruleset = Test_Ruleset;
structure MatchSearch = Test_SimpleMatchSearch;
structure GraphSubst = Test_GraphSubst;

fun list_of_rule_matches r g = 
    let val (r', matches) = RulesetRewriter.rule_matches r g;
    in (r', Seq.list_of matches) end;

val rset = "rulesets/basic_v2.rules" 
              |> XMLReader.read_from_file
              |> IO.Input.Ruleset.input;
          
val g = "graphs/d2-ladder_rw2.graph"
              |> XMLReader.read_from_file
              |> IO.Input.Graph.input;
              
val s = (RulesetRewriter.apply rset g);
val SOME (x1,s) = Seq.pull s;

(* *)
val spider_red = Ruleset.get_rule rset (RuleName.mk "spider.red");

val (r,matches) = list_of_rule_matches spider_red g;
exception bad_match_exp of Match.T;
val SOME bad_m = 
    (map (fn m => (RulesetRewriter.instantiate_rule m r)
                  handle _ => raise bad_match_exp m) matches; 
     NONE)
    handle  bad_match_exp m => SOME m;
val _ = Match.print bad_m;

exception bug_exp of RuleName.name * Rule.T;

  fun applicable_rules' rset matcher rnset g = 
      RuleName.NSet.fold 
      (fn n => fn ms =>
          let val (r,matches) = matcher (Ruleset.get_rule rset n) g 
          in 
            ((map 
              (fn m => 
               ((n,RulesetRewriter.instantiate_rule m r),
                GraphSubst.rewrite g (Rule.get_lhs r) m (Rule.get_rhs r)))
              (Seq.list_of matches))
              handle _ => raise bug_exp (n,r))
            @ ms
           end)
       rnset
       [];
  
  fun apply rset g = 
      applicable_rules' rset RulesetRewriter.rule_matches 
       (Ruleset.get_active rset) g;

apply rset g;

(*
val rset = "rulesets/basic2_v2.rules" 
              |> XMLReader.read_from_file
              |> IO.Input.Ruleset.input;

val g = "graphs/d2-ladder_rw1.graph"
              |> XMLReader.read_from_file
              |> IO.Input.Graph.input;
              
val l = Seq.list_of (RulesetRewriter.apply rset g);
val (a,g) = hd l; 

val _ = g |> IO.Output.Graph.output
          |> XMLWriter.write_to_file "graphs/d2-ladder_rw2.graph"
*)

