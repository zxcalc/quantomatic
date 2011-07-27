local
open Test_MkG;
structure Rule = Test_Rule;
structure Ruleset = Test_Ruleset;
structure IO = Test_IO_Interface_v2;
structure RulesetRewrites = Test_RulesetRewrites;
structure Match = RulesetRewrites.Match;
structure Ruleset = Test_Ruleset;
structure MatchSearch = Test_SimpleMatchSearch;
structure GraphSubst = Test_GraphSubst;
in

val ruleset = "rulesets/basic_v2.rules" 
              |> XMLReader.read_from_file
              |> IO.Input.Ruleset.input;

val isom_rule = Ruleset.get_rule ruleset (RuleName.mk "isometry_red");
val x_copy_0_rule = Ruleset.get_rule ruleset (RuleName.mk "x_copy_0");

fun list_of_rule_matches r g = 
    let val (r', matches) = RulesetRewrites.rule_matches r g;
    in (r', Seq.list_of matches) end;

val _ = Testing.test "rule1:" (fn () => let
  (* Example of a rule that is not valid *)
  val g = G.empty;
  val (b1, g) = g |> G.add_vertex bvert;
  val (b2, g) = g |> G.add_vertex bvert;
  val (n1, g) = g |> G.add_vertex (mkZ zero_angle);
  val (n2, g) = g |> G.add_vertex (mkX zero_angle);
  val (e0, g) = g |> G.add_edge undir_edge n1 n2;
  val (e1, g) = g |> G.add_edge undir_edge b1 n2;
  val (e2, g) = g |> G.add_edge undir_edge b2 n2;
  val (n3, g) = g |> G.add_vertex (mkX zero_angle); (* scalar X *)
  val (n4, g) = g |> G.add_vertex (mkZ zero_angle); (* scalar Z *)
  val (e5, g) = g |> G.add_edge undir_edge n3 n4; (* scalar XZ *)
  val lhs = g;
  val _ = G.print lhs;
  (* rhs *)
  val g = G.empty;
  val g = g |> G.new_named_vertex bvert b1;
  val (n1, g) = g |> G.add_vertex (mkZ zero_angle);
  val (n2, g) = g |> G.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> G.add_edge undir_edge b1 n1;
  val rhs = g;
  val _ = G.print rhs;
  val _ = 
      ((Rule.mk (lhs,rhs); 
        raise ERROR "This rule is bad, it should raise an exception, but did not! :( ")
       handle (Rule.bad_rule_exp _) => 
              Pretty.writeln (Pretty.str "Bad rule was correctly judged to be bad."));
  in writeln "Ex1" end) ();


val _ = Testing.test "rule2: !match(isom_rule, X - X - Z)" (fn () => let
  (* Example of applications of rule that does not match *)
  val g = G.empty;
  val (n1, g) = g |> G.add_vertex (mkX zero_angle);
  val (n2, g) = g |> G.add_vertex (mkX zero_angle);
  val (n3, g) = g |> G.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> G.add_edge undir_edge n1 n3;
  val (e1, g) = g |> G.add_edge undir_edge n2 n3;
  val _ = G.print g;
  val (r',[]) = list_of_rule_matches isom_rule g;
  in writeln "Ex2" end) ();


val _ = Testing.test "rule3: [g1,g2,g3,g4] = subst(isom_lhs, isom_lhs)" (fn () => let
  (* test rewriting isometry lhs with isometry rule *)
  val g = G.empty;
  val (b1, g) = g |> G.add_vertex bvert;
  val (n1, g) = g |> G.add_vertex (mkX zero_angle);
  val (n2, g) = g |> G.add_vertex (mkX zero_angle);
  val (b2, g) = g |> G.add_vertex bvert;
  val (e0, g) = g |> G.add_edge undir_edge b1 n1;
  val (e1, g) = g |> G.add_edge undir_edge n1 n2;
  val (e2, g) = g |> G.add_edge undir_edge n1 n2;
  val (e3, g) = g |> G.add_edge undir_edge n2 b2;
  val _ = G.print g;

  val (r',[m1,m2,m3,m4]) = list_of_rule_matches isom_rule g;
  val _ = Rule.print r';
  val _ = Match.print m1;
  val inst_isom_rule = RulesetRewrites.instantiate_rule m1 r';
  val _ = Rule.print inst_isom_rule;
  val g1 = GraphSubst.rewrite g (Rule.get_lhs r') m1 
                                (Rule.get_rhs r');
  val g2 = GraphSubst.rewrite g (Rule.get_lhs r') m2 
                                (Rule.get_rhs r');
  val g3 = GraphSubst.rewrite g (Rule.get_lhs r') m3 
                                (Rule.get_rhs r');
  val g4 = GraphSubst.rewrite g (Rule.get_lhs r') m4 
                                (Rule.get_rhs r');
  in writeln "success" end) ();

val _ = Testing.test "rule4: rewriting bialgebra (self-matches)" (fn () => let
  val r = Ruleset.get_rule ruleset (RuleName.mk "bialgebra1");
  val g = Rule.get_lhs r;
  val (r',[m1,m2,m3,m4]) = list_of_rule_matches r g;
  val _ = Rule.print r';
  val _ = Match.print m1;
  val inst_isom_rule = RulesetRewrites.instantiate_rule m1 r';
  val _ = Rule.print inst_isom_rule;
  val g1 = GraphSubst.rewrite g (Rule.get_lhs r') m1 
                                (Rule.get_rhs r');
  val g2 = GraphSubst.rewrite g (Rule.get_lhs r') m2 
                                (Rule.get_rhs r');
  val g3 = GraphSubst.rewrite g (Rule.get_lhs r') m3 
                                (Rule.get_rhs r');
  val g4 = GraphSubst.rewrite g (Rule.get_lhs r') m4 
                                (Rule.get_rhs r');
  in writeln "success" end) ();  
  
  (* 
val _ = Testing.test "rule5: bang-box rewrite" (fn () => let
  val spider_red = Ruleset.get_rule rset (RuleName.mk "spider.red");
  val g = "graphs/d2-ladder_rw2.graph"
              |> XMLReader.read_from_file
              |> IO.Input.Graph.input;
  val 
  in () end) ();
  *)   
  
val _ = Testing.assert_no_failed_tests();
val _ = "UNIT TESTS FOR rule PASSED!";
end;
