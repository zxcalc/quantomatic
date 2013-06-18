structure Test_BG_Rule = BangGraphRule(Test_Bang_Graph);

local
  structure Tools = Test_Bang_Graph_Tools(Test_Bang_Graph);
  open Tools;
  structure Rule = Test_BG_Rule;

  fun test_mk_bad_rule (lhs,rhs) =
     (Rule.mk (lhs,rhs);
      writeln "LHS:";
      G.print lhs;
      writeln "RHS:";
      G.print rhs;
      raise ERROR "Bad rule accepted")
     handle (Rule.bad_rule_exp _) => ();

  val _ = Testing.test "empty graphs" Rule.mk (G.empty,G.empty);

  val lhs = G.empty |> add_vunit1 "z1";
  val rhs = G.empty |> add_vunit1 "z1"
                    |> add_vunit2 "x1"
                    |> add_dir_eunit1 "e1" "z1" "x1";
  val _ = Testing.test "empty boundaries" Rule.mk (lhs,rhs);


  val lhs = G.empty |> add_wv "in1"
                    |> add_vunit1 "n1"
                    |> add_dir_eunit1 "e1" "in1" "n1";
  val rhs = G.empty |> add_wv "in1"
                    |> add_vunit2 "n1"
                    |> add_dir_eunit1 "e1" "in1" "n1";
  val _ = Testing.test "one input, no outputs" Rule.mk (lhs,rhs);

  val lhs = G.empty |> add_wv "in1"
                    |> add_wv "out1"
                    |> add_wv "out2"
                    |> add_vunit1 "n1"
                    |> add_vunit2 "n2"
                    |> add_dir_eunit1 "e1" "in1" "n1"
                    |> add_dir_eunit1 "e2" "n1" "out1"
                    |> add_dir_eunit1 "e3" "n2" "out2";
  val rhs = G.empty |> add_wv "in1"
                    |> add_wv "out1"
                    |> add_wv "out2"
                    |> add_vunit2 "n1"
                    |> add_vunit2 "n2"
                    |> add_dir_eunit1 "e1" "in1" "n1"
                    |> add_dir_eunit1 "e2" "n2" "out1"
                    |> add_dir_eunit1 "e3" "n2" "out2";
  val _ = Testing.test "one input, two outputs" Rule.mk (lhs,rhs);

  val lhs = G.empty |> add_wv "in1"
                    |> add_vunit1 "n1"
                    |> add_dir_eunit1 "e1" "in1" "n1";
  val rhs = G.empty |> add_wv "in1"
                    |> add_vunit1 "n1"
                    |> add_dir_eunit2 "e1" "in1" "n1";
  val _ = Testing.test "differing input types" test_mk_bad_rule (lhs,rhs);

  val lhs = G.empty |> add_wv "out1"
                    |> add_wv "out2"
                    |> add_vunit1 "n1"
                    |> add_dir_eunit1 "e1" "n1" "out1"
                    |> add_dir_eunit2 "e2" "n1" "out2";
  val rhs = G.empty |> add_wv "out1"
                    |> add_wv "out2"
                    |> add_vunit1 "n1"
                    |> add_dir_eunit2 "e1" "n1" "out1"
                    |> add_dir_eunit1 "e2" "n1" "out2";
  val _ = Testing.test "differing output types" test_mk_bad_rule (lhs,rhs);

  val lhs = G.empty |> add_wv "bnd1"
                    |> add_vunit1 "n1"
                    |> add_dir_eunit1 "e1" "bnd1" "n1";
  val rhs = G.empty |> add_wv "bnd1"
                    |> add_vunit1 "n1"
                    |> add_dir_eunit1 "e1" "n1" "bnd1";
  val _ = Testing.test "differing boundary directions" test_mk_bad_rule (lhs,rhs);


  val lhs = G.empty |> add_wv "bnd1"
                    |> add_wv "bnd2"
                    |> add_vunit1 "n1"
                    |> add_vunit1 "n2"
                    |> add_vunit2 "n3"
                    |> add_vunit2 "n4"
                    |> add_undir_eunit1 "e1" "n3" "n1"
                    |> add_undir_eunit1 "e2" "bnd1" "n1"
                    |> add_undir_eunit1 "e3" "bnd2" "n1"
                    |> add_undir_eunit1 "e4" "n2" "n4"
  val rhs = G.empty |> add_wv "bnd1"
                    |> add_wv "bnd2"
                    |> add_vunit1 "n1"
                    |> add_vunit2 "n2"
                    |> add_undir_eunit1 "e1" "n1" "bnd1"
                    |> add_undir_eunit1 "e2" "bnd2" "n2";
  val _ = Testing.test "two undirected boundaries" Rule.mk (lhs,rhs);

  val lhs = G.empty |> add_wv "bnd1"
                    |> add_wv "bnd2"
                    |> add_vunit1 "n1"
                    |> add_undir_eunit1 "e1" "n1" "bnd1"
                    |> add_undir_eunit1 "e2" "bnd2" "n1";
  val rhs = G.empty |> add_wv "bnd1"
                    |> add_vunit1 "n1"
                    |> add_undir_eunit1 "e1" "n1" "bnd1";
  val _ = Testing.test "differing numbers of undirected boundaries"
                       test_mk_bad_rule (lhs,rhs);

  val lhs = G.empty |> add_wv "bnd1"
                    |> add_vunit1 "n1"
                    |> add_undir_eunit2 "e1" "n1" "bnd1"
  val rhs = G.empty |> add_wv "bnd1"
                    |> add_vunit1 "n1"
                    |> add_undir_eunit1 "e1" "n1" "bnd1";
  val _ = Testing.test "differing types of undirected boundaries"
                       test_mk_bad_rule (lhs,rhs);

  val g = G.empty
        |> add_wv "in1"
        |> add_wv "in2"
        |> add_wv "out1"
        |> add_wv "out2"
        |> add_vunit1 "n1"
        |> add_dir_eunit1 "e1" "in1" "n1"
        |> add_dir_eunit1 "e2" "in2" "n1"
        |> add_dir_eunit1 "e3" "n1" "out1"
        |> add_dir_eunit1 "e4" "n1" "out2";

  val lhs = g
          |> add_bbox "b1" []
          |> add_bbox "b2" ["in1","in2"]
          |> add_child_bbox "b3" "b2" ["in1"];
  val rhs = lhs;
  val _ = Testing.test "same bboxes" Rule.mk (lhs,rhs);

  val lhs = g
          |> add_bbox "b1" [];
  val rhs = g
          |> add_bbox "b2" [];
  val _ = Testing.test "differing !-box names"
                       test_mk_bad_rule (lhs,rhs);

  val lhs = g
          |> add_bbox "b1" ["in1"];
  val rhs = g
          |> add_bbox "b1" ["in2"];
  val _ = Testing.test "differing !-box contents"
                       test_mk_bad_rule (lhs,rhs);

  val lhs = g
          |> add_bbox "b1" []
          |> add_bbox "b2" ["in1","in2"]
          |> add_child_bbox "b3" "b2" ["in1"];
  val rhs = g
          |> add_bbox "b1" []
          |> add_bbox "b2" ["in1","in2"]
          |> add_bbox "b3" ["in1"];
  val _ = Testing.test "differing !-box parents"
                       test_mk_bad_rule (lhs,rhs);

  (* Rule.symmetric *)


  val lhs = G.empty |> add_wv "in1"
                    |> add_vunit1 "n1"
                    |> add_dir_eunit1 "e1" "in1" "n1"
  val rhs = G.empty |> add_wv "in1"
                    |> add_vunit2 "n1"
                    |> add_vunit2 "n2"
                    |> add_dir_eunit1 "e1" "in1" "n1"
                    |> add_dir_eunit1 "e2" "n1" "n2"
  val rule = Rule.mk (lhs,rhs)
  val rule' = Rule.symmetric rule
  val _ = assert_iso_graph "new RHS iso to old LHS" lhs (Rule.get_rhs rule')
  val _ = assert_iso_graph "new LHS iso to old RHS" rhs (Rule.get_lhs rule')

in
  val _ = Testing.assert_no_failed_tests();
end;
