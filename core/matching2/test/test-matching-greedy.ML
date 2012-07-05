local (* Test matching with concrete graphs *)
  open Test_MkG
  open Test_BGMatchUtil
  structure M = Test_GreedyMatcher

  val g = G.empty
  (* circle *)
  val (v1,g) = g |> G.add_vertex bvert
  val (_ ,g) = g |> G.add_edge dir_edge v1 v1
  (* bare wire *)
  val (v2,g) = g |> G.add_vertex bvert
  val (v3,g) = g |> G.add_vertex bvert
  val (_ ,g) = g |> G.add_edge dir_edge v2 v3
  (* x with two boundaries *)
  val (v4,g) = g |> G.add_vertex bvert
  val (v5,g) = g |> G.add_vertex (mkX zero_angle)
  val (v6,g) = g |> G.add_vertex bvert
  val (_ ,g) = g |> G.add_edge dir_edge v4 v5
  val (_ ,g) = g |> G.add_edge dir_edge v5 v6
  val pat = g
  
  val g = G.empty
  (* circle *)
  val (v1,g) = g |> G.add_vertex bvert
  val (_ ,g) = g |> G.add_edge dir_edge v1 v1
  (* x to x to x *)
  val (v4,g) = g |> G.add_vertex (mkX zero_angle)
  val (v5,g) = g |> G.add_vertex (mkX zero_angle)
  val (v6,g) = g |> G.add_vertex (mkX zero_angle)
  val (_ ,g) = g |> G.add_edge dir_edge v4 v5
  val (_ ,g) = g |> G.add_edge dir_edge v5 v6
  val tgt = g

  val mseq = M.match pat tgt
  val _ = Testing.test "M.match - pat --> tgt" (assert_n_match 2 mseq) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR GREEDY MATCHING WITH CONCRETE GRAPHS PASSED!"
end


local (* Test matching with disjoint !-boxes *)
  open Test_MkG
  open Test_BGMatchUtil
  structure M = Test_GreedyMatcher
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR GREEDY MATCHING WITH DISJOINT !-BOXES PASSED!"
end
