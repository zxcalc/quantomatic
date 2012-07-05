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
  
  val g = G.empty
  (* circle in a !-box *)
  val (v1,g) = g |> G.add_vertex bvert
  val (_ ,g) = g |> G.add_edge dir_edge v1 v1
  val (b1,g) = g |> G.add_bbox
  val g      = g |> G.add_to_bbox b1 (V.NSet.single v1)
  val circles_pat = g

  val g = G.empty
  (* one circle *)
  val (v1,g) = g |> G.add_vertex bvert
  val (_ ,g) = g |> G.add_edge dir_edge v1 v1
  val one_circle = g

  (* two circles *)
  val (v2,g) = g |> G.add_vertex bvert
  val (_ ,g) = g |> G.add_edge dir_edge v2 v2
  val two_circles = g

  (* three circles *)
  val (v3,g) = g |> G.add_vertex bvert
  val (_ ,g) = g |> G.add_edge dir_edge v3 v3
  val three_circles = g

  val mseq = M.match circles_pat G.empty
  val _ = Testing.test "M.match - circles pattern --> empty" (assert_n_match 1 mseq) ()

  val mseq = M.match circles_pat one_circle
  val _ = Testing.test "M.match - circles pattern --> one circle" (assert_n_match 2 mseq) ()

  val mseq = M.match circles_pat two_circles
  val _ = Testing.test "M.match - circles pattern --> two circles" (assert_n_match 5 mseq) ()

  val mseq = M.match circles_pat three_circles
  val _ = Testing.test "M.match - circles pattern --> three circles" (assert_n_match 16 mseq) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR GREEDY MATCHING WITH DISJOINT !-BOXES PASSED!"
end
