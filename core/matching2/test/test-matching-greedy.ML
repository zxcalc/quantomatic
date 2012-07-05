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

  val g = G.empty
  (* circle and bare wire in a !-box *)
  val (v1,g) = g |> G.add_vertex bvert
  val (v2,g) = g |> G.add_vertex bvert
  val (v3,g) = g |> G.add_vertex bvert
  val (_ ,g) = g |> G.add_edge dir_edge v1 v1
  val (_ ,g) = g |> G.add_edge dir_edge v2 v3
  val (b1,g) = g |> G.add_bbox
  val g      = g |> G.add_to_bbox b1 (V.NSet.single v1)
                 |> G.add_to_bbox b1 (V.NSet.single v2)
                 |> G.add_to_bbox b1 (V.NSet.single v3)
  val circle_wire_pat = g

  val g = G.empty
  (* RHS of spider pattern *)
  val (v1,g) = g |> G.add_vertex bvert
  val (v2,g) = g |> G.add_vertex (mkX zero_angle)
  val (v3,g) = g |> G.add_vertex bvert
  val (_ ,g) = g |> G.add_edge dir_edge v1 v2
  val (_ ,g) = g |> G.add_edge dir_edge v2 v3
  val (b1,g) = g |> G.add_bbox
  val (b2,g) = g |> G.add_bbox
  val g      = g |> G.add_to_bbox b1 (V.NSet.single v1)
                 |> G.add_to_bbox b2 (V.NSet.single v3)
  val spider_rhs_pat = g

  val g = G.empty
  val (v1,g) = g |> G.add_vertex (mkX zero_angle)
  val single_x = g

  val (v2,g) = g |> G.add_vertex bvert
  val (_ ,g) = g |> G.add_edge dir_edge v1 v2
  val x_with_output = g

  val (v3,g) = g |> G.add_vertex bvert
  val (v4,g) = g |> G.add_vertex bvert
  val (_ ,g) = g |> G.add_edge dir_edge v3 v1
  val (_ ,g) = g |> G.add_edge dir_edge v4 v1
  val x_with_output_and_two_inputs = g

  val g = single_x
  val (v2,g) = g |> G.add_vertex (mkX zero_angle)
  val (_ ,g) = g |> G.add_edge dir_edge v1 v2
  val x_to_x = g

  val g = single_x
  val (_ ,g) = g |> G.add_edge dir_edge v1 v1
  val x_with_loop = g

  val g = G.empty
  (* Z connected to any number of Xs *)
  val (v1,g) = g |> G.add_vertex (mkZ zero_angle)
  val (v2,g) = g |> G.add_vertex (mkX zero_angle)
  val (_ ,g) = g |> G.add_edge undir_edge v1 v2
  val (b1,g) = g |> G.add_bbox
  val g      = g |> G.add_to_bbox b1 (V.NSet.single v2)
  val star_pat = g

  val g = G.empty
  val (v1,g) = g |> G.add_vertex (mkZ zero_angle)
  val single_z = g

  val (v2,g) = g |> G.add_vertex (mkX zero_angle)
  val (_ ,g) = g |> G.add_edge undir_edge v1 v2
  val star_one_spoke = g

  val (v3,g) = g |> G.add_vertex (mkX zero_angle)
  val (_ ,g) = g |> G.add_edge undir_edge v1 v3
  val star_two_spokes = g

  val (v4,g) = g |> G.add_vertex (mkX zero_angle)
  val (_ ,g) = g |> G.add_edge undir_edge v1 v4
  val star_three_spokes = g

  val mseq = M.match circles_pat G.empty
  val _ = Testing.test "M.match - circles pattern --> empty" (assert_n_match 1 mseq) ()

  val mseq = M.match circles_pat one_circle
  val _ = Testing.test "M.match - circles pattern --> one circle" (assert_n_match 2 mseq) ()

  val mseq = M.match circles_pat two_circles
  val _ = Testing.test "M.match - circles pattern --> two circles" (assert_n_match 5 mseq) ()

  val mseq = M.match circles_pat three_circles
  val _ = Testing.test "M.match - circles pattern --> three circles" (assert_n_match 16 mseq) ()

  val mseq = M.match circle_wire_pat one_circle
  val _ = Testing.test "M.match - circles+wire pattern --> one circle" (assert_n_match 1 mseq) ()

  val mseq = M.match circle_wire_pat two_circles
  val _ = Testing.test "M.match - circles+wire pattern --> two circles" (assert_n_match 3 mseq) ()

  val mseq = M.match spider_rhs_pat one_circle
  val _ = Testing.test "M.match - spider (RHS) pattern --> one circle" (assert_n_match 0 mseq) ()

  val mseq = M.match spider_rhs_pat single_x
  val _ = Testing.test "M.match - spider (RHS) pattern --> single x vertex" (assert_n_match 1 mseq) ()

  val mseq = M.match spider_rhs_pat x_with_output
  val _ = Testing.test "M.match - spider (RHS) pattern --> x->1" (assert_n_match 1 mseq) ()

  val mseq = M.match spider_rhs_pat x_with_output_and_two_inputs
  val _ = Testing.test "M.match - spider (RHS) pattern --> 2->x->1" (assert_n_match 2 mseq) ()

  val mseq = M.match spider_rhs_pat x_to_x
  val _ = Testing.test "M.match - spider (RHS) pattern --> x->x" (assert_n_match 2 mseq) ()

  val mseq = M.match spider_rhs_pat x_with_loop
  val _ = Testing.test "M.match - spider (RHS) pattern --> x with loop" (assert_n_match 1 mseq) ()

  val mseq = M.match star_pat single_x
  val _ = Testing.test "M.match - star pattern --> single x" (assert_n_match 0 mseq) ()

  val mseq = M.match star_pat single_z
  val _ = Testing.test "M.match - star pattern --> single z" (assert_n_match 1 mseq) ()

  val mseq = M.match star_pat star_one_spoke
  val _ = Testing.test "M.match - star pattern --> one-spoke star" (assert_n_match 1 mseq) ()

  val mseq = M.match star_pat star_two_spokes
  val _ = Testing.test "M.match - star pattern --> two-spoke star" (assert_n_match 2 mseq) ()

  val mseq = M.match star_pat star_three_spokes
  val _ = Testing.test "M.match - star pattern --> three-spoke star" (assert_n_match 6 mseq) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR GREEDY MATCHING WITH DISJOINT !-BOXES PASSED!"
end
