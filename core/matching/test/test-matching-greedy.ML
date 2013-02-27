local (* Test matching with concrete graphs *)
  open Test_RG_MkG
  open Test_RG_BGMatchUtil
  structure M = Test_RG_GreedyMatcher

  val c1 = V.mk "c1"
  val in1 = V.mk "in1"
  val in2 = V.mk "in2"
  val out1 = V.mk "out1"
  val out2 = V.mk "out2"
  val x1 = V.mk "x1"
  val x2 = V.mk "x2"
  val x3 = V.mk "x3"

  val g = G.empty
  (* circle *)
  val g = g |> G.add_named_vertex c1 bvert
            |> G.doadd_edge dir_edge c1 c1
  (* bare wire *)
            |> G.add_named_vertex in1 bvert
            |> G.add_named_vertex out1 bvert
            |> G.doadd_edge dir_edge in1 out1
  (* x with two boundaries *)
            |> G.add_named_vertex in2 bvert
            |> G.add_named_vertex x1 (mkX zero_angle)
            |> G.add_named_vertex out2 bvert
            |> G.doadd_edge dir_edge in2 x1
            |> G.doadd_edge dir_edge x1 out2
  val pat = g
  
  val g = G.empty
  (* circle *)
  val g = g |> G.add_named_vertex c1 bvert
            |> G.doadd_edge dir_edge c1 c1
  (* x to x to x *)
            |> G.add_named_vertex x1 (mkX zero_angle)
            |> G.add_named_vertex x2 (mkX zero_angle)
            |> G.add_named_vertex x3 (mkX zero_angle)
            |> G.doadd_edge dir_edge x1 x2
            |> G.doadd_edge dir_edge x2 x3
  val tgt = g

  val _ = Testing.test "M.match - pat --> tgt"
          (fn () => assert_n_match 2 (M.match pat tgt) ()) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR GREEDY MATCHING WITH CONCRETE GRAPHS PASSED!"
end


local (* Test matching with disjoint !-boxes *)
  open Test_RG_MkG
  open Test_RG_BGMatchUtil
  structure M = Test_RG_GreedyMatcher

  val c1 = V.mk "c1"
  val c2 = V.mk "c2"
  val c3 = V.mk "c3"
  val in1 = V.mk "in1"
  val in2 = V.mk "in2"
  val out1 = V.mk "out1"
  val x1 = V.mk "x1"
  val x2 = V.mk "x2"
  val x3 = V.mk "x3"
  val z1 = V.mk "z1"

  val b1 = B.mk "B1"
  val b2 = B.mk "B2"

  val g = G.empty
  (* circle in a !-box *)
  val g = g |> G.add_named_vertex c1 bvert
            |> G.doadd_edge dir_edge c1 c1
            |> G.add_named_bbox b1
            |> G.doadd_to_bbox b1 (V.NSet.single c1)
  val circles_pat = g

  val g = G.empty
  (* one circle *)
  val g = g |> G.add_named_vertex c1 bvert
            |> G.doadd_edge dir_edge c1 c1
  val one_circle = g

  (* two circles *)
  val g = g |> G.add_named_vertex c2 bvert
            |> G.doadd_edge dir_edge c2 c2
  val two_circles = g

  (* three circles *)
  val g = g |> G.add_named_vertex c3 bvert
            |> G.doadd_edge dir_edge c3 c3
  val three_circles = g

  val g = G.empty
  (* circle and bare wire in a !-box *)
  val g = g |> G.add_named_vertex c1 bvert
            |> G.doadd_edge dir_edge c1 c1
            |> G.add_named_vertex in1 bvert
            |> G.add_named_vertex out1 bvert
            |> G.doadd_edge dir_edge in1 out1
            |> G.add_named_bbox b1
            |> G.doadd_to_bbox b1 (V.NSet.of_list [c1,in1,out1])
  val circle_wire_pat = g

  val g = G.empty
  (* RHS of spider pattern *)
  val g = g |> G.add_named_vertex in1 bvert
            |> G.add_named_vertex out1 bvert
            |> G.add_named_vertex x1 (mkX zero_angle)
            |> G.doadd_edge dir_edge in1 x1
            |> G.doadd_edge dir_edge x1 out1
            |> G.add_named_bbox b1
            |> G.add_named_bbox b2
            |> G.doadd_to_bbox b1 (V.NSet.single in1)
            |> G.doadd_to_bbox b2 (V.NSet.single out1)
  val x_any_ins_outs_pat = g

  val g = G.empty
  val g = g |> G.add_named_vertex x1 (mkX zero_angle)
  val single_x = g

  val g = g |> G.add_named_vertex out1 bvert
            |> G.doadd_edge dir_edge x1 out1
  val x_with_output = g

  val g = g |> G.add_named_vertex in1 bvert
            |> G.add_named_vertex in2 bvert
            |> G.doadd_edge dir_edge in1 x1
            |> G.doadd_edge dir_edge in2 x1
  val x_with_output_and_two_inputs = g

  val g = single_x
  val g = g |> G.add_named_vertex x2 (mkX zero_angle)
            |> G.doadd_edge dir_edge x1 x2
  val x_to_x = g

  val g = single_x
  val g = g |> G.doadd_edge dir_edge x1 x1
  val x_with_loop = g

  val g = G.empty
  (* Z connected to any number of Xs *)
  val g = g |> G.add_named_vertex z1 (mkZ zero_angle)
            |> G.add_named_vertex x1 (mkX zero_angle)
            |> G.doadd_edge undir_edge z1 x1
            |> G.add_named_bbox b1
            |> G.doadd_to_bbox b1 (V.NSet.single x1)
  val star_pat = g

  val g = G.empty
  val g = g |> G.add_named_vertex z1 (mkZ zero_angle)
  val single_z = g

  val g = g |> G.add_named_vertex x1 (mkX zero_angle)
            |> G.doadd_edge undir_edge z1 x1
  val star_one_spoke = g

  val g = g |> G.add_named_vertex x2 (mkX zero_angle)
            |> G.doadd_edge undir_edge z1 x2
  val star_two_spokes = g

  val g = g |> G.add_named_vertex x3 (mkX zero_angle)
            |> G.doadd_edge undir_edge z1 x3
  val star_three_spokes = g

  val _ = Testing.test "M.match - circles pattern --> empty"
          (fn () => assert_n_match 1 (M.match circles_pat G.empty) ()) ()

  val _ = Testing.test "M.match - circles pattern --> one circle"
          (fn () => assert_n_match 2 (M.match circles_pat one_circle) ()) ()

  val _ = Testing.test "M.match - circles pattern --> two circles"
          (fn () => assert_n_match 5 (M.match circles_pat two_circles) ()) ()

  val _ = Testing.test "M.match - circles pattern --> three circles"
          (fn () => assert_n_match 16 (M.match circles_pat three_circles) ()) ()

  val _ = Testing.test "M.match - circles+wire pattern --> one circle"
          (fn () => assert_n_match 1 (M.match circle_wire_pat one_circle) ()) ()

  val _ = Testing.test "M.match - circles+wire pattern --> two circles"
          (fn () => assert_n_match 3 (M.match circle_wire_pat two_circles) ()) ()

  val _ = Testing.test "M.match - x any ins or outs pattern --> one circle"
          (fn () => assert_n_match 0 (M.match x_any_ins_outs_pat one_circle) ()) ()

  val _ = Testing.test "M.match - x any ins or outs pattern --> single x vertex"
          (fn () => assert_n_match 1 (M.match x_any_ins_outs_pat single_x) ()) ()

  val _ = Testing.test "M.match - x any ins or outs pattern --> x->1"
          (fn () => assert_n_match 1 (M.match x_any_ins_outs_pat x_with_output) ()) ()

  val _ = Testing.test "M.match - x any ins or outs pattern --> 2->x->1"
          (fn () => assert_n_match 2 (M.match x_any_ins_outs_pat x_with_output_and_two_inputs) ()) ()

  val _ = Testing.test "M.match - x any ins or outs pattern --> x->x"
          (fn () => assert_n_match 2 (M.match x_any_ins_outs_pat x_to_x) ()) ()

  val _ = Testing.test "M.match - x any ins or outs pattern --> x with loop"
          (fn () => assert_n_match 1 (M.match x_any_ins_outs_pat x_with_loop) ()) ()

  val _ = Testing.test "M.match - star pattern --> single x"
          (fn () => assert_n_match 0 (M.match star_pat single_x) ()) ()

  val _ = Testing.test "M.match - star pattern --> single z"
          (fn () => assert_n_match 1 (M.match star_pat single_z) ()) ()

  val _ = Testing.test "M.match - star pattern --> one-spoke star"
          (fn () => assert_n_match 1 (M.match star_pat star_one_spoke) ()) ()

  val _ = Testing.test "M.match - star pattern --> two-spoke star"
          (fn () => assert_n_match 2 (M.match star_pat star_two_spokes) ()) ()

  val _ = Testing.test "M.match - star pattern --> three-spoke star"
          (fn () => assert_n_match 6 (M.match star_pat star_three_spokes) ()) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR GREEDY MATCHING WITH DISJOINT !-BOXES PASSED!"
end


local (* Test bbox expansion on self-loops *)

in

end


local (* Test matching with nested/overlapping !-boxes *)
  open Test_RG_MkG
  open Test_RG_BGMatchUtil
  structure M = Test_RG_GreedyMatcher

  val lvl0 = V.mk "l0"
  val lvl1a = V.mk "l1-a"
  val lvl1b = V.mk "l1-b"
  val lvl1c = V.mk "l1-c"
  val lvl2a = V.mk "l2-a"
  val lvl2b = V.mk "l2-b"
  val lvl2c = V.mk "l2-c"

  val b1 = B.mk "B1"
  val b2 = B.mk "B2"

  val g = G.empty
  val g = g |> G.add_named_vertex lvl0 (mkZ zero_angle)
            |> G.add_named_vertex lvl1a (mkX zero_angle)
            |> G.add_named_vertex lvl2a (mkX zero_angle)
            |> G.doadd_edge dir_edge lvl0 lvl1a
            |> G.doadd_edge dir_edge lvl1a lvl2a
            |> G.add_named_bbox b1
            |> G.add_named_bbox b2
            |> G.doadd_to_bbox b1 (V.NSet.of_list [lvl1a,lvl2a])
            |> G.doadd_to_bbox b2 (V.NSet.single lvl2a)
  val balanced_tree_pat = g

  val g = g |> G.set_bbox_parent b2 b1
  val arbitrary_tree_pat = g

  val g = G.empty
  val g = g |> G.add_named_vertex lvl0 (mkZ zero_angle)
  val min_tree = g

  val g = g |> G.add_named_vertex lvl1a (mkX zero_angle)
            |> G.doadd_edge dir_edge lvl0 lvl1a
  val tree_1 = g

  val g = g |> G.add_named_vertex lvl1b (mkX zero_angle)
            |> G.doadd_edge dir_edge lvl0 lvl1b
  val tree_2 = g

  val g = g |> G.add_named_vertex lvl1c (mkX zero_angle)
            |> G.doadd_edge dir_edge lvl0 lvl1c
  val tree_3 = g

  val g = tree_1
  val g = g |> G.add_named_vertex lvl2a (mkX zero_angle)
            |> G.doadd_edge dir_edge lvl1a lvl2a
  val tree_1_1 = g

  val g = g |> G.add_named_vertex lvl2b (mkX zero_angle)
            |> G.doadd_edge dir_edge lvl1a lvl2b
  val tree_1_2 = g

  val g = tree_2
  val g = g |> G.add_named_vertex lvl2a (mkX zero_angle)
            |> G.doadd_edge dir_edge lvl1a lvl2a
  val tree_2_1 = g

  val g = g |> G.add_named_vertex lvl2b (mkX zero_angle)
            |> G.doadd_edge dir_edge lvl1a lvl2b
  val tree_2_1_1 = g

  fun assert_n_warnings n ms = let
    val ws = Test_RG_BGMatchState.get_warnings ms
  in
    Testing.assert ((Int.toString n)^" warnings") (length ws = n)
  end

  val _ = Testing.test "M.match - balanced tree --> tree {}"
          (fn () => let val mseq = M.match balanced_tree_pat min_tree in
          assert_n_match 1 mseq ();
          assert_n_warnings 1 (Seq.hd mseq) end) ()

  val _ = Testing.test "M.match - arbitrary tree --> tree {}"
          (fn () => assert_n_match 1 (M.match arbitrary_tree_pat min_tree) ()) ()

  val _ = Testing.test "M.match - balanced tree --> tree {1}"
          (fn () => assert_n_match 1 (M.match balanced_tree_pat tree_1) ()) ()

  val _ = Testing.test "M.match - arbitrary tree --> tree {1}"
          (fn () => assert_n_match 1 (M.match arbitrary_tree_pat tree_1) ()) ()

  val _ = Testing.test "M.match - balanced tree --> tree {2}"
          (fn () => assert_n_match 2 (M.match balanced_tree_pat tree_2) ()) ()

  val _ = Testing.test "M.match - arbitrary tree --> tree {2}"
          (fn () => assert_n_match 2 (M.match arbitrary_tree_pat tree_2) ()) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR GREEDY MATCHING WITH OVERLAPPING !-BOXES PASSED!"
end
