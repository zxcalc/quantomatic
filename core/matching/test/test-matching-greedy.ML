local (* Test matching with concrete graphs *)
  open Test_Bang_Match_Tools
  structure M = Test_GreedyMatcher

  val pat = G.empty
             (* circle *)
          |> add_wv "cv"
          |> add_dir_eunit1 "ce" "cv" "cv"
             (* bare wire *)
          |> add_wv "bw-in"
          |> add_wv "bw-out"
          |> add_dir_eunit1 "bw" "bw-in" "bw-out"
             (* nv with two boundaries *)
          |> add_wv "in"
          |> add_vunit1 "nv"
          |> add_wv "out"
          |> add_dir_eunit1 "i" "in" "nv"
          |> add_dir_eunit1 "o" "nv" "out";

  val tgt = G.empty
             (* circle *)
          |> add_wv "cv"
          |> add_dir_eunit1 "ce" "cv" "cv"
             (* nv to nv to nv *)
          |> add_vunit1 "nv1"
          |> add_vunit1 "nv2"
          |> add_vunit1 "nv3"
          |> add_dir_eunit1 "e1" "nv1" "nv2"
          |> add_dir_eunit1 "e2" "nv2" "nv3";

  val _ = Testing.test "M.match - pat --> tgt"
          (fn () => assert_n_match 2 (M.match pat tgt) ()) ()
in
  val _ = Testing.assert_no_failed_tests();
end


local (* Test matching with disjoint !-boxes *)
  open Test_Bang_Match_Tools
  structure M = Test_GreedyMatcher

  val circles_pat = G.empty
         (* circle in a !-box *)
      |> add_wv "cv"
      |> add_dir_eunit1 "ce" "cv" "cv"
      |> add_bbox "B1" ["cv"];

  val one_circle = G.empty
      |> add_wv "c1"
      |> add_dir_eunit1 "ce1" "c1" "c1";
  val two_circles = one_circle
      |> add_wv "c2"
      |> add_dir_eunit1 "ce2" "c2" "c2";
  val three_circles = two_circles
      |> add_wv "c3"
      |> add_dir_eunit1 "ce3" "c3" "c3";

  val circle_wire_pat = G.empty
         (* circle *)
      |> add_wv "cv"
      |> add_dir_eunit1 "ce" "cv" "cv"
         (* bare wire *)
      |> add_wv "in1"
      |> add_wv "out1"
      |> add_dir_eunit1 "bw" "in1" "out1"
         (* in a !-box *)
      |> add_bbox "B1" ["cv","in1","out1"];

  val v_any_ins_outs_pat = G.empty
         (* RHS of spider pattern *)
      |> add_wv "in1"
      |> add_wv "out1"
      |> add_vunit1 "v1"
      |> add_dir_eunit1 "e1" "in1" "v1"
      |> add_dir_eunit1 "e2" "v1" "out1"
      |> add_bbox "B1" ["in1"]
      |> add_bbox "B2" ["out1"];

  val single_v = G.empty
      |> add_vunit1 "v1";

  val v_out = single_v
      |> add_wv "out1"
      |> add_dir_eunit1 "o1" "v1" "out1";

  val in_in_v_out = v_out
      |> add_wv "in1"
      |> add_wv "in2"
      |> add_dir_eunit1 "i1" "in1" "v1"
      |> add_dir_eunit1 "i2" "in2" "v1";

  val v_to_v = single_v
      |> add_vunit1 "v2"
      |> add_dir_eunit1 "e1" "v1" "v2";

  val loop = single_v
      |> add_dir_eunit1 "e1" "v1" "v1";

  val star_pat = G.empty
      |> add_vunit1 "centre"
      |> add_vunit2 "spoke"
      |> add_undir_eunit1 "e" "centre" "spoke"
      |> add_bbox "box" ["spoke"];

  val lonely_spoke = G.empty
      |> add_vunit2 "s";

  val star_zero = G.empty
      |> add_vunit1 "c";

  val star_one = star_zero
      |> add_vunit2 "s1"
      |> add_undir_eunit1 "e1" "c" "s1";

  val star_two = star_one
      |> add_vunit2 "s2"
      |> add_undir_eunit1 "e2" "c" "s2";

  val star_three = star_two
      |> add_vunit2 "s3"
      |> add_undir_eunit1 "e3" "c" "s3";

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

  val _ = Testing.test "M.match - v any ins or outs pattern --> one circle"
          (fn () => assert_n_match 0 (M.match v_any_ins_outs_pat one_circle) ()) ()

  val _ = Testing.test "M.match - v any ins or outs pattern --> single vertex"
          (fn () => assert_n_match 1 (M.match v_any_ins_outs_pat single_v) ()) ()

  val _ = Testing.test "M.match - v any ins or outs pattern --> v->1"
          (fn () => assert_n_match 1 (M.match v_any_ins_outs_pat v_out) ()) ()

  val _ = Testing.test "M.match - v any ins or outs pattern --> 2->v->1"
          (fn () => assert_n_match 2 (M.match v_any_ins_outs_pat in_in_v_out) ()) ()

  val _ = Testing.test "M.match - v any ins or outs pattern --> v->v"
          (fn () => assert_n_match 2 (M.match v_any_ins_outs_pat v_to_v) ()) ()

  val _ = Testing.test "M.match - v any ins or outs pattern --> v with loop"
          (fn () => assert_n_match 1 (M.match v_any_ins_outs_pat loop) ()) ()

  val _ = Testing.test "M.match - star pattern --> lonely spoke"
          (fn () => assert_n_match 0 (M.match star_pat lonely_spoke) ()) ()

  val _ = Testing.test "M.match - star pattern --> zero-spoke star"
          (fn () => assert_n_match 1 (M.match star_pat star_zero) ()) ()

  val _ = Testing.test "M.match - star pattern --> one-spoke star"
          (fn () => assert_n_match 1 (M.match star_pat star_one) ()) ()

  val _ = Testing.test "M.match - star pattern --> two-spoke star"
          (fn () => assert_n_match 2 (M.match star_pat star_two) ()) ()

  val _ = Testing.test "M.match - star pattern --> three-spoke star"
          (fn () => assert_n_match 6 (M.match star_pat star_three) ()) ()
in
  val _ = Testing.assert_no_failed_tests();
end


local (* Test bbox expansion on self-loops *)
  open Test_Bang_Match_Tools
  structure M = Test_GreedyMatcher

  (* TODO *)
in

end


local (* Test matching with nested/overlapping !-boxes *)
  open Test_Bang_Match_Tools
  structure M = Test_GreedyMatcher

  val balanced_tree_pat = G.empty
      |> add_vunit1 "l0"
      |> add_vunit2 "l1-a"
      |> add_vunit2 "l2-a"
      |> add_dir_eunit1 "e1" "l0" "l1-a"
      |> add_dir_eunit1 "e2" "l1-a" "l2-a"
      |> add_bbox "B0" ["l1-a","l2-a"]
      |> add_bbox "B1" ["l2-a"];

  val arbitrary_tree_pat = balanced_tree_pat
      |> G.set_bbox_parent (B.mk "B1") (B.mk "B0");

  val min_tree = G.empty
      |> add_vunit1 "l0";

  val tree_1 = min_tree
      |> add_vunit2 "l1-a"
      |> add_dir_eunit1 "e1" "l0" "l1-a";

  val tree_2 = tree_1
      |> add_vunit2 "l1-b"
      |> add_dir_eunit1 "e2" "l0" "l1-b";

  val tree_3 = tree_2
      |> add_vunit2 "l1-c"
      |> add_dir_eunit1 "e3" "l0" "l1-c";

  val tree_1_1 = tree_1
      |> add_vunit2 "l2-a"
      |> add_dir_eunit1 "e11" "l1-a" "l2-a";

  val tree_1_2 = tree_1_1
      |> add_vunit2 "l2-b"
      |> add_dir_eunit1 "e12" "l1-a" "l2-b";

  val tree_2_1 = tree_2
      |> add_vunit2 "l2-a"
      |> add_dir_eunit1 "e21" "l1-b" "l2-a";

  val tree_2_1_1 = tree_2_1
      |> add_vunit2 "l2-b"
      |> add_dir_eunit1 "e22" "l1-b" "l2-b";

  fun assert_n_warnings n ms = let
    val ws = MS.get_warnings ms
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
end
