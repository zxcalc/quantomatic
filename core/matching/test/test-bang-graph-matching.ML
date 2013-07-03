(*
 * Tests matching of !-graphs against string graphs
 *)
functor Bang_Graph_Matching_Tests(
  Matcher : TEST_BG_MATCH_SEARCH
) =
struct
  structure Tools = Test_Bang_Match_Tools(Matcher);

  (* test matching with concrete graphs *)
  structure SG_Matcher_Tests = String_Graph_Matching_Tests(Tools);

  open Tools;

  (*
   * Disjoint !-boxes
   *)
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
                       (find_n_correct_matches 1) (circles_pat,G.empty);
  val _ = Testing.test "M.match - circles pattern --> one circle"
                       (find_n_correct_matches 2) (circles_pat,one_circle);
  val _ = Testing.test "M.match - circles pattern --> two circles"
                       (find_n_correct_matches 5) (circles_pat,two_circles);
  val _ = Testing.test "M.match - circles pattern --> three circles"
                       (find_n_correct_matches 16) (circles_pat,three_circles);
  val _ = Testing.test "M.match - circles+wire pattern --> one circle"
                       (find_n_correct_matches 1) (circle_wire_pat,one_circle);
  val _ = Testing.test "M.match - circles+wire pattern --> two circles"
                       (find_n_correct_matches 3) (circle_wire_pat,two_circles);
  val _ = Testing.test "M.match - v any ins or outs pattern --> one circle"
                       (find_n_correct_matches 0) (v_any_ins_outs_pat,one_circle);
  val _ = Testing.test "M.match - v any ins or outs pattern --> single vertex"
                       (find_n_correct_matches 1) (v_any_ins_outs_pat,single_v);
  val _ = Testing.test "M.match - v any ins or outs pattern --> v->1"
                       (find_n_correct_matches 1) (v_any_ins_outs_pat,v_out);
  val _ = Testing.test "M.match - v any ins or outs pattern --> 2->v->1"
                       (find_n_correct_matches 2) (v_any_ins_outs_pat,in_in_v_out);
  val _ = Testing.test "M.match - v any ins or outs pattern --> v->v"
                       (find_n_correct_matches 2) (v_any_ins_outs_pat,v_to_v);
  val _ = Testing.test "M.match - v any ins or outs pattern --> v with loop"
                       (find_n_correct_matches 1) (v_any_ins_outs_pat,loop);
  val _ = Testing.test "M.match - star pattern --> lonely spoke"
                       (find_n_correct_matches 0) (star_pat,lonely_spoke);
  val _ = Testing.test "M.match - star pattern --> zero-spoke star"
                       (find_n_correct_matches 1) (star_pat,star_zero);
  val _ = Testing.test "M.match - star pattern --> one-spoke star"
                       (find_n_correct_matches 1) (star_pat,star_one);
  val _ = Testing.test "M.match - star pattern --> two-spoke star"
                       (find_n_correct_matches 2) (star_pat,star_two);
  val _ = Testing.test "M.match - star pattern --> three-spoke star"
                       (find_n_correct_matches 6) (star_pat,star_three);


  (*
   * Expansion on self-loops
   *)
  (* TODO *)


  (*
   * Nested/overlapping !-boxes
   *)

  val balanced_tree_pat = G.empty
      |> add_vunit1 "l0"
      |> add_vunit2 "l1-a"
      |> add_vunit2 "l2-a"
      |> add_dir_eunit1 "e1" "l0" "l1-a"
      |> add_dir_eunit1 "e2" "l1-a" "l2-a"
      |> add_bbox "B0" ["l1-a","l2-a"]
      |> add_bbox "B1" ["l2-a"];

  val arbitrary_tree_pat = balanced_tree_pat
      |> G.add_child_to_bbox (B.mk "B0") (B.mk "B1");

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

  val _ = Testing.test "M.match - balanced tree --> tree {}"
          ((assert_n_wild_bboxes 1) o List.hd o (find_n_correct_matches 1))
          (balanced_tree_pat,min_tree);
  val _ = Testing.test "M.match - arbitrary tree --> tree {}"
                       (find_n_correct_matches 1) (arbitrary_tree_pat,min_tree);
  val _ = Testing.test "M.match - balanced tree --> tree {1}"
                       (find_n_correct_matches 1) (balanced_tree_pat,tree_1);
  val _ = Testing.test "M.match - arbitrary tree --> tree {1}"
                       (find_n_correct_matches 1) (arbitrary_tree_pat,tree_1);
  val _ = Testing.test "M.match - balanced tree --> tree {2}"
                       (find_n_correct_matches 2) (balanced_tree_pat,tree_2);
  val _ = Testing.test "M.match - arbitrary tree --> tree {2}"
                       (find_n_correct_matches 2) (arbitrary_tree_pat,tree_2);


  val _ = Testing.assert_no_failed_tests();
end;

structure Greedy_Matcher_Tests = Bang_Graph_Matching_Tests(Test_GreedyMatcher);

