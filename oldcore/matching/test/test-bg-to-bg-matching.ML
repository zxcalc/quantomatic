(*
 * Tests matching of !-graphs against !-graphs
 *)
functor BG_To_BG_Matching_Tests(
  Tester : BG_MATCH_TESTER
) =
struct
  structure GTools = Test_Bang_Graph_Tools(Tester.G)
  open Tester
  open GTools

  (*
   * Disjoint !-boxes
   *)
  val circles_pat = G.empty
         (* circle in a !-box *)
      |> add_wv "cv"
      |> add_dir_eunit1 "ce" "cv" "cv"
      |> add_bbox "B1" ["cv"]

  val circles_pat_exp = circles_pat
      |> add_wv "c1"
      |> add_dir_eunit1 "ce1" "c1" "c1"

  val circles_pat_copy = circles_pat_exp
      |> add_bbox "B2" ["c1"]

  val cpy_rhs = G.empty
      |> add_vunit1 "v"
      |> add_wv "w"
      |> add_dir_eunit1 "e" "v" "w"
      |> add_bbox "b" ["v","w"]

  val cpy_rhs_exp = cpy_rhs
      |> add_vunit1 "v1"
      |> add_wv "w1"
      |> add_dir_eunit1 "e1" "v1" "w1"

  val cpy_rhs_copy = cpy_rhs_exp
      |> add_bbox "b1" ["v1","w1"]

  val cpy_lhs = G.empty
      |> add_vunit1 "v1"
      |> add_vunit2 "v2"
      |> add_wv "w1"
      |> add_wv "w2"
      |> add_dir_eunit1 "e1" "v1" "w1"
      |> add_dir_eunit1 "e2" "w1" "v2"
      |> add_dir_eunit1 "e3" "v2" "w2"
      |> add_bbox "b1" ["w2"]

  val cpy_lhs_exp = cpy_lhs
      |> add_wv "w3"
      |> add_dir_eunit1 "e4" "v2" "w3"

  val cpy_lhs_copy = cpy_lhs_exp
      |> add_bbox "b2" ["w3"]

  val _ = Testing.test "M.match - circles pattern --> circles pattern"
                       (find_n_correct_matches 2) (circles_pat,circles_pat)
  val _ = Testing.test "M.match - circles pattern --> EXP(circles pattern)"
                       (find_n_correct_matches 4) (circles_pat,circles_pat_exp)
  val _ = Testing.test "M.match - circles pattern --> COPY(circles pattern)"
                       (find_n_correct_matches 5) (circles_pat,circles_pat_copy)
  val _ = Testing.test "M.match - copy law RHS --> copy law RHS"
                       (find_n_correct_matches 2) (cpy_rhs,cpy_rhs)
  val _ = Testing.test "M.match - copy law RHS --> EXP(copy law RHS)"
                       (find_n_correct_matches 4) (cpy_rhs,cpy_rhs_exp)
  val _ = Testing.test "M.match - copy law RHS --> COPY(copy law RHS)"
                       (find_n_correct_matches 5) (cpy_rhs,cpy_rhs_copy)
  val _ = Testing.test "M.match - copy law LHS --> copy law LHS"
                       (find_n_correct_matches 1) (cpy_lhs,cpy_lhs)
  val _ = Testing.test "M.match - copy law LHS --> EXP(copy law LHS)"
                       (find_n_correct_matches 1) (cpy_lhs,cpy_lhs_exp)
  val _ = Testing.test "M.match - copy law LHS --> COPY(copy law LHS)"
                       (find_n_correct_matches 2) (cpy_lhs,cpy_lhs_copy)

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
      |> add_bbox "B1" ["l2-a"]

  val arbitrary_tree_pat = balanced_tree_pat
      |> G.add_child_to_bbox (B.mk "B0") (B.mk "B1")

  val _ = Testing.test "M.match - balanced tree --> balanced tree"
                       (find_n_correct_matches 1) (balanced_tree_pat,balanced_tree_pat)
  val _ = Testing.test "M.match - arbitrary tree --> arbitrary tree"
                       (find_n_correct_matches 1) (arbitrary_tree_pat,arbitrary_tree_pat)
  val _ = Testing.test "M.match - arbitrary tree --> balanced tree"
                       (find_n_correct_matches 0) (arbitrary_tree_pat,balanced_tree_pat)
  val _ = Testing.test "M.match - balanced tree --> arbitrary tree"
                       (find_n_correct_matches 0) (balanced_tree_pat,arbitrary_tree_pat)

  val _ = Testing.assert_no_failed_tests()
end


