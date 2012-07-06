(* testing nested !-boxes *)

structure Test_BBox_Util =
struct
  open Test_MkG
  fun num_bboxes g = B.NSet.cardinality (G.get_bboxes g)
  
  fun assert_n_bboxes g n () =
    if (num_bboxes g) = n
    then () else raise ERROR(
      "expected: " ^ Int.toString n ^ " bboxes, "^
      "got: " ^ Int.toString (num_bboxes g))
end

local
  open Test_BBox_Util;
  
  val g = G.empty
  (* balanced tree of depth 3 *)
  val (v1,g) = g |> G.add_vertex (mkZ zero_angle)
  val (v2,g) = g |> G.add_vertex (mkZ zero_angle)
  val (v3,g) = g |> G.add_vertex (mkZ zero_angle)
  val (_ ,g) = g |> G.add_edge undir_edge v1 v2
  val (_ ,g) = g |> G.add_edge undir_edge v2 v3
  val (b1,g) = g |> G.add_bbox
  val (b2,g) = g |> G.add_bbox
  val g      = g |> G.add_to_bbox b1 (V.NSet.of_list [v2,v3])
  val g      = g |> G.add_to_bbox b2 (V.NSet.of_list [v3])
  val balanced = g

  (* unbalenced tree of depth 3 *)
  val unbalanced = g |> G.set_bbox_parent b2 b1
  (*val _ = G.print unbalanced*)
  
  val _ = Testing.test "unbalanced.b2 should have 1 parent"
    (fn() => if G.bbox_has_parent unbalanced b2
             then () else raise ERROR("has no parent")) ()
  
  val utree_drop = g |> G.drop_bbox b1
  val _ = Testing.test "utree_drop.b2 should have 0 parents"
    (fn() => if G.bbox_has_parent utree_drop b2
             then raise ERROR("still has a parent") else ()) ()
  
  val utree_outer_kill = unbalanced |> G.kill_bbox b1
  (*val _ = G.print utree_outer_kill*)
  
  val _ = Testing.test "utree_outer_kill should have no bboxes"
    (assert_n_bboxes utree_outer_kill 0) ()
    
  val utree_inner_kill = unbalanced |> G.kill_bbox b2
  (*val _ = G.print utree_inner_kill*)
  
  val (_,utree_inner_copy) = unbalanced |> G.fresh_copy1_bbox b2
  val (b1',utree_outer_copy) = unbalanced |> G.fresh_copy1_bbox b1
  
  (* get a hold of the new child *)
  (*val b2' = case BMap.lookup_codf (G.get_bbox_parent_rel utree_outer_copy) b1'
              of SOME (*.....*)*)
  
  val _ = Testing.test "utree_outer_kill should have no bboxes"
    (assert_n_bboxes utree_outer_kill 0) ()
    
  val _ = Testing.test "utree_inner_kill should have 1 bbox"
    (assert_n_bboxes utree_inner_kill 1) ()
    
  val _ = Testing.test "utree_inner_copy should have 3 bboxes"
    (assert_n_bboxes utree_inner_copy 3) ()
    
  val _ = Testing.test "utree_outer_copy should have 4 bboxes"
    (assert_n_bboxes utree_outer_copy 4) ()
  
  val btree100 = balanced |> G.kill_bbox b1 |> G.kill_bbox b2
  
  (* TODO: write me *)
  val _ = Testing.test "btree100 should have 1 vertex" (fn() => ()) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR NESTED BANG-BOX EXPANSION PASSED!"
end