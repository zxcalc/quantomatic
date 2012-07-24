local (* Test matching with concrete graphs *)
  open Test_MkG
  open Test_BGMatchUtil
  structure M = Test_GreedyMatcher
  structure MS = Test_BGMatchState

  val v1 = V.mk "v1"
  val v2 = V.mk "v2"
  val v3 = V.mk "v3"
  val v4 = V.mk "v4"
  val b0 = B.mk "B0"
  val b1 = B.mk "B1"

  val g = G.empty
  val g = g |> G.add_named_vertex v1 bvert
            |> G.add_named_vertex v2 (mkX zero_angle)
            |> G.add_named_vertex v3 bvert
            |> G.add_named_edge (E.mk "e1") dir_edge v1 v2
            |> G.add_named_edge (E.mk "e2") dir_edge v2 v3
            |> G.add_named_bbox b0
            |> G.add_named_bbox b1
            |> G.doadd_to_bbox b0 (V.NSet.single v1)
            |> G.doadd_to_bbox b1 (V.NSet.single v3)
  val spider_pat = g

  val g = g |> G.add_named_vertex v4 (mkZ zero_angle)
            |> G.doadd_to_bbox b0 (V.NSet.single v4)
            |> G.doadd_to_bbox b1 (V.NSet.single v4)
  val spider_pat_plus_v4 = g

  val _ = Testing.test "Original graphs coherent"
          assert_boundary_coherent (spider_pat,spider_pat_plus_v4)

  val ms = MS.init spider_pat G.empty
            |> MS.set_pat_avoids (G.get_vnames spider_pat_plus_v4)
            |> MS.set_pat_tracked (V.NSet.of_list [v1,v3])

  val ms = ms |> MS.expand_pat_bbox b0
              |> MS.expand_pat_bbox b0
              |> MS.kill_pat_bbox b0
              |> MS.expand_pat_bbox b1
              |> MS.kill_pat_bbox b1
  val pat = MS.get_pat ms

  val _ = Testing.test "Correct number of inputs in pat"
          (fn _ => V.NSet.cardinality (G.get_inputs pat) = 2) ()
  val _ = Testing.test "Correct number of outputs in pat"
          (fn _ => V.NSet.cardinality (G.get_outputs pat) = 1) ()

  val _ = Testing.test "Replay works on original graph"
            (MS.replay_pat_bbox_ops ms) spider_pat
  val SOME rhs = Testing.test "Replay works on boundary-coherent graph"
            (MS.replay_pat_bbox_ops ms) spider_pat_plus_v4

  val _ = Testing.test "Expanded graphs coherent"
          assert_boundary_coherent (pat,rhs)
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR !-BOX MATCH STATE UTILS PASSED!"
end

