local (* Test match state functionality *)
  open Test_Bang_Match_Tools
  structure M = Test_GreedyMatcher

  val g = G.empty |> add_wv "v1"
                  |> add_vunit1 "v2"
                  |> add_wv "v3"
                  |> add_dir_eunit1 "e1" "v1" "v2"
                  |> add_dir_eunit1 "e2" "v2" "v3"
                  |> add_bbox "B0" ["v1"]
                  |> add_bbox "B1" ["v3"]
  val spider_pat = g

  val g = g |> add_vunit2 "v4"
            |> add_to_bbox "B0" ["v4"]
            |> add_to_bbox "B1" ["v4"]
  val spider_pat_plus_v4 = g

  val _ = Testing.test "Original graphs coherent"
          assert_boundary_coherent (spider_pat,spider_pat_plus_v4)

  val ms = MS.init spider_pat G.empty
            |> MS.set_pat_avoids (G.get_vnames spider_pat_plus_v4)
            |> MS.set_pat_tracked (V.NSet.of_list (map V.mk ["v1","v3"]))

  val ms = ms |> MS.expand_pat_bbox (B.mk "B0")
              |> MS.expand_pat_bbox (B.mk "B0")
              |> MS.kill_pat_bbox (B.mk "B0")
              |> MS.expand_pat_bbox (B.mk "B1")
              |> MS.kill_pat_bbox (B.mk "B1")
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

