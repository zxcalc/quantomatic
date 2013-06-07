local (* Test  functionality *)
  structure Tools = Test_Bang_Graph_Tools(Test_Bang_Graph);
  open Tools;
  structure BBOp = Test_BBoxOperableGraph;

  local
    structure Rule = BangGraphRule(G);
  in
    fun assert_boundary_coherent (g1,g2) = let
      (* easiest just to try to make a rule out of it *)
      val _ = Rule.mk (g1,g2)
    in
      (g1,g2)
    end
  end

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

  val bbop = BBOp.init spider_pat
                       (G.get_vnames spider_pat_plus_v4)
                       (V.NSet.of_list (map V.mk ["v1","v3"]))

  val bbop = bbop |> BBOp.expand_bbox (B.mk "B0")
                  |> BBOp.expand_bbox (B.mk "B0")
                  |> BBOp.kill_bbox (B.mk "B0")
                  |> BBOp.expand_bbox (B.mk "B1")
                  |> BBOp.kill_bbox (B.mk "B1")
  val new_g = BBOp.get_current_graph bbop

  val _ = Testing.test "Correct number of inputs in successor graph"
          (fn _ => V.NSet.cardinality (G.get_inputs new_g) = 2) ()
  val _ = Testing.test "Correct number of outputs in successor graph"
          (fn _ => V.NSet.cardinality (G.get_outputs new_g) = 1) ()

  val _ = Testing.test "Replay works on original graph"
            (BBOp.replay_bbox_ops bbop) spider_pat
  val rhs = Testing.test "Replay works on boundary-coherent graph"
            (BBOp.replay_bbox_ops bbop) spider_pat_plus_v4

  val _ = case rhs
            of SOME g => Testing.test "Expanded graphs coherent"
                         assert_boundary_coherent (new_g,g)
             | NONE => NONE
in
  val _ = Testing.assert_no_failed_tests();
end

