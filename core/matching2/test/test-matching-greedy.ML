local (* Test matching with concrete graphs *)
  open Test_MkG
  open Test_BGMatchUtil
  structure M = Test_GreedyMatcher
  
  val one_x = G.empty
  val (_,one_x) = one_x |> G.add_named_vertex (V.mk "x1") (mkX zero_angle)
  
  val x_to_x = G.empty
  val (x2,x_to_x) = x_to_x |> G.add_named_vertex (V.mk "x2") (mkX zero_angle)
  val (x3,x_to_x) = x_to_x |> G.add_named_vertex (V.mk "x3") (mkX zero_angle)
  val (_,x_to_x)  = x_to_x |> G.add_edge dir_edge x2 x3
  
  val x_x_loop = x_to_x
  val (_,x_x_loop)  = x_x_loop |> G.add_edge dir_edge x3 x2
  
  val x_to_b = G.empty
  val (x4,x_to_b) = x_to_b |> G.add_named_vertex (V.mk "x4") (mkX zero_angle)
  val (b1,x_to_b) = x_to_b |> G.add_named_vertex (V.mk "b1") bvert
  val (_,x_to_b)  = x_to_b |> G.add_edge dir_edge x4 b1
  
  val b_to_x = G.empty
  val (b2,b_to_x) = b_to_x |> G.add_named_vertex (V.mk "b2") bvert
  val (x5,b_to_x) = b_to_x |> G.add_named_vertex (V.mk "x5") (mkX zero_angle)
  val (_,b_to_x)  = b_to_x |> G.add_edge dir_edge b2 x5
  
  val b_to_x_plus_bw = b_to_x
  val (b2,b_to_x_plus_bw) = b_to_x_plus_bw |> G.add_named_vertex (V.mk "b2") bvert
  val (b3,b_to_x_plus_bw) = b_to_x_plus_bw |> G.add_named_vertex (V.mk "b3") bvert
  val (_,b_to_x_plus_bw)  = b_to_x_plus_bw |> G.add_named_edge (E.mk "e1") dir_edge b2 b3
  
  val b_x_x_b = x_to_x
  val (b4,b_x_x_b) = b_x_x_b |> G.add_named_vertex (V.mk "b4") bvert
  val (b5,b_x_x_b) = b_x_x_b |> G.add_named_vertex (V.mk "b5") bvert
  val (_,b_x_x_b)  = b_x_x_b |> G.add_named_edge (E.mk "e2") dir_edge b4 x2
  val (_,b_x_x_b)  = b_x_x_b |> G.add_named_edge (E.mk "e3") dir_edge x3 b5
  
    
  val mseq = M.match G.empty one_x
  val _ = Testing.test "M.match - empty --> one x" (assert_n_match 1 mseq) ()
  
  val mseq = M.match one_x x_to_x
  val _ = Testing.test "M.match - one x --> x to x" (assert_n_match 0 mseq) ()
  
  val mseq = M.match x_to_x x_to_x
  val _ = Testing.test "M.match - x to x --> x to x" (assert_n_match 1 mseq) ()
  
  val mseq = M.match x_to_b x_to_x
  val _ = Testing.test "M.match - x to b --> x to x" (assert_n_match 1 mseq) ()
  
  val mseq = M.match b_to_x x_to_x
  val _ = Testing.test "M.match - b to x --> x to x" (assert_n_match 1 mseq) ()
  
  val mseq = M.match b_to_x_plus_bw x_to_x
  val _ = Testing.test "M.match - b to x + bare wire --> x to x" (assert_n_match 1 mseq) ()
  
  val mseq = M.match b_x_x_b x_x_loop
  val _ = Testing.test "M.match - b to x to x to b --> x x loop" (assert_n_match 2 mseq) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR GREEDY MATCHING WITH CONCRETE GRAPHS PASSED!"
end


local (* Test matching with disjoint !-boxes *)
  open Test_MkG
  open Test_BGMatchUtil
  structure M = Test_GreedyMatcher
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR GREEDY MATCHING WITH DISJOINT !-BOXES PASSED!"
end
