
local
  open Test_MkG
  open Test_MatchUtil
  structure M = Test_ConcreteMatcher
  
  val g = G.empty
  val b1 = V.mk "b1"
  val b2 = V.mk "b2"
  val g = g |> G.new_named_vertex b1 bvert
            |> G.new_named_vertex b2 bvert
            |> G.new_named_edge (E.mk "e1") dir_edge b1 b2
  val bare_wire = g
  
  val g = G.empty
  val b3 = V.mk "b3"
  val b4 = V.mk "b4"
  val b5 = V.mk "b5"
  val b6 = V.mk "b6"
  val g = g |> G.new_named_vertex b3 bvert
            |> G.new_named_vertex b4 bvert
            |> G.new_named_edge (E.mk "e2") dir_edge b3 b4
            |> G.new_named_vertex b5 bvert
            |> G.new_named_vertex b6 bvert
            |> G.new_named_edge (E.mk "e3") dir_edge b5 b6
  val two_bare_wires = g
  
  val g = G.empty
  val b7 = V.mk "b7"
  val g = g |> G.new_named_vertex b7 bvert
            |> G.new_named_edge (E.mk "e3") dir_edge b7 b7
  val circle = g
  
  val mseq = M.match bare_wire G.empty
  val _ = Testing.test "M.match - empty target" (assert_n_match 0 mseq) ()
  
  val mseq = M.match G.empty bare_wire
  val _ = Testing.test "M.match - empty source" (assert_n_match 1 mseq) ()
  
  val mseq = M.match bare_wire two_bare_wires
  val _ = Testing.test "M.match - 1 wire --> 2 wires" (assert_n_match 2 mseq) ()
  
  val mseq = M.match two_bare_wires bare_wire
  val _ = Testing.test "M.match - 2 wires --> 1 wire" (assert_n_match 2 mseq) ()
  
  val mseq = M.match two_bare_wires two_bare_wires
  val _ = Testing.test "M.match - 2 wires --> 2 wires" (assert_n_match 6 mseq) ()
  
  val mseq = M.match bare_wire circle
  val _ = Testing.test "M.match - 1 wire --> circle" (assert_n_match 1 mseq) ()
  
  (* for circles, there should be as many matches as there are cyclic permutations *) 
  val mseq = M.match two_bare_wires circle
  val _ = Testing.test "M.match - 2 wires --> circle"  (assert_n_match 1 mseq) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR CONCRETE MATCHING OF BARE WIRES PASSED!"
end


local
  open Test_MkG
  open Test_MatchUtil
  structure M = Test_ConcreteMatcher
  
  val g = G.empty
  val g = g |> G.new_named_vertex (V.mk "x1") (mkX zero_angle)
  val one_x = g
  
  val g = G.empty
  val g = g |> G.new_named_vertex (V.mk "x1") (mkX zero_angle)
            |> G.new_named_vertex (V.mk "x2") (mkX zero_angle)
  val two_x = g
  
  val mseq = M.match one_x G.empty
  val _ = Testing.test "M.match - one x --> empty" (assert_n_match 0 mseq) ()
  
  val mseq = M.match one_x one_x
  val _ = Testing.test "M.match - one x --> one x" (assert_n_match 1 mseq) ()
  
  val mseq = M.match one_x two_x
  val _ = Testing.test "M.match - one x --> two x" (assert_n_match 2 mseq) ()
  
  val mseq = M.match two_x one_x
  val _ = Testing.test "M.match - two x --> one x" (assert_n_match 0 mseq) ()
  
  val mseq = M.match two_x two_x
  val _ = Testing.test "M.match - two x --> two x" (assert_n_match 2 mseq) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR CONCRETE MATCHING OF ISOLATED NODE-VERTICES PASSED!"
end

local
  open Test_MkG
  open Test_MatchUtil
  structure M = Test_ConcreteMatcher
  
  val g = G.empty
  val g = g |> G.new_named_vertex (V.mk "x1") (mkX zero_angle)
  val one_x = g
  
  val g = G.empty
  val x2 = V.mk "x2"
  val x3 = V.mk "x3"
  val g = g |> G.new_named_vertex x2 (mkX zero_angle)
            |> G.new_named_vertex x3 (mkX zero_angle)
            |> G.new_named_edge (E.mk "e1") dir_edge x2 x3
  val x_to_x = g
  
  val g = g |> G.new_named_edge (E.mk "e2") dir_edge x3 x2
  val x_x_loop = g
  
  val g = G.empty
  val x4 = V.mk "x4"
  val b1 = V.mk "b1"
  val g = g |> G.new_named_vertex x4 (mkX zero_angle)
            |> G.new_named_vertex b1 bvert
            |> G.new_named_edge (E.mk "e3") dir_edge x4 b1
  val x_to_b = g
  
  val g = G.empty
  val b2 = V.mk "b2"
  val x5 = V.mk "x5"
  val g = g |> G.new_named_vertex b2 bvert
            |> G.new_named_vertex x5 (mkX zero_angle)
            |> G.new_named_edge (E.mk "e4") dir_edge b2 x5
  val b_to_x = g
  
  val b3 = V.mk "b3"
  val b4 = V.mk "b4"
  val g = g |> G.new_named_vertex b3 bvert
            |> G.new_named_vertex b4 bvert
            |> G.new_named_edge (E.mk "e5") dir_edge b3 b4
  val b_to_x_plus_bw = g
  
  val g = x_to_x
  val b5 = V.mk "b5"
  val b6 = V.mk "b6"
  val g = g |> G.new_named_vertex b5 bvert
            |> G.new_named_vertex b6 bvert
            |> G.new_named_edge (E.mk "e6") dir_edge b5 x2
            |> G.new_named_edge (E.mk "e7") dir_edge x3 b6
  val b_x_x_b = g
  
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
  val _ = "UNIT TESTS FOR CONCRETE MATCHING PASSED!"
end


local
  open Test_MkG
  open Test_MatchUtil
  structure M = Test_ConcreteMatcher

  val g = G.empty
  val x1 = V.mk "x1"
  val x2 = V.mk "x2"
  val g = g |> G.new_named_vertex x1 (mkX (LinratAngleExpr.parse "a"))
            |> G.new_named_vertex x2 (mkX (LinratAngleExpr.parse "b"))
            |> G.new_named_edge (E.mk "e1") dir_edge x1 x2
  val diff_angles = g
  
  val g = G.empty
  val x3 = V.mk "x3"
  val x4 = V.mk "x4"
  val g = g |> G.new_named_vertex x3 (mkX (LinratAngleExpr.parse "a"))
            |> G.new_named_vertex x4 (mkX (LinratAngleExpr.parse "a"))
            |> G.new_named_edge (E.mk "e2") dir_edge x3 x4
  val same_angles = g
  
  val g = G.empty
  val x5 = V.mk "x5"
  val x6 = V.mk "x6"
  val g = g |> G.new_named_vertex x5 (mkX (LinratAngleExpr.parse "2"))
            |> G.new_named_vertex x6 (mkX (LinratAngleExpr.parse "1"))
            |> G.new_named_edge (E.mk "e6") dir_edge x5 x6
  val diff_const = g
  
  val g = G.empty
  val x7 = V.mk "x7"
  val x8 = V.mk "x8"
  val g = g |> G.new_named_vertex x7 (mkX (LinratAngleExpr.parse "2"))
            |> G.new_named_vertex x8 (mkX (LinratAngleExpr.parse "2"))
            |> G.new_named_edge (E.mk "e7") dir_edge x7 x8
  val same_const = g

  val mseq = M.match diff_angles diff_angles
  val _ = Testing.test "M.match - diff angles --> diff angles" (assert_n_match 1 mseq) ()
  
  val mseq = M.match diff_angles same_angles
  val _ = Testing.test "M.match - diff angles --> same angles" (assert_n_match 1 mseq) ()
  
  val mseq = M.match diff_angles diff_const
  val _ = Testing.test "M.match - diff angles --> diff const" (assert_n_match 1 mseq) ()
  
  val mseq = M.match diff_angles same_const
  val _ = Testing.test "M.match - diff angles --> same const" (assert_n_match 1 mseq) ()
  
  val mseq = M.match diff_const diff_angles
  val _ = Testing.test "M.match - diff const --> diff angles" (assert_n_match 0 mseq) ()
  
  val mseq = M.match same_angles diff_const
  val _ = Testing.test "M.match - same angles --> diff const" (assert_n_match 0 mseq) ()
  
  val mseq = M.match same_angles diff_angles
  val _ = Testing.test "M.match - same angles --> diff angles" (assert_n_match 0 mseq) ()
  
  val mseq = M.match same_angles same_const
  val _ = Testing.test "M.match - same angles --> same const" (assert_n_match 1 mseq) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR CONCRETE MATCHING WITH ANGLES PASSED!"
end

local
  open Test_MkG
  open Test_MatchUtil
  structure M = Test_ConcreteMatcher

  val g = G.empty
  val b1 = V.mk "b1"
  val g = g |> G.new_named_vertex b1 bvert
            |> G.new_named_edge (E.mk "e1") dir_edge b1 b1
  val circle = g
  
  val g = G.empty
  val b2 = V.mk "b2"
  val b3 = V.mk "b3"
  val g = g |> G.new_named_vertex b2 bvert
            |> G.new_named_edge (E.mk "e2") dir_edge b2 b2
            |> G.new_named_vertex b3 bvert
            |> G.new_named_edge (E.mk "e3") dir_edge b3 b3
  val two_circles = g
  
  val mseq = M.match circle G.empty
  val _ = Testing.test "M.match - circle --> empty"  (assert_n_match 0 mseq) ()
  
  val mseq = M.match circle circle
  val _ = Testing.test "M.match - circle --> circle" (assert_n_match 1 mseq) ()
  
  val mseq = M.match circle two_circles
  val _ = Testing.test "M.match - circle --> two circles" (assert_n_match 2 mseq) ()
  
  val mseq = M.match two_circles two_circles
  val _ = Testing.test "M.match - two circles --> two circles" (assert_n_match 2 mseq) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR CONCRETE MATCHING OF CIRCLES PASSED!"
end

