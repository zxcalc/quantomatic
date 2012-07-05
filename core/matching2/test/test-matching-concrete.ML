
local
  open Test_MkG
  open Test_MatchUtil
  structure M = Test_ConcreteMatcher
  
  val bare_wire = G.empty
  val (b1,bare_wire) = bare_wire |> G.add_named_vertex (V.mk "b1") bvert
  val (b2,bare_wire) = bare_wire |> G.add_named_vertex (V.mk "b2") bvert
  val (_,bare_wire)  = bare_wire |> G.add_named_edge (E.mk "e1") dir_edge b1 b2
  
  val two_bare_wires = G.empty
  val (b3,two_bare_wires) = two_bare_wires |> G.add_named_vertex (V.mk "b3") bvert
  val (b4,two_bare_wires) = two_bare_wires |> G.add_named_vertex (V.mk "b4") bvert
  val (_,two_bare_wires)  = two_bare_wires |> G.add_named_edge (E.mk "e2") dir_edge b3 b4
  val (b5,two_bare_wires) = two_bare_wires |> G.add_named_vertex (V.mk "b5") bvert
  val (b6,two_bare_wires) = two_bare_wires |> G.add_named_vertex (V.mk "b6") bvert
  val (_,two_bare_wires)  = two_bare_wires |> G.add_named_edge (E.mk "e3") dir_edge b5 b6
  
  val circle = G.empty
  val (b7,circle) = circle |> G.add_named_vertex (V.mk "b7") bvert
  val (_,circle)  = circle |> G.add_named_edge (E.mk "e3") dir_edge b7 b7
  
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
  
  val one_x = G.empty
  val (_,one_x) = one_x |> G.add_named_vertex (V.mk "x1") (mkX zero_angle)
  
  val two_x = G.empty
  val (_,two_x) = one_x |> G.add_named_vertex (V.mk "x1") (mkX zero_angle)
  val (_,two_x) = one_x |> G.add_named_vertex (V.mk "x2") (mkX zero_angle)
  
  
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
  
  val one_x = G.empty
  val (x1,one_x) = one_x |> G.add_named_vertex (V.mk "x1") (mkX zero_angle)
  
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
  val _ = "UNIT TESTS FOR CONCRETE MATCHING PASSED!"
end


local
  open Test_MkG
  open Test_MatchUtil
  structure M = Test_ConcreteMatcher
  val g = G.empty
  val (x6,g) = g |> G.add_named_vertex (V.mk "x6") (mkX (LinratAngleExpr.parse "a"))
  val (x7,g) = g |> G.add_named_vertex (V.mk "x7") (mkX (LinratAngleExpr.parse "b"))
  val (_,g)  = g |> G.add_named_edge (E.mk "e4") dir_edge x6 x7
  val diff_angles = g
  
  val g = G.empty
  val (x8,g) = g |> G.add_named_vertex (V.mk "x8") (mkX (LinratAngleExpr.parse "a"))
  val (x9,g) = g |> G.add_named_vertex (V.mk "x9") (mkX (LinratAngleExpr.parse "a"))
  val (_,g)  = g |> G.add_named_edge (E.mk "e5") dir_edge x8 x9
  val same_angles = g
  
  val g = G.empty
  val (x10,g) = g |> G.add_named_vertex (V.mk "x10") (mkX (LinratAngleExpr.parse "2"))
  val (x11,g) = g |> G.add_named_vertex (V.mk "x11") (mkX (LinratAngleExpr.parse "1"))
  val (_,g)  = g |> G.add_named_edge (E.mk "e6") dir_edge x10 x11
  val diff_const = g
  
  val g = G.empty
  val (x12,g) = g |> G.add_named_vertex (V.mk "x12") (mkX (LinratAngleExpr.parse "2"))
  val (x13,g) = g |> G.add_named_vertex (V.mk "x13") (mkX (LinratAngleExpr.parse "2"))
  val (_,g)  = g |> G.add_named_edge (E.mk "e7") dir_edge x12 x13
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
  
  val bare_wire = G.empty
  val (b1,bare_wire) = bare_wire |> G.add_named_vertex (V.mk "b1") bvert
  val (b2,bare_wire) = bare_wire |> G.add_named_vertex (V.mk "b2") bvert
  val (_,bare_wire)  = bare_wire |> G.add_named_edge (E.mk "e1") dir_edge b1 b2
  
  val circle = G.empty
  val (b3,circle) = circle |> G.add_named_vertex (V.mk "b3") bvert
  val (_,circle)  = circle |> G.add_named_edge (E.mk "e2") dir_edge b3 b3
  
  val g = G.empty
  val (b4,g) = g |> G.add_named_vertex (V.mk "b4") bvert
  val (_,g)  = g |> G.add_named_edge (E.mk "e3") dir_edge b4 b4
  val (b5,g) = g |> G.add_named_vertex (V.mk "b5") bvert
  val (_,g)  = g |> G.add_named_edge (E.mk "e4") dir_edge b5 b5
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

