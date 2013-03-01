
local (* circle-matching tests *)
  structure Tools = Test_Match_Tools(Test_ConcreteMatcher);
  open Tools;

  val circle1 = G.empty
      |> add_wv "c1"
      |> add_dir_eunit1 "e1" "c1" "c1";

  val circle2 = G.empty
      |> add_wv "c1"
      |> add_dir_eunit2 "e1" "c1" "c1";

  val two_1circles = circle1
      |> add_wv "c2"
      |> add_dir_eunit1 "e2" "c2" "c2";

  val two_different_circles = circle1
      |> add_wv "c2"
      |> add_dir_eunit2 "e2" "c2" "c2";

  val circle_expr1a = G.empty
      |> add_wv "c1"
      |> add_dir_eexpr1 "e1" "c1" "c1" "a";

  val two_circles_expr1_a_2a = circle_expr1a
      |> add_wv "c2"
      |> add_dir_eexpr1 "e2" "c2" "c2" "2a";

  val circle_expr1pi = G.empty
      |> add_wv "c2"
      |> add_dir_eexpr1 "e2" "c2" "c2" "\\pi";

  val two_circles_expr1_pi_2pi = circle_expr1pi
      |> add_wv "c3"
      |> add_dir_eexpr1 "e3" "c3" "c3" "2\\pi";

  val circle_expr2pi = G.empty
      |> add_wv "c3"
      |> add_dir_eexpr2 "e3" "c3" "c3" "\\pi";
  
  val bare_wire = G.empty
      |> add_wv "in1"
      |> add_wv "out1"
      |> add_dir_eunit1 "e1" "in1" "out1";
  
  val loop = G.empty
      |> add_vunit1 "v1"
      |> add_dir_eunit1 "e1" "v1" "v1";
  
  val _ = Testing.test "match: circle (eunit1) --> empty"
                       (find_n_matches 0) (circle1,G.empty);
  val _ = Testing.test "match: empty --> circle (eunit1)"
                       (find_n_matches 1) (G.empty,circle1);
  val _ = Testing.test "match: circle (eunit1) --> bare wire (eunit1)"
                       (find_n_matches 0) (circle1,bare_wire);
  val _ = Testing.test "match: circle (eunit1) --> loop (vunit1,eunit1)"
                       (find_n_matches 0) (circle1,loop);
  val _ = Testing.test "match: circle (eunit1) --> circle (eunit1)"
                       (find_n_matches 1) (circle1,circle1);
  val _ = Testing.test "match: circle (eunit1) --> circle (eunit2)"
                       (find_n_matches 0) (circle1,circle2);
  val _ = Testing.test "match: circle (eunit1) --> two circles (eunit1)"
                       (find_n_matches 2) (circle1,two_1circles);
  val _ = Testing.test "match: circle (eunit1) --> two circles (eunit1/2)"
                       (find_n_matches 1) (circle1,two_different_circles);
  val _ = Testing.test "match: two circles (eunit1) --> two circles (eunit1)"
                       (find_n_matches 2) (two_1circles,two_1circles);
  val _ = Testing.test "match: two circles (eunit1) --> two circles (eunit1/2)"
                       (find_n_matches 0) (two_1circles,two_different_circles);
  val _ = Testing.test "match: two circles (eunit1/2) --> two circles (eunit1)"
                       (find_n_matches 1)
                       (two_different_circles,two_different_circles);
  val _ = Testing.test "match: circle (eexpr1:a) --> circle (eexpr1:pi)"
                       (find_n_matches 1) (circle_expr1a,circle_expr1pi);
  val _ = Testing.test "match: circle (eexpr1:a) --> circle (eexpr2:pi)"
                       (find_n_matches 0) (circle_expr1a,circle_expr2pi);
  val _ = Testing.test "match: circle (eexpr1:pi) --> circle (eexpr1:a)"
                       (find_n_matches 0) (circle_expr1pi,circle_expr1a);
  val _ = Testing.test "match: two circles (a,2a) --> two circles (pi,2pi)"
                       (find_n_matches 1)
                       (two_circles_expr1_a_2a,two_circles_expr1_pi_2pi);
in
  val _ = Testing.assert_no_failed_tests();
end

local (* bare-wire matching tests *)
  structure Tools = Test_Match_Tools(Test_ConcreteMatcher);
  open Tools;
  
  val bare_wire = G.empty
      |> add_wv "in1"
      |> add_wv "out1"
      |> add_dir_eunit1 "e1" "in1" "out1";
  
  val bare_wire2 = G.empty
      |> add_wv "in1"
      |> add_wv "out1"
      |> add_dir_eunit2 "e1" "in1" "out1";
  
  val two_bare_wires = bare_wire
      |> add_wv "in2"
      |> add_wv "out2"
      |> add_dir_eunit1 "e2" "in2" "out2";
  
  val three_bare_wires = two_bare_wires
      |> add_wv "in3"
      |> add_wv "out3"
      |> add_dir_eunit1 "e3" "in3" "out3";
  
  val two_diff_bare_wires = G.empty
      |> add_wv "in1"
      |> add_wv "out1"
      |> add_dir_eunit1 "e1" "in1" "out1"
      |> add_wv "in2"
      |> add_wv "out2"
      |> add_dir_eunit2 "e2" "in2" "out2";
  
  val circle = G.empty
      |> add_wv "c1"
      |> add_dir_eunit1 "e1" "c1" "c1";
  
  val barbell = G.empty
      |> add_vunit1 "v1"
      |> add_vunit1 "v2"
      |> add_dir_eunit1 "e1" "v1" "v2";
  
  val bare_wire_expr1a = G.empty
      |> add_wv "in1"
      |> add_wv "out1"
      |> add_dir_eexpr1 "e1" "in1" "out1" "a";
  
  val two_bare_wires_expr1_a_2a = bare_wire_expr1a
      |> add_wv "in2"
      |> add_wv "out2"
      |> add_dir_eexpr1 "e2" "in2" "out2" "2a";
  
  val bare_wire_expr1pi = G.empty
      |> add_wv "in1"
      |> add_wv "out1"
      |> add_dir_eexpr1 "e1" "in1" "out1" "\\pi";
  
  val bare_wire_expr2pi = G.empty
      |> add_wv "in1"
      |> add_wv "out1"
      |> add_dir_eexpr2 "e1" "in1" "out1" "\\pi";

  val two_circles_expr1_hpi_pi = G.empty
      |> add_wv "c1"
      |> add_dir_eexpr1 "e1" "c1" "c1" "1/2\\pi"
      |> add_wv "c2"
      |> add_dir_eexpr1 "e2" "c2" "c2" "\\pi";

  val expr_loop_hpi_pi = G.empty
      |> add_vunit1 "x1"
      |> add_vunit1 "x2"
      |> add_dir_eexpr1 "e1" "x1" "x2" "1/2\\pi"
      |> add_dir_eexpr1 "e2" "x2" "x1" "\\pi";
  
  val _ = Testing.test "match: bare wire (eunit1) --> empty"
                       (find_n_matches 0) (bare_wire,G.empty);
  val _ = Testing.test "match: empty --> bare wire (eunit1)"
                       (find_n_matches 1) (G.empty,bare_wire);

  val _ = Testing.test "match: bare wire (eunit1) --> bare wire (eunit1)"
                       (find_n_matches 1) (bare_wire,bare_wire);
  val _ = Testing.test "match: bare wire (eunit1) --> bare wire (eunit2)"
                       (find_n_matches 0) (bare_wire,bare_wire2);

  val _ = Testing.test "match: bare wire (eunit1) --> 2 bare wires (eunit1)"
                       (find_n_matches 2) (bare_wire,two_bare_wires);
  val _ = Testing.test "match: bare wire (eunit1) --> 2 bare wires (eunit1/2)"
                       (find_n_matches 1) (bare_wire,two_diff_bare_wires);

  val _ = Testing.test "match: 2 bare wires (eunit1) --> bare wire (eunit1)"
                       (find_n_matches 2) (two_bare_wires,bare_wire);
  val _ = Testing.test "match: 2 bare wires (eunit1/2) --> bare wire (eunit1)"
                       (find_n_matches 0) (two_diff_bare_wires,bare_wire);
  val _ = Testing.test "match: 2 bare wires (eunit1) --> 2 bare wires (eunit1)"
                       (find_n_matches 6) (two_bare_wires,two_bare_wires);
  val _ = Testing.test "match: 2 bare wires (eunit1/2) --> 2 bare wires (eunit1/2)"
                       (find_n_matches 1) (two_diff_bare_wires,two_diff_bare_wires);

  (* for circles, there should be as many matches as there are cyclic permutations *) 
  val _ = Testing.test "match: bare wire (eunit1) --> circle (eunit1)"
                       (find_n_matches 1) (bare_wire,circle);
  val _ = Testing.test "match: bare wire (eunit2) --> circle (eunit1)"
                       (find_n_matches 0) (bare_wire2,circle);
  val _ = Testing.test "match: 2 bare wires (eunit1) --> circle (eunit1)"
                       (find_n_matches 1) (two_bare_wires,circle);
  val _ = Testing.test "match: 3 bare wires (eunit1) --> circle (eunit1)"
                       (find_n_matches 2) (three_bare_wires,circle);

  val _ = Testing.test "match: bare wire (eunit1) --> barbell (eunit1)"
                       (find_n_matches 1) (bare_wire,barbell);
  val _ = Testing.test "match: bare wire (eunit2) --> barbell (eunit1)"
                       (find_n_matches 0) (bare_wire2,barbell);
  val _ = Testing.test "match: 2 bare wires (eunit1) --> barbell (eunit1)"
                       (find_n_matches 2) (two_bare_wires,barbell);

  val _ = Testing.test "match: bare wire (expr1:a) --> bare wire (expr1:pi)"
                       (find_n_matches 1) (bare_wire_expr1a,bare_wire_expr1pi);
  val _ = Testing.test "match: bare wire (expr1:pi) --> bare wire (expr1:a)"
                       (find_n_matches 0) (bare_wire_expr1pi,bare_wire_expr1a);
  val _ = Testing.test "match: bare wire (expr1:a) --> bare wire (expr2:pi)"
                       (find_n_matches 0) (bare_wire_expr1a,bare_wire_expr2pi);
  val _ = Testing.test "match: bare wire (a) --> 2 circles (1/2pi,pi)"
                       (find_n_matches 2)
                       (bare_wire_expr1a,two_circles_expr1_hpi_pi);
  val _ = Testing.test "match: 2 bare wires (a,2a) --> loop (1/2pi,pi)"
                       (find_n_matches 1)
                       (two_bare_wires_expr1_a_2a,expr_loop_hpi_pi);
  val _ = Testing.test "match: 2 bare wires (a,2a) --> 2 circles (1/2pi,pi)"
                       (find_n_matches 1)
                       (two_bare_wires_expr1_a_2a,two_circles_expr1_hpi_pi);
in
  val _ = Testing.assert_no_failed_tests();
end


local (* basic node-vertex matching *)
  structure Tools = Test_Match_Tools(Test_ConcreteMatcher);
  open Tools;
  
  val single_v = G.empty
      |> add_vunit1 "v1";
  val double_v = G.empty
      |> add_vunit1 "vA"
      |> add_vunit1 "vB";
  val v1_v2 = G.empty
      |> add_vunit1 "vA"
      |> add_vunit2 "v1";

  val v_expr1_a = G.empty
      |> add_vexpr1 "v1" "a";
  val v_expr1_a_2a = G.empty
      |> add_vexpr1 "v1" "a"
      |> add_vexpr1 "v2" "2a";
  val v_expr1_b = G.empty
      |> add_vexpr1 "v1" "b";
  val v_expr1_pi = G.empty
      |> add_vexpr1 "v1" "\\pi";
  val v_expr1_hpi_pi = G.empty
      |> add_vexpr1 "v1" "1/2\\pi"
      |> add_vexpr1 "v2" "\\pi";
  val v_expr2_a = G.empty
      |> add_vexpr2 "v1" "a";
  
  val _ = Testing.test "match: vunit1 --> empty"
                       (find_n_matches 0) (single_v,G.empty);
  val _ = Testing.test "match: empty --> vunit1"
                       (find_n_matches 1) (G.empty,single_v);
  val _ = Testing.test "match: vunit1 --> vunit1"
                       (find_n_matches 1) (single_v,single_v);
  val _ = Testing.test "match: vunit1 --> 2 * vunit1"
                       (find_n_matches 2) (single_v,double_v);
  val _ = Testing.test "match: vunit1 --> vunit1, vunit2"
                       (find_n_matches 1) (single_v,v1_v2);
  val _ = Testing.test "match: 2 * vunit1 --> vunit1"
                       (find_n_matches 0) (double_v,single_v);
  val _ = Testing.test "match: 2 * vunit1 --> 2 * vunit1"
                       (find_n_matches 2) (double_v,double_v);

  val _ = Testing.test "match: vexpr1:a --> vexpr1:a"
                       (find_n_matches 1) (v_expr1_a,v_expr1_a);
  val _ = Testing.test "match: vexpr1:a --> vexpr1:b"
                       (find_n_matches 1) (v_expr1_a,v_expr1_b);
  val _ = Testing.test "match: vexpr1:a --> vexpr1:pi"
                       (find_n_matches 1) (v_expr1_a,v_expr1_pi);
  val _ = Testing.test "match: vexpr1:pi --> vexpr1:a"
                       (find_n_matches 0) (v_expr1_pi,v_expr1_a);
  val _ = Testing.test "match: vexpr1:a --> vexpr2:a"
                       (find_n_matches 0) (v_expr1_a,v_expr2_a);
  val _ = Testing.test "match: vexpr1:a --> vexpr1:1/2pi, vexpr1:pi"
                       (find_n_matches 2) (v_expr1_a,v_expr1_hpi_pi);
  val _ = Testing.test "match: vexpr1:a, vexpr1:2a --> vexpr1:1/2pi, vexpr1:pi"
                       (find_n_matches 1) (v_expr1_a_2a,v_expr1_hpi_pi);
in
  val _ = Testing.assert_no_failed_tests();
end


local (* edge-ordered node-vertex matching *)
  structure Tools = Test_Match_Tools(Test_ConcreteMatcher);
  open Tools;

  val diff_angles = G.empty
      |> add_vexpr1 "x1" "a"
      |> add_vexpr1 "x2" "b"
      |> add_dir_eunit1 "e1" "x1" "x2";

  val same_angles = G.empty
      |> add_vexpr1 "x1" "a"
      |> add_vexpr1 "x2" "a"
      |> add_dir_eunit1 "e1" "x1" "x2";

  val diff_const = G.empty
      |> add_vexpr1 "x1" "1/2"
      |> add_vexpr1 "x2" "1"
      |> add_dir_eunit1 "e1" "x1" "x2";

  val same_const = G.empty
      |> add_vexpr1 "x1" "1"
      |> add_vexpr1 "x2" "1"
      |> add_dir_eunit1 "e1" "x1" "x2";

  val _ = Testing.test "match: vexpr1:a,b --> vexpr1:a,b"
                       (find_n_matches 1) (diff_angles,diff_angles);
  val _ = Testing.test "match: vexpr1:a,b --> vexpr1:a,a"
                       (find_n_matches 1) (diff_angles,same_angles);
  val _ = Testing.test "match: vexpr1:a,b --> vexpr1:1/2,1"
                       (find_n_matches 1) (diff_angles,diff_const);
  val _ = Testing.test "match: vexpr1:a,b --> vexpr1:1,1"
                       (find_n_matches 1) (diff_angles,same_const);

  val _ = Testing.test "match: vexpr1:1,1/2 --> vexpr1:a,b"
                       (find_n_matches 0) (diff_const,diff_angles);

  val _ = Testing.test "match: vexpr1:a,a --> vexpr1:a,b"
                       (find_n_matches 0) (same_angles,diff_angles);
  val _ = Testing.test "match: vexpr1:a,a --> vexpr1:1/2,1"
                       (find_n_matches 0) (same_angles,diff_const);
  val _ = Testing.test "match: vexpr1:a,a --> vexpr1:1,1"
                       (find_n_matches 1) (same_angles,same_const);
in
  val _ = Testing.assert_no_failed_tests();
end

(* TODO: clean up remaining tests and add more
 * - undirected edge tests
 * - edge-with-data tests (non-bare-wire and non-circle)
 *)

local (* misc test? *)
  structure Tools = Test_Match_Tools(Test_ConcreteMatcher);
  open Tools;
  
  val single_v = G.empty
      |> add_vunit1 "v1";
  
  val barbell = G.empty
      |> add_vunit1 "v1"
      |> add_vunit1 "v2"
      |> add_dir_eunit1 "e1" "v1" "v2";

  val loop = barbell
      |> add_dir_eunit1 "e2" "v2" "v1";
  
  val out_v = G.empty
      |> add_vunit1 "v1"
      |> add_wv "out1"
      |> add_dir_eunit1 "e1" "v1" "out1";

  val in_v = G.empty
      |> add_vunit1 "v1"
      |> add_wv "in1"
      |> add_dir_eunit1 "e1" "in1" "v1";
  
  val in_v_and_wire = in_v
      |> add_wv "in2"
      |> add_wv "out2"
      |> add_dir_eunit1 "bw" "in2" "out2";

  val line = barbell
      |> add_wv "in1"
      |> add_wv "out1"
      |> add_dir_eunit1 "e2" "in1" "v1"
      |> add_dir_eunit1 "e3" "v2" "out1";

  val isom_lhs = G.empty
      |> add_vunit1 "a"
      |> add_vunit1 "b"
      |> add_wv "c"
      |> add_wv "d"
      |> add_undir_eunit1 "a" "d" "b"
      |> add_undir_eunit1 "b" "b" "a"
      |> add_undir_eunit1 "c" "b" "a"
      |> add_undir_eunit1 "d" "a" "c";

  val isom_lhs_2 = G.empty
      |> add_wv "Va"
      |> add_vunit1 "Vb"
      |> add_vunit1 "Vc"
      |> add_wv "Vd"
      |> add_undir_eunit1 "Ea" "Va" "Vb"
      |> add_undir_eunit1 "Eb" "Vb" "Vc"
      |> add_undir_eunit1 "Ec" "Vb" "Vc"
      |> add_undir_eunit1 "Ed" "Vc" "Vd";
  
  val _ = Testing.test "match: one vert --> barbell"
                       (find_n_matches 0) (single_v,barbell);
  val _ = Testing.test "match: barbell --> barbell"
                       (find_n_matches 1) (barbell,barbell);
  val _ = Testing.test "match: barbell --> barbell"
                       (find_n_matches 1) (barbell,barbell);
  val _ = Testing.test "match: v with output --> barbell"
                       (find_n_matches 1) (out_v,barbell);
  val _ = Testing.test "match: v with input --> barbell"
                       (find_n_matches 1) (in_v,barbell);
  val _ = Testing.test "match: v with input and bare wire --> barbell"
                       (find_n_matches 1) (in_v_and_wire,barbell);
  val _ = Testing.test "match: line --> loop"
                       (find_n_matches 2) (line,loop);
  val _ = Testing.test "match: isom --> isom"
                       (find_n_matches 4) (isom_lhs,isom_lhs_2);
in
  val _ = Testing.assert_no_failed_tests();
end

