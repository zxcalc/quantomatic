(*
 * Tests matching of string graphs against string graphs
 *)
functor String_Graph_Matching_Tests(
  Matcher : MATCH_SEARCH where type G.EData.data = Test_Graph_Data.edata
                           and type G.OVData.IData.data = Test_Graph_Data.vdata
) =
struct
  structure Tools = Test_Match_Tools(Matcher);
  open Tools;

  (*
   * Circle matching tests
   *)
  val circle1 = G.empty
      |> add_wv "c1"
      |> add_dir_eunit1 "e1" "c1" "c1";

  val circle2 = G.empty
      |> add_wv "c1"
      |> add_dir_eunit2 "e1" "c1" "c1";

  val undir_circle1 = G.empty
      |> add_wv "c1"
      |> add_undir_eunit1 "e1" "c1" "c1";

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
  
  val bare_wire1 = G.empty
      |> add_wv "in1"
      |> add_wv "out1"
      |> add_dir_eunit1 "e1" "in1" "out1";
  
  val loop = G.empty
      |> add_vunit1 "v1"
      |> add_dir_eunit1 "e1" "v1" "v1";
  
  val _ = Testing.test "match: circle (eunit1) --> empty"
                       (find_n_correct_matches 0) (circle1,G.empty);
  val _ = Testing.test "match: empty --> circle (eunit1)"
                       (find_n_correct_matches 1) (G.empty,circle1);

  val _ = Testing.test "match: circle (eunit1) --> bare wire (eunit1)"
                       (find_n_correct_matches 0) (circle1,bare_wire1);
  val _ = Testing.test "match: circle (eunit1) --> loop (vunit1,eunit1)"
                       (find_n_correct_matches 0) (circle1,loop);
  val _ = Testing.test "match: circle (eunit1) --> circle (eunit1)"
                       (find_n_correct_matches 1) (circle1,circle1);
  val _ = Testing.test "match: circle (eunit1) --> circle (eunit2)"
                       (find_n_correct_matches 0) (circle1,circle2);

  val _ = Testing.test "match: circle (eunit1) --> two circles (eunit1)"
                       (find_n_correct_matches 2) (circle1,two_1circles);
  val _ = Testing.test "match: circle (eunit1) --> two circles (eunit1/2)"
                       (find_n_correct_matches 1) (circle1,two_different_circles);
  val _ = Testing.test "match: two circles (eunit1) --> two circles (eunit1)"
                       (find_n_correct_matches 2) (two_1circles,two_1circles);
  val _ = Testing.test "match: two circles (eunit1) --> two circles (eunit1/2)"
                       (find_n_correct_matches 0) (two_1circles,two_different_circles);
  val _ = Testing.test "match: two circles (eunit1/2) --> two circles (eunit1)"
                       (find_n_correct_matches 1)
                       (two_different_circles,two_different_circles);

  val _ = Testing.test "match: circle (eexpr1:a) --> circle (eexpr1:pi)"
                       (find_n_correct_matches 1) (circle_expr1a,circle_expr1pi);
  val _ = Testing.test "match: circle (eexpr1:a) --> circle (eexpr2:pi)"
                       (find_n_correct_matches 0) (circle_expr1a,circle_expr2pi);
  val _ = Testing.test "match: circle (eexpr1:pi) --> circle (eexpr1:a)"
                       (find_n_correct_matches 0) (circle_expr1pi,circle_expr1a);
  val _ = Testing.test "match: two circles (a,2a) --> two circles (pi,2pi)"
                       (find_n_correct_matches 1)
                       (two_circles_expr1_a_2a,two_circles_expr1_pi_2pi);

  val _ = Testing.test "match: undir circle --> undir circle"
                       (find_n_correct_matches 1)
                       (undir_circle1,undir_circle1);
  val _ = Testing.test "match: dir circle --> undir circle"
                       (find_n_correct_matches 0)
                       (circle1,undir_circle1);
  val _ = Testing.test "match: undir circle --> dir circle"
                       (find_n_correct_matches 0)
                       (undir_circle1,circle1);

  (*
   * Bare wire matching tests
   *)
  val undir_bare_wire1 = G.empty
      |> add_wv "in1"
      |> add_wv "out1"
      |> add_undir_eunit1 "e1" "in1" "out1";
  
  val bare_wire2 = G.empty
      |> add_wv "in1"
      |> add_wv "out1"
      |> add_dir_eunit2 "e1" "in1" "out1";
  
  val two_bare_wires = bare_wire1
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
  
  val barbell = G.empty
      |> add_vunit1 "v1"
      |> add_vunit1 "v2"
      |> add_dir_eunit1 "e1" "v1" "v2";
  
  val undir_barbell = G.empty
      |> add_vunit1 "v1"
      |> add_vunit1 "v2"
      |> add_undir_eunit1 "e1" "v1" "v2";
  
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
                       (find_n_correct_matches 0) (bare_wire1,G.empty);
  val _ = Testing.test "match: empty --> bare wire (eunit1)"
                       (find_n_correct_matches 1) (G.empty,bare_wire1);

  val _ = Testing.test "match: bare wire (eunit1) --> bare wire (eunit1)"
                       (find_n_correct_matches 1) (bare_wire1,bare_wire1);
  val _ = Testing.test "match: bare wire (eunit1) --> bare wire (eunit2)"
                       (find_n_correct_matches 0) (bare_wire1,bare_wire2);

  val _ = Testing.test "match: bare wire (eunit1) --> 2 bare wires (eunit1)"
                       (find_n_correct_matches 2) (bare_wire1,two_bare_wires);
  val _ = Testing.test "match: bare wire (eunit1) --> 2 bare wires (eunit1/2)"
                       (find_n_correct_matches 1) (bare_wire1,two_diff_bare_wires);

  val _ = Testing.test "match: 2 bare wires (eunit1) --> bare wire (eunit1)"
                       (find_n_correct_matches 2) (two_bare_wires,bare_wire1);
  val _ = Testing.test "match: 2 bare wires (eunit1/2) --> bare wire (eunit1)"
                       (find_n_correct_matches 0) (two_diff_bare_wires,bare_wire1);
  val _ = Testing.test "match: 2 bare wires (eunit1) --> 2 bare wires (eunit1)"
                       (find_n_correct_matches 6) (two_bare_wires,two_bare_wires);
  val _ = Testing.test "match: 2 bare wires (eunit1/2) --> 2 bare wires (eunit1/2)"
                       (find_n_correct_matches 1) (two_diff_bare_wires,two_diff_bare_wires);

  (* for circles, there should be as many matches as there are cyclic permutations *) 
  val _ = Testing.test "match: bare wire (eunit1) --> circle (eunit1)"
                       (find_n_correct_matches 1) (bare_wire1,circle1);
  val _ = Testing.test "match: bare wire (eunit2) --> circle (eunit1)"
                       (find_n_correct_matches 0) (bare_wire2,circle1);
  val _ = Testing.test "match: 2 bare wires (eunit1) --> circle (eunit1)"
                       (find_n_correct_matches 1) (two_bare_wires,circle1);
  val _ = Testing.test "match: 3 bare wires (eunit1) --> circle (eunit1)"
                       (find_n_correct_matches 2) (three_bare_wires,circle1);

  val _ = Testing.test "match: bare wire (eunit1) --> barbell (eunit1)"
                       (find_n_correct_matches 1) (bare_wire1,barbell);
  val _ = Testing.test "match: bare wire (eunit2) --> barbell (eunit1)"
                       (find_n_correct_matches 0) (bare_wire2,barbell);
  val _ = Testing.test "match: 2 bare wires (eunit1) --> barbell (eunit1)"
                       (find_n_correct_matches 2) (two_bare_wires,barbell);

  val _ = Testing.test "match: bare wire (expr1:a) --> bare wire (expr1:pi)"
                       (find_n_correct_matches 1) (bare_wire_expr1a,bare_wire_expr1pi);
  val _ = Testing.test "match: bare wire (expr1:pi) --> bare wire (expr1:a)"
                       (find_n_correct_matches 0) (bare_wire_expr1pi,bare_wire_expr1a);
  val _ = Testing.test "match: bare wire (expr1:a) --> bare wire (expr2:pi)"
                       (find_n_correct_matches 0) (bare_wire_expr1a,bare_wire_expr2pi);
  val _ = Testing.test "match: bare wire (a) --> 2 circles (1/2pi,pi)"
                       (find_n_correct_matches 2)
                       (bare_wire_expr1a,two_circles_expr1_hpi_pi);
  val _ = Testing.test "match: 2 bare wires (a,2a) --> loop (1/2pi,pi)"
                       (find_n_correct_matches 1)
                       (two_bare_wires_expr1_a_2a,expr_loop_hpi_pi);
  val _ = Testing.test "match: 2 bare wires (a,2a) --> 2 circles (1/2pi,pi)"
                       (find_n_correct_matches 1)
                       (two_bare_wires_expr1_a_2a,two_circles_expr1_hpi_pi);

  val _ = Testing.test "match: undir bare wire (eunit1) --> undir bare wire (eunit1)"
                       (find_n_correct_matches 2)
                       (undir_bare_wire1,undir_bare_wire1);
  val _ = Testing.test "match: dir bare wire (eunit1) --> undir bare wire (eunit1)"
                       (find_n_correct_matches 0)
                       (bare_wire1,undir_bare_wire1);
  val _ = Testing.test "match: undir bare wire (eunit1) --> dir bare wire (eunit1)"
                       (find_n_correct_matches 0)
                       (undir_bare_wire1,bare_wire1);
  val _ = Testing.test "match: undir bare wire (eunit1) --> undir barbell (eunit1)"
                       (find_n_correct_matches 2) (undir_bare_wire1,undir_barbell);

  (*
   * Matching node-vertices against other node-vertices
   *)
  
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
                       (find_n_correct_matches 0) (single_v,G.empty);
  val _ = Testing.test "match: empty --> vunit1"
                       (find_n_correct_matches 1) (G.empty,single_v);
  val _ = Testing.test "match: vunit1 --> vunit1"
                       (find_n_correct_matches 1) (single_v,single_v);
  val _ = Testing.test "match: vunit1 --> 2 * vunit1"
                       (find_n_correct_matches 2) (single_v,double_v);
  val _ = Testing.test "match: vunit1 --> vunit1, vunit2"
                       (find_n_correct_matches 1) (single_v,v1_v2);
  val _ = Testing.test "match: 2 * vunit1 --> vunit1"
                       (find_n_correct_matches 0) (double_v,single_v);
  val _ = Testing.test "match: 2 * vunit1 --> 2 * vunit1"
                       (find_n_correct_matches 2) (double_v,double_v);
  
  val _ = Testing.test "match: vexpr1:a --> vexpr1:a"
                       (find_n_correct_matches 1) (v_expr1_a,v_expr1_a);
  val _ = Testing.test "match: vexpr1:a --> vexpr1:b"
                       (find_n_correct_matches 1) (v_expr1_a,v_expr1_b);
  val _ = Testing.test "match: vexpr1:a --> vexpr1:pi"
                       (find_n_correct_matches 1) (v_expr1_a,v_expr1_pi);
  val _ = Testing.test "match: vexpr1:pi --> vexpr1:a"
                       (find_n_correct_matches 0) (v_expr1_pi,v_expr1_a);
  val _ = Testing.test "match: vexpr1:a --> vexpr2:a"
                       (find_n_correct_matches 0) (v_expr1_a,v_expr2_a);
  val _ = Testing.test "match: vexpr1:a --> vexpr1:1/2pi, vexpr1:pi"
                       (find_n_correct_matches 2) (v_expr1_a,v_expr1_hpi_pi);
  val _ = Testing.test "match: vexpr1:a, vexpr1:2a --> vexpr1:1/2pi, vexpr1:pi"
                       (find_n_correct_matches 1) (v_expr1_a_2a,v_expr1_hpi_pi);


  (*
   * Edge-direction matching
   *)
  val dir_barbell_1_2 = G.empty
      |> add_vunit1 "v1"
      |> add_vunit2 "v2"
      |> add_dir_eunit1 "e1" "v1" "v2";
  val dir_barbell_2_1 = G.empty
      |> add_vunit1 "v1"
      |> add_vunit2 "v2"
      |> add_dir_eunit1 "e1" "v2" "v1";
  val undir_barbell_1_2 = G.empty
      |> add_vunit1 "v1"
      |> add_vunit2 "v2"
      |> add_undir_eunit1 "e1" "v1" "v2";
  val undir_barbell_2_1 = G.empty
      |> add_vunit1 "v1"
      |> add_vunit2 "v2"
      |> add_undir_eunit1 "e1" "v2" "v1";

  val _ = Testing.test "match: 1->2 --> 1->2"
                       (find_n_correct_matches 1)
                       (dir_barbell_1_2,dir_barbell_1_2);
  val _ = Testing.test "match: 1->2 --> 2->1"
                       (find_n_correct_matches 0)
                       (dir_barbell_1_2,dir_barbell_2_1);
  val _ = Testing.test "match: 1->2 --> 1-2"
                       (find_n_correct_matches 0)
                       (dir_barbell_1_2,undir_barbell_1_2);
  val _ = Testing.test "match: 1-2 --> 1-2"
                       (find_n_correct_matches 1)
                       (undir_barbell_1_2,undir_barbell_1_2);
  val _ = Testing.test "match: 1-2 --> 2-1"
                       (find_n_correct_matches 1)
                       (undir_barbell_1_2,undir_barbell_2_1);
  val _ = Testing.test "match: 1-2 --> 1->2"
                       (find_n_correct_matches 0)
                       (undir_barbell_1_2,dir_barbell_1_2);


  (*
   * Edge data matching
   *)
  val barbell_data = G.empty
      |> add_vexpr1 "v1" "a"
      |> add_vexpr2 "v2" "b"
      |> add_dir_eexpr1 "e1" "v1" "v2" "a";
  val barbell_good_data_1 = G.empty
      |> add_vexpr1 "v1" "c"
      |> add_vexpr2 "v2" "b"
      |> add_dir_eexpr1 "e1" "v1" "v2" "c";
  val barbell_good_data_2 = G.empty
      |> add_vexpr1 "v1" "1/4pi"
      |> add_vexpr2 "v2" "1/2pi"
      |> add_dir_eexpr1 "e1" "v1" "v2" "1/4pi";
  val barbell_bad_data_1 = G.empty
      |> add_vexpr1 "v1" "a"
      |> add_vexpr2 "v2" "b"
      |> add_dir_eexpr1 "e1" "v1" "v2" "b";
  val barbell_bad_data_2 = G.empty
      |> add_vexpr1 "v1" "1/4pi"
      |> add_vexpr2 "v2" "1/3pi"
      |> add_dir_eexpr1 "e1" "v1" "v2" "1/2pi";

  val _ = Testing.test "match: barbell data --> barbell data"
                       (find_n_correct_matches 1)
                       (barbell_data,barbell_data);
  val _ = Testing.test "match: barbell data --> barbell good data 1"
                       (find_n_correct_matches 1)
                       (barbell_data,barbell_good_data_1);
  val _ = Testing.test "match: barbell data --> barbell good data 2"
                       (find_n_correct_matches 1)
                       (barbell_data,barbell_good_data_2);
  val _ = Testing.test "match: barbell data --> barbell bad data 1"
                       (find_n_correct_matches 0)
                       (barbell_data,barbell_bad_data_1);
  val _ = Testing.test "match: barbell data --> barbell bad data 2"
                       (find_n_correct_matches 0)
                       (barbell_data,barbell_bad_data_2);

  (*
   * Partial wire matches
   *)
  
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
  
  val barbell_loop = barbell
      |> add_dir_eunit1 "e2" "v2" "v1";
  val barbell_line = barbell
      |> add_wv "in1"
      |> add_wv "out1"
      |> add_dir_eunit1 "e2" "in1" "v1"
      |> add_dir_eunit1 "e3" "v2" "out1";
  
  val _ = Testing.test "match: one vert --> barbell"
                       (find_n_correct_matches 0) (single_v,barbell);
  val _ = Testing.test "match: barbell --> barbell"
                       (find_n_correct_matches 1) (barbell,barbell);
  val _ = Testing.test "match: barbell --> barbell"
                       (find_n_correct_matches 1) (barbell,barbell);
  val _ = Testing.test "match: v with output --> barbell"
                       (find_n_correct_matches 1) (out_v,barbell);
  val _ = Testing.test "match: v with input --> barbell"
                       (find_n_correct_matches 1) (in_v,barbell);
  val _ = Testing.test "match: v with input and bare wire --> barbell"
                       (find_n_correct_matches 1) (in_v_and_wire,barbell);
  val _ = Testing.test "match: barbell line --> barbell loop"
                       (find_n_correct_matches 2) (barbell_line,barbell_loop);

  val _ = Testing.assert_no_failed_tests();
end;

structure Concrete_Matcher_Tests = String_Graph_Matching_Tests(Test_ConcreteMatcher);

