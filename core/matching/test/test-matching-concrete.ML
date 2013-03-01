
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

  val circle_expr1pi = G.empty
      |> add_wv "c2"
      |> add_dir_eexpr1 "e2" "c2" "c2" "\\pi";

  val circle_expr2pi = G.empty
      |> add_wv "c3"
      |> add_dir_eexpr2 "e3" "c3" "c3" "\\pi";
  
  val _ = Testing.test "M.match - circle (eunit1) --> empty"
                       (find_n_matches 0) (circle1,G.empty);
  val _ = Testing.test "M.match - circle (eunit1) --> circle (eunit1)"
                       (find_n_matches 1) (circle1,circle1);
  val _ = Testing.test "M.match - circle (eunit1) --> circle (eunit2)"
                       (find_n_matches 0) (circle1,circle2);
  val _ = Testing.test "M.match - circle (eunit1) --> two circles (eunit1)"
                       (find_n_matches 2) (circle1,two_1circles);
  val _ = Testing.test "M.match - circle (eunit1) --> two circles (eunit1/2)"
                       (find_n_matches 1) (circle1,two_different_circles);
  val _ = Testing.test "M.match - two circles (eunit1) --> two circles (eunit1)"
                       (find_n_matches 2) (two_1circles,two_1circles);
  val _ = Testing.test "M.match - two circles (eunit1) --> two circles (eunit1/2)"
                       (find_n_matches 0) (two_1circles,two_different_circles);
  val _ = Testing.test "M.match - two circles (eunit1/2) --> two circles (eunit1)"
                       (find_n_matches 1) (two_different_circles,two_different_circles);
  val _ = Testing.test "M.match - circle (eexpr1:a) --> circle (eexpr1:pi)"
                       (find_n_matches 1) (circle_expr1a,circle_expr1pi);
  val _ = Testing.test "M.match - circle (eexpr1:a) --> circle (eexpr2:pi)"
                       (find_n_matches 0) (circle_expr1a,circle_expr2pi);
  val _ = Testing.test "M.match - circle (eexpr1:pi) --> circle (eexpr1:a)"
                       (find_n_matches 0) (circle_expr1pi,circle_expr1a);
in
  val _ = Testing.assert_no_failed_tests();
end

local (* bare-wire matching tests *)
  structure Tools = Test_Match_Tools(Test_ConcreteMatcher);
  open Tools;
in
  val _ = Testing.assert_no_failed_tests();
end

local
  structure Tools = Test_Match_Tools(Test_ConcreteMatcher);
  open Tools;
  
  val bare_wire =
        G.empty |> add_wv "in1"
                |> add_wv "out1"
                |> add_dir_eunit1 "e1" "in1" "out1";
  
  val two_bare_wires =
        G.empty |> add_wv "in1"
                |> add_wv "out1"
                |> add_dir_eunit1 "e1" "in1" "out1"
                |> add_wv "in2"
                |> add_wv "out2"
                |> add_dir_eunit1 "e2" "in2" "out2";
  
  val circle =
        G.empty |> add_wv "c1"
                |> add_dir_eunit1 "e1" "c1" "c1";
  
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
end


local
  structure Tools = Test_Match_Tools(Test_ConcreteMatcher);
  open Tools;
  
  val single_v = G.empty |> add_vunit1 "v1";
  val double_v = G.empty |> add_vunit1 "vA"
                         |> add_vunit1 "vB";
  
  val mseq = M.match single_v G.empty
  val _ = Testing.test "M.match - one vert --> empty" (assert_n_match 0 mseq) ()
  
  val mseq = M.match single_v single_v
  val _ = Testing.test "M.match - one vert --> one vert" (assert_n_match 1 mseq) ()
  
  val mseq = M.match single_v double_v
  val _ = Testing.test "M.match - one vert --> two vert" (assert_n_match 2 mseq) ()
  
  val mseq = M.match double_v single_v
  val _ = Testing.test "M.match - two vert --> one vert" (assert_n_match 0 mseq) ()
  
  val mseq = M.match double_v double_v
  val _ = Testing.test "M.match - two vert --> two vert" (assert_n_match 2 mseq) ()
in
  val _ = Testing.assert_no_failed_tests();
end

local
  structure Tools = Test_Match_Tools(Test_ConcreteMatcher);
  open Tools;
  
  val single_v = G.empty |> add_vunit1 "v1";
  
  val barbell = G.empty |> add_vunit1 "v1"
                        |> add_vunit1 "v2"
                        |> add_dir_eunit1 "e1" "v1" "v2";

  val loop = barbell |> add_dir_eunit1 "e2" "v2" "v1";
  
  val out_v = G.empty |> add_vunit1 "v1"
                      |> add_wv "out1"
                      |> add_dir_eunit1 "e1" "v1" "out1";

  val in_v = G.empty |> add_vunit1 "v1"
                     |> add_wv "in1"
                     |> add_dir_eunit1 "e1" "in1" "v1";
  
  val in_v_and_wire = in_v |> add_wv "in2"
                           |> add_wv "out2"
                           |> add_dir_eunit1 "bw" "in2" "out2";

  val line = barbell |> add_wv "in1"
                     |> add_wv "out1"
                     |> add_dir_eunit1 "e2" "in1" "v1"
                     |> add_dir_eunit1 "e3" "v2" "out1";

  val isom_lhs = G.empty |> add_vunit1 "a"
                         |> add_vunit1 "b"
                         |> add_wv "c"
                         |> add_wv "d"
                         |> add_undir_eunit1 "a" "d" "b"
                         |> add_undir_eunit1 "b" "b" "a"
                         |> add_undir_eunit1 "c" "b" "a"
                         |> add_undir_eunit1 "d" "a" "c"

  val isom_lhs_2 = G.empty |> add_wv "Va"
                           |> add_vunit1 "Vb"
                           |> add_vunit1 "Vc"
                           |> add_wv "Vd"
                           |> add_undir_eunit1 "Ea" "Va" "Vb"
                           |> add_undir_eunit1 "Eb" "Vb" "Vc"
                           |> add_undir_eunit1 "Ec" "Vb" "Vc"
                           |> add_undir_eunit1 "Ed" "Vc" "Vd";
  
  val mseq = M.match G.empty single_v
  val _ = Testing.test "M.match - empty --> one vert" (assert_n_match 1 mseq) ()
  
  val mseq = M.match single_v barbell
  val _ = Testing.test "M.match - one vert --> v to v" (assert_n_match 0 mseq) ()
  
  val mseq = M.match barbell barbell
  val _ = Testing.test "M.match - v to v --> v to v" (assert_n_match 1 mseq) ()
  
  val mseq = M.match out_v barbell
  val _ = Testing.test "M.match - v to w --> v to v" (assert_n_match 1 mseq) ()
  
  val mseq = M.match in_v barbell
  val _ = Testing.test "M.match - w to v --> v to v" (assert_n_match 1 mseq) ()
  
  val mseq = M.match in_v_and_wire barbell
  val _ = Testing.test "M.match - w to v + bare wire --> v to v" (assert_n_match 1 mseq) ()
  
  val mseq = M.match line loop
  val _ = Testing.test "M.match - line --> loop" (assert_n_match 2 mseq) ()
  
  val mseq = M.match isom_lhs isom_lhs_2
  val _ = Testing.test "M.match - isom --> isom" (assert_n_match 4 mseq) ()
in
  val _ = Testing.assert_no_failed_tests();
end


local
  structure Tools = Test_Match_Tools(Test_ConcreteMatcher);
  open Tools;

  val diff_angles = G.empty |> add_vexpr1 "x1" "a"
                            |> add_vexpr1 "x2" "b"
                            |> add_dir_eunit1 "e1" "x1" "x2";

  val same_angles = G.empty |> add_vexpr1 "x1" "a"
                            |> add_vexpr1 "x2" "a"
                            |> add_dir_eunit1 "e1" "x1" "x2";

  val diff_const = G.empty |> add_vexpr1 "x1" "2"
                           |> add_vexpr1 "x2" "1"
                           |> add_dir_eunit1 "e1" "x1" "x2";

  val same_const = G.empty |> add_vexpr1 "x1" "2"
                           |> add_vexpr1 "x2" "2"
                           |> add_dir_eunit1 "e1" "x1" "x2";

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
end

