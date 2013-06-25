(* Tests for the OGRAPH signature *)
functor Bang_Graph_Interface_Tests(
  G : TEST_BANG_GRAPH
)
= struct
  (* start by testing that things in the OGRAPH interface still work *)
  structure OGraph_Interface_Tests_Bang_Graph = OGraph_Interface_Tests(Test_Bang_Graph);
  structure Tools = Test_Bang_Graph_Tools(Test_Bang_Graph);
  open Tools;


  val _ = Testing.test "G.has_bbox" (fn _ => let
    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_bbox "b0" ["v1","v2"]
        |> add_bbox_with_parent "b1" "b0" ["v1"]
        |> add_bbox "b2" []
    val _ = Testing.assert "not (G.has_bbox G.empty b0)"
                           (not (G.has_bbox G.empty B.default_name))
    val _ = Testing.assert "G.has_bbox g b0" (G.has_bbox g (B.mk "b0"))
    val _ = Testing.assert "G.has_bbox g b1" (G.has_bbox g (B.mk "b1"))
    val _ = Testing.assert "G.has_bbox g b2" (G.has_bbox g (B.mk "b2"))
    val _ = Testing.assert "G.has_bbox g b9"
                           (not (G.has_bbox g (B.mk "b9")))
    in () end) ()



  val _ = Testing.test "G.get_bboxes" (fn _ => let
    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_bbox "b0" ["v1","v2"]
        |> add_bbox_with_parent "b1" "b0" ["v1"]
        |> add_bbox "b2" []
    val exp = B.NSet.of_list (map B.mk ["b0","b1","b2"])
    val _ = Testing.assert "G.get_bboxes gives correct list"
                           (B.NSet.eq exp (G.get_bboxes g))
    val _ = Testing.assert "G.get_bboxes G.empty is empty"
                           (B.NSet.is_empty (G.get_bboxes G.empty))
    in () end) ()



  val _ = Testing.test "G.get_bboxed" (fn _ => let
    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_wv "w1"
        |> add_wv "w2"
        |> add_bbox "b0" ["v1","v2","w1"]
        |> add_bbox_with_parent "b1" "b0" ["v1"]
        |> add_bbox "b2" ["w1"]
        |> add_bbox "b3" []
    val exp = V.NSet.of_list (map V.mk ["v1","v2","w1"])
    val _ = Testing.assert "G.get_bboxed gives correct list"
                           (V.NSet.eq exp (G.get_bboxed g))
    val _ = Testing.assert "G.get_bboxed G.empty is empty"
                           (V.NSet.is_empty (G.get_bboxed G.empty))
    in () end) ()



  val _ = Testing.test "G.get_unbboxed" (fn _ => let
    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_wv "w1"
        |> add_wv "w2"
        |> add_bbox "b0" ["v1","v2","w1"]
        |> add_bbox_with_parent "b1" "b0" ["v1"]
        |> add_bbox "b2" ["w1"]
        |> add_bbox "b3" []
    val exp = V.NSet.of_list (map V.mk ["v3","w2"])
    val _ = Testing.assert "G.get_unbboxed gives correct list"
                           (V.NSet.eq exp (G.get_unbboxed g))
    val _ = Testing.assert "G.get_unbboxed G.empty is empty"
                           (V.NSet.is_empty (G.get_unbboxed G.empty))
    in () end) ()



  val _ = Testing.test "G.is_bboxed" (fn _ => let
    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_wv "w1"
        |> add_wv "w2"
        |> add_bbox "b0" ["v1","v2","w1"]
        |> add_bbox_with_parent "b1" "b0" ["v1"]
        |> add_bbox "b2" ["w1"]
        |> add_bbox "b3" []
    val _ = Testing.assert "G.is_bboxed v1" (G.is_bboxed g (V.mk "v1"))
    val _ = Testing.assert "G.is_bboxed v2" (G.is_bboxed g (V.mk "v2"))
    val _ = Testing.assert "G.is_bboxed w1" (G.is_bboxed g (V.mk "w1"))
    val _ = Testing.assert "G.is_bboxed v3" (not (G.is_bboxed g (V.mk "v3")))
    val _ = Testing.assert "G.is_bboxed w2" (not (G.is_bboxed g (V.mk "w2")))
    val _ = (G.is_bboxed g (V.mk "v4"); raise ERROR "expected no_such_vertex_exp")
            handle G.no_such_vertex_exp (_,v',g') =>
              (Testing.assert "exception has correct graph" (G.exact_eq g g');
               Testing.assert "exception has correct vertex" ((V.dest v') = "v4"))
    in () end) ()



  val _ = Testing.test "G.get_empty_bboxes" (fn _ => let
    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_wv "w1"
        |> add_wv "w2"
        |> add_bbox "b0" ["v1","v2","w1"]
        |> add_bbox_with_parent "b1" "b0" ["v1"]
        |> add_bbox_with_parent "b2" "b0" []
        |> add_bbox "b3" ["w1"]
        |> add_bbox "b4" []
        |> add_bbox "b5" []
        |> add_bbox_with_parent "b6" "b5" []
    val exp = B.NSet.of_list (map B.mk ["b2","b4","b5","b6"])
    val _ = Testing.assert "G.get_empty_bboxes gives correct list"
                           (B.NSet.eq exp (G.get_empty_bboxes g))
    val _ = Testing.assert "G.get_empty_bboxes G.empty is empty"
                           (B.NSet.is_empty (G.get_empty_bboxes G.empty))
    in () end) ()



  val _ = Testing.test "G.add_named_bbox" (fn () => let
    val b0 = B.mk "b0"
    val b1 = B.mk "b1"
    val g = G.add_named_bbox b0 G.empty
    val _ = case B.NSet.tryget_singleton (G.get_bboxes g)
              of NONE => raise ERROR "add_named_bbox did not add a !-box"
               | SOME b =>
                  if not (B.name_eq (b,b0)) then
                    raise ERROR "add_named_bbox added the wrong name"
                  else ()
    val g' = G.add_named_bbox b1 g
    val exp = B.NSet.of_list [b0,b1]
    val _ = Testing.assert "g' has correct !-boxes"
                           (B.NSet.eq exp (G.get_bboxes g'))
    val _ = ((G.add_named_bbox b0 g';
              raise ERROR "add_named_bbox (name clash) did not throw exception")
             handle G.duplicate_bbox_exp (b_e,g_e) =>
              (Testing.assert "exception has correct graph" (G.exact_eq g' g_e);
               Testing.assert "exception has correct !-box" (B.name_eq (b0,b_e))))
    in () end) ()



  val _ = Testing.test "G.add_bbox" (fn () => let
    val (b0,g) = G.add_bbox G.empty
    val _ = case B.NSet.tryget_singleton (G.get_bboxes g)
              of NONE => raise ERROR "add_bbox did not add a !-box"
               | SOME b =>
                  if not (B.name_eq (b,b0)) then
                    raise ERROR "add_bbox added the wrong name"
                  else ()
    val (b1,g') = G.add_bbox g
    val exp = B.NSet.of_list [b0,b1]
    val _ = Testing.assert "g' has correct !-boxes"
                           (B.NSet.eq exp (G.get_bboxes g'))
    in () end) ()



  (* TODO: add_bbox_anon *)



  val _ = Testing.test "G.delete_bbox" (fn () => let
    val _ = Testing.assert "G.delete_bbox has no effect on G.empty"
                           (G.is_empty (G.delete_bbox B.default_name G.empty))
    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_bbox "b0" ["v1","v2"]
        |> add_bbox_with_parent "b1" "b0" ["v1"]
        |> add_bbox_with_parents "b2" ["b0","b1"] []
        |> add_bbox "b3" ["v1","v2"]
    val _ = assert_g_eq "G.delete_bbox (no such bbox)"
                        g (G.delete_bbox (B.mk "b9") g)
    val g_exp = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_bbox "b0" ["v1","v2"]
        |> add_bbox_with_parent "b1" "b0" ["v1"]
        |> add_bbox_with_parents "b2" ["b0","b1"] []
    val _ = assert_g_eq "G.delete_bbox (b3)"
                        g_exp (G.delete_bbox (B.mk "b3") g)
    val g_exp = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_bbox "b1" ["v1"]
        |> add_bbox_with_parent "b2" "b1" []
        |> add_bbox "b3" ["v1","v2"]
    val _ = assert_g_eq "G.delete_bbox (b0)"
                        g_exp (G.delete_bbox (B.mk "b0") g)
    val g_exp = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_bbox "b0" ["v1","v2"]
        |> add_bbox_with_parent "b2" "b0" []
        |> add_bbox "b3" ["v1","v2"]
    val _ = assert_g_eq "G.delete_bbox (b1)"
                        g_exp (G.delete_bbox (B.mk "b1") g)
    val g_exp = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_bbox "b0" ["v1","v2"]
        |> add_bbox_with_parent "b1" "b0" ["v1"]
        |> add_bbox "b3" ["v1","v2"]
    val _ = assert_g_eq "G.delete_bbox (b2)"
                        g_exp (G.delete_bbox (B.mk "b2") g)
    in () end) ()



  val _ = Testing.test "G.add_to_bbox" (fn () => let
    val _ = (G.add_to_bbox B.default_name (V.NSet.single V.default_name) G.empty;
             raise ERROR "add_to_bbox (empty) incorrect")
            handle G.no_such_vertex_exp _ => ()
                 | G.no_such_bbox_exp _ => ()
    val g_clean = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_vunit2 "v4"
        |> add_wv "w1"
        |> add_wv "w2a"
        |> add_wv "w2b"
        |> add_wv "w3"
        |> add_wv "w4"
        |> add_dir_eunit1 "e1" "w1" "v1"
        |> add_dir_eunit1 "e2" "v1" "w2a"
        |> add_dir_eunit1 "e3" "w2a" "w2b"
        |> add_dir_eunit1 "e4" "w2b" "v2"
        |> add_dir_eunit1 "e5" "v3" "w3"
        |> add_dir_eunit1 "e6" "w3" "v3"
    val g = g_clean
        |> add_bbox "b0" []
        |> add_bbox_with_parent "b1" "b0" []
        |> add_bbox "b2" []
    val _ = (G.add_to_bbox (B.mk "b0") (V.NSet.single (V.mk "v9")) g;
             raise ERROR "add_to_bbox (no such vertex) incorrect")
            handle G.no_such_vertex_exp (_,v_e,g_e) =>
              (Testing.assert "exception has correct graph" (G.exact_eq g g_e);
               Testing.assert "exception has correct vertex" ((V.dest v_e) = "v9"))
    val _ = (G.add_to_bbox (B.mk "b9") (V.NSet.single (V.mk "v1")) g;
             raise ERROR "add_to_bbox (no such !-box) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph" (G.exact_eq g g_e);
               Testing.assert "exception has correct !-box" ((B.dest b_e) = "b9"))
    fun check_add msg (g,bn,vs) (g_exp,vs_exp) =
    let
      val (vs',g') = G.add_to_bbox (B.mk bn) 
                       (V.NSet.of_list (map V.mk vs)) g
      val _ = assert_g_eq msg g_exp g'
      val _ = Testing.assert (msg^": vs return value correct")
                (V.NSet.eq vs' (V.NSet.of_list (map V.mk vs_exp)))
    in () end
    val g_exp = g_clean
        |> add_bbox "b0" ["v4"]
        |> add_bbox_with_parent "b1" "b0" []
        |> add_bbox "b2" []
    val _ = check_add "G.add_to_bbox (b0,v4)"
              (g,"b0",["v4"])
              (g_exp,["v4"])
    val g_exp = g_clean
        |> add_bbox "b0" ["v1","w1","w2a","w2b"]
        |> add_bbox_with_parent "b1" "b0" []
        |> add_bbox "b2" []
    val _ = check_add "G.add_to_bbox (b0,v1)"
              (g,"b0",["v1"])
              (g_exp,["v1","w1","w2a","w2b"])
    val g_exp = g_clean
        |> add_bbox "b0" ["w2a","w2b"]
        |> add_bbox_with_parent "b1" "b0" []
        |> add_bbox "b2" []
    val _ = check_add "G.add_to_bbox (b0,w2a)"
              (g,"b0",["w2a"])
              (g_exp,["w2a","w2b"])
    val g_exp = g_clean
        |> add_bbox "b0" ["w1"]
        |> add_bbox_with_parent "b1" "b0" []
        |> add_bbox "b2" []
    val _ = check_add "G.add_to_bbox (b0,w1)"
              (g,"b0",["w1"])
              (g_exp,["w1"])
    val g_exp = g_clean
        |> add_bbox "b0" ["w1","w4","v3","w3"]
        |> add_bbox_with_parent "b1" "b0" []
        |> add_bbox "b2" []
    val _ = check_add "G.add_to_bbox (b0,w1,w4,v3)"
              (g,"b0",["w1","w4","v3"])
              (g_exp,["w1","w4","v3","w3"])
    val g' = g_exp
    val g_exp = g_clean
        |> add_bbox "b0" ["w1","w4","v3","w3"]
        |> add_bbox_with_parent "b1" "b0" []
        |> add_bbox "b2" ["v3","w3"]
    val _ = check_add "G.add_to_bbox (b2,v3)"
              (g',"b2",["v3"])
              (g_exp,["v3","w3"])
    val _ = (G.add_to_bbox (B.mk "b1")
               (V.NSet.of_list (map V.mk ["v4"])) g;
             raise ERROR "add_to_bbox (not in parent !-box) incorrect")
            handle G.bbox_bad_parent_exp (bp_e,b_e,g_e) =>
              (Testing.assert "exception has correct graph" (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box" (B.dest b_e = "b1");
               Testing.assert "exception has correct parent !-box" (B.dest bp_e = "b0"))
    in () end) ()



  val _ = Testing.test "G.add_to_bbox_anon" (fn () => let
    val _ = (G.add_to_bbox_anon B.default_name (V.NSet.single V.default_name) G.empty;
             raise ERROR "add_to_bbox (empty) incorrect")
            handle G.no_such_vertex_exp _ => ()
                 | G.no_such_bbox_exp _ => ()

    val g_clean = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_vunit2 "v4"
        |> add_wv "w1"
        |> add_wv "w2a"
        |> add_wv "w2b"
        |> add_wv "w3"
        |> add_wv "w4"
        |> add_dir_eunit1 "e1" "w1" "v1"
        |> add_dir_eunit1 "e2" "v1" "w2a"
        |> add_dir_eunit1 "e3" "w2a" "w2b"
        |> add_dir_eunit1 "e4" "w2b" "v2"
        |> add_dir_eunit1 "e5" "v3" "w3"
        |> add_dir_eunit1 "e6" "w3" "v3"
    val g = g_clean
        |> add_bbox "b0" []
        |> add_bbox_with_parent "b1" "b0" []
        |> add_bbox "b2" []
    val _ = (G.add_to_bbox_anon (B.mk "b0") (V.NSet.single (V.mk "v9")) g;
             raise ERROR "add_to_bbox_anon (no such vertex) incorrect")
            handle G.no_such_vertex_exp (_,v_e,g_e) =>
              (Testing.assert "exception has correct graph" (G.exact_eq g g_e);
               Testing.assert "exception has correct vertex" ((V.dest v_e) = "v9"))
    val _ = (G.add_to_bbox_anon (B.mk "b9") (V.NSet.single (V.mk "v1")) g;
             raise ERROR "add_to_bbox_anon (no such !-box) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph" (G.exact_eq g g_e);
               Testing.assert "exception has correct !-box" ((B.dest b_e) = "b9"))
    fun check_add msg (g,bn,vs) g_exp =
    let
      val g' = G.add_to_bbox_anon (B.mk bn) 
                       (V.NSet.of_list (map V.mk vs)) g
      val _ = assert_g_eq msg g_exp g'
    in () end
    val g_exp = g_clean
        |> add_bbox "b0" ["v4"]
        |> add_bbox_with_parent "b1" "b0" []
        |> add_bbox "b2" []
    val _ = check_add "G.add_to_bbox_anon (b0,v4)"
              (g,"b0",["v4"])
              g_exp
    val g_exp = g_clean
        |> add_bbox "b0" ["v1","w1","w2a","w2b"]
        |> add_bbox_with_parent "b1" "b0" []
        |> add_bbox "b2" []
    val _ = check_add "G.add_to_bbox_anon (b0,v1)"
              (g,"b0",["v1"])
              g_exp
    val g_exp = g_clean
        |> add_bbox "b0" ["w2a","w2b"]
        |> add_bbox_with_parent "b1" "b0" []
        |> add_bbox "b2" []
    val _ = check_add "G.add_to_bbox_anon (b0,w2a)"
              (g,"b0",["w2a"])
              g_exp
    val g_exp = g_clean
        |> add_bbox "b0" ["w1"]
        |> add_bbox_with_parent "b1" "b0" []
        |> add_bbox "b2" []
    val _ = check_add "G.add_to_bbox_anon (b0,w1)"
              (g,"b0",["w1"])
              g_exp
    val g_exp = g_clean
        |> add_bbox "b0" ["w1","w4","v3","w3"]
        |> add_bbox_with_parent "b1" "b0" []
        |> add_bbox "b2" []
    val _ = check_add "G.add_to_bbox_anon (b0,w1,w4,v3)"
              (g,"b0",["w1","w4","v3"])
              g_exp
    val g' = g_exp
    val g_exp = g_clean
        |> add_bbox "b0" ["w1","w4","v3","w3"]
        |> add_bbox_with_parent "b1" "b0" []
        |> add_bbox "b2" ["v3","w3"]
    val _ = check_add "G.add_to_bbox_anon (b2,v3)"
              (g',"b2",["v3"])
              g_exp
    in () end) ()



  val _ = Testing.test "G.remove_from_all_bboxes" (fn () => let
    val _ = Testing.assert "G.remove_from_all_bboxes no effect on G.empty"
              (G.is_empty (G.remove_from_all_bboxes (V.NSet.single
              V.default_name) G.empty))

    val g_clean = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_vunit2 "v4"
        |> add_wv "w1"
        |> add_wv "w2a"
        |> add_wv "w2b"
        |> add_wv "w3"
        |> add_wv "w4"
        |> add_dir_eunit1 "e1" "w1" "v1"
        |> add_dir_eunit1 "e2" "v1" "w2a"
        |> add_dir_eunit1 "e3" "w2a" "w2b"
        |> add_dir_eunit1 "e4" "w2b" "v2"
        |> add_dir_eunit1 "e5" "v3" "w3"
        |> add_dir_eunit1 "e6" "w3" "v3"
    val g = g_clean
        |> add_bbox "b0" ["w1","w4","v3","w3"]
        |> add_bbox_with_parent "b1" "b0" ["w1","w3"]
        |> add_bbox "b3" ["v3","w3","v4"]
        |> add_bbox_with_parent "b2" "b3" ["v3","w3"]

    val g_exp = g_clean
        |> add_bbox "b0" ["w1","w4","v3","w3"]
        |> add_bbox_with_parent "b1" "b0" ["w1","w3"]
        |> add_bbox "b3" ["v3","w3"]
        |> add_bbox_with_parent "b2" "b3" ["v3","w3"]
    val _ = assert_g_eq "G.remove_from_all_bboxes (v4)" g_exp
              (G.remove_from_all_bboxes
               (V.NSet.of_list (map V.mk ["v4"]))
               g)
    val g_exp = g_clean
        |> add_bbox "b0" ["w1","w4","w3"]
        |> add_bbox_with_parent "b1" "b0" ["w1","w3"]
        |> add_bbox "b3" ["w3","v4"]
        |> add_bbox_with_parent "b2" "b3" ["w3"]
    val _ = assert_g_eq "G.remove_from_all_bboxes (v3)" g_exp
              (G.remove_from_all_bboxes
               (V.NSet.of_list (map V.mk ["v3"]))
               g)
    val g_exp = g_clean
        |> add_bbox "b0" ["w1","w4","v3","w3"]
        |> add_bbox_with_parent "b1" "b0" ["w1"]
        |> add_bbox "b3" ["v3","w3","v4"]
        |> add_bbox_with_parent "b2" "b3" ["v3","w3"]
    val _ = assert_g_eq "G.remove_from_all_bboxes (w3)" g_exp
              (G.remove_from_all_bboxes
               (V.NSet.of_list (map V.mk ["w3"]))
               g)
    val g_exp = g_clean
        |> add_bbox "b0" ["w1","w4"]
        |> add_bbox_with_parent "b1" "b0" ["w1"]
        |> add_bbox "b3" ["v4"]
        |> add_bbox_with_parent "b2" "b3" []
    val _ = assert_g_eq "G.remove_from_all_bboxes (v3,w3)" g_exp
              (G.remove_from_all_bboxes
               (V.NSet.of_list (map V.mk ["v3","w3"]))
               g)
    in () end) ()



  val _ = Testing.test "G.remove_from_bbox" (fn () => let
    val _ = (G.remove_from_bbox B.default_name (V.NSet.single V.default_name) G.empty;
             raise ERROR "remove_from_bbox (empty) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph" (G.is_empty g_e);
               Testing.assert "exception has correct !-box" (B.name_eq (b_e,B.default_name)))

    val g_clean = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_vunit2 "v4"
        |> add_wv "w1"
        |> add_wv "w2a"
        |> add_wv "w2b"
        |> add_wv "w3"
        |> add_wv "w4"
        |> add_dir_eunit1 "e1" "w1" "v1"
        |> add_dir_eunit1 "e2" "v1" "w2a"
        |> add_dir_eunit1 "e3" "w2a" "w2b"
        |> add_dir_eunit1 "e4" "w2b" "v2"
        |> add_dir_eunit1 "e5" "v3" "w3"
        |> add_dir_eunit1 "e6" "w3" "v3"
    val g = g_clean
        |> add_bbox "b0" ["w1","w4","v3","w3"]
        |> add_bbox_with_parent "b1" "b0" ["w1","w3"]
        |> add_bbox "b2" ["v3","w3","v4"]

    val _ = (G.remove_from_bbox (B.mk "b9") (V.NSet.single (V.mk "v1")) g;
             raise ERROR "remove_from_bbox (unknown !-box) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph" (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box" (B.dest b_e = "b9"))

    fun check_remove msg (g_exp,vs_exp) (g,bn,vs) = let
      val (vset',g') = G.remove_from_bbox (B.mk bn)
                       (V.NSet.of_list (map V.mk vs))
                       g
      val vset_exp = (V.NSet.of_list (map V.mk vs_exp))
      val _ = assert_g_eq msg g_exp g'
      val _ = if V.NSet.eq vset_exp vset' then ()
              else (writeln "Expected vs:";
                    V.NSet.print vset_exp;
                    writeln "Actual vs:";
                    V.NSet.print vset';
                    raise ERROR (msg^": wrong vs"))
    in () end

    val _ = check_remove "G.remove_from_bbox (b3,v4)"
              (g,[])
              (g,"b0",["v9"])

    val g_exp = g_clean
        |> add_bbox "b0" ["w1","w4","v3","w3"]
        |> add_bbox_with_parent "b1" "b0" ["w1","w3"]
        |> add_bbox "b2" ["v3","w3"]
    val _ = check_remove "G.remove_from_bbox (b3,v4)"
              (g_exp,["v4"])
              (g,"b2",["v4"])

    val g_exp = g_clean
        |> add_bbox "b0" ["w1","w4","v3","w3"]
        |> add_bbox_with_parent "b1" "b0" ["w1","w3"]
        |> add_bbox "b2" ["v3","w3","v4"]
    val _ = check_remove "G.remove_from_bbox (b2,w3)"
              (g_exp,[])
              (g,"b2",["w3"])

    val g_exp = g_clean
        |> add_bbox "b0" ["w1","w4","v3","w3"]
        |> add_bbox_with_parent "b1" "b0" ["w1","w3"]
        |> add_bbox "b2" ["v4"]
    val _ = check_remove "G.remove_from_bbox (b2,v3,w3)"
              (g_exp,["v3","w3"])
              (g,"b2",["v3","w3"])

    val g_exp = g_clean
        |> add_bbox "b0" ["w1","w3"]
        |> add_bbox_with_parent "b1" "b0" ["w1","w3"]
        |> add_bbox "b2" ["v3","w3","v4"]
    val _ = check_remove "G.remove_from_bbox (b0,v3,w4)"
              (g_exp,["v3","w4"])
              (g,"b0",["v3","w4"])

    val g_exp = g_clean
        |> add_bbox "b0" ["w1","w4","v3","w3"]
        |> add_bbox_with_parent "b1" "b0" ["w1"]
        |> add_bbox "b2" ["v3","w3","v4"]
    val _ = check_remove "G.remove_from_bbox (b1,v3,w3)"
              (g_exp,["w3"])
              (g,"b1",["v3","w3"])

    val _ = (G.remove_from_bbox (B.mk "b0")
               (V.NSet.of_list (map V.mk ["v3","w3"])) g;
             raise ERROR "remove_from_bbox (in child !-box) incorrect")
            handle G.bbox_bad_parent_exp (bp_e,b_e,g_e) =>
              (Testing.assert "exception has correct graph" (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box" (B.dest b_e = "b1");
               Testing.assert "exception has correct parent !-box" (B.dest bp_e = "b0"))
    in () end) ()



  (* TODO: remove_from_bbox_anon *)



  val _ = Testing.test "G.add_children_to_bbox" (fn () => let
    val g_clean = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_wv "w1"
        |> add_wv "w2"
    val g = g_clean
        |> add_bbox "b0" ["v1","v2","w1"]
        |> add_bbox "b1" ["w1","w2"]
        |> add_bbox_with_parent "b2" "b0" ["w1"]
        |> add_bbox "b3" ["v2"]
        |> add_bbox "b4" []
        |> add_bbox_with_parent "b5" "b1" ["w1","w2"]

    val _ = (G.add_children_to_bbox (B.mk "b9")
                (B.NSet.of_list (map B.mk ["b0","b1"])) g;
             raise ERROR "add_children_to_bbox (unknown parent !-box) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph (unknown parent !-box)"
                              (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box (unknown parent !-box)"
                              (B.dest b_e = "b9"))
    val _ = (G.add_children_to_bbox (B.mk "b0")
                (B.NSet.of_list (map B.mk ["b3","b9"])) g;
             raise ERROR "add_children_to_bbox (unknown child !-box) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct !-box (unknown child !-box)"
                              (B.dest b_e = "b9"))

    val g_exp = g_clean
        |> add_bbox "b0" ["v1","v2","w1"]
        |> add_bbox "b1" ["w1","w2"]
        |> add_bbox_with_parent "b2" "b0" ["w1"]
        |> add_bbox_with_parent "b3" "b0" ["v2"]
        |> add_bbox_with_parent "b4" "b0" []
        |> add_bbox_with_parent "b5" "b1" ["w1","w2"]
    val _ = assert_g_eq "G.add_children_to_bbox (b0:b3,b4)" g_exp
              (G.add_children_to_bbox (B.mk "b0")
               (B.NSet.of_list (map B.mk ["b3","b4"]))
               g)

    val g_exp = g_clean
        |> add_bbox "b0" ["v1","v2","w1"]
        |> add_bbox "b1" ["w1","w2"]
        |> add_bbox_with_parents "b2" ["b0","b1"] ["w1"]
        |> add_bbox "b3" ["v2"]
        |> add_bbox "b4" []
        |> add_bbox_with_parent "b5" "b1" ["w1","w2"]
    val _ = assert_g_eq "G.add_children_to_bbox (b1:b2)" g_exp
              (G.add_children_to_bbox (B.mk "b1")
               (B.NSet.of_list (map B.mk ["b2"]))
               g)

    val _ = (G.add_children_to_bbox (B.mk "b0")
                (B.NSet.of_list (map B.mk ["b1","b2"])) g;
             raise ERROR "add_children_to_bbox (not subgraph) incorrect")
            handle G.bbox_bad_parent_exp (bp_e,b_e,g_e) =>
              (Testing.assert "exception has correct !-box (not subgraph)"
                              (B.dest b_e = "b1");
               Testing.assert "exception has correct parent !-box (not subgraph)"
                              (B.dest bp_e = "b0"))
    val _ = (G.add_children_to_bbox (B.mk "b5")
                (B.NSet.of_list (map B.mk ["b1","b4"])) g;
             raise ERROR "add_children_to_bbox (breaks anti-symmetry) incorrect")
            handle G.bbox_bad_parent_exp (bp_e,b_e,g_e) =>
              (Testing.assert "exception has correct !-box (breaks anti-symmetry)"
                              (B.dest b_e = "b1");
               Testing.assert "exception has correct parent !-box (breaks anti-symmetry)"
                              (B.dest bp_e = "b5"))

    (* transitivity check *)
    val g_clean = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit1 "v3"
    val g = g_clean
        |> add_bbox "b0" ["v1","v2","v3"]
        |> add_bbox_with_parent "b1" "b0" ["v1","v2","v3"]
        |> add_bbox_with_parents "b2" ["b0","b1"] ["v1","v2"]
        |> add_bbox_with_parents "b3" ["b0","b1","b2"] ["v1","v2"]
        |> add_bbox "b4" ["v2","v3"]
        |> add_bbox_with_parent "b5" "b4" ["v2"]
        |> add_bbox_with_parents "b6" ["b4","b5"] ["v2"]
        |> add_bbox_with_parents "b7" ["b4","b5","b6"] []
    val g_exp = g_clean
        |> add_bbox "b0" ["v1","v2","v3"]
        |> add_bbox_with_parent "b1" "b0" ["v1","v2","v3"]
        |> add_bbox_with_parents "b2" ["b0","b1"] ["v1","v2"]
        |> add_bbox_with_parents "b3" ["b0","b1","b2"] ["v1","v2"]
        |> add_bbox "b4" ["v2","v3"]
        |> add_bbox_with_parents "b5" ["b0","b1","b2","b4"] ["v2"]
        |> add_bbox_with_parents "b6" ["b0","b1","b2","b4","b5"] ["v2"]
        |> add_bbox_with_parents "b7" ["b0","b1","b2","b4","b5","b6"] []
    val _ = assert_g_eq "G.add_children_to_bbox (b2:b5,b6)" g_exp
              (G.add_children_to_bbox (B.mk "b2")
               (B.NSet.of_list (map B.mk ["b5","b6"]))
               g)

    in () end) ()



  val _ = Testing.test "G.add_child_to_bbox" (fn () => let
    val g_clean = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_wv "w1"
        |> add_wv "w2"
    val g = g_clean
        |> add_bbox "b0" ["v1","v2","w1"]
        |> add_bbox "b1" ["w1","w2"]
        |> add_bbox_with_parent "b2" "b0" ["w1"]
        |> add_bbox "b3" ["v2"]
        |> add_bbox "b4" []
        |> add_bbox_with_parent "b5" "b1" ["w1","w2"]

    val _ = (G.add_child_to_bbox (B.mk "b9") (B.mk "b0") g;
             raise ERROR "add_child_to_bbox (unknown parent !-box) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph (unknown parent !-box)"
                              (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box (unknown parent !-box)"
                              (B.dest b_e = "b9"))
    val _ = (G.add_child_to_bbox (B.mk "b0") (B.mk "b9") g;
             raise ERROR "add_child_to_bbox (unknown child !-box) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph (unknown child !-box)"
                              (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box (unknown child !-box)"
                              (B.dest b_e = "b9"))

    val g_exp = g_clean
        |> add_bbox "b0" ["v1","v2","w1"]
        |> add_bbox "b1" ["w1","w2"]
        |> add_bbox_with_parent "b2" "b0" ["w1"]
        |> add_bbox_with_parent "b3" "b0" ["v2"]
        |> add_bbox "b4" []
        |> add_bbox_with_parent "b5" "b1" ["w1","w2"]
    val _ = assert_g_eq "G.add_child_to_bbox (b0:b3)" g_exp
              (G.add_child_to_bbox (B.mk "b0") (B.mk "b3") g)

    val g_exp = g_clean
        |> add_bbox "b0" ["v1","v2","w1"]
        |> add_bbox "b1" ["w1","w2"]
        |> add_bbox_with_parents "b2" ["b0","b1"] ["w1"]
        |> add_bbox "b3" ["v2"]
        |> add_bbox "b4" []
        |> add_bbox_with_parent "b5" "b1" ["w1","w2"]
    val _ = assert_g_eq "G.add_child_to_bbox (b1:b2)" g_exp
              (G.add_child_to_bbox (B.mk "b1") (B.mk "b2") g)

    val _ = (G.add_child_to_bbox (B.mk "b0") (B.mk "b1") g;
             raise ERROR "add_child_to_bbox (not subgraph) incorrect")
            handle G.bbox_bad_parent_exp (bp_e,b_e,g_e) =>
              (Testing.assert "exception has correct graph (not subgraph)"
                              (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box (not subgraph)"
                              (B.dest b_e = "b1");
               Testing.assert "exception has correct parent !-box (not subgraph)"
                              (B.dest bp_e = "b0"))
    val _ = (G.add_child_to_bbox (B.mk "b5") (B.mk "b1") g;
             raise ERROR "add_child_to_bbox (breaks anti-symmetry) incorrect")
            handle G.bbox_bad_parent_exp (bp_e,b_e,g_e) =>
              (Testing.assert "exception has correct graph (breaks anti-symmetry)"
                              (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box (breaks anti-symmetry)"
                              (B.dest b_e = "b1");
               Testing.assert "exception has correct parent (breaks anti-symmetry) !-box"
                              (B.dest bp_e = "b5"))

    (* transitivity check *)
    val g_clean = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit1 "v3"
    val g = g_clean
        |> add_bbox "b0" ["v1","v2","v3"]
        |> add_bbox_with_parent "b1" "b0" ["v1","v2","v3"]
        |> add_bbox_with_parents "b2" ["b0","b1"] ["v1","v2"]
        |> add_bbox_with_parents "b3" ["b0","b1","b2"] ["v1","v2"]
        |> add_bbox "b4" ["v2","v3"]
        |> add_bbox_with_parent "b5" "b4" ["v2"]
        |> add_bbox_with_parents "b6" ["b4","b5"] ["v2"]
        |> add_bbox_with_parents "b7" ["b4","b5","b6"] []
    val g_exp = g_clean
        |> add_bbox "b0" ["v1","v2","v3"]
        |> add_bbox_with_parent "b1" "b0" ["v1","v2","v3"]
        |> add_bbox_with_parents "b2" ["b0","b1"] ["v1","v2"]
        |> add_bbox_with_parents "b3" ["b0","b1","b2"] ["v1","v2"]
        |> add_bbox "b4" ["v2","v3"]
        |> add_bbox_with_parents "b5" ["b0","b1","b2","b4"] ["v2"]
        |> add_bbox_with_parents "b6" ["b0","b1","b2","b4","b5"] ["v2"]
        |> add_bbox_with_parents "b7" ["b0","b1","b2","b4","b5","b6"] []
    val _ = assert_g_eq "G.add_child_to_bbox (b2:b5)" g_exp
              (G.add_child_to_bbox (B.mk "b2") (B.mk "b5") g)
    in () end) ()



  val _ = Testing.test "G.remove_child_from_bbox" (fn () => let
    val g_clean = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit1 "v3"
    val g = g_clean
        |> add_bbox "b0" ["v1","v2","v3"]
        |> add_bbox_with_parent "b1" "b0" ["v1","v2","v3"]
        |> add_bbox_with_parents "b2" ["b0","b1"] ["v1","v2"]
        |> add_bbox_with_parent "b3" "b0" ["v2","v3"]
        |> add_bbox_with_parents "b4" ["b0","b1","b2","b3"] ["v2"]

    val _ = (G.remove_child_from_bbox (B.mk "b9") (B.mk "b0") g;
             raise ERROR "remove_child_from_bbox (unknown parent !-box) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph (unknown parent !-box)"
                              (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box (unknown parent !-box)"
                              (B.dest b_e = "b9"))
    val _ = (G.remove_child_from_bbox (B.mk "b0") (B.mk "b9") g;
             raise ERROR "remove_child_from_bbox (unknown child !-box) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph (unknown child !-box)"
                              (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box (unknown child !-box)"
                              (B.dest b_e = "b9"))

    (* not connected *)
    val _ = assert_g_eq "G.remove_child_from_bbox (b1:b3)" g
              (G.remove_child_from_bbox (B.mk "b1") (B.mk "b3") g)

    (* would break transitivity *)
    val _ = assert_g_eq "G.remove_child_from_bbox (b0:b2)" g
              (G.remove_child_from_bbox (B.mk "b0") (B.mk "b2") g)

    val g_exp = g_clean
        |> add_bbox "b0" ["v1","v2","v3"]
        |> add_bbox "b1" ["v1","v2","v3"]
        |> add_bbox_with_parents "b2" ["b0","b1"] ["v1","v2"]
        |> add_bbox_with_parent "b3" "b0" ["v2","v3"]
        |> add_bbox_with_parents "b4" ["b0","b1","b2","b3"] ["v2"]
    val _ = assert_g_eq "G.remove_child_from_bbox (b0:b4)" g_exp
              (G.remove_child_from_bbox (B.mk "b0") (B.mk "b1") g)
    in () end) ()



  val _ = Testing.test "G.clear_bbox_children" (fn () => let
    val g_clean = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit1 "v3"
    val g = g_clean
        |> add_bbox "b0" ["v1","v2","v3"]
        |> add_bbox_with_parent "b1" "b0" ["v1","v2","v3"]
        |> add_bbox_with_parents "b2" ["b0","b1"] ["v1","v2"]
        |> add_bbox_with_parent "b3" "b0" ["v2","v3"]
        |> add_bbox_with_parents "b4" ["b0","b1","b2","b3"] ["v2"]

    val _ = (G.clear_bbox_children (B.mk "b9") g;
             raise ERROR "clear_bbox_children (unknown !-box) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph (unknown !-box)"
                              (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box (unknown !-box)"
                              (B.dest b_e = "b9"))

    (* leaf !-box *)
    val _ = assert_g_eq "G.clear_bbox_children (b4)" g
              (G.clear_bbox_children (B.mk "b4") g)

    val g_exp = g_clean
        |> add_bbox "b0" ["v1","v2","v3"]
        |> add_bbox_with_parent "b1" "b0" ["v1","v2","v3"]
        |> add_bbox_with_parents "b2" ["b0","b1"] ["v1","v2"]
        |> add_bbox_with_parent "b3" "b0" ["v2","v3"]
        |> add_bbox_with_parents "b4" ["b0","b1","b2"] ["v2"]
    val _ = assert_g_eq "G.clear_bbox_children (b3)" g_exp
              (G.clear_bbox_children (B.mk "b3") g)

    val g_exp = g_clean
        |> add_bbox "b0" ["v1","v2","v3"]
        |> add_bbox "b1" ["v1","v2","v3"]
        |> add_bbox_with_parents "b2" ["b1"] ["v1","v2"]
        |> add_bbox "b3" ["v2","v3"]
        |> add_bbox_with_parents "b4" ["b1","b2","b3"] ["v2"]
    val _ = assert_g_eq "G.clear_bbox_children (b0)" g_exp
              (G.clear_bbox_children (B.mk "b0") g)
    in () end) ()



  val _ = Testing.test "G.get_bbox_parents" (fn () => let
    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit1 "v3"
        |> add_bbox "b0" ["v1","v2","v3"]
        |> add_bbox_with_parent "b1" "b0" ["v1","v2","v3"]
        |> add_bbox_with_parents "b2" ["b0","b1"] ["v1","v2"]
        |> add_bbox_with_parent "b3" "b0" ["v2","v3"]
        |> add_bbox_with_parents "b4" ["b0","b1","b2","b3"] ["v2"]

    val _ = (G.get_bbox_parents g (B.mk "b9");
             raise ERROR "get_bbox_parents (unknown !-box) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph (unknown !-box)"
                              (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box (unknown !-box)"
                              (B.dest b_e = "b9"))

    val _ = assert_bboxes "b0" []
                          (G.get_bbox_parents g (B.mk "b0"))
    val _ = assert_bboxes "b3" ["b0"]
                          (G.get_bbox_parents g (B.mk "b3"))
    val _ = assert_bboxes "b4" ["b0","b1","b2","b3"]
                          (G.get_bbox_parents g (B.mk "b4"))
    in () end) ()



  val _ = Testing.test "G.get_bbox_children" (fn () => let
    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit1 "v3"
        |> add_bbox "b0" ["v1","v2","v3"]
        |> add_bbox_with_parent "b1" "b0" ["v1","v2","v3"]
        |> add_bbox_with_parents "b2" ["b0","b1"] ["v1","v2"]
        |> add_bbox_with_parent "b3" "b0" ["v2","v3"]
        |> add_bbox_with_parents "b4" ["b0","b1","b2","b3"] ["v2"]

    val _ = (G.get_bbox_children g (B.mk "b9");
             raise ERROR "get_bbox_children (unknown !-box) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph (unknown !-box)"
                              (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box (unknown !-box)"
                              (B.dest b_e = "b9"))

    val _ = assert_bboxes "b0" ["b1","b2","b3","b4"]
                          (G.get_bbox_children g (B.mk "b0"))
    val _ = assert_bboxes "b3" ["b4"]
                          (G.get_bbox_children g (B.mk "b3"))
    val _ = assert_bboxes "b4" []
                          (G.get_bbox_children g (B.mk "b4"))
    in () end) ()



  val _ = Testing.test "G.bbox_has_parents" (fn () => let
    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit1 "v3"
        |> add_bbox "b0" ["v1","v2","v3"]
        |> add_bbox_with_parent "b1" "b0" ["v1","v2","v3"]
        |> add_bbox_with_parents "b2" ["b0","b1"] ["v1","v2"]
        |> add_bbox_with_parent "b3" "b0" ["v2","v3"]
        |> add_bbox_with_parents "b4" ["b0","b1","b2","b3"] ["v2"]

    val _ = (G.bbox_has_parents g (B.mk "b9");
             raise ERROR "bbox_has_parents (unknown !-box) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph (unknown !-box)"
                              (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box (unknown !-box)"
                              (B.dest b_e = "b9"))

    val _ = Testing.assert "b0 has no parents"
                          (not (G.bbox_has_parents g (B.mk "b0")))
    val _ = Testing.assert "b3 has parents"
                          (G.bbox_has_parents g (B.mk "b3"))
    val _ = Testing.assert "b4 has parents"
                          (G.bbox_has_parents g (B.mk "b4"))
    in () end) ()



  val _ = Testing.test "G.bbox_has_children" (fn () => let
    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit1 "v3"
        |> add_bbox "b0" ["v1","v2","v3"]
        |> add_bbox_with_parent "b1" "b0" ["v1","v2","v3"]
        |> add_bbox_with_parents "b2" ["b0","b1"] ["v1","v2"]
        |> add_bbox_with_parent "b3" "b0" ["v2","v3"]
        |> add_bbox_with_parents "b4" ["b0","b1","b2","b3"] ["v2"]

    val _ = (G.bbox_has_children g (B.mk "b9");
             raise ERROR "bbox_has_children (unknown !-box) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph (unknown !-box)"
                              (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box (unknown !-box)"
                              (B.dest b_e = "b9"))

    val _ = Testing.assert "b0 has children"
                          (G.bbox_has_children g (B.mk "b0"))
    val _ = Testing.assert "b3 has children"
                          (G.bbox_has_children g (B.mk "b3"))
    val _ = Testing.assert "b4 has no children"
                          (not (G.bbox_has_children g (B.mk "b4")))
    in () end) ()



  val _ = Testing.test "G.get_vertices_in_bbox" (fn () => let
    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit1 "v3"
        |> add_bbox "b0" ["v1","v2","v3"]
        |> add_bbox_with_parent "b1" "b0" ["v1","v2","v3"]
        |> add_bbox_with_parents "b2" ["b0","b1"] ["v1","v2"]
        |> add_bbox_with_parent "b3" "b0" ["v2","v3"]
        |> add_bbox_with_parents "b4" ["b0","b1","b2","b3"] ["v2"]
        |> add_bbox "b5" []

    val _ = (G.get_vertices_in_bbox g (B.mk "b9");
             raise ERROR "get_vertices_in_bbox (unknown !-box) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph (unknown !-box)"
                              (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box (unknown !-box)"
                              (B.dest b_e = "b9"))

    val _ = assert_vertices "b0" ["v1","v2","v3"]
                          (G.get_vertices_in_bbox g (B.mk "b0"))
    val _ = assert_vertices "b3" ["v2","v3"]
                          (G.get_vertices_in_bbox g (B.mk "b3"))
    val _ = assert_vertices "b4" ["v2"]
                          (G.get_vertices_in_bbox g (B.mk "b4"))
    val _ = assert_vertices "b5" []
                          (G.get_vertices_in_bbox g (B.mk "b5"))
    in () end) ()



  val _ = Testing.test "G.get_bboxes_containing_vertex" (fn () => let
    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit1 "v3"
        |> add_vunit1 "v4"
        |> add_bbox "b0" ["v1","v2","v3"]
        |> add_bbox_with_parent "b1" "b0" ["v1","v2","v3"]
        |> add_bbox_with_parents "b2" ["b0","b1"] ["v1","v2"]
        |> add_bbox_with_parent "b3" "b0" ["v2","v3"]
        |> add_bbox_with_parents "b4" ["b0","b1","b2","b3"] ["v2"]
        |> add_bbox "b5" []

    val _ = (G.get_bboxes_containing_vertex g (V.mk "v9");
             raise ERROR "get_bboxes_containing_vertex (unknown vertex) incorrect")
            handle G.no_such_vertex_exp (_,v_e,g_e) =>
              (Testing.assert "exception has correct graph (unknown vertex)"
                              (G.exact_eq g_e g);
               Testing.assert "exception has correct vertex (unknown vertex)"
                              (V.dest v_e = "v9"))

    val _ = assert_bboxes "v1" ["b0","b1","b2"]
                          (G.get_bboxes_containing_vertex g (V.mk "v1"))
    val _ = assert_bboxes "v2" ["b0","b1","b2","b3","b4"]
                          (G.get_bboxes_containing_vertex g (V.mk "v2"))
    val _ = assert_bboxes "v3" ["b0","b1","b3"]
                          (G.get_bboxes_containing_vertex g (V.mk "v3"))
    val _ = assert_bboxes "v4" []
                          (G.get_bboxes_containing_vertex g (V.mk "v4"))
    in () end) ()



  val _ = Testing.test "G.kill_bbox" (fn () => let
    val _ = (G.kill_bbox B.default_name G.empty;
             raise ERROR "kill_bbox (empty) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph" (G.is_empty g_e);
               Testing.assert "exception has correct !-box" (B.name_eq (b_e,B.default_name)))

    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_vunit2 "v4"
        |> add_bbox "b0" ["v1","v2"]
        |> add_bbox_with_parent "b1" "b0" ["v1"]
        |> add_bbox_with_parents "b2" ["b0","b1"] []
        |> add_bbox "b4" ["v2"]
        |> add_bbox_with_parent "b3" "b4" ["v2"]
        |> add_bbox "b5" ["v3"]
    val _ = (G.kill_bbox (B.mk "b9") g;
             raise ERROR "kill_bbox (empty) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph" (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box" (B.dest b_e = "b9"))

    val g_exp = G.empty
        |> add_vunit2 "v3"
        |> add_vunit2 "v4"
        |> add_bbox "b4" []
        |> add_bbox_with_parent "b3" "b4" []
        |> add_bbox "b5" ["v3"]
    val _ = assert_g_eq "G.kill_bbox (b0)"
                        g_exp (G.kill_bbox (B.mk "b0") g)
    val g_exp = G.empty
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_vunit2 "v4"
        |> add_bbox "b0" ["v2"]
        |> add_bbox "b4" ["v2"]
        |> add_bbox_with_parent "b3" "b4" ["v2"]
        |> add_bbox "b5" ["v3"]
    val _ = assert_g_eq "G.kill_bbox (b1)"
                        g_exp (G.kill_bbox (B.mk "b1") g)
    val g_exp = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_vunit2 "v4"
        |> add_bbox "b0" ["v1","v2"]
        |> add_bbox_with_parent "b1" "b0" ["v1"]
        |> add_bbox "b4" ["v2"]
        |> add_bbox_with_parent "b3" "b4" ["v2"]
        |> add_bbox "b5" ["v3"]
    val _ = assert_g_eq "G.kill_bbox (b2)"
                        g_exp (G.kill_bbox (B.mk "b2") g)
    val g_exp = G.empty
        |> add_vunit1 "v1"
        |> add_vunit2 "v3"
        |> add_vunit2 "v4"
        |> add_bbox "b0" ["v1"]
        |> add_bbox_with_parent "b1" "b0" ["v1"]
        |> add_bbox_with_parents "b2" ["b0","b1"] []
        |> add_bbox "b4" []
        |> add_bbox "b5" ["v3"]
    val _ = assert_g_eq "G.kill_bbox (b3)"
                        g_exp (G.kill_bbox (B.mk "b3") g)
    val g_exp = G.empty
        |> add_vunit1 "v1"
        |> add_vunit2 "v3"
        |> add_vunit2 "v4"
        |> add_bbox "b0" ["v1"]
        |> add_bbox_with_parent "b1" "b0" ["v1"]
        |> add_bbox_with_parents "b2" ["b0","b1"] []
        |> add_bbox "b5" ["v3"]
    val _ = assert_g_eq "G.kill_bbox (b4)"
                        g_exp (G.kill_bbox (B.mk "b4") g)
    val g_exp = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v4"
        |> add_bbox "b0" ["v1","v2"]
        |> add_bbox_with_parent "b1" "b0" ["v1"]
        |> add_bbox_with_parents "b2" ["b0","b1"] []
        |> add_bbox "b4" ["v2"]
        |> add_bbox_with_parent "b3" "b4" ["v2"]
    val _ = assert_g_eq "G.kill_bbox (b5)"
                        g_exp (G.kill_bbox (B.mk "b5") g)
    in () end) ()



  val _ = Testing.test "G.drop_bbox" (fn () => let
    val _ = (G.drop_bbox B.default_name G.empty;
             raise ERROR "drop_bbox (empty) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph" (G.is_empty g_e);
               Testing.assert "exception has correct !-box" (B.name_eq (b_e,B.default_name)))

    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_bbox "b0" ["v1","v2"]
        |> add_bbox_with_parent "b1" "b0" ["v1"]
        |> add_bbox_with_parents "b2" ["b0","b1"] []
        |> add_bbox "b3" ["v1","v2"]
    val _ = (G.drop_bbox (B.mk "b9") g;
             raise ERROR "drop_bbox (empty) incorrect")
            handle G.no_such_bbox_exp (_,b_e,g_e) =>
              (Testing.assert "exception has correct graph" (G.exact_eq g_e g);
               Testing.assert "exception has correct !-box" (B.dest b_e = "b9"))

    val g_exp = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_bbox "b0" ["v1","v2"]
        |> add_bbox_with_parent "b1" "b0" ["v1"]
        |> add_bbox_with_parents "b2" ["b0","b1"] []
    val _ = assert_g_eq "G.drop_bbox (b3)"
                        g_exp (G.drop_bbox (B.mk "b3") g)
    val g_exp = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_bbox "b1" ["v1"]
        |> add_bbox_with_parent "b2" "b1" []
        |> add_bbox "b3" ["v1","v2"]
    val _ = assert_g_eq "G.drop_bbox (b0)"
                        g_exp (G.drop_bbox (B.mk "b0") g)
    val g_exp = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_bbox "b0" ["v1","v2"]
        |> add_bbox_with_parent "b2" "b0" []
        |> add_bbox "b3" ["v1","v2"]
    val _ = assert_g_eq "G.drop_bbox (b1)"
                        g_exp (G.drop_bbox (B.mk "b1") g)
    val g_exp = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit2 "v3"
        |> add_bbox "b0" ["v1","v2"]
        |> add_bbox_with_parent "b1" "b0" ["v1"]
        |> add_bbox "b3" ["v1","v2"]
    val _ = assert_g_eq "G.drop_bbox (b2)"
                        g_exp (G.drop_bbox (B.mk "b2") g)
    in () end) ()



  val _ = Testing.test "G.merge_bboxes" (fn () => let
    val g_clean = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit1 "v3"

    val g = g_clean
        |> add_bbox "bl" ["v1"]
        |> add_bbox "br" ["v3"]
    val _ = (G.merge_bboxes B.NSet.empty g;
             raise ERROR "merge_bboxes (none given) incorrect")
            handle G.merge_bbox_exp _ => ()
    val _ = (G.merge_bboxes (B.NSet.of_list (map B.mk ["bl","bm","br"])) g;
             raise ERROR "merge_bboxes (unknown !-box) incorrect")
            handle G.no_such_bbox_exp (_,b_e,_) =>
              (Testing.assert "exception has correct !-box" (B.dest b_e = "bm"))

    val g = g_clean
        |> add_bbox "bl" ["v1"]
        |> add_bbox "br" ["v3"]
    val (bm,g_merged) =
        G.merge_bboxes (B.NSet.of_list (map B.mk ["bl","br"])) g
    val g_exp = g_clean
        |> add_bbox (B.dest bm) ["v1","v3"]
    val _ = assert_g_eq "G.merge_bboxes (1)" g_exp g_merged

    val g = g_clean
        |> add_bbox "bp" ["v1","v2","v3"]
        |> add_bbox_with_parent "bl" "bp" ["v1"]
        |> add_bbox_with_parent "br" "bp" ["v3"]
    val (bm,g_merged) =
        G.merge_bboxes (B.NSet.of_list (map B.mk ["bl","br"])) g
    val g_exp = g_clean
        |> add_bbox "bp" ["v1","v2","v3"]
        |> add_bbox_with_parent (B.dest bm) "bp" ["v1","v3"]
    val _ = assert_g_eq "G.merge_bboxes (2)" g_exp g_merged

    val g = g_clean
        |> add_bbox "bp" ["v1","v2","v3"]
        |> add_bbox_with_parent "bl" "bp" ["v1"]
        |> add_bbox_with_parent "bm" "bp" ["v2"]
        |> add_bbox_with_parent "br" "bp" ["v3"]
    val (bm,g_merged) =
        G.merge_bboxes (B.NSet.of_list (map B.mk ["bl","bm","br"])) g
    val g_exp = g_clean
        |> add_bbox "bp" ["v1","v2","v3"]
        |> add_bbox_with_parent (B.dest bm) "bp" ["v1","v2","v3"]
    val _ = assert_g_eq "G.merge_bboxes (3)" g_exp g_merged

    val g = g_clean
        |> add_bbox "bp" ["v1","v2","v3"]
        |> add_bbox_with_parent "bl" "bp" ["v1"]
        |> add_bbox_with_parent "bm" "bp" ["v2"]
        |> add_bbox "br" ["v3"]
    val _ = (G.merge_bboxes (B.NSet.of_list (map B.mk ["bl","bm","br"])) g;
             raise ERROR "merge_bboxes (different parents) incorrect")
            handle G.merge_bbox_exp _ => ()

    val g = g_clean
        |> add_bbox "bp" ["v1","v2","v3"]
        |> add_bbox "bl" ["v1","v2"]
        |> add_bbox "br" ["v2","v3"]
    val _ = (G.merge_bboxes (B.NSet.of_list (map B.mk ["bl","br"])) g;
             raise ERROR "merge_bboxes (overlapping) incorrect")
            handle G.merge_bbox_exp _ => ()
    in () end) ()



  val _ = Testing.test "G.expand_bbox" (fn () => let
    (* TODO: val expand_bbox : B.name -> (VSub.T * ESub.T * BSub.T) * T
                             -> (VSub.T * ESub.T * BSub.T) * T *)
    in () end) ()



  val _ = Testing.test "G.copy_bbox" (fn () => let
    (* TODO: val copy_bbox : B.name -> (VSub.T * ESub.T * BSub.T) * T
                           -> (VSub.T * ESub.T * BSub.T) * T *)
    in () end) ()



  val _ = Testing.test "G.fresh_copy_bbox" (fn () => let
    (* TODO: val fresh_copy_bbox : B.name -> T -> (VSub.T * ESub.T * BSub.T) * T *)
    in () end) ()



  (* TODO: fresh_copy_bbox_anon *)



  val _ = Testing.test "G.fresh_expand_bbox" (fn () => let
    (* TODO: val fresh_expand_bbox : B.name -> T -> (VSub.T * ESub.T * BSub.T) * T *)
    in () end) ()



  val _ = Testing.test "G.rename_bang_graph" (fn () => let
    (* TODO: val rename_bang_graph : (VSub.T * ESub.T * BSub.T) -> T ->
                                     (VSub.T * ESub.T * BSub.T) * T *)
    in () end) ()



  val _ = Testing.test "G.rename_bang_graph" (fn () => let
    (* TODO: val rename_bang_graph_anon : (VSub.T * ESub.T * BSub.T) -> T -> T *)
    in () end) ()



  val _ = Testing.test "G.rename_bang_graph" (fn () => let
    (* TODO: val rename_bboxes : BSub.T -> T -> T *)
    in () end) ()



  val _ = Testing.test "G.rename_bang_graph" (fn () => let
    (* TODO: val rename_bbox : B.name -> B.name -> T -> T *)
    in () end) ()



  val _ = Testing.test "G.rename_bang_graph" (fn () => let
    (* TODO: val rename_bbox_opt : B.name -> B.name -> T -> T option *)
    in () end) ()


  (* TODO: tests for !-box openness preservation:
   * add_edge(_anon)
   * add_named_edge
   * merge_by_subgraph
   * merge_by_vertices
   * normalise
   * minimise
   * split_edge
   * plug(_anon)
   * plug_and_normalise(_anon)
   * plug_and_minimise(_anon)
   *
   * TODO: renamings don't muck up !-box containment relation:
   * rename_ograph(_anon)
   * rename_vertices
   * rename_vertex
   * rename_vertex_opt
   *
   * TODO: functions account for !-boxes:
   * is_intersection (?)
   * get_plugging
   * are_pluggable
   * get_open_subgraph (?)
   * is_subgraph
   * exact_eq
   *)

  val _ = Testing.assert_no_failed_tests()
end;

local
  structure BG_Tests = Bang_Graph_Interface_Tests(Test_Bang_Graph);
in val _ = () end;

