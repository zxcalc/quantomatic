(* Tests for the OGRAPH signature *)
functor OGraph_Interface_Tests(
  G : TEST_OGRAPH
)
= struct
  structure Tools = Test_OGraph_Tools(G)
  open Tools

  (* TODO: tests for Graph.Data wrappers? *)
  (* TODO: subst_in_vdata *)
  (* TODO: subst_in_edata *)
  (* TODO: apply_data_subst *)



  val _ = Testing.test "G.empty" (fn () => let
    val _ = assert_vertices "G.empty" [] (G.get_vertices G.empty)
    val _ = assert_edges "G.empty" [] (G.get_edges G.empty)
    in () end) ()



  val _ = Testing.test "G.is_empty" (fn () => let
    val _ = Testing.assert "is_empty true when empty"
                           (G.is_empty G.empty)
    val _ = Testing.assert "is_empty false when not empty"
                           (not (G.is_empty (add_vunit1 "a" G.empty)))
    in () end) ()



  val _ = Testing.test "G.has_vertex" (fn () => let
    val _ = Testing.assert "empty" (not (G.has_vertex G.empty (V.mk "a")));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = Testing.assert "a" (G.has_vertex g (V.mk "a"));
    val _ = Testing.assert "b" (G.has_vertex g (V.mk "b"));
    val _ = Testing.assert "e" (not (G.has_vertex g (V.mk "e")));
  in () end) ();



  val _ = Testing.test "G.has_edge" (fn () => let
    val _ = Testing.assert "empty" (not (G.has_edge G.empty (E.mk "a")));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = Testing.assert "e" (G.has_edge g (E.mk "e"));
    val _ = Testing.assert "f" (G.has_edge g (E.mk "f"));
    val _ = Testing.assert "a" (not (G.has_edge g (E.mk "a")));
  in () end) ();



  val _ = Testing.test "G.get_edge_source" (fn () => let
    val _ = (G.get_edge_source G.empty (E.mk "a");
              raise ERROR "G.get_edge_source did not raise exception")
            handle G.no_such_edge_exp (_,en,g) =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = Testing.assert "source of e is a"
            (V.name_eq (G.get_edge_source g (E.mk "e"),V.mk "a"));
    val _ = Testing.assert "source of f is b"
            (V.name_eq (G.get_edge_source g (E.mk "f"),V.mk "b"));
    val _ = (G.get_edge_source g (E.mk "a");
              raise ERROR "G.get_edge_source did not raise exception")
            handle G.no_such_edge_exp (_,en,g') =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.get_edge_target" (fn () => let
    val _ = (G.get_edge_target G.empty (E.mk "a");
              raise ERROR "G.get_edge_target did not raise exception")
            handle G.no_such_edge_exp (_,en,g) =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = Testing.assert "target of e is b"
            (V.name_eq (G.get_edge_target g (E.mk "e"),V.mk "b"));
    val _ = Testing.assert "target of f is b"
            (V.name_eq (G.get_edge_target g (E.mk "f"),V.mk "b"));
    val _ = (G.get_edge_target g (E.mk "a");
              raise ERROR "G.get_edge_target did not raise exception")
            handle G.no_such_edge_exp (_,en,g') =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.get_edge_info" (fn () => let
    val _ = (G.get_edge_info G.empty (E.mk "a");
              raise ERROR "G.get_edge_info (empty) did not raise exception")
            handle G.no_such_edge_exp (_,en,g) =>
              (Testing.assert "correct ename in exp (empty)"
                              (E.string_of_name en = "a");
               Testing.assert "correct graph in exp (empty)"
                              (G.is_empty g))

    val angle = Expr.parse "x + z"
    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eexpr2_a "f" "b" "b" angle;

    val _ = (G.get_edge_info g (E.mk "a");
              raise ERROR "G.get_edge_info (unknown edge) did not raise exception")
            handle G.no_such_edge_exp (_,en,g_e) =>
              (Testing.assert "correct ename in exp (unknown edge)"
                              (E.string_of_name en = "a");
               Testing.assert "correct graph in exp (unknown edge)"
                              (G.exact_eq g_e g))

    val _ =
      let
        val ((dod,ed),(s,t)) = G.get_edge_info g (E.mk "e")
      in
        Testing.assert "e: correct dir_or_undir" (dod = Undirected);
        Testing.assert "e: correct data" (G.edata_eq (ed,eunit1));
        Testing.assert "e: correct source" (V.name_eq (V.mk "a",s));
        Testing.assert "e: correct target" (V.name_eq (V.mk "b",t))
      end

    val _ =
      let
        val ((dod,ed),(s,t)) = G.get_edge_info g (E.mk "f")
      in
        Testing.assert "e: correct dir_or_undir" (dod = Directed);
        Testing.assert "e: correct data" (G.edata_eq (ed,eexpr2_a angle));
        Testing.assert "e: correct source" (V.name_eq (V.mk "b",s));
        Testing.assert "e: correct target" (V.name_eq (V.mk "b",t))
      end
    in () end) ();



  val _ = Testing.test "G.edge_get_other_vertex" (fn () => let
    val _ = (G.edge_get_other_vertex G.empty (E.mk "a") (V.mk "a");
              raise ERROR "G.edge_get_other_vertex did not raise exception")
            handle G.no_such_edge_exp (_,en,g) =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = Testing.assert "target of e is b"
            (V.name_eq (G.edge_get_other_vertex g (E.mk "e") (V.mk "a"), V.mk "b"));
    val _ = Testing.assert "source of e is b"
            (V.name_eq (G.edge_get_other_vertex g (E.mk "e") (V.mk "b"), V.mk "a"));
    val _ = Testing.assert "target of f is b"
            (V.name_eq (G.edge_get_other_vertex g (E.mk "f") (V.mk "b"), V.mk "b"));
    val _ = (G.edge_get_other_vertex g (E.mk "a") (V.mk "a");
              raise ERROR "G.edge_get_other_vertex did not raise exception")
            handle G.no_such_edge_exp (_,en,g') =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    val _ = (G.edge_get_other_vertex g (E.mk "e") (V.mk "c");
              raise ERROR "G.edge_get_other_vertex did not raise exception")
            handle G.not_an_endpoint_exp (_,en,vn,g') =>
              (Testing.assert "correct vname in exp" (V.name_eq (vn,V.mk "c"));
               Testing.assert "correct ename in exp" (E.name_eq (en,E.mk "e"));
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    val _ = (G.edge_get_other_vertex g (E.mk "f") (V.mk "a");
              raise ERROR "G.edge_get_other_vertex did not raise exception")
            handle G.not_an_endpoint_exp (_,en,vn,g') =>
              (Testing.assert "correct vname in exp" (V.name_eq (vn,V.mk "a"));
               Testing.assert "correct ename in exp" (E.name_eq (en,E.mk "f"));
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.get_in_edges" (fn () => let
    val _ = (G.get_in_edges G.empty (V.mk "a");
              raise ERROR "G.get_in_edges did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = Testing.assert "in edges of a = []"
            (E.NSet.is_empty (G.get_in_edges g (V.mk "a")));
    val _ = Testing.assert "in edges of b = [e,f]"
            (E.NSet.eq (G.get_in_edges g (V.mk "b"))
                       (E.NSet.of_list (map E.mk ["e","f"])));

    val _ = (G.get_in_edges g (V.mk "c");
             raise ERROR "G.get_in_edges did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp" (V.name_eq (vn,V.mk "c"));
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.get_out_edges" (fn () => let
    val _ = (G.get_out_edges G.empty (V.mk "a");
              raise ERROR "G.get_out_edges did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = Testing.assert "out edges of a = [e]"
            (E.NSet.eq (G.get_out_edges g (V.mk "a"))
                       (E.NSet.of_list (map E.mk ["e"])));
    val _ = Testing.assert "out edges of b = [f]"
            (E.NSet.eq (G.get_out_edges g (V.mk "b"))
                       (E.NSet.of_list (map E.mk ["f"])));

    val _ = (G.get_out_edges g (V.mk "c");
             raise ERROR "G.get_out_edges did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp" (V.name_eq (vn,V.mk "c"));
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.get_adj_edges" (fn () => let
    val _ = (G.get_adj_edges G.empty (V.mk "a");
              raise ERROR "G.get_adj_edges (empty) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp (empty)"
                              (V.string_of_name vn = "a");
               Testing.assert "correct graph in exp (empty)"
                              (G.is_empty g));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = Testing.assert "adj edges of a = [e]"
            (E.NSet.eq (G.get_adj_edges g (V.mk "a"))
                       (E.NSet.of_list (map E.mk ["e"])));
    val _ = Testing.assert "adj edges of b = [e,f]"
            (E.NSet.eq (G.get_adj_edges g (V.mk "b"))
                       (E.NSet.of_list (map E.mk ["e","f"])));

    val _ = (G.get_adj_edges g (V.mk "c");
             raise ERROR "G.get_adj_edges (unknown vertex) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (unknown vertex)"
                              (V.name_eq (vn,V.mk "c"));
               Testing.assert "correct graph in exp (unknown vertex)"
                              (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.get_in_dir_edges" (fn () => let
    val _ = (G.get_in_dir_edges G.empty (V.mk "a");
              raise ERROR "G.get_in_dir_edges did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_vunit2 "c"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b"
                    |> add_dir_eunit1 "g" "b" "a"
                    |> add_dir_eunit1 "h" "a" "b"
                    |> add_undir_eunit1 "i" "a" "c"
                    |> add_undir_eunit1 "j" "c" "a"

    val _ = Testing.assert "in dir edges of a = [g]"
            (E.NSet.eq (G.get_in_dir_edges g (V.mk "a"))
                       (E.NSet.of_list (map E.mk ["g"])));
    val _ = Testing.assert "in dir edges of b = [f,h]"
            (E.NSet.eq (G.get_in_dir_edges g (V.mk "b"))
                       (E.NSet.of_list (map E.mk ["f","h"])));
    val _ = Testing.assert "in dir edges of c = []"
            (E.NSet.is_empty (G.get_in_dir_edges g (V.mk "c")));

    val _ = (G.get_in_dir_edges g (V.mk "e");
             raise ERROR "G.get_in_dir_edges did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp" (V.name_eq (vn,V.mk "e"));
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.get_out_dir_edges" (fn () => let
    val _ = (G.get_out_dir_edges G.empty (V.mk "a");
              raise ERROR "G.get_out_dir_edges did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_vunit2 "c"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b"
                    |> add_dir_eunit1 "g" "b" "a"
                    |> add_dir_eunit1 "h" "a" "b"
                    |> add_undir_eunit1 "i" "a" "c"
                    |> add_undir_eunit1 "j" "c" "a"

    val _ = Testing.assert "out dir edges of a = [h]"
            (E.NSet.eq (G.get_out_dir_edges g (V.mk "a"))
                       (E.NSet.of_list (map E.mk ["h"])));
    val _ = Testing.assert "out dir edges of b = [f,g]"
            (E.NSet.eq (G.get_out_dir_edges g (V.mk "b"))
                       (E.NSet.of_list (map E.mk ["f","g"])));
    val _ = Testing.assert "out dir edges of c = []"
            (E.NSet.is_empty (G.get_out_dir_edges g (V.mk "c")));

    val _ = (G.get_out_dir_edges g (V.mk "e");
             raise ERROR "G.get_out_dir_edges did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp" (V.name_eq (vn,V.mk "e"));
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.get_adj_undir_edges" (fn () => let
    val _ = (G.get_adj_undir_edges G.empty (V.mk "a");
              raise ERROR "G.get_adj_undir_edges did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_vunit2 "c"
                    |> add_vunit2 "d"
                    |> add_undir_eunit1 "e" "a" "d"
                    |> add_dir_eunit2 "f" "b" "b"
                    |> add_dir_eunit1 "g" "b" "a"
                    |> add_dir_eunit1 "h" "a" "b"
                    |> add_undir_eunit1 "i" "a" "c"
                    |> add_undir_eunit1 "j" "c" "a"

    val _ = Testing.assert "adj undir edges of a = [e,i,j]"
            (E.NSet.eq (G.get_adj_undir_edges g (V.mk "a"))
                       (E.NSet.of_list (map E.mk ["e","i","j"])));
    val _ = Testing.assert "adj undir edges of b = []"
            (E.NSet.is_empty (G.get_adj_undir_edges g (V.mk "b")));
    val _ = Testing.assert "adj undir edges of c = [i,j]"
            (E.NSet.eq (G.get_adj_undir_edges g (V.mk "c"))
                       (E.NSet.of_list (map E.mk ["i","j"])));
    val _ = Testing.assert "adj undir edges of d = [e]"
            (E.NSet.eq (G.get_adj_undir_edges g (V.mk "d"))
                       (E.NSet.of_list (map E.mk ["e"])));

    val _ = (G.get_adj_undir_edges g (V.mk "e");
             raise ERROR "G.get_adj_undir_edges did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp" (V.name_eq (vn,V.mk "e"));
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.get_adj_vertices" (fn () => let
    val _ = (G.get_adj_vertices G.empty (V.mk "a");
              raise ERROR "G.get_adj_vertices (empty) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp (empty)"
                              (V.string_of_name vn = "a");
               Testing.assert "correct graph in exp (empty)"
                              (G.is_empty g));

    val g = G.empty |> add_vunit1 "v1"
                    |> add_vunit1 "v2"
                    |> add_vunit1 "v3"
                    |> add_vunit1 "v4"
                    |> add_vunit1 "v5"
                    |> add_wv "v6"
                    |> add_vunit1 "v7"
                    |> add_vunit1 "v8"
                    |> add_undir_eunit1 "e1" "v2" "v2"
                    |> add_dir_eunit1 "e2" "v3" "v3"
                    |> add_undir_eunit1 "e3" "v4" "v4"
                    |> add_dir_eunit1 "e4" "v4" "v5"
                    |> add_dir_eunit1 "e5" "v4" "v6"
                    |> add_dir_eunit1 "e6" "v5" "v7"
                    |> add_dir_eunit1 "e7" "v7" "v6"
                    |> add_undir_eunit1 "e8" "v7" "v8"
                    |> add_undir_eunit1 "e9" "v8" "v7"

    val _ = (G.get_adj_vertices g (V.mk "v0");
             raise ERROR "G.get_adj_vertices (unknown vertex) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (unknown vertex)"
                              (V.name_eq (vn,V.mk "v0"));
               Testing.assert "correct graph in exp (unknown vertex)"
                              (G.exact_eq g g'));

    val _ = Testing.assert "adj vertices of v1 = []"
            (V.NSet.is_empty (G.get_adj_vertices g (V.mk "v1")));
    val _ = Testing.assert "adj vertices of v2 = []"
            (V.NSet.is_empty (G.get_adj_vertices g (V.mk "v2")));
    val _ = Testing.assert "adj vertices of v3 = []"
            (V.NSet.is_empty (G.get_adj_vertices g (V.mk "v3")));
    val _ = Testing.assert "adj vertices of v4 = [v5,v6]"
            (V.NSet.eq (G.get_adj_vertices g (V.mk "v4"))
                       (V.NSet.of_list (map V.mk ["v5","v6"])));
    val _ = Testing.assert "adj vertices of v6 = [v4,v7]"
            (V.NSet.eq (G.get_adj_vertices g (V.mk "v6"))
                       (V.NSet.of_list (map V.mk ["v4","v7"])));
    val _ = Testing.assert "adj vertices of v7 = [v5,v6,v8]"
            (V.NSet.eq (G.get_adj_vertices g (V.mk "v7"))
                       (V.NSet.of_list (map V.mk ["v5","v6","v8"])));
    in () end) ();



  val _ = Testing.test "G.get_adj_vertices_to_set" (fn () => let
    val _ = (G.get_adj_vertices_to_set G.empty (V.NSet.single (V.mk "a"));
              raise ERROR "G.get_adj_vertices_to_set (empty) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp (empty)"
                              (V.string_of_name vn = "a");
               Testing.assert "correct graph in exp (empty)"
                              (G.is_empty g));

    val g = G.empty |> add_vunit1 "v1"
                    |> add_vunit1 "v2"
                    |> add_vunit1 "v3"
                    |> add_wv "v4"
                    |> add_vunit1 "v5"
                    |> add_vunit1 "v6"
                    |> add_vunit1 "v7"
                    |> add_wv "v8"
                    |> add_undir_eunit1 "e1" "v2" "v3"
                    |> add_dir_eunit1 "e2" "v3" "v4"
                    |> add_dir_eunit1 "e3" "v4" "v6"
                    |> add_dir_eunit1 "e4" "v5" "v3"
                    |> add_dir_eunit1 "e5" "v5" "v6"
                    |> add_dir_eunit1 "e6" "v5" "v7"
                    |> add_dir_eunit1 "e7" "v6" "v7"
                    |> add_undir_eunit1 "e8" "v7" "v8"
                    |> add_undir_eunit1 "e9" "v8" "v7"

    val _ = (G.get_adj_vertices_to_set g (V.NSet.of_list (map V.mk ["v0","v1"]));
             raise ERROR "G.get_adj_vertices_to_set (unknown vertex) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (unknown vertex)"
                              (V.name_eq (vn,V.mk "v0"));
               Testing.assert "correct graph in exp (unknown vertex)"
                              (G.exact_eq g g'));

    val _ = Testing.assert "adj vertices of [v1] = []"
            (V.NSet.is_empty (G.get_adj_vertices_to_set g
              (V.NSet.of_list (map V.mk ["v1"]))))
    val _ = Testing.assert "adj vertices of [v5,v6] = [v3,v4,v7]"
            (V.NSet.eq (G.get_adj_vertices_to_set g
                          (V.NSet.of_list (map V.mk ["v5","v6"])))
                       (V.NSet.of_list (map V.mk ["v3","v4","v7"])))
    in () end) ();



  val _ = Testing.test "G.get_successor_vertices" (fn () => let
    val _ = (G.get_successor_vertices G.empty (V.mk "a");
              raise ERROR "G.get_successor_vertices (empty) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp (empty)"
                              (V.string_of_name vn = "a");
               Testing.assert "correct graph in exp (empty)"
                              (G.is_empty g))

    val g = G.empty |> add_vunit1 "v1"
                    |> add_vunit1 "v2"
                    |> add_vunit1 "v3"
                    |> add_vunit1 "v4"
                    |> add_vunit1 "v5"
                    |> add_wv "v6"
                    |> add_vunit1 "v7"
                    |> add_vunit1 "v8"
                    |> add_undir_eunit1 "e1" "v2" "v2"
                    |> add_dir_eunit1 "e2" "v3" "v3"
                    |> add_undir_eunit1 "e3" "v4" "v4"
                    |> add_dir_eunit1 "e4" "v4" "v5"
                    |> add_dir_eunit1 "e5" "v4" "v6"
                    |> add_dir_eunit1 "e6" "v5" "v7"
                    |> add_dir_eunit1 "e7" "v7" "v6"
                    |> add_undir_eunit1 "e8" "v7" "v8"
                    |> add_undir_eunit1 "e9" "v8" "v7"

    val _ = (G.get_successor_vertices g (V.mk "v0");
             raise ERROR "G.get_successor_vertices (unknown vertex) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (unknown vertex)"
                              (V.name_eq (vn,V.mk "v0"));
               Testing.assert "correct graph in exp (unknown vertex)"
                              (G.exact_eq g g'))

    val _ = Testing.assert "successor vertices of v1 = []"
            (V.NSet.is_empty (G.get_successor_vertices g (V.mk "v1")))
    val _ = Testing.assert "successor vertices of v2 = []"
            (V.NSet.is_empty (G.get_successor_vertices g (V.mk "v2")))
    val _ = Testing.assert "successor vertices of v3 = []"
            (V.NSet.is_empty (G.get_successor_vertices g (V.mk "v3")))
    val _ = Testing.assert "successor vertices of v4 = [v5,v6]"
            (V.NSet.eq (G.get_successor_vertices g (V.mk "v4"))
                       (V.NSet.of_list (map V.mk ["v5","v6"])))
    val _ = Testing.assert "successor vertices of v5 = [v7]"
            (V.NSet.eq (G.get_successor_vertices g (V.mk "v5"))
                       (V.NSet.of_list (map V.mk ["v7"])))
    val _ = Testing.assert "successor vertices of v6 = []"
            (V.NSet.is_empty (G.get_successor_vertices g (V.mk "v6")))
    val _ = Testing.assert "successor vertices of v7 = [v6]"
            (V.NSet.eq (G.get_successor_vertices g (V.mk "v7"))
                       (V.NSet.of_list (map V.mk ["v6"])))
    val _ = Testing.assert "successor vertices of v8 = []"
            (V.NSet.is_empty (G.get_successor_vertices g (V.mk "v8")))
    in () end) ()



  val _ = Testing.test "G.get_predecessor_vertices" (fn () => let
    val _ = (G.get_predecessor_vertices G.empty (V.mk "a");
              raise ERROR "G.get_predecessor_vertices (empty) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp (empty)"
                              (V.string_of_name vn = "a");
               Testing.assert "correct graph in exp (empty)"
                              (G.is_empty g))

    val g = G.empty |> add_vunit1 "v1"
                    |> add_vunit1 "v2"
                    |> add_vunit1 "v3"
                    |> add_vunit1 "v4"
                    |> add_vunit1 "v5"
                    |> add_wv "v6"
                    |> add_vunit1 "v7"
                    |> add_vunit1 "v8"
                    |> add_undir_eunit1 "e1" "v2" "v2"
                    |> add_dir_eunit1 "e2" "v3" "v3"
                    |> add_undir_eunit1 "e3" "v4" "v4"
                    |> add_dir_eunit1 "e4" "v4" "v5"
                    |> add_dir_eunit1 "e5" "v4" "v6"
                    |> add_dir_eunit1 "e6" "v5" "v7"
                    |> add_dir_eunit1 "e7" "v7" "v6"
                    |> add_undir_eunit1 "e8" "v7" "v8"
                    |> add_undir_eunit1 "e9" "v8" "v7"

    val _ = (G.get_predecessor_vertices g (V.mk "v0");
             raise ERROR "G.get_predecessor_vertices (unknown vertex) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (unknown vertex)"
                              (V.name_eq (vn,V.mk "v0"));
               Testing.assert "correct graph in exp (unknown vertex)"
                              (G.exact_eq g g'))

    val _ = Testing.assert "predecessor vertices of v1 = []"
            (V.NSet.is_empty (G.get_predecessor_vertices g (V.mk "v1")))
    val _ = Testing.assert "predecessor vertices of v2 = []"
            (V.NSet.is_empty (G.get_predecessor_vertices g (V.mk "v2")))
    val _ = Testing.assert "predecessor vertices of v3 = []"
            (V.NSet.is_empty (G.get_predecessor_vertices g (V.mk "v3")))
    val _ = Testing.assert "predecessor vertices of v4 = []"
            (V.NSet.is_empty (G.get_predecessor_vertices g (V.mk "v4")))
    val _ = Testing.assert "predecessor vertices of v5 = [v4]"
            (V.NSet.eq (G.get_predecessor_vertices g (V.mk "v5"))
                       (V.NSet.of_list (map V.mk ["v4"])))
    val _ = Testing.assert "predecessor vertices of v6 = [v4,v7]"
            (V.NSet.eq (G.get_predecessor_vertices g (V.mk "v6"))
                       (V.NSet.of_list (map V.mk ["v4","v7"])))
    val _ = Testing.assert "predecessor vertices of v7 = [v5]"
            (V.NSet.eq (G.get_predecessor_vertices g (V.mk "v7"))
                       (V.NSet.of_list (map V.mk ["v5"])))
    val _ = Testing.assert "predecessor vertices of v8 = []"
            (V.NSet.is_empty (G.get_predecessor_vertices g (V.mk "v8")))
    in () end) ();



  val _ = Testing.test "G.get_sibling_vertices" (fn () => let
    val _ = (G.get_sibling_vertices G.empty (V.mk "a");
              raise ERROR "G.get_sibling_vertices (empty) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp (empty)"
                              (V.string_of_name vn = "a");
               Testing.assert "correct graph in exp (empty)"
                              (G.is_empty g))

    val g = G.empty |> add_vunit1 "v1"
                    |> add_vunit1 "v2"
                    |> add_vunit1 "v3"
                    |> add_vunit1 "v4"
                    |> add_vunit1 "v5"
                    |> add_wv "v6"
                    |> add_vunit1 "v7"
                    |> add_vunit1 "v8"
                    |> add_undir_eunit1 "e1" "v2" "v2"
                    |> add_dir_eunit1 "e2" "v3" "v3"
                    |> add_undir_eunit1 "e3" "v4" "v4"
                    |> add_undir_eunit1 "e4" "v4" "v5"
                    |> add_dir_eunit1 "e5" "v4" "v6"
                    |> add_undir_eunit1 "e6" "v5" "v7"
                    |> add_dir_eunit1 "e7" "v7" "v6"
                    |> add_undir_eunit1 "e8" "v7" "v8"
                    |> add_undir_eunit1 "e9" "v8" "v7"

    val _ = (G.get_sibling_vertices g (V.mk "v0");
             raise ERROR "G.get_sibling_vertices (unknown vertex) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (unknown vertex)"
                              (V.name_eq (vn,V.mk "v0"));
               Testing.assert "correct graph in exp (unknown vertex)"
                              (G.exact_eq g g'))

    val _ = Testing.assert "sibling vertices of v1 = []"
            (V.NSet.is_empty (G.get_sibling_vertices g (V.mk "v1")))
    val _ = Testing.assert "sibling vertices of v2 = []"
            (V.NSet.is_empty (G.get_sibling_vertices g (V.mk "v2")))
    val _ = Testing.assert "sibling vertices of v3 = []"
            (V.NSet.is_empty (G.get_sibling_vertices g (V.mk "v3")))
    val _ = Testing.assert "sibling vertices of v4 = [v5]"
            (V.NSet.eq (G.get_sibling_vertices g (V.mk "v4"))
                       (V.NSet.of_list (map V.mk ["v5"])))
    val _ = Testing.assert "sibling vertices of v5 = [v4,v7]"
            (V.NSet.eq (G.get_sibling_vertices g (V.mk "v5"))
                       (V.NSet.of_list (map V.mk ["v4","v7"])))
    val _ = Testing.assert "sibling vertices of v6 = []"
            (V.NSet.is_empty (G.get_sibling_vertices g (V.mk "v6")))
    val _ = Testing.assert "sibling vertices of v7 = [v5,v8]"
            (V.NSet.eq (G.get_sibling_vertices g (V.mk "v7"))
                       (V.NSet.of_list (map V.mk ["v5","v8"])))
    val _ = Testing.assert "sibling vertices of v8 = [v7]"
            (V.NSet.eq (G.get_sibling_vertices g (V.mk "v8"))
                       (V.NSet.of_list (map V.mk ["v7"])))
    in () end) ();



  val _ = Testing.test "G.adj_edge_classes" (fn () => let
    val _ = (G.adj_edge_classes G.empty (V.mk "a");
              raise ERROR "G.adj_edge_classes (empty) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp (empty)"
                              (V.string_of_name vn = "a");
               Testing.assert "correct graph in exp (empty)"
                              (G.is_empty g))

    val g = G.empty |> add_vunit1 "v1"
                    |> add_wv "v2"
                    |> add_vunit1 "v3"
                    |> add_wv "v4"
                    |> add_vunit1 "v5"
                    |> add_vunit1 "v6"
                    |> add_dir_eunit1 "e1" "v1" "v2"
                    |> add_undir_eunit1 "e2" "v1" "v3"
                    |> add_dir_eunit1 "e3" "v4" "v1"
                    |> add_undir_eunit1 "e4" "v5" "v1"
                    |> add_dir_eunit1 "e5" "v1" "v5"
                    |> add_dir_eunit1 "e6" "v5" "v1"
                    |> add_dir_eunit1 "e7" "v5" "v5"
                    |> add_undir_eunit1 "e8" "v5" "v5"

    val _ = (G.adj_edge_classes g (V.mk "v0");
             raise ERROR "G.adj_edge_classes (unknown vertex) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (unknown vertex)"
                              (V.name_eq (vn,V.mk "v0"));
               Testing.assert "correct graph in exp (unknown vertex)"
                              (G.exact_eq g g'))

    val _ = Testing.assert "adj edge classes of v6"
            (let
              val (ie,oe,ae) = G.adj_edge_classes g (V.mk "v6")
             in
               E.NSet.is_empty ie andalso
               E.NSet.is_empty oe andalso
               E.NSet.is_empty ae
             end)
    val _ = Testing.assert "adj edge classes of v1"
            (let
              val (ie,oe,ae) = G.adj_edge_classes g (V.mk "v1")
             in
               E.NSet.eq ie (E.NSet.of_list (map E.mk ["e3","e6"]))
               andalso
               E.NSet.eq oe (E.NSet.of_list (map E.mk ["e1","e5"]))
               andalso
               E.NSet.eq ae (E.NSet.of_list (map E.mk ["e2","e4"]))
             end)
    val _ = Testing.assert "adj edge classes of v5"
            (let
              val (ie,oe,ae) = G.adj_edge_classes g (V.mk "v5")
             in
               E.NSet.eq ie (E.NSet.of_list (map E.mk ["e5","e7"]))
               andalso
               E.NSet.eq oe (E.NSet.of_list (map E.mk ["e6","e7"]))
               andalso
               E.NSet.eq ae (E.NSet.of_list (map E.mk ["e4","e8"]))
             end)
    in () end) ();



  val _ = Testing.test "G.get_vdata_tab" (fn () => let
    val _ = Testing.assert "G.get_vdata_tab on G.empty is empty"
            (VTab.is_empty (G.get_vdata_tab G.empty))

    val g = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_undir_eunit1 "e" "a" "b"
        |> add_dir_eunit2 "f" "b" "b"
    val exptab = VTab.empty
        |> VTab.set (V.mk "a",vunit1)
        |> VTab.set (V.mk "b",vunit2)
    val _ = Testing.assert "get_vdata_tab correct"
              (VTab.tab_eq G.vdata_eq (exptab,G.get_vdata_tab g))
    in () end) ();



  val _ = Testing.test "G.get_edata_tab" (fn () => let
    val _ = Testing.assert "G.get_edata_tab on G.empty is empty"
            (ETab.is_empty (G.get_edata_tab G.empty))

    val g = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_undir_eunit1 "e" "a" "b"
        |> add_dir_eunit2 "f" "b" "b"
    val exptab = ETab.empty
        |> ETab.set (E.mk "e",eunit1)
        |> ETab.set (E.mk "f",eunit2)
    val _ = Testing.assert "get_edata_tab correct"
              (ETab.tab_eq G.edata_eq (exptab,G.get_edata_tab g))
    in () end) ();



  val _ = Testing.test "G.get_vertex_data" (fn () => let
    val _ = (G.get_vertex_data G.empty (V.mk "a");
             raise ERROR "G.get_vertex_data did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.exact_eq g G.empty))

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b"

    val (vd,(ins,outs)) = (G.get_vertex_data g (V.mk "a"),
                           (G.get_in_edges g   (V.mk "a"),
                            G.get_out_edges g  (V.mk "a")))
    val _ = Testing.assert "get_vertex_data correct data"
              (G.vdata_eq (vd,vunit1));
    val _ = assert_edges "ins 1" [] ins;
    val _ = assert_edges "outs 1" ["e"] outs;

    val (vd,(ins,outs)) = (G.get_vertex_data g (V.mk "b"),
                           (G.get_in_edges g   (V.mk "b"),
                            G.get_out_edges g  (V.mk "b")))
    val _ = Testing.assert "get_vertex_data correct data" (G.vdata_eq (vd,vunit2));
    val _ = assert_edges "ins 2" ["e","f"] ins;
    val _ = assert_edges "outs 2" ["f"] outs;

    val _ = (G.get_vertex_data g (V.mk "e");
             raise ERROR "G.get_vertex_data did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "e");
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.get_edge_data" (fn () => let
    val _ = (G.get_edge_data G.empty (E.mk "a");
              raise ERROR "G.get_edge_data did not raise exception")
            handle G.no_such_edge_exp (_,en,g) =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = Testing.assert "data of 'e' correct"
            (G.edata_eq (G.get_edge_data g (E.mk "e"),eunit1));
    val _ = Testing.assert "data of 'f' correct"
            (G.edata_eq (G.get_edge_data g (E.mk "f"),eunit2));

    val _ = (G.get_edge_data g (E.mk "c");
             raise ERROR "G.get_edge_data did not raise exception")
            handle G.no_such_edge_exp (_,en,g') =>
              (Testing.assert "correct ename in exp" (E.name_eq (en,E.mk "c"));
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.get_edge_dir" (fn () => let
    val _ = (G.get_edge_dir G.empty (E.mk "a");
              raise ERROR "G.get_edge_dir (empty) did not raise exception")
            handle G.no_such_edge_exp (_,en,g) =>
              (Testing.assert "correct ename in exp (empty)" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp (empty)" (G.is_empty g))

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b"

    val _ = Testing.assert "dir of 'e' correct"
            (G.get_edge_dir g (E.mk "e") = Undirected)
    val _ = Testing.assert "dir of 'f' correct"
            (G.get_edge_dir g (E.mk "f") = Directed)

    val _ = (G.get_edge_data g (E.mk "a");
             raise ERROR "G.get_edge_data (unknown edge) did not raise exception")
            handle G.no_such_edge_exp (_,en,g') =>
              (Testing.assert "correct ename in exp (unknown edge)" (E.name_eq (en,E.mk "a"));
               Testing.assert "correct graph in exp (unknown edge)" (G.exact_eq g g'))
    in () end) ()



  val _ = Testing.test "G.get_edge_dir_and_data" (fn () => let
    val _ = (G.get_edge_dir_and_data G.empty (E.mk "a");
             raise ERROR "G.get_edge_dir_and_data did not raise exception")
            handle G.no_such_edge_exp (_,en,g') =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.exact_eq G.empty g'));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val ((dir,ed),(src,tgt)) = (G.get_edge_dir_and_data g (E.mk "e"),
                                (G.get_edge_source g      (E.mk "e"),
                                 G.get_edge_target g      (E.mk "e")))
    val _ = Testing.assert "get_edge_dir_and_data correct data" (G.edata_eq (ed,eunit1));
    val _ = Testing.assert "get_edge_dir_and_data correct direction"
              (case dir of Directed => false | Undirected => true);
    val _ = Testing.assert "get_edge_source correct" (V.name_eq (src,V.mk "a"));
    val _ = Testing.assert "get_edge_target correct" (V.name_eq (tgt,V.mk "b"));
    val ((dir,ed),(src,tgt)) = (G.get_edge_dir_and_data g (E.mk "f"),
                                (G.get_edge_source g      (E.mk "f"),
                                 G.get_edge_target g      (E.mk "f")))
    val _ = Testing.assert "get_edge_dir_and_data correct data" (G.edata_eq (ed,eunit2));
    val _ = Testing.assert "get_edge_dir_and_data correct direction"
              (case dir of Directed => true | Undirected => false);
    val _ = Testing.assert "get_edge_dir_and_data correct src" (V.name_eq (src,V.mk "b"));
    val _ = Testing.assert "get_edge_dir_and_data correct tgt" (V.name_eq (tgt,V.mk "b"));

    val _ = (G.get_edge_dir_and_data g (E.mk "a");
             raise ERROR "G.get_edge_dir_and_data did not raise exception")
            handle G.no_such_edge_exp (_,en,g') =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.set_vertex_data" (fn () => let
    val _ = (G.set_vertex_data vunit1 (V.mk "a") G.empty;
              raise ERROR "G.set_vertex_data did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_dir_eunit1 "e" "a" "b"
                    |> add_undir_eunit2 "f" "b" "a";

    val g' = G.set_vertex_data vunit2 (V.mk "a") g;
    val _ = assert_vertex_info "g'" g'
              [("a",(vunit2,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))];
    val _ = assert_edge_info "g'" g'
              [("e",((Directed,eunit1),("a","b"))),
               ("f",((Undirected,eunit2),("b","a")))];

    val g'' = G.set_vertex_data vunit2 (V.mk "b") g';
    val _ = assert_vertex_info "g''" g''
              [("a",(vunit2,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))];
    val _ = assert_edge_info "g''" g''
              [("e",((Directed,eunit1),("a","b"))),
               ("f",((Undirected,eunit2),("b","a")))];
    in () end) ();



  val _ = Testing.test "G.set_edge_data" (fn () => let
    val _ = (G.set_edge_data eunit1 (E.mk "a") G.empty;
              raise ERROR "G.set_edge_data did not raise exception")
            handle G.no_such_edge_exp (_,en,g) =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_dir_eunit1 "e" "a" "b"
                    |> add_undir_eunit2 "f" "b" "a";

    val g' = G.set_edge_data eunit2 (E.mk "e") g;
    val _ = assert_vertex_info "g'" g'
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))];
    val _ = assert_edge_info "g'" g'
              [("e",((Directed,eunit2),("a","b"))),
               ("f",((Undirected,eunit2),("b","a")))];

    val g'' = G.set_edge_data eunit2 (E.mk "f") g';
    val _ = assert_vertex_info "g''" g''
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))];
    val _ = assert_edge_info "g''" g''
              [("e",((Directed,eunit2),("a","b"))),
               ("f",((Undirected,eunit2),("b","a")))];
    in () end) ();



  val _ = Testing.test "G.set_edge_dir" (fn () => let
    val _ = (G.set_edge_dir Directed (E.mk "a") G.empty;
              raise ERROR "G.set_edge_dir (empty) did not raise exception")
            handle G.no_such_edge_exp (_,en,g) =>
              (Testing.assert "correct ename in exp (empty)"
                              (E.string_of_name en = "a");
               Testing.assert "correct graph in exp (empty)"
                              (G.is_empty g))

    val g = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_dir_eunit1 "e" "a" "b"
        |> add_undir_eunit2 "f" "b" "a"
        |> add_dir_eunit1 "g" "a" "b"
        |> add_undir_eunit2 "h" "b" "a"

    val _ = (G.set_edge_dir Directed (E.mk "a") G.empty;
              raise ERROR "G.set_edge_dir (unknown) did not raise exception")
            handle G.no_such_edge_exp (_,en,g) =>
              (Testing.assert "correct ename in exp (unknown)"
                              (E.string_of_name en = "a");
               Testing.assert "correct graph in exp (unknown)"
                              (G.is_empty g))

    val _ = assert_g_eq "G.set_edge_dir (1)" g
            (G.set_edge_dir Directed (E.mk "e") g)
    val _ = assert_g_eq "G.set_edge_dir (2)" g
            (G.set_edge_dir Undirected (E.mk "f") g)
    val g_exp = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_undir_eunit1 "e" "a" "b"
        |> add_undir_eunit2 "f" "b" "a"
        |> add_dir_eunit1 "g" "a" "b"
        |> add_undir_eunit2 "h" "b" "a"
    val _ = assert_g_eq "G.set_edge_dir (3)" g_exp
            (G.set_edge_dir Undirected (E.mk "e") g)
    val g_exp = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_dir_eunit1 "e" "a" "b"
        |> add_dir_eunit2 "f" "b" "a"
        |> add_dir_eunit1 "g" "a" "b"
        |> add_undir_eunit2 "h" "b" "a"
    val _ = assert_g_eq "G.set_edge_dir (4)" g_exp
            (G.set_edge_dir Directed (E.mk "f") g)
    in () end) ();



  val _ = Testing.test "G.update_vertex_data" (fn () => let
    val _ = (G.update_vertex_data I (V.mk "a") G.empty;
              raise ERROR "G.get_update_vertex_data did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_dir_eunit1 "e" "a" "b"
                    |> add_undir_eunit2 "f" "b" "a";

    fun swap_vd vd =
      if G.vdata_eq (vd,vert Data.VUnit1)
      then vert Data.VUnit2
      else
        if G.vdata_eq (vd,vert Data.VUnit2)
        then vert Data.VUnit1
        else vd;

    val g' = G.update_vertex_data swap_vd (V.mk "a") g;
    val _ = assert_vertex_info "g'" g'
              [("a",(vunit2,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))];
    val _ = assert_edge_info "g'" g'
              [("e",((Directed,eunit1),("a","b"))),
               ("f",((Undirected,eunit2),("b","a")))];

    val g'' = G.update_vertex_data swap_vd (V.mk "b") g';
    val _ = assert_vertex_info "g''" g''
              [("a",(vunit2,(["f"],["e"]))),
               ("b",(vunit1,(["e"],["f"])))];
    val _ = assert_edge_info "g''" g''
              [("e",((Directed,eunit1),("a","b"))),
               ("f",((Undirected,eunit2),("b","a")))];
    in () end) ();




  val _ = Testing.test "G.update_edge_data" (fn () => let
    val _ = (G.update_edge_data I (E.mk "a") G.empty;
              raise ERROR "G.get_update_edge_data did not raise exception")
            handle G.no_such_edge_exp (_,en,g) =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_dir_eunit1 "e" "a" "b"
                    |> add_undir_eunit2 "f" "b" "a";

    fun swap_ed Data.EUnit1 = Data.EUnit2
      | swap_ed Data.EUnit2 = Data.EUnit1
      | swap_ed ed = ed;

    val g' = G.update_edge_data swap_ed (E.mk "e") g;
    val _ = assert_vertex_info "g'" g'
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))];
    val _ = assert_edge_info "g'" g'
              [("e",((Directed,eunit2),("a","b"))),
               ("f",((Undirected,eunit2),("b","a")))];

    val g'' = G.update_edge_data swap_ed (E.mk "f") g';
    val _ = assert_vertex_info "g''" g''
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))];
    val _ = assert_edge_info "g''" g''
              [("e",((Directed,eunit2),("a","b"))),
               ("f",((Undirected,eunit1),("b","a")))];
    in () end) ();



  val _ = Testing.test "G.update_edge_dir" (fn () => let
    val _ = (G.update_edge_dir I (E.mk "a") G.empty;
              raise ERROR "G.update_edge_dir (empty) did not raise exception")
            handle G.no_such_edge_exp (_,en,g) =>
              (Testing.assert "correct ename in exp (empty)"
                              (E.string_of_name en = "a");
               Testing.assert "correct graph in exp (empty)"
                              (G.is_empty g))

    val g = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_dir_eunit1 "e" "a" "b"
        |> add_undir_eunit2 "f" "b" "a"
        |> add_dir_eunit1 "g" "a" "b"
        |> add_undir_eunit2 "h" "b" "a"

    val _ = (G.update_edge_dir I (E.mk "a") G.empty;
              raise ERROR "G.update_edge_dir (unknown) did not raise exception")
            handle G.no_such_edge_exp (_,en,g) =>
              (Testing.assert "correct ename in exp (unknown)"
                              (E.string_of_name en = "a");
               Testing.assert "correct graph in exp (unknown)"
                              (G.is_empty g))

    val _ = assert_g_eq "G.update_edge_dir (1)" g
            (G.update_edge_dir
                (fn Directed => Directed
                  | Undirected => raise ERROR "bad input (1)")
              (E.mk "e") g)
    val _ = assert_g_eq "G.update_edge_dir (2)" g
            (G.update_edge_dir
                (fn Undirected => Undirected
                  | Directed => raise ERROR "bad input (2)")
              (E.mk "f") g)
    val g_exp = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_undir_eunit1 "e" "a" "b"
        |> add_undir_eunit2 "f" "b" "a"
        |> add_dir_eunit1 "g" "a" "b"
        |> add_undir_eunit2 "h" "b" "a"
    val _ = assert_g_eq "G.update_edge_dir (3)" g_exp
            (G.update_edge_dir
                (fn Directed => Undirected
                  | Undirected => raise ERROR "bad input (3)")
              (E.mk "e") g)
    val g_exp = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_dir_eunit1 "e" "a" "b"
        |> add_dir_eunit2 "f" "b" "a"
        |> add_dir_eunit1 "g" "a" "b"
        |> add_undir_eunit2 "h" "b" "a"
    val _ = assert_g_eq "G.update_edge_dir (4)" g_exp
            (G.update_edge_dir
                (fn Undirected => Directed
                  | Directed => raise ERROR "bad input (4)")
              (E.mk "f") g)
    in () end) ();



  val _ = Testing.test "G.is_wire_vertex" (fn () => let
    val _ = (G.is_wire_vertex G.empty (V.mk "a");
              raise ERROR "G.update_edge_dir (empty) did not raise exception")
            handle G.no_such_vertex_exp (_,en,g) =>
              (Testing.assert "correct vertex in exp (empty)"
                              (V.string_of_name en = "a");
               Testing.assert "correct graph in exp (empty)"
                              (G.is_empty g))

    val g = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_wv "c"
        |> add_wv "d"
        |> add_dir_eunit1 "e" "a" "c"
        |> add_undir_eunit2 "f" "c" "b"
        |> add_dir_eunit1 "g" "d" "d"
        |> add_dir_eunit1 "h" "a" "a"

    val _ = (G.is_wire_vertex g (V.mk "e");
              raise ERROR "G.update_edge_dir (unknown) did not raise exception")
            handle G.no_such_vertex_exp (_,en,g_e) =>
              (Testing.assert "correct vertex in exp (unknown)"
                              (V.string_of_name en = "e");
               Testing.assert "correct graph in exp (unknown)"
                              (G.exact_eq g_e g))

    val _ = Testing.assert "not is_wire_vertex a"
            (not (G.is_wire_vertex g (V.mk "a")))
    val _ = Testing.assert "not is_wire_vertex b"
            (not (G.is_wire_vertex g (V.mk "b")))
    val _ = Testing.assert "is_wire_vertex c"
            (G.is_wire_vertex g (V.mk "c"))
    val _ = Testing.assert "is_wire_vertex d"
            (G.is_wire_vertex g (V.mk "d"))
    in () end) ();



  val _ = Testing.test "G.is_node_vertex" (fn () => let
    val _ = (G.is_node_vertex G.empty (V.mk "a");
              raise ERROR "G.update_edge_dir (empty) did not raise exception")
            handle G.no_such_vertex_exp (_,en,g) =>
              (Testing.assert "correct vertex in exp (empty)"
                              (V.string_of_name en = "a");
               Testing.assert "correct graph in exp (empty)"
                              (G.is_empty g))

    val g = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_wv "c"
        |> add_wv "d"
        |> add_dir_eunit1 "e" "a" "c"
        |> add_undir_eunit2 "f" "c" "b"
        |> add_dir_eunit1 "g" "d" "d"
        |> add_dir_eunit1 "h" "a" "a"

    val _ = (G.is_node_vertex g (V.mk "e");
              raise ERROR "G.update_edge_dir (unknown) did not raise exception")
            handle G.no_such_vertex_exp (_,en,g_e) =>
              (Testing.assert "correct vertex in exp (unknown)"
                              (V.string_of_name en = "e");
               Testing.assert "correct graph in exp (unknown)"
                              (G.exact_eq g_e g))

    val _ = Testing.assert "is_node_vertex a"
            (G.is_node_vertex g (V.mk "a"))
    val _ = Testing.assert "is_node_vertex b"
            (G.is_node_vertex g (V.mk "b"))
    val _ = Testing.assert "not is_node_vertex c"
            (not (G.is_node_vertex g (V.mk "c")))
    val _ = Testing.assert "not is_node_vertex d"
            (not (G.is_node_vertex g (V.mk "d")))
    in () end) ();


  val _ = Testing.test "G.get_vertices" (fn () => let
    val _ = Testing.assert "empty" (V.NSet.is_empty (G.get_vertices G.empty));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = assert_vertices "G.get_vertices" ["a","b"] (G.get_vertices g);
  in () end) ();



  val _ = Testing.test "G.get_wire_vertices" (fn () => let
    val _ = Testing.assert "empty" (V.NSet.is_empty (G.get_wire_vertices G.empty));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_wv "c"
                    |> add_wv "d"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b"
                    |> add_dir_eunit2 "g" "b" "c"

    val _ = assert_vertices "G.get_wire_vertices"
            ["c","d"]
            (G.get_wire_vertices g);
    in () end) ();



  val _ = Testing.test "G.get_node_vertices" (fn () => let
    val _ = Testing.assert "empty" (V.NSet.is_empty (G.get_node_vertices G.empty));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_wv "c"
                    |> add_wv "d"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b"
                    |> add_dir_eunit2 "g" "b" "c"

    val _ = assert_vertices "G.get_node_vertices"
            ["a","b"]
            (G.get_node_vertices g);
    in () end) ();



  val _ = Testing.test "G.get_edges" (fn () => let
    val _ = Testing.assert "empty" (E.NSet.is_empty (G.get_edges G.empty));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = assert_edges "G.get_edges" ["e","f"] (G.get_edges g);
  in () end) ();



  val _ = Testing.test "G.is_subgraph" (fn () => let
    val _ = Testing.assert "empty (= empty" (G.is_subgraph G.empty G.empty)
    val g1 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_vexpr1 "c" "x"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_undir_eunit2 "f" "b" "a"
    val _ = Testing.assert "g1 (= g1" (G.is_subgraph g1 g1)
    val _ = Testing.assert "empty (= g1" (G.is_subgraph G.empty g1)
    val _ = Testing.assert "g1 (/= empty" (not (G.is_subgraph g1 G.empty))
    val g2 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_vexpr1 "c" "x"
                     |> add_undir_eunit2 "f" "b" "a"
    val _ = Testing.assert "g2 (= g1" (G.is_subgraph g2 g1)
    val _ = Testing.assert "g1 (/= g2" (not (G.is_subgraph g1 g2))
    val g3 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_undir_eunit2 "f" "b" "a"
    val _ = Testing.assert "g3 (= g1" (G.is_subgraph g3 g1)
    val _ = Testing.assert "g1 (/= g3" (not (G.is_subgraph g1 g3))
    val g4 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_vexpr1 "c" "y"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_undir_eunit2 "f" "b" "a"
    val _ = Testing.assert "g4 (/= g1" (not (G.is_subgraph g4 g1))
    val _ = Testing.assert "g1 (/= g4" (not (G.is_subgraph g1 g4))
    val g5 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "d"
                     |> add_vexpr1 "c" "x"
                     |> add_dir_eunit1 "e" "a" "d"
                     |> add_undir_eunit2 "f" "d" "a"
    val _ = Testing.assert "g5 (/= g1" (not (G.is_subgraph g5 g1))
    val _ = Testing.assert "g1 (/= g5" (not (G.is_subgraph g1 g5))
    val g6 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_vexpr1 "c" "x"
                     |> add_dir_eunit1 "g" "a" "b"
                     |> add_undir_eunit2 "f" "b" "a"
    val _ = Testing.assert "g6 (/= g1" (not (G.is_subgraph g6 g1))
    val _ = Testing.assert "g1 (/= g6" (not (G.is_subgraph g1 g6))
    val g7 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_vexpr1 "c" "x"
                     |> add_dir_eunit2 "e" "a" "b"
                     |> add_undir_eunit2 "f" "b" "a"
    val _ = Testing.assert "g7 (/= g1" (not (G.is_subgraph g7 g1))
    val _ = Testing.assert "g1 (/= g7" (not (G.is_subgraph g1 g7))
    (* NB: direction of Undirected edges matters! *)
    val g8 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_vexpr1 "c" "x"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_undir_eunit2 "f" "a" "b"
    val _ = Testing.assert "g1 (/= g8" (not (G.is_subgraph g1 g8))
    val _ = Testing.assert "g8 (/= g1" (not (G.is_subgraph g8 g1))
    in () end) ();



  val _ = Testing.test "G.exact_eq" (fn () => let
    val _ = Testing.assert "empty = empty" (G.exact_eq G.empty G.empty)

    val g1 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_undir_eunit2 "f" "b" "a"
    (* g2 = g1 *)
    val g2 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_undir_eunit2 "f" "b" "a"
    (* g3 has different vertex data (b) *)
    val g3 = G.empty |> add_vunit1 "a"
                     |> add_vunit1 "b"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_undir_eunit2 "f" "b" "a"
    (* g4 has different edge directedness (f) *)
    val g4 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_dir_eunit2 "f" "b" "a"
    (* g5 has different edge data (e) *)
    val g5 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit2 "e" "a" "b"
                     |> add_undir_eunit2 "f" "b" "a"
    (* g6 has different orientation of undirected edge (f) *)
    val g6 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_undir_eunit2 "f" "a" "b"
    (* g6 has different orientation of directed edge (e) *)
    val g7 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit1 "e" "b" "a"
                     |> add_undir_eunit2 "f" "b" "a"

    val _ = Testing.assert "g1 = g2" (G.exact_eq g1 g2)
    val _ = Testing.assert "g1 != g3" (not (G.exact_eq g1 g3))
    val _ = Testing.assert "g1 != g4" (not (G.exact_eq g1 g4))
    val _ = Testing.assert "g1 != g5" (not (G.exact_eq g1 g5))
    val _ = Testing.assert "g1 != g6" (not (G.exact_eq g1 g6))
    val _ = Testing.assert "g1 != g7" (not (G.exact_eq g1 g7))
    in () end) ()



  val _ = Testing.test "G.add_vertex" (fn () => let
    val (vn,g) = G.add_vertex vunit1 G.empty;
    val _ = assert_vertex_info "g" g [(V.string_of_name vn,(vunit1,([],[])))];
    val _ = assert_edge_info "g" g [];

    val (vn',g') = G.add_vertex vunit2 g;
    val _ = Testing.assert "new name" (not (V.name_eq (vn,vn')));
    val _ = assert_vertex_info "g'" g' [(V.string_of_name vn,(vunit1,([],[]))),
                                (V.string_of_name vn',(vunit2,([],[])))];
    val _ = assert_edge_info "g'" g' [];
    in () end) ();



  (* TODO: add_vertex_anon *)



  val _ = Testing.test "G.add_named_vertex" (fn () => let
    val v_a = V.mk "a";
    val v_b = V.mk "b";

    val g = G.add_named_vertex v_a vunit1 G.empty;
    val _ = assert_vertex_info "g" g [("a",(vunit1,([],[])))];
    val _ = assert_edges "g" [] (G.get_edges g);

    val g' = G.add_named_vertex v_b vunit2 g;
    val _ = assert_vertex_info "g'" g'
              [("a",(vunit1,([],[]))),("b",(vunit2,([],[])))];
    val _ = assert_edges "g'" [] (G.get_edges g');

    val (vn, g'') = G.add_vertex vunit1 g';
    val _ = Testing.assert "correct name (!= a)" (not (V.name_eq (vn,v_a)));
    val _ = Testing.assert "correct name (!= b)" (not (V.name_eq (vn,v_b)));
    val _ = assert_vertex_info "g''" g''
              [("a",(vunit1,([],[]))),
               ("b",(vunit2,([],[]))),
               (V.string_of_name vn,(vunit1,([],[])))];
    val _ = assert_edges "g''" [] (G.get_edges g'');
    in () end) ();



  val _ = Testing.test "G.add_edge" (fn () => let
    val va = V.mk "a";
    val vb = V.mk "b";

    val _ = (G.add_edge (Directed,eunit1) va vb G.empty;
             raise ERROR "G.add_edge did not raise exception (1)")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (1)"
                  ((V.name_eq (vn,va)) orelse (V.name_eq (vn,vb)));
               Testing.assert "correct graph in exp (1)" (G.exact_eq G.empty g'));

    val g = G.empty |> add_vunit1 "a";

    val _ = (G.add_edge (Directed,eunit1) va vb g;
             raise ERROR "G.add_edge did not raise exception (2)")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (2)" (V.name_eq (vn,vb));
               Testing.assert "correct graph in exp (2)" (G.exact_eq g g'));
    val _ = (G.add_edge (Directed,eunit1) vb va g;
             raise ERROR "G.add_edge did not raise exception (3)")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (3)" (V.name_eq (vn,vb));
               Testing.assert "correct graph in exp (3)" (G.exact_eq g g'));
    val (en,g') = G.add_edge (Directed,eunit1) va va g;
    val enstr = E.string_of_name en;
    val _ = assert_edge_info "g'" g' [(enstr,((Directed,eunit1),("a","a")))];
    val _ = assert_vertex_info "g'" g' [("a",(vunit1,([enstr],[enstr])))];

    val g = g |> add_vunit2 "b";

    val (en,g') = G.add_edge (Directed,eunit1) va vb g;
    val enstr = E.string_of_name en;
    val _ = assert_edge_info "g'" g'
              [(enstr,((Directed,eunit1),("a","b")))];
    val _ = assert_vertex_info "g'" g'
              [("a",(vunit1,([],[enstr]))),
               ("b",(vunit2,([enstr],[])))];

    val (en',g'') = G.add_edge (Undirected,eunit2) vb va g';
    val enstr' = E.string_of_name en';
    val _ = assert_edge_info "g''" g''
              [(enstr,((Directed,eunit1),("a","b"))),
               (enstr',((Undirected,eunit2),("b","a")))];
    val _ = assert_vertex_info "g''" g''
              [("a",(vunit1,([enstr'],[enstr]))),
               ("b",(vunit2,([enstr],[enstr'])))];
    in () end) ();



  (* TODO: add_edge_anon *)



  val _ = Testing.test "G.add_named_edge" (fn () => let
    val va = V.mk "a";
    val vb = V.mk "b";
    val ee = E.mk "e";
    val ef = E.mk "f";

    val _ = (G.add_named_edge ee (Directed,eunit1) va vb G.empty;
             raise ERROR "G.add_named_edge did not raise exception (1)")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (1)"
                  ((V.name_eq (vn,va)) orelse (V.name_eq (vn,vb)));
               Testing.assert "correct graph in exp (1)" (G.exact_eq G.empty g'));

    val g = G.empty |> add_vunit1 "a";

    val _ = (G.add_named_edge ee (Directed,eunit1) va vb g;
             raise ERROR "G.add_named_edge did not raise exception (2)")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (2)" (V.name_eq (vn,vb));
               Testing.assert "correct graph in exp (2)" (G.exact_eq g g'));
    val _ = (G.add_named_edge ee (Directed,eunit1) vb va g;
             raise ERROR "G.add_named_edge did not raise exception (3)")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (3)" (V.name_eq (vn,vb));
               Testing.assert "correct graph in exp (3)" (G.exact_eq g g'));
    val g' = G.add_named_edge ee (Directed,eunit1) va va g;
    val _ = assert_edge_info "g'" g' [("e",((Directed,eunit1),("a","a")))];
    val _ = assert_vertex_info "g'" g' [("a",(vunit1,(["e"],["e"])))];

    val g = g |> add_vunit2 "b";

    val g' = G.add_named_edge ee (Directed,eunit1) va vb g;
    val _ = assert_edge_info "g'" g'
              [("e",((Directed,eunit1),("a","b")))];
    val _ = assert_vertex_info "g'" g'
              [("a",(vunit1,([],["e"]))),
               ("b",(vunit2,(["e"],[])))];

    val g'' = G.add_named_edge ef (Undirected,eunit2) vb va g';
    val _ = assert_edge_info "g''" g''
              [("e",((Directed,eunit1),("a","b"))),
               ("f",((Undirected,eunit2),("b","a")))];
    val _ = assert_vertex_info "g''" g''
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))];

    val (en,g''') = G.add_edge (Directed,eunit2) vb vb g'';
    val _ = Testing.assert "correct name (4) (new)"
              (not (E.name_eq (en,ee)) andalso not (E.name_eq (en,ef)));
    val _ = assert_edge_info "g'''" g'''
              [("e",((Directed,eunit1),("a","b"))),
               ("f",((Undirected,eunit2),("b","a"))),
               (E.string_of_name en,((Directed,eunit2),("b","b")))];
    val enstr = E.string_of_name en;
    val _ = assert_vertex_info "g'''" g'''
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e",enstr],["f",enstr])))];
    in () end) ();



  val _ = Testing.test "G.delete_edge" (fn () => let
    val _ = G.delete_edge (E.mk "a") G.empty;

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val g' = G.delete_edge (E.mk "e") g;
    val _ = assert_vertex_info "g'" g'
              [("a",(vunit1,([],[]))),
               ("b",(vunit2,(["f"],["f"])))];
    val _ = assert_edge_info "g'" g'
              [("f",((Directed,eunit2),("b","b")))];
    in () end) ();



  val _ = Testing.test "G.delete_vertex" (fn () => let
    val _ = G.delete_vertex (V.mk "a") G.empty;

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val g' = G.delete_vertex (V.mk "a") g;
    val _ = assert_vertex_info "g'" g' [("b",(vunit2,(["f"],["f"])))];
    val _ = assert_edge_info "g'" g' [("f",((Directed,eunit2),("b","b")))];

    val g' = G.delete_vertex (V.mk "b") g;
    val _ = assert_vertex_info "g'" g' [("a",(vunit1,([],[])))];
    val _ = assert_edge_info "g'" g' [];

    val g' = G.delete_vertex (V.mk "a") g';
    val _ = assert_vertex_info "g'" g' [];
    val _ = assert_edge_info "g'" g' [];
    in () end) ();



  val _ = Testing.test "G.swap_edge_dir" (fn () => let
    val _ = (G.swap_edge_dir (E.mk "a") G.empty;
              raise ERROR "G.swap_edge_dir did not raise exception")
            handle G.no_such_edge_exp (_,en,g) =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_dir_eunit1 "e" "a" "b"
                    |> add_undir_eunit2 "f" "b" "a";

    val g' = G.swap_edge_dir (E.mk "e") g;
    val _ = assert_vertex_info "g'" g'
              [("a",(vunit1,(["e","f"],[]))),
               ("b",(vunit2,([],["e","f"])))];
    val _ = assert_edge_info "g'" g'
              [("e",((Directed,eunit1),("b","a"))),
               ("f",((Undirected,eunit2),("b","a")))];

    val g' = G.swap_edge_dir (E.mk "f") g;
    val _ = assert_vertex_info "g'" g'
              [("a",(vunit1,([],["e","f"]))),
               ("b",(vunit2,(["e","f"],[])))];
    val _ = assert_edge_info "g'" g'
              [("e",((Directed,eunit1),("a","b"))),
               ("f",((Undirected,eunit2),("a","b")))];
    in () end) ();



  val _ = Testing.test "G.is_intersection" (fn () => let
    val _ = Testing.assert "G.is_intersection G.empty"
            (G.is_intersection G.empty G.empty G.empty)

    val g1 = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_vexpr1 "c" "x"
        |> add_wv "d"
        |> add_wv "e"
        |> add_wv "f"
        |> add_dir_eunit1 "e" "a" "d"
        |> add_dir_eunit1 "f" "d" "b"
        |> add_undir_eunit2 "g" "b" "e"
        |> add_undir_eunit1 "h" "b" "f"
        |> add_undir_eunit1 "i" "c" "f"
    val _ = Testing.assert "G.is_intersection g1 g1 g1"
            (G.is_intersection g1 g1 g1)

    val g2 = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_vexpr1 "c_" "x"
        |> add_wv "d"
        |> add_wv "e_"
        |> add_wv "f_"
        |> add_dir_eunit1 "e" "a" "d"
        |> add_dir_eunit1 "f" "d" "b"
        |> add_undir_eunit2 "g_" "b" "e_"
        |> add_undir_eunit1 "h_" "b" "f_"
        |> add_undir_eunit1 "i_" "c_" "f_"

    val g1_g2 = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_wv "d"
        |> add_dir_eunit1 "e" "a" "d"
        |> add_dir_eunit1 "f" "d" "b"
    val _ = Testing.assert "G.is_intersection g1_g2 g1 g2"
            (G.is_intersection g1_g2 g1 g2)

    val g1_g2' = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_wv "d"
        |> add_dir_eunit1 "e" "d" "a"
        |> add_dir_eunit1 "f" "b" "d"
    val _ = Testing.assert "! G.is_intersection g1_g2' g1 g2"
            (not (G.is_intersection g1_g2' g1 g2))

    val g3 = G.empty
        |> add_vunit1 "a_"
        |> add_vunit2 "b"
        |> add_vexpr1 "c" "x"
        |> add_wv "d_"
        |> add_wv "e_"
        |> add_wv "f"
        |> add_dir_eunit1 "e_" "a_" "d_"
        |> add_dir_eunit1 "f_" "d_" "b"
        |> add_undir_eunit2 "g_" "b" "e_"
        |> add_undir_eunit1 "h" "b" "f"
        |> add_undir_eunit1 "i" "c" "f"

    val g1_g3 = G.empty
        |> add_vunit2 "b"
        |> add_vexpr1 "c" "x"
        |> add_wv "f"
        |> add_undir_eunit1 "h" "b" "f"
        |> add_undir_eunit1 "i" "c" "f"
    val _ = Testing.assert "G.is_intersection g1_g3 g1 g3"
            (G.is_intersection g1_g3 g1 g3)

    val g4 = G.empty
        |> add_vunit1 "a_"
        |> add_vunit2 "b"
        |> add_vexpr1 "c" "x"
        |> add_wv "d_"
        |> add_wv "e_"
        |> add_wv "f"
        |> add_dir_eunit1 "e_" "a_" "d_"
        |> add_dir_eunit1 "f_" "d_" "b"
        |> add_undir_eunit2 "g_" "b" "e_"
        |> add_undir_eunit1 "h_" "b" "f"
        |> add_undir_eunit1 "i_" "c" "f"

    val g1_g4 = G.empty
        |> add_vunit2 "b"
        |> add_vexpr1 "c" "x"
        |> add_wv "f"
    val _ = Testing.assert "G.is_intersection g1_g4 g1 g4"
            (G.is_intersection g1_g4 g1 g4)

    (* NB: edge direction matters, even for Undirected edges *)
    val g5 = G.empty
        |> add_vunit1 "a_"
        |> add_vunit2 "b"
        |> add_vexpr1 "c" "x"
        |> add_wv "d_"
        |> add_wv "e_"
        |> add_wv "f"
        |> add_dir_eunit1 "e_" "a_" "d_"
        |> add_dir_eunit1 "f_" "d_" "b"
        |> add_undir_eunit2 "g_" "b" "e_"
        |> add_undir_eunit1 "h" "b" "f"
        |> add_undir_eunit1 "i" "f" "c"

    val g1_g5 = G.empty
        |> add_vunit2 "b"
        |> add_vexpr1 "c" "x"
        |> add_wv "f"
        |> add_undir_eunit1 "h" "b" "f"
        |> add_undir_eunit1 "i" "c" "f"
    val _ = Testing.assert "not G.is_intersection g1_g5 g1 g5"
            (not (G.is_intersection g1_g5 g1 g5))
    in () end) ();



  val _ = Testing.test "G.merge_by_subgraph" (fn () => let
    val _ = assert_g_eq "merge of empty graphs" G.empty
            (G.merge_by_subgraph G.empty G.empty G.empty)

    val g1 = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_vexpr1 "c" "x"
        |> add_wv "d"
        |> add_wv "e"
        |> add_wv "f"
        |> add_dir_eunit1 "e" "a" "d"
        |> add_dir_eunit1 "f" "d" "b"
        |> add_undir_eunit2 "g" "b" "e"
        |> add_undir_eunit1 "h" "b" "f"
        |> add_undir_eunit1 "i" "c" "f"

    val g2 = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_vexpr1 "c_" "x"
        |> add_wv "d"
        |> add_wv "e_"
        |> add_wv "f_"
        |> add_dir_eunit1 "e" "a" "d"
        |> add_dir_eunit1 "f" "d" "b"
        |> add_undir_eunit2 "g_" "b" "e_"
        |> add_undir_eunit1 "h_" "b" "f_"
        |> add_undir_eunit1 "i_" "c_" "f_"

    val g1_g2 = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_wv "d"
        |> add_dir_eunit1 "e" "a" "d"
        |> add_dir_eunit1 "f" "d" "b"

    val g_exp = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_vexpr1 "c" "x"
        |> add_vexpr1 "c_" "x"
        |> add_wv "d"
        |> add_wv "e_"
        |> add_wv "f_"
        |> add_wv "e"
        |> add_wv "f"
        |> add_dir_eunit1 "e" "a" "d"
        |> add_dir_eunit1 "f" "d" "b"
        |> add_undir_eunit2 "g_" "b" "e_"
        |> add_undir_eunit1 "h_" "b" "f_"
        |> add_undir_eunit1 "i_" "c_" "f_"
        |> add_undir_eunit2 "g" "b" "e"
        |> add_undir_eunit1 "h" "b" "f"
        |> add_undir_eunit1 "i" "c" "f"

    val _ = assert_g_eq "merge of g1 and g2" g_exp
            (G.merge_by_subgraph g1_g2 g1 g2)
    in () end) ();



  val _ = Testing.test "G.merge_by_vertices" (fn () => let
    val _ = assert_g_eq "merge of empty graphs" G.empty
            (G.merge_by_vertices G.empty G.empty)

    val g1 = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_vexpr1 "c" "x"
        |> add_wv "d"
        |> add_wv "e"
        |> add_wv "f"
        |> add_dir_eunit1 "e" "a" "d"
        |> add_dir_eunit1 "f" "d" "b"
        |> add_undir_eunit2 "g" "b" "e"
        |> add_undir_eunit1 "h" "b" "f"
        |> add_undir_eunit1 "i" "c" "f"

    val g2 = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_vexpr1 "c_" "x"
        |> add_wv "d"
        |> add_wv "e_"
        |> add_wv "f_"
        |> add_dir_eunit1 "e_" "a" "d"
        |> add_dir_eunit1 "f_" "d" "b"
        |> add_undir_eunit2 "g_" "b" "e_"
        |> add_undir_eunit1 "h_" "b" "f_"
        |> add_undir_eunit1 "i_" "c_" "f_"

    val g_exp = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_vexpr1 "c" "x"
        |> add_vexpr1 "c_" "x"
        |> add_wv "d"
        |> add_wv "e_"
        |> add_wv "f_"
        |> add_wv "e"
        |> add_wv "f"
        |> add_dir_eunit1 "e" "a" "d"
        |> add_dir_eunit1 "f" "d" "b"
        |> add_undir_eunit2 "g" "b" "e"
        |> add_undir_eunit1 "h" "b" "f"
        |> add_undir_eunit1 "i" "c" "f"
        |> add_dir_eunit1 "e_" "a" "d"
        |> add_dir_eunit1 "f_" "d" "b"
        |> add_undir_eunit2 "g_" "b" "e_"
        |> add_undir_eunit1 "h_" "b" "f_"
        |> add_undir_eunit1 "i_" "c_" "f_"

    val _ = assert_g_eq "merge of g1 and g2" g_exp
            (G.merge_by_vertices g1 g2)
    in () end) ();



  val _ = Testing.test "G.get_arity" (fn () => let
    val _ = (G.get_arity G.empty (V.mk "a");
              raise ERROR "G.get_arity (empty) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp (empty)"
                              (V.string_of_name vn = "a");
               Testing.assert "correct graph in exp (empty)"
                              (G.is_empty g))

    val g = G.empty
        |> add_vunit1 "v1"
        |> add_wv "v2"
        |> add_vunit1 "v3"
        |> add_wv "v4"
        |> add_vunit1 "v5"
        |> add_vunit1 "v6"
        |> add_dir_eunit1 "e1" "v1" "v2"
        |> add_undir_eunit1 "e2" "v1" "v3"
        |> add_dir_eunit1 "e3" "v4" "v1"
        |> add_undir_eunit1 "e4" "v5" "v1"
        |> add_dir_eunit1 "e5" "v1" "v5"
        |> add_dir_eunit1 "e6" "v5" "v1"
        |> add_dir_eunit1 "e7" "v5" "v5"
        |> add_undir_eunit1 "e8" "v5" "v5"

    val _ = (G.get_arity g (V.mk "v0");
             raise ERROR "G.get_arity (unknown vertex) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (unknown vertex)"
                              (V.name_eq (vn,V.mk "v0"));
               Testing.assert "correct graph in exp (unknown vertex)"
                              (G.exact_eq g g'))

    fun assert_arity_eq v rep =
    let val a1 = G.get_arity g (V.mk v) in
      if Arity.rep_of a1 = rep
      then ()
      else ( writeln ("Expected ("^v^"):");
            Pretty.writeln (Pretty.block 
              ( [Pretty.str "(in:",
                 Pretty.str (Int.toString (#no_in rep))]
              @ [Pretty.str ", out:",
                 Pretty.str (Int.toString (#no_out rep))]
              @ [Pretty.str ", undir:",
                 Pretty.str (Int.toString (#no_undir rep))]
              @ [Pretty.str ")"] ));
            writeln ("Actual ("^v^"):");
            Arity.print a1;
            Testing.assert ("arity of "^v) false)
    end

    val _ = assert_arity_eq "v1" {no_in = 2, no_out = 2, no_undir = 2}
    val _ = assert_arity_eq "v2" {no_in = 1, no_out = 0, no_undir = 0}
    val _ = assert_arity_eq "v3" {no_in = 0, no_out = 0, no_undir = 1}
    val _ = assert_arity_eq "v4" {no_in = 0, no_out = 1, no_undir = 0}
    val _ = assert_arity_eq "v5" {no_in = 2, no_out = 2, no_undir = 3}
    val _ = assert_arity_eq "v6" {no_in = 0, no_out = 0, no_undir = 0}
    in () end) ();



  val _ = Testing.test "G.get_self_loops" (fn () => let
    val _ = (G.get_self_loops G.empty (V.mk "a");
              raise ERROR "G.get_self_loops (empty) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp (empty)"
                              (V.string_of_name vn = "a");
               Testing.assert "correct graph in exp (empty)"
                              (G.is_empty g))

    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit1 "v2"
        |> add_vunit1 "v3"
        |> add_wv "v4"
        |> add_wv "v5"
        |> add_wv "v6"
        |> add_dir_eunit1 "loop1" "v1" "v1"
        |> add_undir_eunit1 "loop2" "v1" "v1"
        |> add_dir_eunit1 "loop3" "v2" "v2"
        |> add_undir_eunit1 "loop4" "v4" "v4"
        |> add_dir_eunit1 "e1" "v1" "v2"
        |> add_undir_eunit1 "e2" "v1" "v3"
        |> add_dir_eunit1 "e3" "v5" "v1"

    val _ = (G.get_self_loops g (V.mk "v0");
             raise ERROR "G.get_self_loops (unknown vertex) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (unknown vertex)"
                              (V.name_eq (vn,V.mk "v0"));
               Testing.assert "correct graph in exp (unknown vertex)"
                              (G.exact_eq g g'))

    val _ = assert_edges "self-loops of v1 = [loop1,loop2]"
                         ["loop1","loop2"]
                         (G.get_self_loops g (V.mk "v1"))
    val _ = assert_edges "self-loops of v2 = [loop3]"
                         ["loop3"]
                         (G.get_self_loops g (V.mk "v2"))
    val _ = assert_edges "self-loops of v3 = []" []
                         (G.get_self_loops g (V.mk "v3"))
    val _ = assert_edges "self-loops of v4 = [loop4]"
                         ["loop4"]
                         (G.get_self_loops g (V.mk "v4"))
    val _ = assert_edges "self-loops of v5 = []" []
                         (G.get_self_loops g (V.mk "v5"))
    val _ = assert_edges "self-loops of v6 = []" []
                         (G.get_self_loops g (V.mk "v6"))
    in () end) ();



  val _ = Testing.test "G.delete_subgraph" (fn () => let
    val _ = assert_g_eq "G.delete_subgraph (empty,empty)" G.empty
                        (G.delete_subgraph G.empty G.empty)

    val g1 = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_vexpr1 "c" "x"
        |> add_dir_eunit1 "e" "a" "b"
        |> add_undir_eunit2 "f" "b" "a"
    val g2 = G.empty
        |> add_vunit1 "a"
        |> add_vunit2 "b"
        |> add_undir_eunit2 "f" "b" "a"
    val g_exp = G.empty
        |> add_vexpr1 "c" "x"

    val _ = assert_g_eq "G.delete_subgraph (g1\\empty)" g1
                        (G.delete_subgraph G.empty g1)
    val _ = assert_g_eq "G.delete_subgraph (g1\\g2)" g_exp
                        (G.delete_subgraph g2 g1)
    in () end) ();



  val _ = Testing.test "G.has_edges_between" (fn () => let
    val _ = (G.has_edges_between G.empty (V.mk "a") (V.mk "a");
              raise ERROR "G.has_edges_between (empty) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp (empty)"
                              (V.string_of_name vn = "a");
               Testing.assert "correct graph in exp (empty)"
                              (G.is_empty g))

    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit2 "v2"
        |> add_vexpr1 "v3" "x"
        |> add_vunit2 "v4"
        |> add_undir_eunit2 "u1" "v1" "v2"
        |> add_dir_eunit1 "d1" "v1" "v3"
        |> add_undir_eunit2 "u2" "v3" "v4"
        |> add_dir_eunit1 "d2" "v4" "v3"
        |> add_dir_eunit1 "d3" "v3" "v4"
        |> add_undir_eunit1 "l1" "v2" "v2"
        |> add_dir_eunit1 "l2" "v3" "v3"

    val _ = (G.has_edges_between g (V.mk "v0") (V.mk "v1");
             raise ERROR "G.has_edges_between (unknown vertex 1) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (unknown vertex 1)"
                              (V.name_eq (vn,V.mk "v0"));
               Testing.assert "correct graph in exp (unknown vertex 1)"
                              (G.exact_eq g g'))
    val _ = (G.has_edges_between g (V.mk "v1") (V.mk "v0");
             raise ERROR "G.has_edges_between (unknown vertex 2) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (unknown vertex 2)"
                              (V.name_eq (vn,V.mk "v0"));
               Testing.assert "correct graph in exp (unknown vertex 2)"
                              (G.exact_eq g g'))

    val _ = Testing.assert "edges v1-v2"
            (G.has_edges_between g (V.mk "v1") (V.mk "v2"))
    val _ = Testing.assert "edges v2-v1"
            (G.has_edges_between g (V.mk "v2") (V.mk "v1"))
    val _ = Testing.assert "edges v1-v3"
            (G.has_edges_between g (V.mk "v1") (V.mk "v3"))
    val _ = Testing.assert "edges v3-v1"
            (G.has_edges_between g (V.mk "v3") (V.mk "v1"))
    val _ = Testing.assert "edges v3-v4"
            (G.has_edges_between g (V.mk "v3") (V.mk "v4"))
    val _ = Testing.assert "no edges v2-v4"
            (not (G.has_edges_between g (V.mk "v2") (V.mk "v4")))
    val _ = Testing.assert "no edges v1-v1"
            (not (G.has_edges_between g (V.mk "v1") (V.mk "v1")))
    val _ = Testing.assert "edges v2-v2"
            (G.has_edges_between g (V.mk "v2") (V.mk "v2"))
    val _ = Testing.assert "edges v3-v3"
            (G.has_edges_between g (V.mk "v3") (V.mk "v3"))
    in () end) ();



  val _ = Testing.test "G.edges_between" (fn () => let
    val _ = (G.edges_between G.empty (V.mk "a") (V.mk "a");
              raise ERROR "G.edges_between (empty) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp (empty)"
                              (V.string_of_name vn = "a");
               Testing.assert "correct graph in exp (empty)"
                              (G.is_empty g))

    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit2 "v2"
        |> add_vexpr1 "v3" "x"
        |> add_vunit2 "v4"
        |> add_undir_eunit2 "u1" "v1" "v2"
        |> add_dir_eunit1 "d1" "v1" "v3"
        |> add_undir_eunit2 "u2" "v3" "v4"
        |> add_dir_eunit1 "d2" "v4" "v3"
        |> add_dir_eunit1 "d3" "v3" "v4"
        |> add_undir_eunit1 "l1" "v2" "v2"
        |> add_dir_eunit1 "l2" "v3" "v3"

    val _ = (G.edges_between g (V.mk "v0") (V.mk "v1");
             raise ERROR "G.edges_between (unknown vertex 1) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (unknown vertex 1)"
                              (V.name_eq (vn,V.mk "v0"));
               Testing.assert "correct graph in exp (unknown vertex 1)"
                              (G.exact_eq g g'))
    val _ = (G.edges_between g (V.mk "v1") (V.mk "v0");
             raise ERROR "G.edges_between (unknown vertex 2) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (unknown vertex 2)"
                              (V.name_eq (vn,V.mk "v0"));
               Testing.assert "correct graph in exp (unknown vertex 2)"
                              (G.exact_eq g g'))

    fun assert_edges_between v1 v2 exp =
      (assert_edges ("edges "^v1^"-"^v2) exp
                    (G.edges_between g (V.mk v1) (V.mk v2));
       assert_edges ("edges "^v2^"-"^v1) exp
                    (G.edges_between g (V.mk v2) (V.mk v1)))

    val _ = assert_edges_between "v1" "v2" ["u1"]
    val _ = assert_edges_between "v1" "v3" ["d1"]
    val _ = assert_edges_between "v3" "v4" ["u2","d2","d3"]
    val _ = assert_edges_between "v2" "v4" []
    val _ = assert_edges_between "v1" "v1" []
    val _ = assert_edges_between "v2" "v2" ["l1"]
    val _ = assert_edges_between "v3" "v3" ["l2"]
    in () end) ();



  val _ = Testing.test "G.dir_edges_between" (fn () => let
    val _ = (G.dir_edges_between G.empty (V.mk "a") (V.mk "a");
              raise ERROR "G.dir_edges_between (empty) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp (empty)"
                              (V.string_of_name vn = "a");
               Testing.assert "correct graph in exp (empty)"
                              (G.is_empty g))

    val g = G.empty
        |> add_vunit1 "v1"
        |> add_vunit2 "v2"
        |> add_vexpr1 "v3" "x"
        |> add_vunit2 "v4"
        |> add_undir_eunit2 "u1" "v1" "v2"
        |> add_dir_eunit1 "d1" "v1" "v3"
        |> add_undir_eunit2 "u2" "v3" "v4"
        |> add_dir_eunit1 "d2" "v4" "v3"
        |> add_dir_eunit1 "d3" "v3" "v4"
        |> add_undir_eunit1 "l1" "v2" "v2"
        |> add_dir_eunit1 "l2" "v3" "v3"

    val _ = (G.dir_edges_between g (V.mk "v0") (V.mk "v1");
             raise ERROR "G.dir_edges_between (unknown vertex 1) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (unknown vertex 1)"
                              (V.name_eq (vn,V.mk "v0"));
               Testing.assert "correct graph in exp (unknown vertex 1)"
                              (G.exact_eq g g'))
    val _ = (G.dir_edges_between g (V.mk "v1") (V.mk "v0");
             raise ERROR "G.dir_edges_between (unknown vertex 2) did not raise exception")
            handle G.no_such_vertex_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (unknown vertex 2)"
                              (V.name_eq (vn,V.mk "v0"));
               Testing.assert "correct graph in exp (unknown vertex 2)"
                              (G.exact_eq g g'))

    fun assert_edges_between v1 v2 forward backward = let
      val (f,b) = G.dir_edges_between g (V.mk v1) (V.mk v2)
    in
      assert_edges ("edges "^v1^"->"^v2) forward f;
      assert_edges ("edges "^v2^"->"^v1) backward b
    end

    val _ = assert_edges_between "v1" "v2" [] []
    val _ = assert_edges_between "v1" "v3" ["d1"] []
    val _ = assert_edges_between "v3" "v4" ["d3"] ["d2"]
    val _ = assert_edges_between "v2" "v4" [] []
    val _ = assert_edges_between "v1" "v1" [] []
    val _ = assert_edges_between "v2" "v2" [] []
    val _ = assert_edges_between "v3" "v3" ["l2"] ["l2"]
    in () end) ();



  val _ = Testing.test "G.rename_ograph" (fn () => let
    (* TODO: val rename_ograph      : (VSub.T * ESub.T) -> T -> (VSub.T * ESub.T) * T *)
    in () end) ();



  (* TODO: val rename_ograph_anon : (VSub.T * ESub.T) -> T -> T *)



  val _ = Testing.test "G.rename_vertices" (fn () => let
    (* TODO: val rename_vertices    : VSub.T -> T -> T *)
    in () end) ();



  val _ = Testing.test "G.rename_vertex" (fn () => let
    (* TODO: val rename_vertex      : V.name -> V.name -> T -> T *)
    in () end) ();



  val _ = Testing.test "G.rename_vertex_opt" (fn () => let
    (* TODO: val rename_vertex_opt  : V.name -> V.name -> T -> T option *)
    in () end) ();



  val _ = Testing.test "G.rename_edges" (fn () => let
    (* TODO: val rename_edges       : ESub.T -> T -> T *)
    in () end) ();



  val _ = Testing.test "G.rename_edge" (fn () => let
    (* TODO: val rename_edge        : E.name -> E.name -> T -> T *)
    in () end) ();



  val _ = Testing.test "G.rename_edge_opt" (fn () => let
    (* TODO: val rename_edge_opt    : E.name -> E.name -> T -> T option *)
    in () end) ();



  local
    val g = G.empty
        |> add_wv "b1"
        |> add_wv "b2"
        |> add_wv "b3"
        |> add_wv "b4"
        |> add_wv "i1"
        |> add_wv "i2"
        |> add_wv "i3"
        |> add_wv "o1"
        |> add_wv "o3"
        |> add_wv "s1"
        |> add_wv "c1"
        |> add_wv "c2"
        |> add_wv "c3"
        |> add_wv "w1"
        |> add_wv "w2"
        |> add_wv "w3"
        |> add_wv "w4"
        |> add_wv "w5"
        |> add_vunit1 "v1"
        |> add_vunit2 "v2"
        |> add_vexpr1 "v3" "x"
        |> add_dir_eunit1 "bw1" "i3" "o3"
        |> add_undir_eunit1 "bw2" "b3" "b4"
        |> add_undir_eunit1 "b1" "b1" "v1"
        |> add_undir_eunit2 "b2" "b2" "w4"
        |> add_undir_eunit2 "b2'" "w4" "v1"
        |> add_dir_eunit1 "i1" "i1" "v1"
        |> add_dir_eunit2 "i2" "i2" "w5"
        |> add_dir_eunit2 "i2'" "w5" "v3"
        |> add_dir_eunit1 "o1" "v2" "o1"
        |> add_undir_eunit1 "c1" "c1" "c1"
        |> add_dir_eunit2 "c2" "c2" "c3"
        |> add_dir_eunit2 "c3" "c3" "c2"
        |> add_dir_eunit1 "e1" "v1" "w1"
        |> add_dir_eunit1 "e1'" "w1" "v3"
        |> add_undir_eunit1 "e2" "v2" "w2"
        |> add_undir_eunit1 "e2'" "w2" "v3"
        |> add_dir_eunit1 "e3" "v3" "w3"
        |> add_dir_eunit1 "e3'" "w3" "v3"
  in
    val _ = Testing.test "G.get_boundary" (fn () => let
      val _ = assert_vertices "G.get_boundary (empty)" []
                              (G.get_boundary G.empty)
      val _ = assert_vertices "G.get_boundary (g)"
                              ["b1","b2","b3","b4","i1","i2","i3","o1","o3","s1"]
                              (G.get_boundary g)
      in () end) ()


    val _ = Testing.test "G.get_inputs" (fn () => let
      val _ = assert_vertices "G.get_inputs (empty)" []
                              (G.get_inputs G.empty)
      val _ = assert_vertices "G.get_inputs (g)"
                              ["i1","i2","i3"]
                              (G.get_inputs g)
      in () end) ()


    val _ = Testing.test "G.get_outputs" (fn () => let
      val _ = assert_vertices "G.get_outputs (empty)" []
                              (G.get_outputs G.empty)
      val _ = assert_vertices "G.get_outputs (g)"
                              ["o1","o3"]
                              (G.get_outputs g)
      in () end) ()


    val _ = Testing.test "G.is_boundary" (fn () => let
      val _ = (G.is_boundary G.empty (V.mk "a");
                raise ERROR "G.is_boundary (empty) did not raise exception")
              handle G.no_such_vertex_exp (_,vn,g) =>
                (Testing.assert "correct vname in exp (empty)"
                                (V.string_of_name vn = "a");
                 Testing.assert "correct graph in exp (empty)"
                                (G.is_empty g))

      val _ = (G.is_boundary g (V.mk "v0");
               raise ERROR "G.is_boundary (unknown vertex) did not raise exception")
              handle G.no_such_vertex_exp (_,vn,g') =>
                (Testing.assert "correct vname in exp (unknown vertex)"
                                (V.name_eq (vn,V.mk "v0"));
                 Testing.assert "correct graph in exp (unknown vertex)"
                                (G.exact_eq g g'))

      val _ = map (fn v => Testing.assert v (G.is_boundary g (V.mk v)))
              ["b1","b2","b3","b4","i1","i2","i3","o1","o3","s1"]
      val _ = map (fn v => Testing.assert v (not (G.is_boundary g (V.mk v))))
              ["c1","c2","c3","w1","w2","w3","w4","w5","v1","v2","v3"]
      in () end) ()


    val _ = Testing.test "G.is_input" (fn () => let
      val _ = (G.is_input G.empty (V.mk "a");
                raise ERROR "G.is_input (empty) did not raise exception")
              handle G.no_such_vertex_exp (_,vn,g) =>
                (Testing.assert "correct vname in exp (empty)"
                                (V.string_of_name vn = "a");
                 Testing.assert "correct graph in exp (empty)"
                                (G.is_empty g))

      val _ = (G.is_input g (V.mk "v0");
               raise ERROR "G.is_input (unknown vertex) did not raise exception")
              handle G.no_such_vertex_exp (_,vn,g') =>
                (Testing.assert "correct vname in exp (unknown vertex)"
                                (V.name_eq (vn,V.mk "v0"));
                 Testing.assert "correct graph in exp (unknown vertex)"
                                (G.exact_eq g g'))

      val _ = map (fn v => Testing.assert v (G.is_input g (V.mk v)))
              ["i1","i2","i3"]
      val _ = map (fn v => Testing.assert v (not (G.is_input g (V.mk v))))
              ["b1","b2","b3","b4","o1","o3","s1","c1","c2","c3",
               "w1","w2","w3","w4","w5","v1","v2","v3"]
      in () end) ()


    val _ = Testing.test "G.is_output" (fn () => let
      val _ = (G.is_output G.empty (V.mk "a");
                raise ERROR "G.is_output (empty) did not raise exception")
              handle G.no_such_vertex_exp (_,vn,g) =>
                (Testing.assert "correct vname in exp (empty)"
                                (V.string_of_name vn = "a");
                 Testing.assert "correct graph in exp (empty)"
                                (G.is_empty g))

      val _ = (G.is_output g (V.mk "v0");
               raise ERROR "G.is_output (unknown vertex) did not raise exception")
              handle G.no_such_vertex_exp (_,vn,g') =>
                (Testing.assert "correct vname in exp (unknown vertex)"
                                (V.name_eq (vn,V.mk "v0"));
                 Testing.assert "correct graph in exp (unknown vertex)"
                                (G.exact_eq g g'))

      val _ = map (fn v => Testing.assert v (G.is_output g (V.mk v)))
              ["o1","o3"]
      val _ = map (fn v => Testing.assert v (not (G.is_output g (V.mk v))))
              ["b1","b2","b3","b4","i1","i2","i3","s1","c1","c2","c3",
               "w1","w2","w3","w4","w5","v1","v2","v3"]
      in () end) ()
  end



  val _ = Testing.test "G.wv_get_other_edge" (fn () => let
    (* Given a wire vertex and an edge, give the other connected edge, if it
       exists. In the case of a wire-vertex with a single, given self-loop,
       return NONE. *)
    (* TODO: val wv_get_other_edge : T -> V.name -> E.name -> E.name option *)
    in () end) ();



  local (* wires *)
    val g = G.empty |> add_wv "w1"
                    |> add_wv "w2"
                    |> add_wv "w3"
                    |> add_wv "w4"
                    |> add_wv "w5"
                    |> add_wv "w6"
                    |> add_wv "w7"
                    |> add_wv "w8"
                    |> add_wv "c1"
                    |> add_wv "c2"
                    |> add_wv "c3"
                    |> add_vunit1 "n1"
                    |> add_vunit1 "n2"
                    |> add_vunit1 "n3"
                    |> add_dir_eunit1 "e1" "n1" "w1"
                    |> add_dir_eunit1 "e2" "w1" "w2"
                    |> add_dir_eunit1 "e3" "w2" "n2"
                    |> add_dir_eunit1 "e4" "n2" "w3"
                    |> add_dir_eunit1 "e5" "w3" "w4"
                    |> add_dir_eunit1 "e6" "n2" "n3"
                    |> add_dir_eunit1 "e7" "n3" "w5"
                    |> add_dir_eunit1 "e8" "w5" "n3"
                    |> add_dir_eunit1 "ce1" "c1" "c2"
                    |> add_dir_eunit1 "ce2" "c2" "c1"
                    |> add_dir_eunit1 "ce3" "c3" "c3"
                    |> add_undir_eunit1 "f1" "n2" "w6"
                    |> add_undir_eunit1 "f2" "w6" "n3"
                    |> add_undir_eunit1 "f3" "n2" "w7"
                    |> add_undir_eunit1 "f4" "n3" "w7"
                    |> add_undir_eunit1 "f5" "w8" "n2"
                    |> add_undir_eunit1 "f6" "w8" "n3";
    val g_raw = g;

    fun wire v1 v2 vs es =
          (V.mk v1, V.mk v2, V.NSet.of_list (map V.mk vs),
                             E.NSet.of_list (map E.mk es));
    val exp_wire_e1 = wire "n1" "n2" ["w1","w2"] ["e1","e2","e3"];
    val exp_wire_e2 = exp_wire_e1;
    val exp_wire_e3 = exp_wire_e1;
    val exp_wire_e4 = wire "n2" "w4" ["w3"] ["e4","e5"];
    val exp_wire_e5 = exp_wire_e4;
    val exp_wire_e6 = wire "n2" "n3" [] ["e6"];
    val exp_wire_e7 = wire "n3" "n3" ["w5"] ["e7","e8"];
    val exp_wire_e8 = exp_wire_e7;
    (* this circle wire is special, as either vertex could be
     * considered the endpoint *)
    val exp_wire_ce1_a = wire "c1" "c1" ["c2"] ["ce1","ce2"];
    val exp_wire_ce1_b = wire "c2" "c2" ["c1"] ["ce1","ce2"];
    val exp_wire_ce2_a = exp_wire_ce1_a;
    val exp_wire_ce2_b = exp_wire_ce1_b;
    val exp_wire_ce3 = wire "c3" "c3" [] ["ce3"];
    val exp_wire_f1 = wire "n2" "n3" ["w6"] ["f1","f2"];
    val exp_wire_f2 = exp_wire_f1;
    val exp_wire_f3 = wire "n2" "n3" ["w7"] ["f3","f4"];
    val exp_wire_f4 = exp_wire_f3;
    val exp_wire_f5 = wire "n2" "n3" ["w8"] ["f5","f6"];
    val exp_wire_f6 = exp_wire_f5;
  in
    val _ = Testing.test "G.wire_eqwire_eq" (fn () => let
      (* TODO: val wire_eq : wire -> wire -> bool *) in () end) ();


    val _ = Testing.test "G.wire_ord" (fn () => let
      (* TODO: val wire_ord : wire * wire -> order *)
      in () end) ();


    val _ = Testing.test "G.delete_wire" (fn () => let
      (* TODO: val delete_wire : wire -> T -> T *)
      in () end) ();


    val _ = Testing.test "G.get_wire_containing_edge"
      (fn () =>
        let
          fun test_wire_for_edge exp_wire en =
              if G.wire_eq exp_wire (G.get_wire_containing_edge g_raw (E.mk en))
              then ()
              else raise ERROR ("Incorrect wire returned for edge "^en)

          val _ = test_wire_for_edge exp_wire_e1 "e1"
          val _ = test_wire_for_edge exp_wire_e2 "e2"
          val _ = test_wire_for_edge exp_wire_e3 "e3"
          val _ = test_wire_for_edge exp_wire_e4 "e4"
          val _ = test_wire_for_edge exp_wire_e5 "e5"
          val _ = test_wire_for_edge exp_wire_e6 "e6"
          val _ = test_wire_for_edge exp_wire_e7 "e7"
          val _ = test_wire_for_edge exp_wire_e8 "e8"
          val _ =
            if G.wire_eq exp_wire_ce1_a (G.get_wire_containing_edge g_raw (E.mk "ce1"))
            then ()
            else
              if G.wire_eq exp_wire_ce1_b (G.get_wire_containing_edge g_raw (E.mk "ce1"))
              then ()
              else raise ERROR "Incorrect wire returned for edge ce1"
          val _ =
            if G.wire_eq exp_wire_ce2_a (G.get_wire_containing_edge g_raw (E.mk "ce2"))
            then ()
            else
              if G.wire_eq exp_wire_ce2_b (G.get_wire_containing_edge g_raw (E.mk "ce2"))
              then ()
              else raise ERROR "Incorrect wire returned for edge ce2"
          val _ = test_wire_for_edge exp_wire_ce3 "ce3"
          val _ = test_wire_for_edge exp_wire_f1 "f1"
          val _ = test_wire_for_edge exp_wire_f2 "f2"
          val _ = test_wire_for_edge exp_wire_f3 "f3"
          val _ = test_wire_for_edge exp_wire_f4 "f4"
          val _ = test_wire_for_edge exp_wire_f5 "f5"
          val _ = test_wire_for_edge exp_wire_f6 "f6"
        in () end) ()


    val _ = Testing.test "G.get_wires_for_edges" (fn () => let
      (* TODO: val get_wires_for_edges : T -> E.NSet.T -> wire list *)
      in () end) ();


    val _ = Testing.test "G.get_wire_list"
      (fn () =>
        let
          val exp_wire_list = [exp_wire_e1,exp_wire_e4,exp_wire_e6,exp_wire_e7,
                               exp_wire_ce3,exp_wire_f1,exp_wire_f3,exp_wire_f5,
                               (G.get_wire_containing_edge g_raw (E.mk "ce1"))];
          val real_wire_list = G.get_wire_list g_raw;
          fun c_wire_lists (w::ws) (w'::ws') =
            if G.wire_eq w w' then c_wire_lists ws ws'
            else raise ERROR ("wire lists differ EXPECTED: "^Pretty.string_of (G.pretty_wire w)^
                              "  GOT: "^Pretty.string_of (G.pretty_wire w'))
            | c_wire_lists [] [] = ()
            | c_wire_lists _ _   = raise ERROR "failed, wire lists of different lengths"
        in
          c_wire_lists (sort G.wire_ord exp_wire_list)
                       (sort G.wire_ord real_wire_list)
        end) ()


    (*val _ = Testing.test "G.normalise"
      (fn () =>
        let
          val g = G.empty |> add_wv "w1"
                          |> add_wv "w2"
                          |> add_wv "w4"
                          |> add_wv "w5"
                          |> add_wv "w6"
                          |> add_wv "w7"
                          |> add_wv "w8"
                          |> add_wv "w9"
                          |> add_wv "w10"
                          |> add_wv "w11"
                          |> add_wv "w12"
                          |> add_wv "w13"
                          |> add_wv "w14"
                          |> add_wv "c1"
                          |> add_wv "c3"
                          |> add_vunit1 "n1"
                          |> add_vunit1 "n2"
                          |> add_vunit1 "n3"
                          |> add_dir_eunit1 "e1" "n1" "w1"
                          |> add_dir_eunit1 "e2" "w1" "w2"
                          |> add_dir_eunit1 "e3" "w2" "n2"
                          |> add_dir_eunit1 "e4" "n2" "w4"
                          |> add_dir_eunit1 "e6a" "n2" "w9"
                          |> add_dir_eunit1 "e6b" "w9" "w10"
                          |> add_dir_eunit1 "e6c" "w10" "n3"
                          |> add_dir_eunit1 "e7" "n3" "w5"
                          |> add_dir_eunit1 "e8a" "w5" "w11"
                          |> add_dir_eunit1 "e8b" "w11" "n3"
                          |> add_dir_eunit1 "ce1" "c1" "c1"
                          |> add_dir_eunit1 "ce3" "c3" "c3"
                          |> add_undir_eunit1 "f1" "n2" "w6"
                          |> add_undir_eunit1 "f2a" "w6" "w12"
                          |> add_undir_eunit1 "f2b" "w12" "n3"
                          |> add_undir_eunit1 "f3" "n2" "w7"
                          |> add_undir_eunit1 "f4a" "w7" "w13"
                          |> add_undir_eunit1 "f4b" "n3" "w13"
                          |> add_undir_eunit1 "f5" "w8" "n2"
                          |> add_undir_eunit1 "f6a" "w8" "w14"
                          |> add_undir_eunit1 "f6b" "w14" "n3";
          val g_exp_norm = g;
          val g_norm = G.normalise g_raw
        in
          if GIso.test g_norm g_exp_norm then ()
          else (writeln "Expected"; G.print g_exp_norm;
                writeln "Got"; G.print g_norm;
                raise ERROR "Minimise gave incorrect graph")
        end) ();*)


    (*val _ = Testing.test "G.minimise"
      (fn () =>
        val g = G.empty |> add_wv "w4"
                        |> add_wv "c1"
                        |> add_wv "c3"
                        |> add_vunit1 "n1"
                        |> add_vunit1 "n2"
                        |> add_vunit1 "n3"
                        |> add_dir_eunit1 "e1" "n1" "n2"
                        |> add_dir_eunit1 "e4" "n2" "w4"
                        |> add_dir_eunit1 "e6" "n2" "n3"
                        |> add_dir_eunit1 "e7" "n3" "n3"
                        |> add_dir_eunit1 "ce1" "c1" "c1"
                        |> add_dir_eunit1 "ce3" "c3" "c3"
                        |> add_undir_eunit1 "f1" "n2" "n3"
                        |> add_undir_eunit1 "f3" "n2" "n3"
                        |> add_undir_eunit1 "f5" "n2" "n3";
        val g_exp_min = g;
        let
          val g_min = G.minimise g_raw
        in
          if GIso.test g_min g_exp_min then ()
          else (writeln "Expected"; G.print g_exp_min;
                writeln "Got"; G.print g_min;
                raise ERROR "Minimise gave incorrect graph")
        end) ();*)


    val _ = Testing.test "G.split_edge" (fn () => let
      (* TODO: val split_edge : E.name -> T -> (E.name * V.name * E.name) * T *)
      in () end) ();
  end


  local (* plugging *)
    val g = G.empty |> add_vunit1 "n1"
                    |> add_vunit1 "n2"
                    |> add_vunit1 "n3"
                    |> add_vunit1 "n4"
                    |> add_wv "in1"
                    |> add_wv "in2"
                    |> add_wv "out1"
                    |> add_wv "out2"
                    |> add_wv "w1"
                    |> add_wv "w2"
                    |> add_wv "w3"
                    |> add_wv "w4"
                    |> add_wv "bnd1"
                    |> add_wv "bnd2"
                    |> add_wv "bnd3"
                    |> add_wv "bnd4"
                    |> add_dir_eunit1 "e1" "in1" "n1"
                    |> add_dir_eunit2 "e2" "in2" "n2"
                    |> add_dir_eunit1 "e3" "n1" "out1"
                    |> add_dir_eunit2 "e4" "n3" "out2"
                    |> add_dir_eunit1 "e5" "n1" "w1"
                    |> add_dir_eunit1 "e6" "w1" "n1"
                    |> add_undir_eunit1 "f1" "bnd1" "n1"
                    |> add_undir_eunit2 "f2" "bnd2" "n2"
                    |> add_undir_eunit1 "f3" "n3" "bnd3"
                    |> add_undir_eunit2 "f4" "n1" "bnd4"
                    |> add_undir_eunit1 "f5" "n2" "w2"
                    |> add_undir_eunit1 "f6" "n2" "w2"
                    |> add_undir_eunit2 "f7" "n3" "w3"
                    |> add_undir_eunit2 "f8" "w3" "n3";
  in
    val _ = Testing.test "G.get_plugging" (fn () =>
      let
        fun test_dir_plugging inv outv g =
          case G.get_plugging g (V.mk inv) (V.mk outv)
            of SOME ((Directed,_),(v1,v2)) =>
                 Testing.assert
                    ("G.get_plugging ("^inv^","^outv^"): correct vertex names")
                    ((V.name_eq (v1,V.mk outv)) andalso
                     (V.name_eq (v2,V.mk inv)))
             | SOME _ =>
                 Testing.assert
                    ("G.get_plugging ("^inv^","^outv^"): correct directedness")
                    false
             | NONE   =>
                 Testing.assert ("G.get_plugging ("^inv^","^outv^") != NONE")
                    false
        fun test_undir_plugging b1 b2 g =
          case G.get_plugging g (V.mk b1) (V.mk b2)
            of SOME ((Undirected,_),(v1,v2)) =>
                 Testing.assert
                    ("G.get_plugging ("^b1^","^b2^"): correct vertex names")
                    (V.NSet.eq (V.NSet.of_list (map V.mk [b1,b2]))
                               (V.NSet.of_list [v1,v2]))
             | SOME _ =>
                 Testing.assert
                    ("G.get_plugging ("^b1^","^b2^"): correct directedness")
                    false
             | NONE   =>
                 Testing.assert ("G.get_plugging ("^b1^","^b2^") != NONE")
                    false
        fun test_no_plugging b1 b2 g =
          Testing.assert ("G.get_plugging ("^b1^","^b2^") = NONE")
            (case G.get_plugging g (V.mk b1) (V.mk b2)
                of SOME _ => false
                 | NONE   => true)

        val _ = test_dir_plugging "in1" "out1" g
        val _ = test_dir_plugging "in2" "out2" g
        val _ = test_undir_plugging "bnd1" "bnd3" g
        val _ = test_undir_plugging "bnd4" "bnd2" g
        val _ = test_no_plugging "in1" "in2" g
        val _ = test_no_plugging "in1" "out2" g
        val _ = test_no_plugging "out1" "w1" g
        val _ = test_no_plugging "bnd1" "bnd2" g
        val _ = test_no_plugging "bnd1" "w1" g
      in () end) ()


    val _ = Testing.test "G.are_pluggable" (fn () =>
      let
        fun test_pluggable n1 n2 g =
          Testing.assert ("G.are_pluggable ("^n1^","^n2^")")
                         (G.are_pluggable g (V.mk n1) (V.mk n2))
        fun test_not_pluggable n1 n2 g =
          Testing.assert ("not G.are_pluggable ("^n1^","^n2^")")
                         (not (G.are_pluggable g (V.mk n1) (V.mk n2)))

        (* FIXME: w4 is an isolated point; should this be pluggable? *)
        val _ = test_pluggable "in1" "out1" g;
        val _ = test_pluggable "in2" "out2" g;
        val _ = test_not_pluggable "in1" "out2" g;
        val _ = test_not_pluggable "in2" "out1" g;
        val _ = test_not_pluggable "in1" "in2" g;
        val _ = test_not_pluggable "in1" "w1" g;
        val _ = test_not_pluggable "w1" "out2" g;
        val _ = test_pluggable "bnd1" "bnd3" g;
        val _ = test_pluggable "bnd4" "bnd2" g;
        val _ = test_not_pluggable "bnd1" "bnd2" g;
        val _ = test_not_pluggable "bnd1" "bnd4" g;
        val _ = test_not_pluggable "bnd4" "bnd3" g;
        val _ = test_not_pluggable "bnd3" "bnd2" g;
        val _ = test_not_pluggable "bnd1" "w2" g;
        val _ = test_not_pluggable "bnd1" "w3" g;
        val _ = test_not_pluggable "bnd1" "w1" g;
        val _ = test_not_pluggable "bnd3" "w2" g;
        val _ = test_not_pluggable "in1" "bnd3" g;
        val _ = test_not_pluggable "bnd3" "in1" g;
        val _ = test_not_pluggable "out1" "bnd3" g;
        val _ = test_not_pluggable "bnd3" "out1" g;
      in () end) ()


    val _ = Testing.test "G.plug" (fn () =>
      let
        fun test_dir_plug inv outv g =
          let val (e,g') = G.plug (V.mk inv) (V.mk outv) g in
            case G.get_edge_info g' e
              of ((Directed,_),(v1,v2)) =>
                   Testing.assert
                      ("G.plug ("^inv^","^outv^"): correct vertex names")
                      ((V.name_eq (v1,V.mk outv)) andalso
                       (V.name_eq (v2,V.mk inv)))
               | _ =>
                   Testing.assert
                      ("G.plug ("^inv^","^outv^"): correct directedness")
                      false
          end
        fun test_undir_plug b1 b2 g =
          let val (e,g') = G.plug (V.mk b1) (V.mk b2) g in
            case G.get_edge_info g' e
              of ((Undirected,_),(v1,v2)) =>
                   Testing.assert
                      ("G.plug ("^b1^","^b2^"): correct vertex names")
                      (V.NSet.eq (V.NSet.of_list (map V.mk [b1,b2]))
                                 (V.NSet.of_list [v1,v2]))
               | _ =>
                   Testing.assert
                      ("G.plug ("^b1^","^b2^"): correct directedness")
                      false
          end
        val _ = test_dir_plug "in1" "out1" g;
        val _ = test_dir_plug "in2" "out2" g;
        val _ = test_undir_plug "bnd1" "bnd3" g;
        val _ = test_undir_plug "bnd4" "bnd2" g;
      in () end) ()


    (* TODO: plug_anon *)


    val _ = Testing.test "G.plug_and_minimise (in1,out1)"
            (fn () =>
            let
              val in1 = V.mk "in1"
              val out1 = V.mk "out1"
              val ((s,t,wvs,es),g') = G.plug_and_minimise in1 out1 g
            in
               (Testing.assert "correct vertex names"
                  (V.NSet.eq (V.NSet.single (V.mk "n1"))
                             (V.NSet.of_list [s,t]));
                Testing.assert "no wire verts"
                  (V.NSet.is_empty wvs);
                case E.NSet.tryget_singleton es
                  of SOME e =>
                       (case G.get_edge_dir g' e
                         of Directed => ()
                          | _ => raise ERROR "Wrong directedness")
                   | NONE => raise ERROR "Wrong edge count"
               )
            end
            ) ();
    val _ = Testing.test "G.plug_and_minimise (out2,in2)"
            (fn () =>
            let
              val out2 = V.mk "out2"
              val in2 = V.mk "in2"
              val ((s,t,wvs,es),g') = G.plug_and_minimise out2 in2 g
            in
               (Testing.assert "correct vertex names"
                  (V.NSet.eq (V.NSet.of_list (map V.mk ["n2","n3"]))
                             (V.NSet.of_list [s,t]));
                Testing.assert "no wire verts"
                  (V.NSet.is_empty wvs);
                case E.NSet.tryget_singleton es
                  of SOME e =>
                       (case G.get_edge_dir g' e
                          of Directed => ()
                           | _ => raise ERROR "Wrong directedness")
                   | NONE => raise ERROR "Wrong edge count"
               )
            end
            ) ();


    (* TODO: plug_and_minimise_anon *)


    val _ = Testing.test "G.plug_and_normalise (in1,out1)"
            (fn () =>
            let
              val in1 = V.mk "in1"
              val out1 = V.mk "out1"
              val ((s,t,wvs,es),_) = G.plug_and_normalise in1 out1 g
            in
               (Testing.assert "correct vertex names"
                  (V.NSet.eq (V.NSet.single (V.mk "n1"))
                             (V.NSet.of_list [s,t]));
                Testing.assert "2 wire verts"
                  (V.NSet.cardinality wvs = 2);
                Testing.assert "2 edges"
                  (E.NSet.cardinality es = 3)
               )
            end
            ) ()


    (* TODO: plug_and_normalise_anon *)
  end


  val _ = Testing.test "get_open_subgraph" (fn () => let
    val g = G.empty |> add_vunit1 "n1"
                    |> add_vunit1 "n2"
                    |> add_vunit1 "n3"
                    |> add_wv "w1"
                    |> add_wv "w2"
                    |> add_wv "w3"
                    |> add_wv "w4"
                    |> add_wv "w5"
                    |> add_wv "w6"
                    |> add_wv "w7"
                    |> add_wv "w8"
                    |> add_wv "w9"
                    |> add_wv "w10"
                    |> add_wv "w11"
                    |> add_wv "w12"
                    |> add_wv "w13"
                    |> add_wv "w17"
                    |> add_wv "w18"
                    |> add_wv "w20"
                    |> add_dir_eunit1 "c1" "w1" "w1"
                    |> add_dir_eunit1 "bw1" "w2" "w3"
                    |> add_dir_eunit1 "e1" "n1" "w4"
                    |> add_dir_eunit1 "e2" "w4" "w5"
                    |> add_dir_eunit1 "e3" "w5" "n1"
                    |> add_dir_eunit1 "e4" "n2" "w9"
                    |> add_dir_eunit1 "e5" "w9" "w10"
                    |> add_dir_eunit1 "e6" "w10" "n3"
                    |> add_undir_eunit1 "f1" "w6" "n2"
                    |> add_undir_eunit1 "f2" "n2" "w7"
                    |> add_undir_eunit1 "f3" "w7" "w8"
                    |> add_undir_eunit1 "f4" "w8" "n3"
                      (* these will be connected to later *)
                    |> add_undir_eunit1 "g1" "n3" "w12"
                    |> add_dir_eunit1 "g2" "n3" "w13"
                    |> add_dir_eunit1 "g3" "n2" "w11"
                    |> add_undir_eunit1 "g4" "w12" "w17"
                    |> add_dir_eunit1 "g6" "w13" "w18"
                    |> add_dir_eunit1 "g8" "w11" "w20"
    val exp_subgraph = g;

    val retain = V.NSet.of_list (map V.mk ["n1","n2","n3","w1","w2","w3"]);

    val g = g |> add_vunit1 "n4"
              |> add_vunit1 "n5"
              |> add_wv "w14"
              |> add_wv "w15"
              |> add_wv "w16"
              |> add_wv "w19"
              |> add_dir_eunit1 "c2" "w14" "w14"
              |> add_dir_eunit1 "bw2" "w15" "w16"
              |> add_undir_eunit1 "g5" "w17" "n4"
              |> add_dir_eunit1 "g7" "w18" "n4"
              |> add_undir_eunit1 "f5" "n4" "w19"
              |> add_dir_eunit1 "g9" "w20" "n5"
    val full_graph = g;
    val g = G.get_open_subgraph full_graph retain
    val _ = Testing.assert "subgraph was as expected"
              (G.exact_eq g exp_subgraph)
  in () end) ();



  val _ = Testing.assert_no_failed_tests()
end;

(* We don't test Test_OGraph here, as it will actually get tested in
  * test-banggraph.ML *)

