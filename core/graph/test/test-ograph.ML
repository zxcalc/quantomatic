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
    val _ = assert_vnames [] (G.get_vertices G.empty)
    val _ = assert_enames [] (G.get_edges G.empty)
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



  (* TODO: get_edge_info *)



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



  (* TODO: get_adj_edges *)
  (* TODO: get_in_dir_edges *)
  (* TODO: get_out_dir_edges *)
  (* TODO: get_adj_undir_edges *)
  (* TODO: get_adj_vertices *)
  (* TODO: get_adj_vertices_to_set *)
  (* TODO: get_successor_vertices *)
  (* TODO: get_predecessor_vertices *)
  (* TODO: get_sibling_vertices *)
  (* TODO: adj_edge_classes *)
  (* TODO: get_vdata_tab *)
  (* TODO: get_edata_tab *)



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
    val _ = assert_enames [] ins;
    val _ = assert_enames ["e"] outs;

    val (vd,(ins,outs)) = (G.get_vertex_data g (V.mk "b"),
                           (G.get_in_edges g   (V.mk "b"),
                            G.get_out_edges g  (V.mk "b")))
    val _ = Testing.assert "get_vertex_data correct data" (G.vdata_eq (vd,vunit2));
    val _ = assert_enames ["e","f"] ins;
    val _ = assert_enames ["f"] outs;

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



  (* TODO: get_edge_dir *)



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
    val _ = assert_vertices g'
              [("a",(vunit2,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))];
    val _ = assert_edges g'
              [("e",((Directed,eunit1),("a","b"))),
               ("f",((Undirected,eunit2),("b","a")))];

    val g'' = G.set_vertex_data vunit2 (V.mk "b") g';
    val _ = assert_vertices g''
              [("a",(vunit2,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))];
    val _ = assert_edges g''
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
    val _ = assert_vertices g'
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))];
    val _ = assert_edges g'
              [("e",((Directed,eunit2),("a","b"))),
               ("f",((Undirected,eunit2),("b","a")))];

    val g'' = G.set_edge_data eunit2 (E.mk "f") g';
    val _ = assert_vertices g''
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))];
    val _ = assert_edges g''
              [("e",((Directed,eunit2),("a","b"))),
               ("f",((Undirected,eunit2),("b","a")))];
    in () end) ();



  (* TODO: set_edge_dir *)



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
    val _ = assert_vertices g'
              [("a",(vunit2,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))];
    val _ = assert_edges g'
              [("e",((Directed,eunit1),("a","b"))),
               ("f",((Undirected,eunit2),("b","a")))];

    val g'' = G.update_vertex_data swap_vd (V.mk "b") g';
    val _ = assert_vertices g''
              [("a",(vunit2,(["f"],["e"]))),
               ("b",(vunit1,(["e"],["f"])))];
    val _ = assert_edges g''
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
    val _ = assert_vertices g'
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))];
    val _ = assert_edges g'
              [("e",((Directed,eunit2),("a","b"))),
               ("f",((Undirected,eunit2),("b","a")))];

    val g'' = G.update_edge_data swap_ed (E.mk "f") g';
    val _ = assert_vertices g''
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))];
    val _ = assert_edges g''
              [("e",((Directed,eunit2),("a","b"))),
               ("f",((Undirected,eunit1),("b","a")))];
    in () end) ();



  (* TODO: update_edge_dir *)
  (* TODO: is_wire_vertex *)
  (* TODO: is_node_vertex *)



  val _ = Testing.test "G.get_vertices" (fn () => let 
    val _ = Testing.assert "empty" (V.NSet.is_empty (G.get_vertices G.empty));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = assert_vnames ["a","b"] (G.get_vertices g);
  in () end) ();



  (* TODO: get_wire_vertices *)
  (* TODO: get_node_vertices *)



  val _ = Testing.test "G.get_edges" (fn () => let 
    val _ = Testing.assert "empty" (E.NSet.is_empty (G.get_edges G.empty));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = assert_enames ["e","f"] (G.get_edges g);
  in () end) ();



  (* TODO: is_subgraph *)



  val _ = Testing.test "G.exact_eq" (fn () => let 
    val _ = Testing.assert "empty = empty" (G.exact_eq G.empty G.empty)

    val g1 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_undir_eunit2 "f" "b" "a"
    val g2 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_undir_eunit2 "f" "b" "a"
    val g3 = G.empty |> add_vunit1 "a"
                     |> add_vunit1 "b"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_undir_eunit2 "f" "b" "a"
    val g4 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_dir_eunit2 "f" "b" "a"
    val g5 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit2 "e" "a" "b"
                     |> add_undir_eunit2 "f" "b" "a"
    val g6 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_undir_eunit2 "f" "a" "b"
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
    val _ = assert_vertices g [(V.string_of_name vn,(vunit1,([],[])))];
    val _ = assert_edges g [];

    val (vn',g') = G.add_vertex vunit2 g;
    val _ = Testing.assert "new name" (not (V.name_eq (vn,vn')));
    val _ = assert_vertices g' [(V.string_of_name vn,(vunit1,([],[]))),
                                (V.string_of_name vn',(vunit2,([],[])))];
    val _ = assert_edges g' [];
    in () end) ();



  (* TODO: add_vertex_anon *)



  val _ = Testing.test "G.add_named_vertex" (fn () => let 
    val v_a = V.mk "a";
    val v_b = V.mk "b";

    val g = G.add_named_vertex v_a vunit1 G.empty;
    val _ = assert_vertices g [("a",(vunit1,([],[])))];
    val _ = assert_enames [] (G.get_edges g);

    val g' = G.add_named_vertex v_b vunit2 g;
    val _ = assert_vertices g'
              [("a",(vunit1,([],[]))),("b",(vunit2,([],[])))];
    val _ = assert_enames [] (G.get_edges g');

    val (vn, g'') = G.add_vertex vunit1 g';
    val _ = Testing.assert "correct name (!= a)" (not (V.name_eq (vn,v_a)));
    val _ = Testing.assert "correct name (!= b)" (not (V.name_eq (vn,v_b)));
    val _ = assert_vertices g''
              [("a",(vunit1,([],[]))),
               ("b",(vunit2,([],[]))),
               (V.string_of_name vn,(vunit1,([],[])))];
    val _ = assert_enames [] (G.get_edges g'');
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
    val _ = assert_edges g' [(enstr,((Directed,eunit1),("a","a")))];
    val _ = assert_vertices g' [("a",(vunit1,([enstr],[enstr])))];

    val g = g |> add_vunit2 "b";

    val (en,g') = G.add_edge (Directed,eunit1) va vb g;
    val enstr = E.string_of_name en;
    val _ = assert_edges g'
              [(enstr,((Directed,eunit1),("a","b")))];
    val _ = assert_vertices g'
              [("a",(vunit1,([],[enstr]))),
               ("b",(vunit2,([enstr],[])))];

    val (en',g'') = G.add_edge (Undirected,eunit2) vb va g';
    val enstr' = E.string_of_name en';
    val _ = assert_edges g''
              [(enstr,((Directed,eunit1),("a","b"))),
               (enstr',((Undirected,eunit2),("b","a")))];
    val _ = assert_vertices g''
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
    val _ = assert_edges g' [("e",((Directed,eunit1),("a","a")))];
    val _ = assert_vertices g' [("a",(vunit1,(["e"],["e"])))];

    val g = g |> add_vunit2 "b";

    val g' = G.add_named_edge ee (Directed,eunit1) va vb g;
    val _ = assert_edges g'
              [("e",((Directed,eunit1),("a","b")))];
    val _ = assert_vertices g'
              [("a",(vunit1,([],["e"]))),
               ("b",(vunit2,(["e"],[])))];

    val g'' = G.add_named_edge ef (Undirected,eunit2) vb va g';
    val _ = assert_edges g''
              [("e",((Directed,eunit1),("a","b"))),
               ("f",((Undirected,eunit2),("b","a")))];
    val _ = assert_vertices g''
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))];

    val (en,g''') = G.add_edge (Directed,eunit2) vb vb g'';
    val _ = Testing.assert "correct name (4) (new)"
              (not (E.name_eq (en,ee)) andalso not (E.name_eq (en,ef)));
    val _ = assert_edges g'''
              [("e",((Directed,eunit1),("a","b"))),
               ("f",((Undirected,eunit2),("b","a"))),
               (E.string_of_name en,((Directed,eunit2),("b","b")))];
    val enstr = E.string_of_name en;
    val _ = assert_vertices g'''
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
    val _ = assert_vertices g'
              [("a",(vunit1,([],[]))),
               ("b",(vunit2,(["f"],["f"])))];
    val _ = assert_edges g'
              [("f",((Directed,eunit2),("b","b")))];
    in () end) ();



  val _ = Testing.test "G.delete_vertex" (fn () => let
    val _ = G.delete_vertex (V.mk "a") G.empty;

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val g' = G.delete_vertex (V.mk "a") g;
    val _ = assert_vertices g' [("b",(vunit2,(["f"],["f"])))];
    val _ = assert_edges g' [("f",((Directed,eunit2),("b","b")))];

    val g' = G.delete_vertex (V.mk "b") g;
    val _ = assert_vertices g' [("a",(vunit1,([],[])))];
    val _ = assert_edges g' [];

    val g' = G.delete_vertex (V.mk "a") g';
    val _ = assert_vertices g' [];
    val _ = assert_edges g' [];
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
    val _ = assert_vertices g'
              [("a",(vunit1,(["e","f"],[]))),
               ("b",(vunit2,([],["e","f"])))];
    val _ = assert_edges g'
              [("e",((Directed,eunit1),("b","a"))),
               ("f",((Undirected,eunit2),("b","a")))];

    val g' = G.swap_edge_dir (E.mk "f") g;
    val _ = assert_vertices g'
              [("a",(vunit1,([],["e","f"]))),
               ("b",(vunit2,(["e","f"],[])))];
    val _ = assert_edges g'
              [("e",((Directed,eunit1),("a","b"))),
               ("f",((Undirected,eunit2),("a","b")))];
    in () end) ();



  (* TODO: is_intersection *)
  (* TODO: merge_by_subgraph *)
  (* TODO: merge_by_vertices *)
  (* TODO: get_arity *)
  (* TODO: get_self_loops *)
  (* TODO: delete_subgraph *)
  (* TODO: has_edges_between *)
  (* TODO: edges_between *)
  (* TODO: dir_edges_between *)
  (* TODO: rename_ograph *)
  (* TODO: rename_ograph_anon *)
  (* TODO: rename_vertices *)
  (* TODO: rename_vertex *)
  (* TODO: rename_vertex_opt *)
  (* TODO: rename_edges *)
  (* TODO: rename_edge *)
  (* TODO: rename_edge_opt *)
  (* TODO: get_boundary *)
  (* TODO: get_inputs *)
  (* TODO: get_outputs *)
  (* TODO: is_boundary *)
  (* TODO: is_input *)
  (* TODO: is_output *)
  (* TODO: wv_get_other_edge *)





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
    (* TODO: wire_eq *)
    (* TODO: wire_ord *)
    (* TODO: delete_wire *)


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


    (* TODO: get_wires_for_edges *)


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


    (* TODO: split_edge *)
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


    (* TODO: do_plug *)


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


    (* TODO: do_plug_and_minimise *)


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


    (* TODO: do_plug_and_normalise *)
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

local
  structure OG_Tests = OGraph_Interface_Tests(Test_OGraph);
in val _ = () end;

