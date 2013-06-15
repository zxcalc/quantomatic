(* Test functions in Graph *)

functor Graph_Interface_Tests(
  structure G : TEST_OGRAPH
)
= struct
  structure Tools = Test_OGraph_Tools(
    structure G  = G
  )
  open Tools

  (*
   * Tests for BASIC_GRAPH
   *)

  val _ = Testing.test "G.empty" (fn () => let 
    val _ = assert_vnames [] (G.get_vertices G.empty);
    val _ = assert_enames [] (G.get_edges G.empty);
    in () end) ();

(*  val _ = Testing.test "G.get_xnames" (fn () => let 
    val _ = Testing.assert "get_xnames empty"
              (X.NSet.is_empty (G.get_xnames G.empty));
    val g = G.empty |> add_vexpr1 "a" "2\\pi + w"
                    |> add_vexpr1 "b" "2/3*x"
                    |> add_vexpr2 "c" "3w - v"
                    |> add_undir_eexpr1 "e" "a" "b" "v + y"
                    |> add_undir_eexpr2 "f" "b" "c" "y + z";
    val _ = assert_xnames ["v","w","x","y","z"] (G.get_xnames g);
    in () end) ();
*)



  val _ = Testing.test "G.get_vertex_data" (fn () => let 
    val _ = (G.get_vertex_data G.empty (V.mk "a");
             raise ERROR "G.get_vertex_data did not raise exception")
            handle G.no_such_vname_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.exact_eq g G.empty));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

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
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "e");
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();




  val _ = Testing.test "G.get_edge_dir_and_data" (fn () => let 
    val _ = (G.get_edge_dir_and_data G.empty (E.mk "a");
             raise ERROR "G.get_edge_dir_and_data did not raise exception")
            handle G.no_such_ename_exp (_,en,g') =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.exact_eq G.empty g'));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val ((dir,ed),(src,tgt)) = (G.get_edge_dir_and_data g (E.mk "e"),
                                (G.get_edge_source g      (E.mk "e"),
                                 G.get_edge_target g      (E.mk "e")))
    val _ = Testing.assert "get_edge_dir_and_data correct data" (G.Data.edata_eq (ed,eunit1));
    val _ = Testing.assert "get_edge_dir_and_data correct direction"
              (case dir of Directed => false | Undirected => true);
    val _ = Testing.assert "get_edge_source correct" (V.name_eq (src,V.mk "a"));
    val _ = Testing.assert "get_edge_target correct" (V.name_eq (tgt,V.mk "b"));
    val ((dir,ed),(src,tgt)) = (G.get_edge_dir_and_data g (E.mk "f"),
                                (G.get_edge_source g      (E.mk "f"),
                                 G.get_edge_target g      (E.mk "f")))
    val _ = Testing.assert "get_edge_dir_and_data correct data" (G.Data.edata_eq (ed,eunit2));
    val _ = Testing.assert "get_edge_dir_and_data correct direction"
              (case dir of Directed => true | Undirected => false);
    val _ = Testing.assert "get_edge_dir_and_data correct src" (V.name_eq (src,V.mk "b"));
    val _ = Testing.assert "get_edge_dir_and_data correct tgt" (V.name_eq (tgt,V.mk "b"));

    val _ = (G.get_edge_dir_and_data g (E.mk "a");
             raise ERROR "G.get_edge_dir_and_data did not raise exception")
            handle G.no_such_ename_exp (_,en,g') =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();




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




  val _ = Testing.test "G.add_named_edge" (fn () => let 
    val va = V.mk "a";
    val vb = V.mk "b";
    val ee = E.mk "e";
    val ef = E.mk "f";

    val _ = (G.add_named_edge ee (Directed,eunit1) va vb G.empty;
             raise ERROR "G.add_named_edge did not raise exception (1)")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (1)"
                  ((V.name_eq (vn,va)) orelse (V.name_eq (vn,vb)));
               Testing.assert "correct graph in exp (1)" (G.exact_eq G.empty g'));

    val g = G.empty |> add_vunit1 "a";

    val _ = (G.add_named_edge ee (Directed,eunit1) va vb g;
             raise ERROR "G.add_named_edge did not raise exception (2)")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (2)" (V.name_eq (vn,vb));
               Testing.assert "correct graph in exp (2)" (G.exact_eq g g'));
    val _ = (G.add_named_edge ee (Directed,eunit1) vb va g;
             raise ERROR "G.add_named_edge did not raise exception (3)")
            handle G.no_such_vname_exp (_,vn,g') =>
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
    (* deleters no longer raise on absence *)
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
    (* deleters no longer raise on absence *)
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


  val _ = Testing.test "G.update_vertex_data" (fn () => let 
    val _ = (G.update_vertex_data I (V.mk "a") G.empty;
              raise ERROR "G.get_update_vertex_data did not raise exception")
            handle G.no_such_vname_exp (_,vn,g) =>
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
            handle G.no_such_ename_exp (_,en,g) =>
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





  (*
   * Tests for GRAPH
   *)

  val _ = Testing.test "G.is_empty" (fn () => let 
    val _ = Testing.assert "empty graph is empty" (G.is_empty G.empty);
    val g = G.empty |> add_vunit1 "a";
    val _ = Testing.assert "non-empty graph is not empty" (not (G.is_empty g));
    in () end) ();



  val _ = Testing.test "G.get_edge_source" (fn () => let 
    val _ = (G.get_edge_source G.empty (E.mk "a");
              raise ERROR "G.get_edge_source did not raise exception")
            handle G.no_such_ename_exp (_,en,g) =>
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
            handle G.no_such_ename_exp (_,en,g') =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.get_edge_target" (fn () => let 
    val _ = (G.get_edge_target G.empty (E.mk "a");
              raise ERROR "G.get_edge_target did not raise exception")
            handle G.no_such_ename_exp (_,en,g) =>
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
            handle G.no_such_ename_exp (_,en,g') =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.edge_get_other_vertex" (fn () => let 
    val _ = (G.edge_get_other_vertex G.empty (E.mk "a") (V.mk "a");
              raise ERROR "G.edge_get_other_vertex did not raise exception")
            handle G.no_such_ename_exp (_,en,g) =>
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
            handle G.no_such_ename_exp (_,en,g') =>
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
            handle G.no_such_vname_exp (_,vn,g) =>
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
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp" (V.name_eq (vn,V.mk "c"));
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.get_out_edges" (fn () => let 
    val _ = (G.get_out_edges G.empty (V.mk "a");
              raise ERROR "G.get_out_edges did not raise exception")
            handle G.no_such_vname_exp (_,vn,g) =>
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
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp" (V.name_eq (vn,V.mk "c"));
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.get_vertex_data" (fn () => let 
    val _ = (G.get_vertex_data G.empty (V.mk "a");
              raise ERROR "G.get_vertex_data did not raise exception")
            handle G.no_such_vname_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = Testing.assert "data of 'a' correct"
            (G.vdata_eq (G.get_vertex_data g (V.mk "a"),vunit1));
    val _ = Testing.assert "data of 'b' correct"
            (G.vdata_eq (G.get_vertex_data g (V.mk "b"),vunit2));

    val _ = (G.get_vertex_data g (V.mk "c");
             raise ERROR "G.get_vertex_data did not raise exception")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp" (V.name_eq (vn,V.mk "c"));
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.get_edge_data" (fn () => let 
    val _ = (G.get_edge_data G.empty (E.mk "a");
              raise ERROR "G.get_edge_data did not raise exception")
            handle G.no_such_ename_exp (_,en,g) =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = Testing.assert "data of 'e' correct"
            (G.Data.edata_eq (G.get_edge_data g (E.mk "e"),eunit1));
    val _ = Testing.assert "data of 'f' correct"
            (G.Data.edata_eq (G.get_edge_data g (E.mk "f"),eunit2));

    val _ = (G.get_edge_data g (E.mk "c");
             raise ERROR "G.get_edge_data did not raise exception")
            handle G.no_such_ename_exp (_,en,g') =>
              (Testing.assert "correct ename in exp" (E.name_eq (en,E.mk "c"));
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();



  val _ = Testing.test "G.set_vertex_data" (fn () => let 
    val _ = (G.set_vertex_data vunit1 (V.mk "a") G.empty;
              raise ERROR "G.set_vertex_data did not raise exception")
            handle G.no_such_vname_exp (_,vn,g) =>
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
            handle G.no_such_ename_exp (_,en,g) =>
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



  (*val _ = Testing.test "G.apply_data_subst" (fn () => let 
    val _ = Testing.assert "apply_data_subst no effect"
              (G.is_empty (G.apply_data_subst G.Param.empty_subst G.empty));
    (* TODO: more tests *)
    in () end) ();



  val _ = Testing.test "G.exact_eq" (fn () => let 
    val _ = Testing.assert "empty = empty" (G.exact_eq G.empty G.empty);

    val g1 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_undir_eunit2 "f" "b" "a";
    val g2 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_undir_eunit2 "f" "b" "a";
    val g3 = G.empty |> add_vunit1 "a"
                     |> add_vunit1 "b"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_undir_eunit2 "f" "b" "a";
    val g4 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_dir_eunit2 "f" "b" "a";
    val g5 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit2 "e" "a" "b"
                     |> add_undir_eunit2 "f" "b" "a";
    val g6 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit1 "e" "a" "b"
                     |> add_undir_eunit2 "f" "a" "b";
    val g7 = G.empty |> add_vunit1 "a"
                     |> add_vunit2 "b"
                     |> add_dir_eunit1 "e" "b" "a"
                     |> add_undir_eunit2 "f" "b" "a";

    val _ = Testing.assert "g1 = g2" (G.exact_eq g1 g2);
    val _ = Testing.assert "g1 != g3" (not (G.exact_eq g1 g3));
    val _ = Testing.assert "g1 != g4" (not (G.exact_eq g1 g4));
    val _ = Testing.assert "g1 != g5" (not (G.exact_eq g1 g5));
    val _ = Testing.assert "g1 != g6" (not (G.exact_eq g1 g6));
    val _ = Testing.assert "g1 != g7" (not (G.exact_eq g1 g7));
    in () end) ();
 *)


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

  (* doadd_vertex is difficult to test... *)




  val _ = Testing.test "G.add_named_vertex" (fn () => let 
    val v_a = V.mk "a";
    val v_b = V.mk "b";

    val g = G.add_named_vertex v_a vunit1 G.empty;
    val _ = assert_vertices g [("a",(vunit1,([],[])))];
    val _ = assert_edges g [];

    val g' = G.add_named_vertex v_b vunit2 g;
    val _ = assert_vertices g' [("a",(vunit1,([],[]))),
                             ("b",(vunit2,([],[])))];
    val _ = assert_edges g' [];

    val _ = (G.add_named_vertex v_a vunit1 g';
            raise ERROR "G.add_named_vertex did not raise exception")
            handle G.vname_already_exists_exp (vn,gexp) =>
              (Testing.assert "correct vname in exp"
                              (V.name_eq (vn,v_a));
               Testing.assert "correct graph in exp"
                              (G.exact_eq g' gexp));
    in () end) ();



  val _ = Testing.test "G.add_edge" (fn () => let 
    val va = V.mk "a";
    val vb = V.mk "b";

    val _ = (G.add_edge (Directed,eunit1) va vb G.empty;
             raise ERROR "G.add_edge did not raise exception (1)")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (1)"
                  ((V.name_eq (vn,va)) orelse (V.name_eq (vn,vb)));
               Testing.assert "correct graph in exp (1)" (G.exact_eq G.empty g'));

    val g = G.empty |> add_vunit1 "a";

    val _ = (G.add_edge (Directed,eunit1) va vb g;
             raise ERROR "G.add_edge did not raise exception (2)")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (2)" (V.name_eq (vn,vb));
               Testing.assert "correct graph in exp (2)" (G.exact_eq g g'));
    val _ = (G.add_edge (Directed,eunit1) vb va g;
             raise ERROR "G.add_edge did not raise exception (3)")
            handle G.no_such_vname_exp (_,vn,g') =>
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



  val _ = Testing.test "G.add_named_edge" (fn () => let 
    val va = V.mk "a";
    val vb = V.mk "b";
    val ee = E.mk "e";
    val ef = E.mk "f";

    val _ = (G.add_named_edge ee (Directed,eunit1) va vb G.empty;
             raise ERROR "G.add_named_edge did not raise exception (1)")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (1)"
                  ((V.name_eq (vn,va)) orelse (V.name_eq (vn,vb)));
               Testing.assert "correct graph in exp (1)" (G.exact_eq G.empty g'));

    val g = G.empty |> add_vunit1 "a";

    val _ = (G.add_named_edge ee (Directed,eunit1) va vb g;
             raise ERROR "G.add_named_edge did not raise exception (2)")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (2)" (V.name_eq (vn,vb));
               Testing.assert "correct graph in exp (2)" (G.exact_eq g g'));
    val _ = (G.add_named_edge ee (Directed,eunit1) vb va g;
             raise ERROR "G.add_named_edge did not raise exception (3)")
            handle G.no_such_vname_exp (_,vn,g') =>
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

    val _ = (G.add_named_edge ee (Directed,eunit1) va vb g'';
             raise ERROR "G.add_named_edge did not raise exception (4)")
            handle G.ename_already_exists_exp (en,gex) =>
              (Testing.assert "correct ename in exp (2)" (E.name_eq (en,ee));
               Testing.assert "correct graph in exp (2)" (G.exact_eq g'' gex));
    in () end) ();

  (* doadd_edge is difficult to test... *)



  val _ = Testing.test "G.swap_edge_dir" (fn () => let 
    val _ = (G.swap_edge_dir (E.mk "a") G.empty;
              raise ERROR "G.swap_edge_dir did not raise exception")
            handle G.no_such_ename_exp (_,en,g) =>
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



  val _ = Testing.test "G.get_vertices" (fn () => let 
    val _ = Testing.assert "empty" (V.NSet.is_empty (G.get_vertices G.empty));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = assert_vnames ["a","b"] (G.get_vertices g);
  in () end) ();



  val _ = Testing.test "G.get_edges" (fn () => let 
    val _ = Testing.assert "empty" (E.NSet.is_empty (G.get_edges G.empty));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = assert_enames ["e","f"] (G.get_edges g);
  in () end) ();



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



  val _ = Testing.test "G.try_rename1_vname" (fn () => let 
    val _ = Testing.assert "G.try_rename1_vname makes no changes (1)"
              (G.is_empty (the (G.try_rename1_vname (V.mk "a") (V.mk "b")
                                                    G.empty)));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = Testing.assert "G.try_rename1_vname makes no changes (2)"
              (G.exact_eq g (the (G.try_rename1_vname (V.mk "c") (V.mk "d") g)));
    val _ = assert_none "a->b" (G.try_rename1_vname (V.mk "a") (V.mk "b") g);

    val g' =
    case G.try_rename1_vname (V.mk "a") (V.mk "c") g
      of NONE => raise ERROR "rename a to c failed"
       | SOME g' =>
           (assert_vertices g'
                      [("c",(vunit1,([],["e"]))),
                       ("b",(vunit2,(["e","f"],["f"])))];
            assert_edges g'
                      [("e",((Undirected,eunit1),("c","b"))),
                       ("f",((Directed,eunit2),("b","b")))];
            g');

    val _ =
    case G.try_rename1_vname (V.mk "b") (V.mk "a") g'
      of NONE => raise ERROR "rename b to a failed"
       | SOME g'' =>
           (assert_vertices g''
                      [("c",(vunit1,([],["e"]))),
                       ("a",(vunit2,(["e","f"],["f"])))];
            assert_edges g''
                      [("e",((Undirected,eunit1),("c","a"))),
                       ("f",((Directed,eunit2),("a","a")))]);

    val g = G.empty |> add_vexpr1 "a" "2\\pi + w"
                    |> add_vexpr1 "b" "2/3*x"
                    |> add_vexpr2 "c" "3w - v"
                    |> add_dir_eexpr1 "e" "a" "b" "v + y"
                    |> add_undir_eexpr2 "f" "b" "c" "y + z";
    val g' =
    case G.try_rename1_vname (V.mk "b") (V.mk "d") g
      of NONE => raise ERROR "rename b to d failed"
       | SOME g' => g';
    val _ = assert_vertices g'
              [("a",(vexpr1 "2\\pi + w",([],["e"]))),
               ("d",(vexpr1 "2/3*x",(["e"],["f"]))),
               ("c",(vexpr2 "3w - v",(["f"],[])))];
    val _ = assert_edges g'
              [("e",((Directed,eexpr1 "v + y"),("a","d"))),
               ("f",((Undirected,eexpr2 "y + z"),("d","c")))];
    in () end) ();




  val _ = Testing.test "G.try_rename1_ename" (fn () => let 
    val _ = Testing.assert "G.try_rename1_ename makes no changes"
              (G.is_empty (the (G.try_rename1_ename (E.mk "a") (E.mk "b") G.empty)));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = Testing.assert "G.try_rename1_ename makes no changes (2)"
              (G.exact_eq g (the (G.try_rename1_ename (E.mk "c") (E.mk "d") g)));
    val _ = assert_none "f->e" (G.try_rename1_ename (E.mk "f") (E.mk "e") g);

    val g' =
    case G.try_rename1_ename (E.mk "f") (E.mk "d") g
      of NONE => raise ERROR "rename b to d failed"
       | SOME g' => g';
    val _ = assert_vertices g'
              [("a",(vunit1,([],["e"]))),
               ("b",(vunit2,(["e","d"],["d"])))];
    val _ = assert_edges g'
              [("e",((Undirected,eunit1),("a","b"))),
               ("d",((Directed,eunit2),("b","b")))];

    val g'' =
    case G.try_rename1_ename (E.mk "e") (E.mk "f") g'
      of NONE => raise ERROR "rename e to f failed"
       | SOME g' => g';
    val _ = assert_vertices g''
              [("a",(vunit1,([],["f"]))),
               ("b",(vunit2,(["f","d"],["d"])))];
    val _ = assert_edges g''
              [("f",((Undirected,eunit1),("a","b"))),
               ("d",((Directed,eunit2),("b","b")))];

    val g = G.empty |> add_vexpr1 "a" "2\\pi + w"
                    |> add_vexpr1 "b" "2/3*x"
                    |> add_vexpr2 "c" "3w - v"
                    |> add_dir_eexpr1 "e" "a" "b" "v + y"
                    |> add_undir_eexpr2 "f" "b" "c" "y + z";
    val g' =
    case G.try_rename1_ename (E.mk "e") (E.mk "d") g
      of NONE => raise ERROR "rename e to d failed"
       | SOME g' => g';
    val _ = assert_vertices g'
              [("a",(vexpr1 "2\\pi + w",([],["d"]))),
               ("b",(vexpr1 "2/3*x",(["d"],["f"]))),
               ("c",(vexpr2 "3w - v",(["f"],[])))];
    val _ = assert_edges g'
              [("d",((Directed,eexpr1 "v + y"),("a","b"))),
               ("f",((Undirected,eexpr2 "y + z"),("b","c")))];
    in () end) ();




  (* TODO: finish tests for GRAPH signature *)

  (* G.VertexCRnm.rename *)
  (* G.EdgeCRnm.rename *)
  (* G.rename *)
  (* G.rename_apart *)
  (* G.merge_by_vertices *)
  (* G.delete_subgraph *)
  (* G.get_vertex_list *)
  (* G.get_edge_list *)
  (* G.has_edges_between *)
  (* G.edges_between *)
  (* G.enames_between *)
  (* G.dir_enames_between *)
  (* G.dir_edges_between *)
  (* G.adj_vnames *)
  (* G.adj_edge_classes *)
  (* G.fold_adj *)
  (* G.incident_vertices *)
  (* G.get_self_loops *)
  (* G.in_enames *)
  (* G.out_enames *)
  (* G.pred_enames *)
  (* G.succ_enames *)
  (* G.get_arity *)
  (* G.dft *)
  (* G.dft_with_roots *)
  (* G.convert_to_dag *)
  (* G.spanning_dag *)
  (* G.spanning_tree *)
  (* G.get_dag_ranks *)
  (* G.copy_subgraph *)

  val _ = Testing.assert_no_failed_tests();
end;

local
  structure G_Tests = Graph_Interface_Tests(
    structure G = Test_OGraph
  )
in val _ = () end;


