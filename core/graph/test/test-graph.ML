(* Test functions in Graph *)

functor Graph_Interface_Tests(
  structure G : GRAPH where type EData.data = Test_Graph_Data.edata
  val vert : Test_Graph_Data.vdata -> G.VData.data
  val unwrap_vert : G.VData.data -> Test_Graph_Data.vdata
)
= struct
  structure Tools = Test_Graph_Tools(
    structure G  = G
    val vert = vert
    val unwrap_vert = unwrap_vert
  );
  open Tools;

  (*
   * Tests for BASIC_GRAPH
   *)

  val _ = Testing.test "G.empty" (fn () => let 
    val _ = assert_vertices [] (G.get_vertices G.empty);
    val _ = assert_edges [] (G.get_edges G.empty);
    in () end) ();




  val _ = Testing.test "G.get_xnames" (fn () => let 
    val _ = Testing.assert "get_xnames empty"
              (X.NSet.is_empty (G.get_xnames G.empty));
    val g = G.empty |> add_vexpr1 "a" "2\\pi + w"
                    |> add_vexpr1 "b" "2/3*x"
                    |> add_vexpr2 "c" "3w - v"
                    |> add_undir_eexpr1 "e" "a" "b" "v + y"
                    |> add_undir_eexpr2 "f" "b" "c" "y + z";
    val _ = assert_xnames ["v","w","x","y","z"] (G.get_xnames g);
    in () end) ();




  val _ = Testing.test "G.get_vertex" (fn () => let 
    val _ = (G.get_vertex G.empty (V.mk "a");
             raise ERROR "G.get_vertex did not raise exception")
            handle G.no_such_vname_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.exact_eq g G.empty));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val (vd,(ins,outs)) = G.get_vertex g (V.mk "a")
    val _ = Testing.assert "get_vertex correct data"
              (G.VData.data_eq (vd,vunit1));
    val _ = assert_enames [] ins;
    val _ = assert_enames ["e"] outs;

    val (vd,(ins,outs)) = G.get_vertex g (V.mk "b")
    val _ = Testing.assert "get_vertex correct data" (G.VData.data_eq (vd,vunit2));
    val _ = assert_enames ["e","f"] ins;
    val _ = assert_enames ["f"] outs;

    val _ = (G.get_vertex g (V.mk "e");
             raise ERROR "G.get_vertex did not raise exception")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "e");
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();




  val _ = Testing.test "G.get_edge" (fn () => let 
    val _ = (G.get_edge G.empty (E.mk "a");
             raise ERROR "G.get_edge did not raise exception")
            handle G.no_such_ename_exp (_,en,g') =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.exact_eq G.empty g'));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val ((dir,ed),(src,tgt)) = G.get_edge g (E.mk "e")
    val _ = Testing.assert "get_edge correct data" (G.EData.data_eq (ed,eunit1));
    val _ = Testing.assert "get_edge correct direction"
              (case dir of G.Directed => false | G.UnDirected => true);
    val _ = Testing.assert "get_edge correct src" (V.name_eq (src,V.mk "a"));
    val _ = Testing.assert "get_edge correct tgt" (V.name_eq (tgt,V.mk "b"));
    val ((dir,ed),(src,tgt)) = G.get_edge g (E.mk "f")
    val _ = Testing.assert "get_edge correct data" (G.EData.data_eq (ed,eunit2));
    val _ = Testing.assert "get_edge correct direction"
              (case dir of G.Directed => true | G.UnDirected => false);
    val _ = Testing.assert "get_edge correct src" (V.name_eq (src,V.mk "b"));
    val _ = Testing.assert "get_edge correct tgt" (V.name_eq (tgt,V.mk "b"));

    val _ = (G.get_edge g (E.mk "a");
             raise ERROR "G.get_edge did not raise exception")
            handle G.no_such_ename_exp (_,en,g') =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    in () end) ();




  val _ = Testing.test "G.add_vertex_named_like" (fn () => let 
    val v_a = V.mk "a";
    val v_b = V.mk "b";

    val (vn,g) = G.add_vertex_named_like v_a vunit1 G.empty;
    val _ = Testing.assert "correct name (a)" (V.name_eq (vn,v_a));
    val _ = assert_vertices [("a",(vunit1,([],[])))] (G.get_vertices g);
    val _ = assert_edges [] (G.get_edges g);

    val (vn,g') = G.add_vertex_named_like v_b vunit2 g;
    val _ = Testing.assert "correct name (b)" (V.name_eq (vn,v_b));
    val _ = assert_vertices
              [("a",(vunit1,([],[]))),("b",(vunit2,([],[])))]
              (G.get_vertices g');
    val _ = assert_edges [] (G.get_edges g');

    val (vn,g'') = G.add_vertex_named_like v_a vunit1 g';
    val _ = Testing.assert "correct name (!= a)" (not (V.name_eq (vn,v_a)));
    val _ = Testing.assert "correct name (!= b)" (not (V.name_eq (vn,v_b)));
    val _ = assert_vertices
              [("a",(vunit1,([],[]))),
               ("b",(vunit2,([],[]))),
               (V.string_of_name vn,(vunit1,([],[])))]
              (G.get_vertices g'');
    val _ = assert_edges [] (G.get_edges g'');
    in () end) ();




  val _ = Testing.test "G.add_edge_named_like" (fn () => let 
    val va = V.mk "a";
    val vb = V.mk "b";
    val ee = E.mk "e";
    val ef = E.mk "f";

    val _ = (G.add_edge_named_like ee (G.Directed,eunit1) va vb G.empty;
             raise ERROR "G.add_edge_named_like did not raise exception (1)")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (1)"
                  ((V.name_eq (vn,va)) orelse (V.name_eq (vn,vb)));
               Testing.assert "correct graph in exp (1)" (G.exact_eq G.empty g'));

    val g = G.empty |> add_vunit1 "a";

    val _ = (G.add_edge_named_like ee (G.Directed,eunit1) va vb g;
             raise ERROR "G.add_edge_named_like did not raise exception (2)")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (2)" (V.name_eq (vn,vb));
               Testing.assert "correct graph in exp (2)" (G.exact_eq g g'));
    val _ = (G.add_edge_named_like ee (G.Directed,eunit1) vb va g;
             raise ERROR "G.add_edge_named_like did not raise exception (3)")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (3)" (V.name_eq (vn,vb));
               Testing.assert "correct graph in exp (3)" (G.exact_eq g g'));
    val (en,g') = G.add_edge_named_like ee (G.Directed,eunit1) va va g;
    val _ = Testing.assert "correct name (1) (e)" (E.name_eq (en,ee));
    val _ = assert_edges [("e",((G.Directed,eunit1),("a","a")))] (G.get_edges g');
    val _ = assert_vertices [("a",(vunit1,(["e"],["e"])))] (G.get_vertices g');

    val g = g |> add_vunit2 "b";

    val (en,g') = G.add_edge_named_like ee (G.Directed,eunit1) va vb g;
    val _ = Testing.assert "correct name (2) (e)" (E.name_eq (en,ee));
    val _ = assert_edges
              [("e",((G.Directed,eunit1),("a","b")))]
              (G.get_edges g');
    val _ = assert_vertices
              [("a",(vunit1,([],["e"]))),
               ("b",(vunit2,(["e"],[])))]
              (G.get_vertices g');

    val (en,g'') = G.add_edge_named_like ef (G.UnDirected,eunit2) vb va g';
    val _ = Testing.assert "correct name (3) (f)" (E.name_eq (en,ef));
    val _ = assert_edges
              [("e",((G.Directed,eunit1),("a","b"))),
               ("f",((G.UnDirected,eunit2),("b","a")))]
              (G.get_edges g'');
    val _ = assert_vertices
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))]
              (G.get_vertices g'');

    val (en,g''') = G.add_edge_named_like ef (G.Directed,eunit2) vb vb g'';
    val _ = Testing.assert "correct name (4) (new)"
              (not (E.name_eq (en,ee)) andalso not (E.name_eq (en,ef)));
    val _ = assert_edges
              [("e",((G.Directed,eunit1),("a","b"))),
               ("f",((G.UnDirected,eunit2),("b","a"))),
               (E.string_of_name en,((G.Directed,eunit2),("b","b")))]
              (G.get_edges g''');
    val enstr = E.string_of_name en;
    val _ = assert_vertices
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e",enstr],["f",enstr])))]
              (G.get_vertices g''');
    in () end) ();




  val _ = Testing.test "G.delete_edge" (fn () => let 
    val _ = (G.delete_edge (E.mk "a") G.empty;
             raise ERROR "G.delete_edge did not raise exception (1)")
            handle G.no_such_ename_exp (_,en,g') =>
              (Testing.assert "correct ename in exp (1)"
                              (E.name_eq (en,E.mk "a"));
               Testing.assert "correct graph in exp (1)"
                              (G.exact_eq G.empty g'));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = (G.delete_edge (E.mk "a") g;
             raise ERROR "G.delete_edge did not raise exception (2)")
            handle G.no_such_ename_exp (_,en,g') =>
              (Testing.assert "correct ename in exp (2)"
                              (E.name_eq (en,E.mk "a"));
               Testing.assert "correct graph in exp (2)"
                              (G.exact_eq g g'));

    val g' = G.delete_edge (E.mk "e") g;
    val _ = assert_vertices
              [("a",(vunit1,([],[]))),
               ("b",(vunit2,(["f"],["f"])))]
              (G.get_vertices g');
    val _ = assert_edges
              [("f",((G.Directed,eunit2),("b","b")))]
              (G.get_edges g');
    in () end) ();




  val _ = Testing.test "G.delete_vertex" (fn () => let 
    val _ = (G.delete_vertex (V.mk "a") G.empty;
             raise ERROR "G.delete_vertex did not raise exception (1)")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (1)"
                              (V.name_eq (vn,V.mk "a"));
               Testing.assert "correct graph in exp (1)"
                              (G.exact_eq G.empty g'));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = (G.delete_vertex (V.mk "e") g;
             raise ERROR "G.delete_vertex did not raise exception (2)")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (2)"
                              (V.name_eq (vn,V.mk "e"));
               Testing.assert "correct graph in exp (2)"
                              (G.exact_eq g g'));

    val g' = G.delete_vertex (V.mk "a") g;
    val _ = assert_vertices [("b",(vunit2,(["f"],["f"])))]
              (G.get_vertices g');
    val _ = assert_edges [("f",((G.Directed,eunit2),("b","b")))]
              (G.get_edges g');

    val g' = G.delete_vertex (V.mk "b") g;
    val _ = assert_vertices [("a",(vunit1,([],[])))] (G.get_vertices g');
    val _ = assert_edges [] (G.get_edges g');

    val g' = G.delete_vertex (V.mk "a") g';
    val _ = assert_vertices [] (G.get_vertices g');
    val _ = assert_edges [] (G.get_edges g');
    in () end) ();




  val _ = Testing.test "G.rename_xname" (fn () => let 
    val _ = Testing.assert "G.rename_xname makes no changes"
              (G.is_empty (G.rename_xname (X.mk "a") (X.mk "b") G.empty));

    val g = G.empty |> add_vexpr1 "a" "2\\pi + w"
                    |> add_vexpr1 "b" "2/3*x"
                    |> add_vexpr2 "c" "3w - v"
                    |> add_dir_eexpr1 "e" "a" "b" "v + y"
                    |> add_undir_eexpr2 "f" "b" "c" "y + z";

    val g' = G.rename_xname (X.mk "x") (X.mk "u") g;
    val _ = assert_xnames ["v","w","u","y","z"] (G.get_xnames g');
    val _ = assert_vertices
              [("a",(vexpr1 "2\\pi + w",([],["e"]))),
               ("b",(vexpr1 "2/3*u",(["e"],["f"]))),
               ("c",(vexpr2 "3w - v",(["f"],[])))]
              (G.get_vertices g');
    val _ = assert_edges
              [("e",((G.Directed,eexpr1 "v + y"),("a","b"))),
               ("f",((G.UnDirected,eexpr2 "y + z"),("b","c")))]
              (G.get_edges g');

    val g' = G.rename_xname (X.mk "v") (X.mk "u") g;
    val _ = assert_xnames ["u","w","x","y","z"] (G.get_xnames g');
    val _ = assert_vertices
              [("a",(vexpr1 "2\\pi + w",([],["e"]))),
               ("b",(vexpr1 "2/3*x",(["e"],["f"]))),
               ("c",(vexpr2 "3w - u",(["f"],[])))]
              (G.get_vertices g');
    val _ = assert_edges
              [("e",((G.Directed,eexpr1 "u + y"),("a","b"))),
               ("f",((G.UnDirected,eexpr2 "y + z"),("b","c")))]
              (G.get_edges g');
    in () end) ();




  val _ = Testing.test "G.rename_vname" (fn () => let 
    val _ = Testing.assert "G.rename_vname makes no changes (1)"
              (G.is_empty (G.rename_vname (V.mk "a") (V.mk "b") G.empty));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = Testing.assert "G.rename_vname makes no changes (2)"
              (G.exact_eq g (G.rename_vname (V.mk "c") (V.mk "d") g));
    val _ = (G.rename_vname (V.mk "a") (V.mk "b") g;
            raise ERROR "G.rename_vname did not raise exception")
            handle G.vname_already_exists_exp (vn,gexp) =>
              (Testing.assert "correct vname in exp"
                              (V.name_eq (vn,V.mk "b"));
               Testing.assert "correct graph in exp"
                              (G.exact_eq g gexp));

    val g' = G.rename_vname (V.mk "a") (V.mk "c") g;
    val _ = assert_vertices
              [("c",(vunit1,([],["e"]))),
               ("b",(vunit2,(["e","f"],["f"])))]
              (G.get_vertices g');
    val _ = assert_edges
              [("e",((G.UnDirected,eunit1),("c","b"))),
               ("f",((G.Directed,eunit2),("b","b")))]
              (G.get_edges g');

    val g'' = G.rename_vname (V.mk "b") (V.mk "a") g';
    val _ = assert_vertices
              [("c",(vunit1,([],["e"]))),
               ("a",(vunit2,(["e","f"],["f"])))]
              (G.get_vertices g'');
    val _ = assert_edges
              [("e",((G.UnDirected,eunit1),("c","a"))),
               ("f",((G.Directed,eunit2),("a","a")))]
              (G.get_edges g'');

    val g = G.empty |> add_vexpr1 "a" "2\\pi + w"
                    |> add_vexpr1 "b" "2/3*x"
                    |> add_vexpr2 "c" "3w - v"
                    |> add_dir_eexpr1 "e" "a" "b" "v + y"
                    |> add_undir_eexpr2 "f" "b" "c" "y + z";
    val g' = G.rename_vname (V.mk "b") (V.mk "d") g;
    val _ = assert_xnames ["v","w","x","y","z"] (G.get_xnames g');
    val _ = assert_vertices
              [("a",(vexpr1 "2\\pi + w",([],["e"]))),
               ("d",(vexpr1 "2/3*x",(["e"],["f"]))),
               ("c",(vexpr2 "3w - v",(["f"],[])))]
              (G.get_vertices g');
    val _ = assert_edges
              [("e",((G.Directed,eexpr1 "v + y"),("a","d"))),
               ("f",((G.UnDirected,eexpr2 "y + z"),("d","c")))]
              (G.get_edges g');
    fun get_deps g = (G.Param.dep_vnames_of_xname (G.get_param g)) o X.mk;
    val _ = assert_vnames ["c"] (get_deps g' "v");
    val _ = assert_vnames ["a","c"] (get_deps g' "w");
    val _ = assert_vnames ["d"] (get_deps g' "x");
    val _ = assert_vnames [] (get_deps g' "y");
    val _ = assert_vnames [] (get_deps g' "z");
    in () end) ();




  val _ = Testing.test "G.rename_ename" (fn () => let 
    val _ = Testing.assert "G.rename_ename makes no changes"
              (G.is_empty (G.rename_ename (E.mk "a") (E.mk "b") G.empty));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = Testing.assert "G.rename_ename makes no changes (2)"
              (G.exact_eq g (G.rename_ename (E.mk "c") (E.mk "d") g));
    val _ = (G.rename_ename (E.mk "f") (E.mk "e") g;
            raise ERROR "G.rename_ename did not raise exception")
            handle G.ename_already_exists_exp (en,g') =>
              (Testing.assert "correct vname in exp"
                              (E.name_eq (en,E.mk "e"));
               Testing.assert "correct graph in exp"
                              (G.exact_eq g g'));

    val g' = G.rename_ename (E.mk "f") (E.mk "d") g;
    val _ = assert_vertices
              [("a",(vunit1,([],["e"]))),
               ("b",(vunit2,(["e","d"],["d"])))]
              (G.get_vertices g');
    val _ = assert_edges
              [("e",((G.UnDirected,eunit1),("a","b"))),
               ("d",((G.Directed,eunit2),("b","b")))]
              (G.get_edges g');

    val g'' = G.rename_ename (E.mk "e") (E.mk "f") g';
    val _ = assert_vertices
              [("a",(vunit1,([],["f"]))),
               ("b",(vunit2,(["f","d"],["d"])))]
              (G.get_vertices g'');
    val _ = assert_edges
              [("f",((G.UnDirected,eunit1),("a","b"))),
               ("d",((G.Directed,eunit2),("b","b")))]
              (G.get_edges g'');

    val g = G.empty |> add_vexpr1 "a" "2\\pi + w"
                    |> add_vexpr1 "b" "2/3*x"
                    |> add_vexpr2 "c" "3w - v"
                    |> add_dir_eexpr1 "e" "a" "b" "v + y"
                    |> add_undir_eexpr2 "f" "b" "c" "y + z";
    val g' = G.rename_ename (E.mk "e") (E.mk "d") g;
    val _ = assert_xnames ["v","w","x","y","z"] (G.get_xnames g');
    val _ = assert_vertices
              [("a",(vexpr1 "2\\pi + w",([],["d"]))),
               ("b",(vexpr1 "2/3*x",(["d"],["f"]))),
               ("c",(vexpr2 "3w - v",(["f"],[])))]
              (G.get_vertices g');
    val _ = assert_edges
              [("d",((G.Directed,eexpr1 "v + y"),("a","b"))),
               ("f",((G.UnDirected,eexpr2 "y + z"),("b","c")))]
              (G.get_edges g');
    fun get_deps g = (G.Param.dep_enames_of_xname (G.get_param g)) o X.mk;
    val _ = assert_enames ["d"] (get_deps g' "v");
    val _ = assert_enames [] (get_deps g' "w");
    val _ = assert_enames [] (get_deps g' "x");
    val _ = assert_enames ["d","f"] (get_deps g' "y");
    val _ = assert_enames ["f"] (get_deps g' "z");
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
      if G.VData.data_eq (vd,vert Data.VUnit1)
      then vert Data.VUnit2
      else
        if G.VData.data_eq (vd,vert Data.VUnit2)
        then vert Data.VUnit1
        else vd;

    val g' = G.update_vertex_data swap_vd (V.mk "a") g;
    val _ = assert_vertices
              [("a",(vunit2,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))]
              (G.get_vertices g');
    val _ = assert_edges
              [("e",((G.Directed,eunit1),("a","b"))),
               ("f",((G.UnDirected,eunit2),("b","a")))]
              (G.get_edges g');

    val g'' = G.update_vertex_data swap_vd (V.mk "b") g';
    val _ = assert_vertices
              [("a",(vunit2,(["f"],["e"]))),
               ("b",(vunit1,(["e"],["f"])))]
              (G.get_vertices g'');
    val _ = assert_edges
              [("e",((G.Directed,eunit1),("a","b"))),
               ("f",((G.UnDirected,eunit2),("b","a")))]
              (G.get_edges g'');
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
    val _ = assert_vertices
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))]
              (G.get_vertices g');
    val _ = assert_edges
              [("e",((G.Directed,eunit2),("a","b"))),
               ("f",((G.UnDirected,eunit2),("b","a")))]
              (G.get_edges g');

    val g'' = G.update_edge_data swap_ed (E.mk "f") g';
    val _ = assert_vertices
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))]
              (G.get_vertices g'');
    val _ = assert_edges
              [("e",((G.Directed,eunit2),("a","b"))),
               ("f",((G.UnDirected,eunit1),("b","a")))]
              (G.get_edges g'');
    in () end) ();





  (*
   * Tests for GRAPH
   *)

  val _ = Testing.test "G.is_empty" (fn () => let 
    val _ = Testing.assert "empty graph is empty" (G.is_empty G.empty);
    val g = G.empty |> add_vunit1 "a";
    val _ = Testing.assert "non-empty graph is not empty" (not (G.is_empty g));
    in () end) ();



  val _ = Testing.test "G.lookup_vertex" (fn () => let 
    val _ = assert_none "lookup_vertex" (G.lookup_vertex G.empty (V.mk "a"));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ =
    case G.lookup_vertex g (V.mk "a")
      of NONE => raise ERROR "Got NONE for vertex a"
       | SOME (vd,(ins,outs)) =>
           (Testing.assert "get_vertex correct data"
                           (G.VData.data_eq (vd,vunit1));
            assert_enames [] ins;
            assert_enames ["e"] outs);
 
    val _ =
    case G.lookup_vertex g (V.mk "b")
      of NONE => raise ERROR "Got NONE for vertex b"
       | SOME (vd,(ins,outs)) =>
           (Testing.assert "get_vertex correct data"
                           (G.VData.data_eq (vd,vunit2));
            assert_enames ["e","f"] ins;
            assert_enames ["f"] outs);

    val _ = assert_none "lookup_vertex" (G.lookup_vertex g (V.mk "e"));
    in () end) ();



  val _ = Testing.test "G.lookup_edge" (fn () => let 
    val _ = assert_none "lookup_edge" (G.lookup_edge G.empty (E.mk "a"));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ =
    case G.lookup_edge g (E.mk "e")
      of NONE => raise ERROR "Got NONE for edge e"
       | SOME ((dir,ed),(src,tgt)) =>
           (Testing.assert "get_edge correct data" (G.EData.data_eq (ed,eunit1));
            Testing.assert "get_edge correct direction"
                      (case dir of G.Directed => false | G.UnDirected => true);
            Testing.assert "get_edge correct src" (V.name_eq (src,V.mk "a"));
            Testing.assert "get_edge correct tgt" (V.name_eq (tgt,V.mk "b")));

    val _ =
    case G.lookup_edge g (E.mk "f")
      of NONE => raise ERROR "Got NONE for edge f"
       | SOME ((dir,ed),(src,tgt)) =>
           (Testing.assert "get_edge correct data" (G.EData.data_eq (ed,eunit2));
            Testing.assert "get_edge correct direction"
                      (case dir of G.Directed => true | G.UnDirected => false);
            Testing.assert "get_edge correct src" (V.name_eq (src,V.mk "b"));
            Testing.assert "get_edge correct tgt" (V.name_eq (tgt,V.mk "b")));

    val _ = assert_none "lookup_edge" (G.lookup_edge g (E.mk "a"));
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
            (V.name_eq (G.edge_get_other_vertex g (E.mk "e") (V.mk "a"),V.mk "b"));
    val _ = Testing.assert "source of e is b"
            (V.name_eq (G.edge_get_other_vertex g (E.mk "e") (V.mk "b"),V.mk "a"));
    val _ = Testing.assert "target of f is b"
            (V.name_eq (G.edge_get_other_vertex g (E.mk "f") (V.mk "b"),V.mk "b"));
    val _ = (G.edge_get_other_vertex g (E.mk "a") (V.mk "a");
              raise ERROR "G.edge_get_other_vertex did not raise exception")
            handle G.no_such_ename_exp (_,en,g') =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    val _ = (G.edge_get_other_vertex g (E.mk "e") (V.mk "c");
              raise ERROR "G.edge_get_other_vertex did not raise exception")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp" (V.name_eq (vn,V.mk "c"));
               Testing.assert "correct graph in exp" (G.exact_eq g g'));
    val _ = (G.edge_get_other_vertex g (E.mk "f") (V.mk "a");
              raise ERROR "G.edge_get_other_vertex did not raise exception")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp" (V.name_eq (vn,V.mk "a"));
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
            (G.VData.data_eq (G.get_vertex_data g (V.mk "a"),vunit1));
    val _ = Testing.assert "data of 'b' correct"
            (G.VData.data_eq (G.get_vertex_data g (V.mk "b"),vunit2));

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
            (G.EData.data_eq (G.get_edge_data g (E.mk "e"),eunit1));
    val _ = Testing.assert "data of 'f' correct"
            (G.EData.data_eq (G.get_edge_data g (E.mk "f"),eunit2));

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
    val _ = assert_vertices
              [("a",(vunit2,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))]
              (G.get_vertices g');
    val _ = assert_edges
              [("e",((G.Directed,eunit1),("a","b"))),
               ("f",((G.UnDirected,eunit2),("b","a")))]
              (G.get_edges g');

    val g'' = G.set_vertex_data vunit2 (V.mk "b") g';
    val _ = assert_vertices
              [("a",(vunit2,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))]
              (G.get_vertices g'');
    val _ = assert_edges
              [("e",((G.Directed,eunit1),("a","b"))),
               ("f",((G.UnDirected,eunit2),("b","a")))]
              (G.get_edges g'');
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
    val _ = assert_vertices
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))]
              (G.get_vertices g');
    val _ = assert_edges
              [("e",((G.Directed,eunit2),("a","b"))),
               ("f",((G.UnDirected,eunit2),("b","a")))]
              (G.get_edges g');

    val g'' = G.set_edge_data eunit2 (E.mk "f") g';
    val _ = assert_vertices
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))]
              (G.get_vertices g'');
    val _ = assert_edges
              [("e",((G.Directed,eunit2),("a","b"))),
               ("f",((G.UnDirected,eunit2),("b","a")))]
              (G.get_edges g'');
    in () end) ();



  val _ = Testing.test "G.apply_data_subst" (fn () => let 
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



  val _ = Testing.test "G.add_vertex" (fn () => let 
    val (vn,g) = G.add_vertex vunit1 G.empty;
    val _ = assert_vertices [(V.string_of_name vn,(vunit1,([],[])))]
                            (G.get_vertices g);
    val _ = assert_edges [] (G.get_edges g);

    val (vn',g') = G.add_vertex vunit2 g;
    val _ = Testing.assert "new name" (not (V.name_eq (vn,vn')));
    val _ = assert_vertices [(V.string_of_name vn,(vunit1,([],[]))),
                             (V.string_of_name vn',(vunit2,([],[])))]
                            (G.get_vertices g');
    val _ = assert_edges [] (G.get_edges g');
    in () end) ();

  (* doadd_vertex is difficult to test... *)




  val _ = Testing.test "G.add_named_vertex" (fn () => let 
    val v_a = V.mk "a";
    val v_b = V.mk "b";

    val g = G.add_named_vertex v_a vunit1 G.empty;
    val _ = assert_vertices [("a",(vunit1,([],[])))] (G.get_vertices g);
    val _ = assert_edges [] (G.get_edges g);

    val g' = G.add_named_vertex v_b vunit2 g;
    val _ = assert_vertices [("a",(vunit1,([],[]))),
                             ("b",(vunit2,([],[])))]
                            (G.get_vertices g');
    val _ = assert_edges [] (G.get_edges g');

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

    val _ = (G.add_edge (G.Directed,eunit1) va vb G.empty;
             raise ERROR "G.add_edge did not raise exception (1)")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (1)"
                  ((V.name_eq (vn,va)) orelse (V.name_eq (vn,vb)));
               Testing.assert "correct graph in exp (1)" (G.exact_eq G.empty g'));

    val g = G.empty |> add_vunit1 "a";

    val _ = (G.add_edge (G.Directed,eunit1) va vb g;
             raise ERROR "G.add_edge did not raise exception (2)")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (2)" (V.name_eq (vn,vb));
               Testing.assert "correct graph in exp (2)" (G.exact_eq g g'));
    val _ = (G.add_edge (G.Directed,eunit1) vb va g;
             raise ERROR "G.add_edge did not raise exception (3)")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (3)" (V.name_eq (vn,vb));
               Testing.assert "correct graph in exp (3)" (G.exact_eq g g'));
    val (en,g') = G.add_edge (G.Directed,eunit1) va va g;
    val enstr = E.string_of_name en;
    val _ = assert_edges [(enstr,((G.Directed,eunit1),("a","a")))]
                         (G.get_edges g');
    val _ = assert_vertices [("a",(vunit1,([enstr],[enstr])))]
                            (G.get_vertices g');

    val g = g |> add_vunit2 "b";

    val (en,g') = G.add_edge (G.Directed,eunit1) va vb g;
    val enstr = E.string_of_name en;
    val _ = assert_edges
              [(enstr,((G.Directed,eunit1),("a","b")))]
              (G.get_edges g');
    val _ = assert_vertices
              [("a",(vunit1,([],[enstr]))),
               ("b",(vunit2,([enstr],[])))]
              (G.get_vertices g');

    val (en',g'') = G.add_edge (G.UnDirected,eunit2) vb va g';
    val enstr' = E.string_of_name en';
    val _ = assert_edges
              [(enstr,((G.Directed,eunit1),("a","b"))),
               (enstr',((G.UnDirected,eunit2),("b","a")))]
              (G.get_edges g'');
    val _ = assert_vertices
              [("a",(vunit1,([enstr'],[enstr]))),
               ("b",(vunit2,([enstr],[enstr'])))]
              (G.get_vertices g'');
    in () end) ();



  val _ = Testing.test "G.add_named_edge" (fn () => let 
    val va = V.mk "a";
    val vb = V.mk "b";
    val ee = E.mk "e";
    val ef = E.mk "f";

    val _ = (G.add_named_edge ee (G.Directed,eunit1) va vb G.empty;
             raise ERROR "G.add_named_edge did not raise exception (1)")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (1)"
                  ((V.name_eq (vn,va)) orelse (V.name_eq (vn,vb)));
               Testing.assert "correct graph in exp (1)" (G.exact_eq G.empty g'));

    val g = G.empty |> add_vunit1 "a";

    val _ = (G.add_named_edge ee (G.Directed,eunit1) va vb g;
             raise ERROR "G.add_named_edge did not raise exception (2)")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (2)" (V.name_eq (vn,vb));
               Testing.assert "correct graph in exp (2)" (G.exact_eq g g'));
    val _ = (G.add_named_edge ee (G.Directed,eunit1) vb va g;
             raise ERROR "G.add_named_edge did not raise exception (3)")
            handle G.no_such_vname_exp (_,vn,g') =>
              (Testing.assert "correct vname in exp (3)" (V.name_eq (vn,vb));
               Testing.assert "correct graph in exp (3)" (G.exact_eq g g'));
    val g' = G.add_named_edge ee (G.Directed,eunit1) va va g;
    val _ = assert_edges [("e",((G.Directed,eunit1),("a","a")))] (G.get_edges g');
    val _ = assert_vertices [("a",(vunit1,(["e"],["e"])))] (G.get_vertices g');

    val g = g |> add_vunit2 "b";

    val g' = G.add_named_edge ee (G.Directed,eunit1) va vb g;
    val _ = assert_edges
              [("e",((G.Directed,eunit1),("a","b")))]
              (G.get_edges g');
    val _ = assert_vertices
              [("a",(vunit1,([],["e"]))),
               ("b",(vunit2,(["e"],[])))]
              (G.get_vertices g');

    val g'' = G.add_named_edge ef (G.UnDirected,eunit2) vb va g';
    val _ = assert_edges
              [("e",((G.Directed,eunit1),("a","b"))),
               ("f",((G.UnDirected,eunit2),("b","a")))]
              (G.get_edges g'');
    val _ = assert_vertices
              [("a",(vunit1,(["f"],["e"]))),
               ("b",(vunit2,(["e"],["f"])))]
              (G.get_vertices g'');

    val _ = (G.add_named_edge ee (G.Directed,eunit1) va vb g'';
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
    val _ = assert_vertices
              [("a",(vunit1,(["e","f"],[]))),
               ("b",(vunit2,([],["e","f"])))]
              (G.get_vertices g');
    val _ = assert_edges
              [("e",((G.Directed,eunit1),("b","a"))),
               ("f",((G.UnDirected,eunit2),("b","a")))]
              (G.get_edges g');

    val g' = G.swap_edge_dir (E.mk "f") g;
    val _ = assert_vertices
              [("a",(vunit1,([],["e","f"]))),
               ("b",(vunit2,(["e","f"],[])))]
              (G.get_vertices g');
    val _ = assert_edges
              [("e",((G.Directed,eunit1),("a","b"))),
               ("f",((G.UnDirected,eunit2),("a","b")))]
              (G.get_edges g');
    in () end) ();



  val _ = Testing.test "G.get_vnames" (fn () => let 
    val _ = Testing.assert "empty" (V.NSet.is_empty (G.get_vnames G.empty));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = assert_vnames ["a","b"] (G.get_vnames g);
  in () end) ();



  val _ = Testing.test "G.get_enames" (fn () => let 
    val _ = Testing.assert "empty" (E.NSet.is_empty (G.get_enames G.empty));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = assert_enames ["e","f"] (G.get_enames g);
  in () end) ();



  val _ = Testing.test "G.has_vname" (fn () => let 
    val _ = Testing.assert "empty" (not (G.has_vname G.empty (V.mk "a")));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = Testing.assert "a" (G.has_vname g (V.mk "a"));
    val _ = Testing.assert "b" (G.has_vname g (V.mk "b"));
    val _ = Testing.assert "e" (not (G.has_vname g (V.mk "e")));
  in () end) ();



  val _ = Testing.test "G.has_ename" (fn () => let 
    val _ = Testing.assert "empty" (not (G.has_ename G.empty (E.mk "a")));

    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";

    val _ = Testing.assert "e" (G.has_ename g (E.mk "e"));
    val _ = Testing.assert "f" (G.has_ename g (E.mk "f"));
    val _ = Testing.assert "a" (not (G.has_ename g (E.mk "a")));
  in () end) ();




  val _ = Testing.test "G.try_rename1_xname" (fn () => let 
    val _ = Testing.assert "G.try_rename1_xname makes no changes"
              (G.is_empty (the (G.try_rename1_xname (X.mk "a") (X.mk "b")
                                                    G.empty)));

    val g = G.empty |> add_vexpr1 "a" "2\\pi + w"
                    |> add_vexpr1 "b" "2/3*x"
                    |> add_vexpr2 "c" "3w - v"
                    |> add_dir_eexpr1 "e" "a" "b" "v + y"
                    |> add_undir_eexpr2 "f" "b" "c" "y + z";

    val _ =
    case G.try_rename1_xname (X.mk "x") (X.mk "u") g
      of NONE => raise ERROR "rename x to u failed"
       | SOME g' =>
           (assert_xnames ["v","w","u","y","z"] (G.get_xnames g');
            assert_vertices
                      [("a",(vexpr1 "2\\pi + w",([],["e"]))),
                       ("b",(vexpr1 "2/3*u",(["e"],["f"]))),
                       ("c",(vexpr2 "3w - v",(["f"],[])))]
                      (G.get_vertices g');
            assert_edges
                      [("e",((G.Directed,eexpr1 "v + y"),("a","b"))),
                       ("f",((G.UnDirected,eexpr2 "y + z"),("b","c")))]
                      (G.get_edges g'));

    val _ =
    case G.try_rename1_xname (X.mk "v") (X.mk "u") g
      of NONE => raise ERROR "rename x to u failed"
       | SOME g' =>
           (assert_xnames ["u","w","x","y","z"] (G.get_xnames g');
            assert_vertices
                      [("a",(vexpr1 "2\\pi + w",([],["e"]))),
                       ("b",(vexpr1 "2/3*x",(["e"],["f"]))),
                       ("c",(vexpr2 "3w - u",(["f"],[])))]
                      (G.get_vertices g');
            assert_edges
                      [("e",((G.Directed,eexpr1 "u + y"),("a","b"))),
                       ("f",((G.UnDirected,eexpr2 "y + z"),("b","c")))]
                      (G.get_edges g'));
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
           (assert_vertices
                      [("c",(vunit1,([],["e"]))),
                       ("b",(vunit2,(["e","f"],["f"])))]
                      (G.get_vertices g');
            assert_edges
                      [("e",((G.UnDirected,eunit1),("c","b"))),
                       ("f",((G.Directed,eunit2),("b","b")))]
                      (G.get_edges g');
            g');

    val _ =
    case G.try_rename1_vname (V.mk "b") (V.mk "a") g'
      of NONE => raise ERROR "rename b to a failed"
       | SOME g'' =>
           (assert_vertices
                      [("c",(vunit1,([],["e"]))),
                       ("a",(vunit2,(["e","f"],["f"])))]
                      (G.get_vertices g'');
            assert_edges
                      [("e",((G.UnDirected,eunit1),("c","a"))),
                       ("f",((G.Directed,eunit2),("a","a")))]
                      (G.get_edges g''));

    val g = G.empty |> add_vexpr1 "a" "2\\pi + w"
                    |> add_vexpr1 "b" "2/3*x"
                    |> add_vexpr2 "c" "3w - v"
                    |> add_dir_eexpr1 "e" "a" "b" "v + y"
                    |> add_undir_eexpr2 "f" "b" "c" "y + z";
    val g' =
    case G.try_rename1_vname (V.mk "b") (V.mk "d") g
      of NONE => raise ERROR "rename b to d failed"
       | SOME g' => g';
    val _ = assert_xnames ["v","w","x","y","z"] (G.get_xnames g');
    val _ = assert_vertices
              [("a",(vexpr1 "2\\pi + w",([],["e"]))),
               ("d",(vexpr1 "2/3*x",(["e"],["f"]))),
               ("c",(vexpr2 "3w - v",(["f"],[])))]
              (G.get_vertices g');
    val _ = assert_edges
              [("e",((G.Directed,eexpr1 "v + y"),("a","d"))),
               ("f",((G.UnDirected,eexpr2 "y + z"),("d","c")))]
              (G.get_edges g');
    fun get_deps g = (G.Param.dep_vnames_of_xname (G.get_param g)) o X.mk;
    val _ = assert_vnames ["c"] (get_deps g' "v");
    val _ = assert_vnames ["a","c"] (get_deps g' "w");
    val _ = assert_vnames ["d"] (get_deps g' "x");
    val _ = assert_vnames [] (get_deps g' "y");
    val _ = assert_vnames [] (get_deps g' "z");
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
    val _ = assert_vertices
              [("a",(vunit1,([],["e"]))),
               ("b",(vunit2,(["e","d"],["d"])))]
              (G.get_vertices g');
    val _ = assert_edges
              [("e",((G.UnDirected,eunit1),("a","b"))),
               ("d",((G.Directed,eunit2),("b","b")))]
              (G.get_edges g');

    val g'' =
    case G.try_rename1_ename (E.mk "e") (E.mk "f") g'
      of NONE => raise ERROR "rename e to f failed"
       | SOME g' => g';
    val _ = assert_vertices
              [("a",(vunit1,([],["f"]))),
               ("b",(vunit2,(["f","d"],["d"])))]
              (G.get_vertices g'');
    val _ = assert_edges
              [("f",((G.UnDirected,eunit1),("a","b"))),
               ("d",((G.Directed,eunit2),("b","b")))]
              (G.get_edges g'');

    val g = G.empty |> add_vexpr1 "a" "2\\pi + w"
                    |> add_vexpr1 "b" "2/3*x"
                    |> add_vexpr2 "c" "3w - v"
                    |> add_dir_eexpr1 "e" "a" "b" "v + y"
                    |> add_undir_eexpr2 "f" "b" "c" "y + z";
    val g' =
    case G.try_rename1_ename (E.mk "e") (E.mk "d") g
      of NONE => raise ERROR "rename e to d failed"
       | SOME g' => g';
    val _ = assert_xnames ["v","w","x","y","z"] (G.get_xnames g');
    val _ = assert_vertices
              [("a",(vexpr1 "2\\pi + w",([],["d"]))),
               ("b",(vexpr1 "2/3*x",(["d"],["f"]))),
               ("c",(vexpr2 "3w - v",(["f"],[])))]
              (G.get_vertices g');
    val _ = assert_edges
              [("d",((G.Directed,eexpr1 "v + y"),("a","b"))),
               ("f",((G.UnDirected,eexpr2 "y + z"),("b","c")))]
              (G.get_edges g');
    fun get_deps g = (G.Param.dep_enames_of_xname (G.get_param g)) o X.mk;
    val _ = assert_enames ["d"] (get_deps g' "v");
    val _ = assert_enames [] (get_deps g' "w");
    val _ = assert_enames [] (get_deps g' "x");
    val _ = assert_enames ["d","f"] (get_deps g' "y");
    val _ = assert_enames ["f"] (get_deps g' "z");
    in () end) ();




  val _ = Testing.test "G.CRnmX.rename" (fn () => let 
    val g = G.empty |> add_vexpr1 "a" "2\\pi + w"
                    |> add_vexpr1 "b" "2/3*x"
                    |> add_vexpr2 "c" "3w - v"
                    |> add_dir_eexpr1 "e" "a" "b" "v + y"
                    |> add_undir_eexpr2 "f" "b" "c" "y + z";

    val xnames = G.get_xnames g;
    val xrn = X.Rnm.mk_from_avoids xnames;
    val (xrn', g') = G.CRnmX.rename xrn g;
    val exp_xnames = X.NSet.fold (X.NSet.add o (X.Rnm.get xrn'))
                     xnames X.NSet.empty;
    val _ = Testing.assert "get_xnames changed"
            (X.NSet.eq exp_xnames (G.get_xnames g'));
    fun mapxn old = X.string_of_name (X.Rnm.get xrn' (X.mk old));
    val _ = assert_vertices
              [("a",(vexpr1 ("2\\pi + "^(mapxn "w")),([],["e"]))),
               ("b",(vexpr1 ("2/3*"^(mapxn "x")),(["e"],["f"]))),
               ("c",(vexpr2 ("3"^(mapxn "w")^" - "^(mapxn "v")),(["f"],[])))]
              (G.get_vertices g');
    val _ = assert_edges
              [("e",((G.Directed,eexpr1 ((mapxn "v")^"+"^(mapxn "y"))),("a","b"))),
               ("f",((G.UnDirected,eexpr2 ((mapxn "y")^"+"^(mapxn "z"))),("b","c")))]
              (G.get_edges g');
    in g end) ();

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
    structure G = Test_Graph
    val vert = I
    val unwrap_vert = I
  )
in val _ = () end;


