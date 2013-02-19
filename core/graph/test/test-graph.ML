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

  val _ = Testing.test "Empty graph test" (fn () => let 
    val _ = Testing.assert "is_empty" (G.is_empty G.empty);
    val _ = Testing.assert "get_xnames empty"
              (X.NSet.is_empty (G.get_xnames G.empty));
    val _ = (G.get_vertex G.empty (V.mk "a"); raise ERROR "G.get_vertex did not raise exception")
            handle G.no_such_vname_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));
    val _ = (G.get_edge G.empty (E.mk "a");
             raise ERROR "G.get_edge did not raise exception")
            handle G.no_such_ename_exp (_,en,g) =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));
    val _ = (G.delete_vertex (V.mk "a") G.empty;
             raise ERROR "G.delete_vertex did not raise exception")
            handle G.no_such_vname_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));
    val _ = (G.delete_edge (E.mk "a") G.empty;
             raise ERROR "G.delete_edge did not raise exception")
            handle G.no_such_ename_exp (_,en,g) =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));
    val _ = Testing.assert "G.rename_xname makes no changes"
              (G.is_empty (G.rename_xname (X.mk "a") (X.mk "b") G.empty));
    val _ = Testing.assert "G.rename_vname makes no changes"
              (G.is_empty (G.rename_vname (V.mk "a") (V.mk "b") G.empty));
    val _ = Testing.assert "G.rename_ename makes no changes"
              (G.is_empty (G.rename_ename (E.mk "a") (E.mk "b") G.empty));
    val _ = (G.update_vertex_data I (V.mk "a") G.empty;
              raise ERROR "G.get_update_vertex_data did not raise exception")
            handle G.no_such_vname_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));
    val _ = (G.update_edge_data I (E.mk "a") G.empty;
              raise ERROR "G.get_update_edge_data did not raise exception")
            handle G.no_such_ename_exp (_,en,g) =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));
    val _ = Testing.assert "verts empty" (V.NTab.is_name_empty (G.get_vertices G.empty));
    val _ = Testing.assert "edges empty" (E.NTab.is_name_empty (G.get_edges G.empty));

    val _ = assert_none "lookup_vertex" (G.lookup_vertex G.empty (V.mk "a"));
    val _ = assert_none "lookup_edge" (G.lookup_edge G.empty (E.mk "a"));
    val _ = (G.get_edge_source G.empty (E.mk "a");
              raise ERROR "G.get_edge_source did not raise exception")
            handle G.no_such_ename_exp (_,en,g) =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));
    val _ = (G.get_edge_target G.empty (E.mk "a");
              raise ERROR "G.get_edge_target did not raise exception")
            handle G.no_such_ename_exp (_,en,g) =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));
    val _ = (G.edge_get_other_vertex G.empty (E.mk "a") (V.mk "v");
              raise ERROR "G.edge_get_other_vertex did not raise exception")
            handle G.no_such_ename_exp (_,en,g) =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));
    val _ = (G.get_in_edges G.empty (V.mk "a");
              raise ERROR "G.get_in_edges did not raise exception")
            handle G.no_such_vname_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));
    val _ = (G.get_out_edges G.empty (V.mk "a");
              raise ERROR "G.get_out_edges did not raise exception")
            handle G.no_such_vname_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));
    val _ = (G.get_vertex_data G.empty (V.mk "a");
              raise ERROR "G.get_vertex_data did not raise exception")
            handle G.no_such_vname_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));
    val _ = (G.get_edge_data G.empty (E.mk "a");
              raise ERROR "G.get_edge_data did not raise exception")
            handle G.no_such_ename_exp (_,en,g) =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));
    val _ = (G.set_vertex_data vunit1 (V.mk "a") G.empty;
              raise ERROR "G.set_vertex_data did not raise exception")
            handle G.no_such_vname_exp (_,vn,g) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));
    val _ = (G.set_edge_data eunit1 (E.mk "a") G.empty;
              raise ERROR "G.set_edge_data did not raise exception")
            handle G.no_such_ename_exp (_,en,g) =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));
    val _ = Testing.assert "apply_data_subst no effect"
              (G.is_empty (G.apply_data_subst G.Param.empty_subst G.empty));
    val _ = Testing.assert "empty = empty" (G.exact_eq G.empty G.empty);
    val _ = (G.swap_edge_dir (E.mk "a") G.empty;
              raise ERROR "G.swap_edge_dir did not raise exception")
            handle G.no_such_ename_exp (_,en,g) =>
              (Testing.assert "correct ename in exp" ((E.string_of_name en) = "a");
               Testing.assert "correct graph in exp" (G.is_empty g));
    val _ = Testing.assert "vnames empty" (V.NSet.is_empty (G.get_vnames G.empty));
    val _ = Testing.assert "enames empty" (E.NSet.is_empty (G.get_enames G.empty));
    val _ = Testing.assert "no such vname" (not (G.has_vname G.empty (V.mk "a")));
    val _ = Testing.assert "no such ename" (not (G.has_ename G.empty (E.mk "a")));
    in () end) ();


  val _ = Testing.test "Basic graph test" (fn () => let 
    val g = G.empty |> add_vunit1 "a"
                    |> add_vunit2 "b"
                    |> add_undir_eunit1 "e" "a" "b"
                    |> add_dir_eunit2 "f" "b" "b";
    val _ = assert_vnames ["a","b"] (G.get_vnames g);
    val _ = assert_enames ["e","f"] (G.get_enames g);
    val _ = Testing.assert "get_xnames empty"
              (X.NSet.is_empty (G.get_xnames g));
    val (vd,(ins,outs)) = G.get_vertex g (V.mk "a")
    val _ = Testing.assert "get_vertex correct data"
              (G.VData.data_eq (vd,vunit1));
    val _ = assert_enames [] ins;
    val _ = assert_enames ["e"] outs;
    val (vd,(ins,outs)) = G.get_vertex g (V.mk "b")
    val _ = Testing.assert "get_vertex correct data" (G.VData.data_eq (vd,vunit2));
    val _ = assert_enames ["e","f"] ins;
    val _ = assert_enames ["f"] outs;
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
    val g' = G.delete_vertex (V.mk "a") g;
    val _ = assert_vnames ["b"] (G.get_vnames g');
    val g' = G.delete_vertex (V.mk "b") g;
    val _ = assert_vnames ["a"] (G.get_vnames g');
    val g' = G.delete_vertex (V.mk "a") g';
    val _ = assert_vnames [] (G.get_vnames g');
    val _ = (G.rename_vname (V.mk "a") (V.mk "b") g;
            raise ERROR "G.rename_vname did not raise exception")
            handle G.vname_already_exists_exp (vn,gexp) =>
              (Testing.assert "correct vname in exp" ((V.string_of_name vn) = "b");
               Testing.assert "correct graph in exp" (G.exact_eq g gexp));
    val g' = G.rename_vname (V.mk "a") (V.mk "c") g;
    val _ = assert_vnames ["b","c"] (G.get_vnames g');
    val _ = (G.rename_ename (E.mk "f") (E.mk "e") g;
            raise ERROR "G.rename_ename did not raise exception")
            handle G.ename_already_exists_exp (en,gexp) =>
              (Testing.assert "correct vname in exp" ((E.string_of_name en) = "e");
               Testing.assert "correct graph in exp" (G.exact_eq g gexp));
    val g' = G.rename_ename (E.mk "f") (E.mk "d") g;
    val _ = assert_enames ["d","e"] (G.get_enames g');
    val g' = G.update_vertex_data (K vunit2) (V.mk "a") g;
    val (vd,_) = G.get_vertex g' (V.mk "a")
    val _ = Testing.assert "update_vertex_data correct data" (G.VData.data_eq (vd,vunit2));
    val g' = G.update_edge_data (K eunit1) (E.mk "f") g;
    val ((_,ed),_) = G.get_edge g' (E.mk "f")
    val _ = Testing.assert "update_edge_data correct data" (G.EData.data_eq (ed,eunit1));
    in g end) ();

  val _ = Testing.test "Variable name management test" (fn () => let 
    val g = G.empty |> add_vexpr1 "a" "2\\pi + w"
                    |> add_vexpr1 "b" "2/3*x"
                    |> add_vexpr2 "c" "3w - v"
                    |> add_undir_eexpr1 "e" "a" "b" "v + y"
                    |> add_undir_eexpr2 "f" "b" "c" "y + z";
    val _ = assert_xnames ["v","w","x","y","z"] (G.get_xnames g);
    val _ = assert_vnames ["c"]
              (G.Param.dep_vnames_of_xname (G.get_param g) (X.mk "v"));
    val _ = assert_vnames ["a","c"]
              (G.Param.dep_vnames_of_xname (G.get_param g) (X.mk "w"));
    val _ = assert_vnames ["b"]
              (G.Param.dep_vnames_of_xname (G.get_param g) (X.mk "x"));
    val _ = assert_vnames []
              (G.Param.dep_vnames_of_xname (G.get_param g) (X.mk "y"));
    val _ = assert_enames ["e"]
              (G.Param.dep_enames_of_xname (G.get_param g) (X.mk "v"));
    val _ = assert_enames []
              (G.Param.dep_enames_of_xname (G.get_param g) (X.mk "w"));
    val _ = assert_enames ["e","f"]
              (G.Param.dep_enames_of_xname (G.get_param g) (X.mk "y"));
    val _ = assert_enames ["f"]
              (G.Param.dep_enames_of_xname (G.get_param g) (X.mk "z"));

    val g' = G.rename_vname (V.mk "b") (V.mk "d") g;
    val _ = assert_xnames ["v","w","x","y","z"] (G.get_xnames g');
    val _ = assert_vnames ["d"]
              (G.Param.dep_vnames_of_xname (G.get_param g') (X.mk "x"));

    val g' = G.rename_ename (E.mk "f") (E.mk "g") g;
    val _ = assert_xnames ["v","w","x","y","z"] (G.get_xnames g');
    val _ = assert_enames ["e","g"]
              (G.Param.dep_enames_of_xname (G.get_param g') (X.mk "y"));

    val g' = G.rename_xname (X.mk "x") (X.mk "u") g;
    val _ = assert_xnames ["v","w","u","y","z"] (G.get_xnames g');
    val _ = assert_vnames ["b"]
              (G.Param.dep_vnames_of_xname (G.get_param g') (X.mk "u"));
    val (vd,_) = G.get_vertex g' (V.mk "b")
    val _ = Testing.assert "rename_xname correct data (x -> u)"
              (G.VData.data_eq (vd,vexpr1 "2/3*u"));

    val g' = G.rename_xname (X.mk "v") (X.mk "u") g;
    val _ = assert_xnames ["u","w","x","y","z"] (G.get_xnames g');
    val _ = assert_vnames ["c"]
              (G.Param.dep_vnames_of_xname (G.get_param g') (X.mk "u"));
    val _ = assert_enames ["e"]
              (G.Param.dep_enames_of_xname (G.get_param g') (X.mk "u"));
    val (vd,_) = G.get_vertex g' (V.mk "c")
    val _ = Testing.assert "rename_xname correct data (v -> u)"
              (G.VData.data_eq (vd,vexpr2 "3w - u"));

    val xnames = G.get_xnames g;
    val xrn = X.Rnm.mk_from_avoids xnames;
    val (_, g') = G.CRnmX.rename xrn g;
    fun no_overlap msg xnset =
    let
      val overlap = X.NSet.intersect xnames xnset;
      val _ = Testing.assert msg (X.NSet.is_empty overlap);
    in () end;
    val _ = no_overlap "All names changed" (G.get_xnames g');

    val _ = V.NTab.fold (fn (vn,(vd,_)) => fn () =>
        let val strvn = V.string_of_name vn in
        case unwrap_vert vd
          of Data.VExpr1 a => no_overlap (strvn^" names changed") (Expr.get_vars a)
           | Data.VExpr2 a => no_overlap (strvn^" names changed") (Expr.get_vars a)
           | _ => raise ERROR ("Bad data for "^strvn)
        end
      ) (G.get_vertices g') ()
    val _ = E.NTab.fold (fn (en,((_,ed),_)) => fn () =>
        let val stren = E.string_of_name en in
        case ed
          of Data.EExpr1 a => no_overlap (stren^" names changed") (Expr.get_vars a)
           | Data.EExpr2 a => no_overlap (stren^" names changed") (Expr.get_vars a)
           | _ => raise ERROR ("Bad data for "^stren)
        end
      ) (G.get_edges g') ()

    in g end) ();

  val _ = Testing.assert_no_failed_tests();
end;

structure Graph_Interface_Tests_Basic = Graph_Interface_Tests(
  structure G = Test_Graph
  val vert = I
  val unwrap_vert = I
);


