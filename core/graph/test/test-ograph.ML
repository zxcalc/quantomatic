functor OGraph_Interface_Tests(
  G : TEST_OGRAPH
)
= struct
  structure Tools = Test_OGraph_Tools(structure G = G);

  (* start by testing that things in the GRAPH interface still work *)
  structure Graph_Interface_Tests_OGraph = Graph_Interface_Tests(
    structure G = Tools.G
    val vert = Tools.nv
    val unwrap_vert = Tools.unwrap_nv
  );

  open Tools;


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

    fun test_wire_for_edge exp_wire en =
    let
      fun check_wire () =
        if G.wire_eq exp_wire (G.get_wire_containing_edge g_raw (E.mk en))
        then ()
        else raise ERROR ("Incorrect wire returned for edge "^en)
    in
      Testing.test ("Wire for edge "^en) check_wire ()
    end

    val _ = test_wire_for_edge exp_wire_e1 "e1";
    val _ = test_wire_for_edge exp_wire_e2 "e2";
    val _ = test_wire_for_edge exp_wire_e3 "e3";
    val _ = test_wire_for_edge exp_wire_e4 "e4";
    val _ = test_wire_for_edge exp_wire_e5 "e5";
    val _ = test_wire_for_edge exp_wire_e6 "e6";
    val _ = test_wire_for_edge exp_wire_e7 "e7";
    val _ = test_wire_for_edge exp_wire_e8 "e8";
    val _ = Testing.test "Wire for edge ce1"
      (fn () =>
        if G.wire_eq exp_wire_ce1_a (G.get_wire_containing_edge g_raw (E.mk "ce1"))
        then ()
        else
          if G.wire_eq exp_wire_ce1_b (G.get_wire_containing_edge g_raw (E.mk "ce1"))
          then ()
          else raise ERROR "Incorrect wire returned for edge ce1"
      ) ();
    val _ = Testing.test "Wire for edge ce2"
      (fn () =>
        if G.wire_eq exp_wire_ce2_a (G.get_wire_containing_edge g_raw (E.mk "ce2"))
        then ()
        else
          if G.wire_eq exp_wire_ce2_b (G.get_wire_containing_edge g_raw (E.mk "ce2"))
          then ()
          else raise ERROR "Incorrect wire returned for edge ce2"
      ) ();
    val _ = test_wire_for_edge exp_wire_ce3 "ce3";
    val _ = test_wire_for_edge exp_wire_f1 "f1";
    val _ = test_wire_for_edge exp_wire_f2 "f2";
    val _ = test_wire_for_edge exp_wire_f3 "f3";
    val _ = test_wire_for_edge exp_wire_f4 "f4";
    val _ = test_wire_for_edge exp_wire_f5 "f5";
    val _ = test_wire_for_edge exp_wire_f6 "f6";


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
        end) ();



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

    (*val _ = Testing.test "G.minimise"
      (fn () =>
        let
          val g_min = G.minimise g_raw
        in
          if GIso.test g_min g_exp_min then ()
          else (writeln "Expected"; G.print g_exp_min;
                writeln "Got"; G.print g_min;
                raise ERROR "Minimise gave incorrect graph")
        end) ();*)

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

    (*val _ = Testing.test "G.normalise"
      (fn () =>
        let
          val g_norm = G.normalise g_raw
        in
          if GIso.test g_norm g_exp_norm then ()
          else (writeln "Expected"; G.print g_exp_norm;
                writeln "Got"; G.print g_norm;
                raise ERROR "Minimise gave incorrect graph")
        end) ();*)
  in val _ = Testing.assert_no_failed_tests() end;


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
    fun test_pluggable n1 n2 g =
      Testing.test ("G.are_pluggable ("^n1^","^n2^")")
        (fn () => Testing.assert (n1^" and "^n2^" are pluggable")
                    (G.are_pluggable g (V.mk n1) (V.mk n2))) ();
    fun test_not_pluggable n1 n2 g =
      Testing.test ("G.are_pluggable ("^n1^","^n2^")")
        (fn () => Testing.assert (n1^" and "^n2^" are not pluggable")
                    (not (G.are_pluggable g (V.mk n1) (V.mk n2)))) ();

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

    fun test_dir_plugging inv outv g =
      Testing.test ("G.get_plugging ("^inv^","^outv^")")
          (fn () =>
          case G.get_plugging g (V.mk inv) (V.mk outv)
            of SOME ((Directed,_),(v1,v2)) =>
                 Testing.assert "correct vertex names"
                    ((V.name_eq (v1,V.mk outv)) andalso
                     (V.name_eq (v2,V.mk inv)))
             | SOME _ => raise ERROR "Wrong directedness"
             | NONE   => raise ERROR "No plugging found") ();
    fun test_undir_plugging b1 b2 g =
      Testing.test ("G.get_plugging ("^b1^","^b2^")")
          (fn () =>
          case G.get_plugging g (V.mk b1) (V.mk b2)
            of SOME ((Undirected,_),(v1,v2)) =>
                 Testing.assert "correct vertex names"
                    (V.NSet.eq (V.NSet.of_list (map V.mk [b1,b2]))
                               (V.NSet.of_list [v1,v2]))
             | SOME _ => raise ERROR "Wrong directedness"
             | NONE   => raise ERROR "No plugging found") ();
    fun test_no_plugging b1 b2 g =
      Testing.test ("G.get_plugging ("^b1^","^b2^")")
          (fn () =>
          case G.get_plugging g (V.mk b1) (V.mk b2)
            of SOME _ => raise ERROR "Unexpected plugging found"
             | NONE   => ()) ();

    val _ = test_dir_plugging "in1" "out1" g
    val _ = test_dir_plugging "in2" "out2" g
    val _ = test_undir_plugging "bnd1" "bnd3" g
    val _ = test_undir_plugging "bnd4" "bnd2" g
    val _ = test_no_plugging "in1" "in2" g
    val _ = test_no_plugging "in1" "out2" g
    val _ = test_no_plugging "out1" "w1" g
    val _ = test_no_plugging "bnd1" "bnd2" g
    val _ = test_no_plugging "bnd1" "w1" g

    fun test_dir_plug inv outv g =
      Testing.test ("G.plug ("^inv^","^outv^")")
          (fn () =>
          let val (e,g') = G.plug (V.mk inv) (V.mk outv) g in
            case G.get_edge_info g' e
              of ((Directed,_),(v1,v2)) =>
                   Testing.assert "correct vertex names"
                      ((V.name_eq (v1,V.mk outv)) andalso
                       (V.name_eq (v2,V.mk inv)))
               | _ => raise ERROR "Wrong directedness"
          end) ();
    fun test_undir_plug b1 b2 g =
      Testing.test ("G.plug ("^b1^","^b2^")")
          (fn () =>
          let val (e,g') = G.plug (V.mk b1) (V.mk b2) g in
            case G.get_edge_info g' e
              of ((Undirected,_),(v1,v2)) =>
                   Testing.assert "correct vertex names"
                      (V.NSet.eq (V.NSet.of_list (map V.mk [b1,b2]))
                                 (V.NSet.of_list [v1,v2]))
               | _ => raise ERROR "Wrong directedness"
          end) ();

    val _ = test_dir_plug "in1" "out1" g;
    val _ = test_dir_plug "in2" "out2" g;
    val _ = test_undir_plug "bnd1" "bnd3" g;
    val _ = test_undir_plug "bnd4" "bnd2" g;

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
            ) ();
  in val _ = Testing.assert_no_failed_tests() end;


  local (* subgraphs *)
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

    val _ = Testing.test "get_open_subgraph" (fn () => let
            val g = G.get_open_subgraph full_graph retain
            val _ = Testing.assert "subgraph was as expected"
                      (G.exact_eq g exp_subgraph)
          in () end) ();
  in val _ = Testing.assert_no_failed_tests() end;

end;

local
  structure OG_Tests = OGraph_Interface_Tests(Test_OGraph);
in val _ = () end;

