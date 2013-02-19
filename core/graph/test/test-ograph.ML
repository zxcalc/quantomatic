functor OGraph_Interface_Tests(
  G : OGRAPH where type EData.data = Test_Graph_Data.edata
              and type OVData.IData.data = Test_Graph_Data.vdata
)
= struct
  structure Tools = Test_OGraph_Tools(G);

  (* start by testing that things in the GRAPH interface still work *)
  structure Graph_Interface_Tests_OGraph = Graph_Interface_Tests(
    structure G = Tools.G
    val vert = Tools.nv
    val unwrap_vert = Tools.unwrap_nv
  );

  open Tools;
end;

structure OGraph_Interface_Tests_Basic = OGraph_Interface_Tests(Test_OGraph);


(* Test wire functions *)
local
  open Test_RG_MkG
  val b1 = V.mk "b1"
  val b2 = V.mk "b2"
  val b3 = V.mk "b3"
  val b4 = V.mk "b4"
  val b5 = V.mk "b5"
  val b6 = V.mk "b6"
  val b7 = V.mk "b7"
  val b8 = V.mk "b8"
  val b9 = V.mk "b9"
  val b10 = V.mk "b10"
  val b11 = V.mk "b11"
  val b12 = V.mk "b12"
  val b13 = V.mk "b13"
  val c1 = V.mk "c1"
  val c2 = V.mk "c2"
  val c3 = V.mk "c3"
  val r1 = V.mk "r1"
  val r2 = V.mk "r2"
  val r3 = V.mk "r3"

  val g = G.empty
  val g = g |> G.add_named_vertex b1 bvert
            |> G.add_named_vertex b2 bvert
            |> G.add_named_vertex b3 bvert
            |> G.add_named_vertex b4 bvert
            |> G.add_named_vertex b5 bvert
            |> G.add_named_vertex b6 bvert
            |> G.add_named_vertex b7 bvert
            |> G.add_named_vertex b8 bvert
            |> G.add_named_vertex c1 bvert
            |> G.add_named_vertex c2 bvert
            |> G.add_named_vertex c3 bvert
            |> G.add_named_vertex r1 (mkX zero_angle)
            |> G.add_named_vertex r2 (mkX zero_angle)
            |> G.add_named_vertex r3 (mkX zero_angle)
  val (e1,g)  = g|> G.add_edge dir_edge r1 b1
  val (e2,g)  = g|> G.add_edge dir_edge b1 b2
  val (e3,g)  = g|> G.add_edge dir_edge b2 r2
  val (e4,g)  = g|> G.add_edge dir_edge r2 b3
  val (e5,g)  = g|> G.add_edge dir_edge b3 b4
  val (e6,g)  = g|> G.add_edge dir_edge r2 r3
  val (e7,g)  = g|> G.add_edge dir_edge r3 b5
  val (e8,g)  = g|> G.add_edge dir_edge b5 r3
  val (e9,g)  = g|> G.add_edge dir_edge c1 c2
  val (e10,g) = g|> G.add_edge dir_edge c2 c1
  val (e11,g) = g|> G.add_edge dir_edge c3 c3
  
  val (e12,g) = g|> G.add_edge undir_edge r2 b6
  val (e13,g) = g|> G.add_edge undir_edge b6 r3
  val (e14,g) = g|> G.add_edge undir_edge r2 b7
  val (e15,g) = g|> G.add_edge undir_edge r3 b7
  val (e16,g) = g|> G.add_edge undir_edge b8 r2
  val (e17,g) = g|> G.add_edge undir_edge b8 r3
  val g_raw = g

  val g = G.empty
  val g = g |> G.add_named_vertex b4 bvert
            |> G.add_named_vertex c1 bvert
            |> G.add_named_vertex c3 bvert
            |> G.add_named_vertex r1 (mkX zero_angle)
            |> G.add_named_vertex r2 (mkX zero_angle)
            |> G.add_named_vertex r3 (mkX zero_angle)
            |> G.doadd_edge dir_edge r1 r2
            |> G.doadd_edge dir_edge r2 b4
            |> G.doadd_edge dir_edge r2 r3
            |> G.doadd_edge dir_edge r3 r3
            |> G.doadd_edge dir_edge c1 c1
            |> G.doadd_edge dir_edge c3 c3
            |> G.doadd_edge undir_edge r2 r3
            |> G.doadd_edge undir_edge r2 r3
            |> G.doadd_edge undir_edge r2 r3
  val g_min = g

  val g = G.empty
  val g = g |> G.add_named_vertex b1 bvert
            |> G.add_named_vertex b2 bvert
            |> G.add_named_vertex b3 bvert
            |> G.add_named_vertex b4 bvert
            |> G.add_named_vertex b5 bvert
            |> G.add_named_vertex b6 bvert
            |> G.add_named_vertex b7 bvert
            |> G.add_named_vertex b8 bvert
            |> G.add_named_vertex b9 bvert
            |> G.add_named_vertex b10 bvert
            |> G.add_named_vertex b11 bvert
            |> G.add_named_vertex b12 bvert
            |> G.add_named_vertex b13 bvert
            |> G.add_named_vertex c1 bvert
            |> G.add_named_vertex c3 bvert
            |> G.add_named_vertex r1 (mkX zero_angle)
            |> G.add_named_vertex r2 (mkX zero_angle)
            |> G.add_named_vertex r3 (mkX zero_angle)
            |> G.doadd_edge dir_edge r1 b1
            |> G.doadd_edge dir_edge b1 b2
            |> G.doadd_edge dir_edge b2 r2
            |> G.doadd_edge dir_edge r2 b4
            |> G.doadd_edge dir_edge r2 b3
            |> G.doadd_edge dir_edge b3 b5
            |> G.doadd_edge dir_edge b5 r3
            |> G.doadd_edge dir_edge r3 b6
            |> G.doadd_edge dir_edge b6 b7
            |> G.doadd_edge dir_edge b7 r3
            |> G.doadd_edge dir_edge c1 c1
            |> G.doadd_edge dir_edge c3 c3
            |> G.doadd_edge undir_edge r2 b8
            |> G.doadd_edge undir_edge b8 b9
            |> G.doadd_edge undir_edge b9 r3
            |> G.doadd_edge undir_edge r2 b10
            |> G.doadd_edge undir_edge b10 b11
            |> G.doadd_edge undir_edge b11 r3
            |> G.doadd_edge undir_edge r2 b12
            |> G.doadd_edge undir_edge b12 b13
            |> G.doadd_edge undir_edge b13 r3
  val g_norm = g
  
  val wire1 = G.get_wire_containing_edge g_raw e2
  val wire2 = G.get_wire_containing_edge g_raw e5
  val wire3 = G.get_wire_containing_edge g_raw e6
  val wire4 = G.get_wire_containing_edge g_raw e7
  val wire5 = G.get_wire_containing_edge g_raw e9
  val wire5' = G.get_wire_containing_edge g_raw e10
  val wire6 = G.get_wire_containing_edge g_raw e11
  val wire7 = G.get_wire_containing_edge g_raw e12
  val wire8 = G.get_wire_containing_edge g_raw e14
  val wire8' = G.get_wire_containing_edge g_raw e15
  val wire9 = G.get_wire_containing_edge g_raw e16
  val wire9' = G.get_wire_containing_edge g_raw e17
  val _ = G.print g_raw
  
  val wires     = sort G.wire_ord [wire1,wire2,wire3,wire4,wire5,wire6,wire7,wire8,wire9]
  val wires'    = sort G.wire_ord (G.get_wire_list g_raw)
  
  val _ = (Pretty.writeln o Pretty.str) "wires: "
  val _ = map (Pretty.writeln o G.pretty_wire) wires
  val _ = (Pretty.writeln o Pretty.str) "wires': "
  val _ = map (Pretty.writeln o G.pretty_wire) wires'
  
  
  fun c_wire_lists (w::ws) (w'::ws') =
    if G.wire_eq w w' then c_wire_lists ws ws'
    else raise ERROR ("wire lists differ EXPECTED: "^Pretty.string_of (G.pretty_wire w)^
                      "  GOT: "^Pretty.string_of (G.pretty_wire w'))
    | c_wire_lists [] [] = ()
    | c_wire_lists _ _   = raise ERROR "failed, wire lists of different lengths"
  
  val _ = Testing.test "G.get_wire_containing_edge - interior wire"
  (fn () => if G.wire_eq (r1, r2, V.NSet.of_list [b1, b2], E.NSet.of_list [e1, e2, e3]) wire1 then I
            else raise ERROR "interior wire incorrect") ()
  val _ = Testing.test "G.get_wire_containing_edge - boundary wire"
  (fn () => if G.wire_eq (r2, b4, V.NSet.of_list [b3], E.NSet.of_list [e4, e5]) wire2 then I
            else raise ERROR "boundary wire incorrect") ()
  val _ = Testing.test "G.get_wire_containing_edge - no interior wvs"
  (fn () => if G.wire_eq (r2, r3, V.NSet.empty, E.NSet.of_list [e6]) wire3 then I
            else raise ERROR "no interior wvs incorrect") ()
  val _ = Testing.test "G.get_wire_containing_edge - self loop"
  (fn () => if G.wire_eq (r3, r3, V.NSet.of_list [b5], E.NSet.of_list [e7, e8]) wire4 then I
            else raise ERROR "self loop incorrect") ()
  (* c1 has the minimal end, so it should be chosen as the endpoint *)
  val _ = Testing.test "G.get_wire_containing_edge - circle1"
  (fn () => if (G.wire_eq (c1, c1, V.NSet.of_list [c2], E.NSet.of_list [e9,e10]) wire5)
            then I
            else raise ERROR ("circle1 incorrect: " ^ (Pretty.string_of o G.pretty_wire) wire5)) ()
  val _ = Testing.test "G.get_wire_containing_edge - circle1 (other edge)"
  (fn () => if (G.wire_eq (c1, c1, V.NSet.of_list [c2], E.NSet.of_list [e9,e10]) wire5')
            then I
            else raise ERROR ("circle1 (other edge) incorrect: " ^ (Pretty.string_of o G.pretty_wire) wire5')) ()
  val _ = Testing.test "G.get_wire_containing_edge - circle2"
  (fn () => if G.wire_eq (c3, c3, V.NSet.empty, E.NSet.of_list [e11]) wire6 then I
            else raise ERROR "circle2 incorrect") ()
  val _ = Testing.test "G.get_wire_containing_edge - undirected"
  (fn () => if G.wire_eq (r2, r3, V.NSet.of_list [b6], E.NSet.of_list [e12,e13]) wire7 then I
            else raise ERROR "undirected incorrect") ()
  val _ = Testing.test "G.get_wire_containing_edge - undirected 2"
  (fn () => if G.wire_eq (r2, r3, V.NSet.of_list [b7], E.NSet.of_list [e14,e15]) wire8 then I
            else raise ERROR "undirected 2 incorrect") ()
  val _ = Testing.test "G.get_wire_containing_edge - undirected 2'"
  (fn () => if G.wire_eq wire8 wire8' then I
            else raise ERROR "undirected 2' incorrect") ()
  val _ = Testing.test "G.get_wire_containing_edge - undirected 3"
  (fn () => if G.wire_eq (r2, r3, V.NSet.of_list [b8], E.NSet.of_list [e16,e17]) wire9 then I
            else raise ERROR "undirected 3 incorrect") ()
  val _ = Testing.test "G.get_wire_containing_edge - undirected 3'"
  (fn () => if G.wire_eq wire9 wire9' then I
            else raise ERROR "undirected 3' incorrect") ()
  val _ = Testing.test "G.get_wire_list"
  (fn () => c_wire_lists wires wires') ()
  val _ = Testing.test "G.minimise"
    (fn () =>
      let
        val g_min' = G.minimise g_raw
      in
        if GIso.test g_min' g_min then ()
        else (writeln "Expected"; G.print g_min;
              writeln "Got"; G.print g_min';
              raise ERROR "Minimise gave incorrect graph")
      end) ()
  val _ = Testing.test "G.normalise"
    (fn () =>
      let
        val g_norm' = G.normalise g_raw
      in
        if GIso.test g_norm' g_norm then ()
        else (writeln "Expected"; G.print g_norm;
              writeln "Got"; G.print g_norm';
              raise ERROR "Normalise gave incorrect graph")
      end) ()

in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR WIRE FUNCTIONS PASSED!"
end; (* ends local namespace used for testing *)


(* Test plugging functions *)
local
  open Test_RG_MkG;

  val x1 = V.mk "x1"
  val x2 = V.mk "x2"
  val x3 = V.mk "x3"
  val x4 = V.mk "x4"
  val in1 = V.mk "in1"
  val in2 = V.mk "in2"
  val in3 = V.mk "in3"
  val in4 = V.mk "in4"
  val out1 = V.mk "out1"
  val out2 = V.mk "out2"
  val out3 = V.mk "out3"
  val out4 = V.mk "out4"
  val wv1 = V.mk "wv1"
  val wv2 = V.mk "wv2"
  val wv3 = V.mk "wv3"
  val wv4 = V.mk "wv4"
  val b1 = V.mk "b1"
  val b2 = V.mk "b2"
  val b3 = V.mk "b3"
  val b4 = V.mk "b4"

  val g = G.empty;
  val g = g |> G.add_named_vertex x1 (mkX zero_angle)
            |> G.add_named_vertex x2 (mkX zero_angle)
            |> G.add_named_vertex x3 (mkX zero_angle)
            |> G.add_named_vertex x4 (mkX zero_angle)

            |> G.add_named_vertex in1 bvert
            |> G.add_named_vertex in2 bvert
            |> G.add_named_vertex in3 bvert
            |> G.add_named_vertex in4 bvert

            |> G.add_named_vertex out1 bvert
            |> G.add_named_vertex out2 bvert
            |> G.add_named_vertex out3 bvert
            |> G.add_named_vertex out4 bvert

            |> G.add_named_vertex wv1 bvert
            |> G.add_named_vertex wv2 bvert
            |> G.add_named_vertex wv3 bvert
            |> G.add_named_vertex wv4 bvert

            |> G.add_named_vertex b1 bvert
            |> G.add_named_vertex b2 bvert
            |> G.add_named_vertex b3 bvert
            |> G.add_named_vertex b4 bvert

            |> G.doadd_edge dir_edge in1 x1
            |> G.doadd_edge dir_edge in2 x2
            |> G.doadd_edge dir_edge x1 out1
            |> G.doadd_edge dir_edge x3 out3
            |> G.doadd_edge dir_edge x1 wv1
            |> G.doadd_edge dir_edge wv1 x1

            |> G.doadd_edge undir_edge b1 x1
            |> G.doadd_edge undir_edge b2 x2
            |> G.doadd_edge undir_edge x3 b3
            |> G.doadd_edge undir_edge x1 b4
            |> G.doadd_edge undir_edge x2 wv2
            |> G.doadd_edge undir_edge x2 wv2
            |> G.doadd_edge undir_edge x3 wv3
            |> G.doadd_edge undir_edge wv3 x3

  val _ = Testing.test "G.are_pluggable (1)"
          (fn () => Testing.assert "pluggable"
                    (G.are_pluggable g in1 out1)) ()
  val _ = Testing.test "G.are_pluggable (2)"
          (fn () => Testing.assert "pluggable"
                    (G.are_pluggable g in2 out3)) ()
  val _ = Testing.test "G.are_pluggable (3)"
          (fn () => Testing.assert "not pluggable"
                    (not (G.are_pluggable g in1 in2))) ()
  val _ = Testing.test "G.are_pluggable (4)"
          (fn () => Testing.assert "not pluggable"
                    (not (G.are_pluggable g in1 wv1))) ()
  val _ = Testing.test "G.are_pluggable (5)"
          (fn () => Testing.assert "not pluggable"
                    (not (G.are_pluggable g wv1 out3))) ()
  val _ = Testing.test "G.are_pluggable (6)"
          (fn () => Testing.assert "pluggable"
                    (G.are_pluggable g b1 b2)) ()
  val _ = Testing.test "G.are_pluggable (7)"
          (fn () => Testing.assert "pluggable"
                    (G.are_pluggable g b1 b3)) ()
  val _ = Testing.test "G.are_pluggable (8)"
          (fn () => Testing.assert "pluggable"
                    (G.are_pluggable g b4 b3)) ()
  val _ = Testing.test "G.are_pluggable (9)"
          (fn () => Testing.assert "not pluggable"
                    (not (G.are_pluggable g b1 wv2))) ()
  val _ = Testing.test "G.are_pluggable (10)"
          (fn () => Testing.assert "not pluggable"
                    (not (G.are_pluggable g b1 wv3))) ()
  val _ = Testing.test "G.are_pluggable (10)"
          (fn () => Testing.assert "not pluggable"
                    (not (G.are_pluggable g b1 wv1))) ()
  val _ = Testing.test "G.are_pluggable (11)"
          (fn () => Testing.assert "not pluggable"
                    (not (G.are_pluggable g b3 wv2))) ()
  val _ = Testing.test "G.are_pluggable (12)"
          (fn () => Testing.assert "not pluggable"
                    (not (G.are_pluggable g in1 b3))) ()
  val _ = Testing.test "G.are_pluggable (13)"
          (fn () => Testing.assert "not pluggable"
                    (not (G.are_pluggable g b3 in1))) ()
  val _ = Testing.test "G.are_pluggable (14)"
          (fn () => Testing.assert "not pluggable"
                    (not (G.are_pluggable g out1 b3))) ()
  val _ = Testing.test "G.are_pluggable (15)"
          (fn () => Testing.assert "not pluggable"
                    (not (G.are_pluggable g b3 out1))) ()

  val _ = Testing.test "G.get_plugging (1)"
          (fn () =>
          case G.get_plugging g in1 out1 of
               SOME ((G.Directed,_),(v1,v2)) =>
                 Testing.assert "correct vertex names"
                    ((V.name_eq (v1,out1)) andalso
                     (V.name_eq (v2,in1)))
             | SOME _ => raise ERROR "Wrong directedness"
             | NONE   => raise ERROR "No plugging found") ()
  val _ = Testing.test "G.get_plugging (2)"
          (fn () =>
          case G.get_plugging g out3 in2 of
               SOME ((G.Directed,_),(v1,v2)) =>
                 Testing.assert "correct vertex names"
                    ((V.name_eq (v1,out3)) andalso
                     (V.name_eq (v2,in2)))
             | SOME _ => raise ERROR "Wrong directedness"
             | NONE   => raise ERROR "No plugging found") ()
  val _ = Testing.test "G.get_plugging (3)"
          (fn () =>
          case G.get_plugging g b1 b2 of
               SOME ((G.UnDirected,_),(v1,v2)) =>
                 Testing.assert "correct vertex names"
                    (V.NSet.eq (V.NSet.of_list [b1,b2])
                               (V.NSet.of_list [v1,v2]))
             | SOME _ => raise ERROR "Wrong directedness"
             | NONE   => raise ERROR "No plugging found") ()
  val _ = Testing.test "G.get_plugging (4)"
          (fn () =>
          case G.get_plugging g b1 b3 of
               SOME ((G.UnDirected,_),(v1,v2)) =>
                 Testing.assert "correct vertex names"
                    (V.NSet.eq (V.NSet.of_list [b1,b3])
                               (V.NSet.of_list [v1,v2]))
             | SOME _ => raise ERROR "Wrong directedness"
             | NONE   => raise ERROR "No plugging found") ()
  val _ = Testing.test "G.get_plugging (5)"
          (fn () =>
          case G.get_plugging g b4 b3 of
               SOME ((G.UnDirected,_),(v1,v2)) =>
                 Testing.assert "correct vertex names"
                    (V.NSet.eq (V.NSet.of_list [b4,b3])
                               (V.NSet.of_list [v1,v2]))
             | SOME _ => raise ERROR "Wrong directedness"
             | NONE   => raise ERROR "No plugging found") ()

  val _ = Testing.test "G.plug (1)"
          (fn () =>
          let
            val (e,g') = G.plug in1 out1 g
          in
            case G.get_edge g' e of
                 ((G.Directed,_),(v1,v2)) =>
                   Testing.assert "correct vertex names"
                      ((V.name_eq (v1,out1)) andalso
                       (V.name_eq (v2,in1)))
               | _ => raise ERROR "Wrong directedness"
          end) ()
  val _ = Testing.test "G.plug (2)"
          (fn () =>
          let
            val (e,g') = G.plug out3 in2 g
          in
            case G.get_edge g' e of
                 ((G.Directed,_),(v1,v2)) =>
                   Testing.assert "correct vertex names"
                      ((V.name_eq (v1,out3)) andalso
                       (V.name_eq (v2,in2)))
               | _ => raise ERROR "Wrong directedness"
          end) ()
  val _ = Testing.test "G.plug (3)"
          (fn () =>
          let
            val (e,g') = G.plug b1 b2 g
          in
            case G.get_edge g' e of
                 ((G.UnDirected,_),(v1,v2)) =>
                   Testing.assert "correct vertex names"
                      (V.NSet.eq (V.NSet.of_list [b1,b2])
                                 (V.NSet.of_list [v1,v2]))
               | _ => raise ERROR "Wrong directedness"
          end) ()
  val _ = Testing.test "G.plug (4)"
          (fn () =>
          let
            val (e,g') = G.plug b1 b3 g
          in
            case G.get_edge g' e of
                 ((G.UnDirected,_),(v1,v2)) =>
                   Testing.assert "correct vertex names"
                      (V.NSet.eq (V.NSet.of_list [b1,b3])
                                 (V.NSet.of_list [v1,v2]))
               | _ => raise ERROR "Wrong directedness"
          end) ()
  val _ = Testing.test "G.plug (5)"
          (fn () =>
          let
            val (e,g') = G.plug b4 b3 g
          in
            case G.get_edge g' e of
                 ((G.UnDirected,_),(v1,v2)) =>
                   Testing.assert "correct vertex names"
                      (V.NSet.eq (V.NSet.of_list [b4,b3])
                                 (V.NSet.of_list [v1,v2]))
               | _ => raise ERROR "Wrong directedness"
          end) ()

  val _ = Testing.test "G.plug_and_minimise (1)"
          (fn () =>
          let
            val ((s,t,wvs,es),g') = G.plug_and_minimise in1 out1 g
          in
             (Testing.assert "correct vertex names"
                (V.NSet.eq (V.NSet.single x1)
                           (V.NSet.of_list [s,t]));
              Testing.assert "no wire verts"
                (V.NSet.is_empty wvs);
              case E.NSet.tryget_singleton es of
                   SOME e =>
                     (case G.get_edge g' e of
                          ((G.Directed,_),_) => ()
                        | _ => raise ERROR "Wrong directedness")
                 | NONE => raise ERROR "Wrong edge count"
             )
          end
          ) ()
  val _ = Testing.test "G.plug_and_minimise (2)"
          (fn () =>
          let
            val ((s,t,wvs,es),g') = G.plug_and_minimise out3 in2 g
          in
             (Testing.assert "correct vertex names"
                (V.NSet.eq (V.NSet.of_list [x2,x3])
                           (V.NSet.of_list [s,t]));
              Testing.assert "no wire verts"
                (V.NSet.is_empty wvs);
              case E.NSet.tryget_singleton es of
                   SOME e =>
                     (case G.get_edge g' e of
                          ((G.Directed,_),_) => ()
                        | _ => raise ERROR "Wrong directedness")
                 | NONE => raise ERROR "Wrong edge count"
             )
          end
          ) ()

  val _ = Testing.test "G.plug_and_normalise (1)"
          (fn () =>
          let
            val ((s,t,wvs,es),_) = G.plug_and_normalise in1 out1 g
          in
             (Testing.assert "correct vertex names"
                (V.NSet.eq (V.NSet.single x1)
                           (V.NSet.of_list [s,t]));
              Testing.assert "2 wire verts"
                (V.NSet.cardinality wvs = 2);
              Testing.assert "2 edges"
                (E.NSet.cardinality es = 3)
             )
          end
          ) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR PLUGGING PASSED!"
end; (* ends local namespace used for testing *)


(* Test subgraph functions *)
local
  open Test_RG_MkG;

  val x1 = V.mk "x1"
  val x2 = V.mk "x2"
  val x3 = V.mk "x3"
  val x4 = V.mk "x4"
  val x5 = V.mk "x5"
  val wv1 = V.mk "wv1"
  val wv2 = V.mk "wv2"
  val wv3 = V.mk "wv3"
  val wv4 = V.mk "wv4"
  val wv5 = V.mk "wv5"
  val wv6 = V.mk "wv6"
  val wv7 = V.mk "wv7"
  val wv8 = V.mk "wv8"
  val wv9 = V.mk "wv9"
  val wv10 = V.mk "wv10"
  val wv11 = V.mk "wv11"
  val wv12 = V.mk "wv12"
  val wv13 = V.mk "wv13"
  val wv14 = V.mk "wv14"
  val wv15 = V.mk "wv15"
  val wv16 = V.mk "wv16"
  val wv17 = V.mk "wv17"
  val wv18 = V.mk "wv18"
  val wv19 = V.mk "wv19"
  val wv20 = V.mk "wv20"

  val g = G.empty;
  val g = g |> G.add_named_vertex x1 (mkX zero_angle)
            |> G.add_named_vertex x2 (mkX zero_angle)
            |> G.add_named_vertex x3 (mkX zero_angle)

            |> G.add_named_vertex wv1 bvert
            |> G.add_named_vertex wv2 bvert
            |> G.add_named_vertex wv3 bvert
            |> G.add_named_vertex wv4 bvert
            |> G.add_named_vertex wv5 bvert
            |> G.add_named_vertex wv6 bvert
            |> G.add_named_vertex wv7 bvert
            |> G.add_named_vertex wv8 bvert
            |> G.add_named_vertex wv9 bvert
            |> G.add_named_vertex wv10 bvert
            |> G.add_named_vertex wv11 bvert
            |> G.add_named_vertex wv12 bvert
            |> G.add_named_vertex wv13 bvert

            (* circle *)
            |> G.doadd_edge dir_edge   wv1 wv1
            (* bare wire *)
            |> G.doadd_edge undir_edge wv2 wv3
            (* main graph *)
            |> G.doadd_edge dir_edge   x1  wv4
            |> G.doadd_edge dir_edge   wv4 wv5
            |> G.doadd_edge dir_edge   wv5 x1
            |> G.doadd_edge undir_edge wv6 x2
            |> G.doadd_edge undir_edge x2  wv7
            |> G.doadd_edge undir_edge wv7 wv8
            |> G.doadd_edge undir_edge wv8 x3
            |> G.doadd_edge dir_edge   x2  wv9
            |> G.doadd_edge dir_edge   wv9 wv10
            |> G.doadd_edge dir_edge   wv10 x3

            (* these will be connected to later *)
            |> G.doadd_edge undir_edge x3   wv12
            |> G.doadd_edge dir_edge   x3   wv13
            |> G.doadd_edge dir_edge   x2   wv11
  val exp_subgraph = g

  val retain = V.NSet.of_list [x1,x2,x3,wv1,wv2,wv3]

  val g = g |> G.add_named_vertex x4 (mkX zero_angle)
            |> G.add_named_vertex x5 (mkX zero_angle)

            |> G.add_named_vertex wv14 bvert
            |> G.add_named_vertex wv15 bvert
            |> G.add_named_vertex wv16 bvert
            |> G.add_named_vertex wv17 bvert
            |> G.add_named_vertex wv18 bvert
            |> G.add_named_vertex wv19 bvert
            |> G.add_named_vertex wv20 bvert

            (* circle *)
            |> G.doadd_edge undir_edge wv14 wv14
            (* bare wire *)
            |> G.doadd_edge dir_edge   wv15 wv16
            (* main graph *)
            |> G.doadd_edge undir_edge wv12 wv17
            |> G.doadd_edge undir_edge wv17 x4
            |> G.doadd_edge dir_edge   wv13 wv18
            |> G.doadd_edge dir_edge   wv18 x4
            |> G.doadd_edge undir_edge x4   wv19
            |> G.doadd_edge dir_edge   wv11 wv20
            |> G.doadd_edge dir_edge   wv20 x5
  val full_graph = g

in
  val _ = Testing.test "get_open_subgraph" (fn () => let
    val g = G.get_open_subgraph retain full_graph
    val _ = writeln "Subgraph was:"
    val _ = G.print g
    val _ = Testing.assert "subgraph was as expected"
              (G.exact_eq g exp_subgraph)
  in writeln "success" end
  ) ()

  val _ = Testing.assert_no_failed_tests();
  val _ = writeln "UNIT TESTS FOR SUBGRAPHS PASSED!"
end; (* ends local namespace used for testing *)


