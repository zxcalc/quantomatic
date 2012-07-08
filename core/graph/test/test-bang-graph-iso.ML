local
  open Test_BBox_Util

  val g1 = G.empty
  val (n_a_1, g1) = g1 |> G.add_vertex (mkZ zero_angle);
  val (n_b_1, g1) = g1 |> G.add_vertex mkH;
  val (_, g1) = g1 |> G.add_edge undir_edge n_a_1 n_b_1;
  val (_, g1) = g1 |> G.add_edge undir_edge n_a_1 n_b_1;

  val g2 = G.empty
  val (n_a_2, g2) = g2 |> G.add_vertex mkH;
  val (n_b_2, g2) = g2 |> G.add_vertex (mkZ zero_angle);
  val (_, g2) = g2 |> G.add_edge undir_edge n_a_2 n_b_2;
  val (_, g2) = g2 |> G.add_edge undir_edge n_a_2 n_b_2;

  val g3 = g1
  val (_, g3) = g3 |> G.add_bbox;
  val (b1_3, g3) = g3 |> G.add_bbox;
  val g3 = g3 |> G.add_to_bbox b1_3 (V.NSet.single n_a_1);

  val g4 = g2
  val (b0_4, g4) = g4 |> G.add_bbox;
  val (_, g4) = g4 |> G.add_bbox;
  val g4 = g4 |> G.add_to_bbox b0_4 (V.NSet.single n_b_2);

  fun seq_count sq =
        (case Seq.pull sq of
              NONE   => 0
            | SOME (_,sq') => 1 + (seq_count sq'));
  fun assert_iso_count 0 iso =
        (case Seq.pull iso of
              NONE   => ()
            | SOME (_,iso') => raise ERROR ("Too many isomorphisms found (" ^
            (Int.toString (seq_count iso')) ^ " too many"))
    | assert_iso_count n iso =
        (case Seq.pull iso of
              NONE   => raise ERROR ("Not enough isomorphisms found (needed " ^
              (Int.toString n) ^ " more)")
            | SOME (_,iso') => assert_iso_count (n-1) iso');

  val _ = Testing.test "GIso.test [self; no !-boxes]"
          (fn () => if GIso.test g1 g1 then () else raise ERROR ("Should be iso")) ()
  val _ = Testing.test "GIso.get [self; no !-boxes]"
          (fn () => assert_iso_count 2 (GIso.get g1 g1)) ()

  val _ = Testing.test "GIso.test [g1,g2: no !-boxes]"
          (fn () => if GIso.test g1 g2 then () else raise ERROR ("Should be iso")) ()
  val _ = Testing.test "GIso.get [g1,g2: no !-boxes]"
          (fn () => assert_iso_count 2 (GIso.get g1 g2)) ()

  val _ = Testing.test "GIso.test [self; disjoint !-boxes]"
          (fn () => if GIso.test g3 g3 then () else raise ERROR ("Should be iso")) ()
  val _ = Testing.test "GIso.get [self; disjoint no !-boxes]"
          (fn () => assert_iso_count 2 (GIso.get g3 g3)) ()

  val _ = Testing.test "GIso.test [g3,g4: disjoint !-boxes]"
          (fn () => if GIso.test g3 g4 then () else raise ERROR ("Should be iso")) ()
  val _ = Testing.test "GIso.get [g3,g4: disjoint !-boxes]"
          (fn () => assert_iso_count 2 (GIso.get g3 g4)) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR !-GRAPH ISOS PASSED!"
end

