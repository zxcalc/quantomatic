local
  open Test_Bang_Graph_Tools;

  val g1 = G.empty |> add_vunit1 "v1"
                   |> add_vunit2 "v2"
                   |> add_undir_eunit1 "e1" "v1" "v2"
                   |> add_undir_eunit1 "e2" "v1" "v2";

  val g2 = G.empty |> add_vunit2 "v1"
                   |> add_vunit1 "v2"
                   |> add_undir_eunit1 "e1" "v2" "v1"
                   |> add_undir_eunit1 "e2" "v2" "v1";

  val g3 = g1 |> add_bbox "b1" []
              |> add_bbox "b2" ["v1"];

  val g4 = g2 |> add_bbox "b0" []
              |> add_bbox "b1" ["v2"];

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
end

