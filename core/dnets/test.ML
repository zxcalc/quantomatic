local
  open Test_MkG
  open Test_MatchUtil
  
  structure L = Literal

  val g = G.empty
  
  val v1 = V.mk "v1"
  val v2 = V.mk "v2"
  val v3 = V.mk "v3"
  val v4 = V.mk "v4"

  val e1 = E.mk "e1"
  val e2 = E.mk "e2"
  val e3 = E.mk "e3"

  val bvert = Test_Graph.OVData.EVert;
  
  val g = g |> G.add_named_vertex v1 bvert
            |> G.add_named_vertex v2 bvert
            |> G.add_named_vertex v3 bvert
            |> G.add_named_vertex v4 bvert
            |> G.add_named_edge e1 dir_edge v1 v2
            |> G.add_named_edge e1 dir_edge v1 v3
            |> G.add_named_edge e1 dir_edge v2 v4

  val lit = L.mk g v1
  val _ = Testing.test "Literal name" (assert_n_match v1 (#name lit)) ()
  val _ = Testing.test "Literal kind" (assert_n_match bvert (#kind lit)) ()
  val _ = Testing.test "Literal input arity" (assert_n_match 0 (#input_arity lit)) ()
  val _ = Testing.test "Literal input mult" (assert_n_match None (#input_mult lit)) ()
  val _ = Testing.test "Literal output arity" (assert_n_match 2 (#output_arity lit)) ()
  val _ = Testing.test "Literal output mult" (assert_n_match None (#output_mult lit)) ()

in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR DNETS PASSED!"
end