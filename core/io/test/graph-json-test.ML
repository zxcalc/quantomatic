
local
  open Test_Bang_Graph_Tools;
  structure L = LinratAngleExpr;
  structure C = LinratAngleExpr.Coeff;
  structure GIn = GJsonInput;
  structure GOut = GJsonOutput;
  val expr_1_json = "{\"vars\":{\"a\":{\"num\":1,\"denom\":3},\"b\":{\"num\":2,\"denom\":1}}}"
  val expr_1 = L.mk C.zero [(X.mk "a",C.mk 1 3), (X.mk "b",C.mk 2 1)]
  val expr_2_json = "{\"pi\":{\"num\":1,\"denom\":1}}"
  val expr_2 = L.pi
  val expr_3_json = "{\"pi\":{\"num\":1,\"denom\":2},\"vars\":{\"\\\\alpha\":{\"num\":1,\"denom\":1}}}"
  val expr_3 = L.parse "1/2\\pi + \\alpha"
  val g = G.empty |> add_wv "w1"
                  |> add_wv "w2"
                  |> add_vunit1 "n1"
                  |> add_vexpr1 "n2" "0"
                  |> add_vexpr2 "n3" "0"
                  |> add_vexpr1_a "n4" expr_1
                  |> add_vexpr2_a "n5" expr_2
                  |> add_dir_eunit1 "e1" "w1" "n1"
                  |> add_undir_eunit2 "e2" "n3" "n4"
                  |> add_dir_eexpr1_a "e3" "n5" "n5" expr_3
                  |> add_bbox "b1" ["w1","n1","n2"]
                  |> add_child_bbox "b2" "b1" ["w1","n1"]
                  |> add_bbox "b3" [];
  val g_json =
  "{"^
    "\"wire_vertices\":[\"w1\",\"w2\"],"^
    "\"node_vertices\":{"^
      "\"n1\":{\"data\":\"VUnit1\"},"^
      "\"n2\":{\"data\":{\"type\":\"VExpr1\",\"angle\":null}},"^
      "\"n3\":{\"data\":{\"type\":\"VExpr2\"}},"^
      "\"n4\":{\"data\":{\"type\":\"VExpr1\",\"angle\":"^expr_1_json^"}},"^
      "\"n5\":{\"data\":{\"type\":\"VExpr2\",\"angle\":"^expr_2_json^"}}"^
    "},"^
    "\"dir_edges\":{"^
      "\"e1\":{\"src\":\"w1\",\"tgt\":\"n1\",\"data\":\"EUnit1\"},"^
      "\"e3\":{\"src\":\"n5\",\"tgt\":\"n5\",\"data\":{\"type\":\"EExpr1\",\"angle\":"^expr_3_json^"}}"^
    "},"^
    "\"undir_edges\":{"^
      "\"e2\":{\"src\":\"n3\",\"tgt\":\"n4\",\"data\":{\"type\":\"EUnit2\"}}"^
    "},"^
    "\"bang_boxes\":{"^
      "\"b1\":{\"contents\":[\"w1\",\"n1\",\"n2\"]},"^
      "\"b2\":{\"contents\":[\"w1\",\"n1\"],\"parent\":\"b1\"},"^
      "\"b3\":{}"^
    "}"^
  "}";
  fun test_parse (json_str,exp_expr) = let
    val e = GIn.input (Json.of_string json_str)
  in
    if G.exact_eq exp_expr e
    then ()
    else
      (writeln "Expected graph:";
       G.print exp_expr;
       writeln "Graph from JSON:";
       G.print e;
       raise ERROR "Got wrong graph")
  end
  fun test_reparse expr = let
    val e = GIn.input (GOut.output expr)
  in
    if G.exact_eq expr e
    then ()
    else
      (writeln "Expected graph:";
       G.print expr;
       writeln "Graph from JSON:";
       G.print e;
       raise ERROR "Got wrong graph")
  end
in
  val _ = Testing.test "Graph I/O: parse empty"
            test_parse ("{}",G.empty);

  val _ = Testing.test "Graph I/O: reparse empty"
            test_reparse G.empty;

  val _ = Testing.test "Graph I/O: parse example graph"
            test_parse (g_json,g);

  val _ = Testing.test "Graph I/O: reparse example graph"
            test_reparse g;

  val _ = Testing.assert_no_failed_tests();
end;
