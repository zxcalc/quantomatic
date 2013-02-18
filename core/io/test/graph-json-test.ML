structure Test_VertexDataInputJSON : INPUT_JSON =
struct
  open JsonInputUtils;
  structure L = InputLinratJSON;
  type data = Test_RG_InternVData.data;
  val get_angle = L.input oo (get_easy Json.Null)
  val to_lower = String.implode o (map Char.toLower) o String.explode
  fun input (Json.String t) =
        (case to_lower t
           of "h" => Test_RG_InternVData.Hnd
            | "x" => Test_RG_InternVData.Xnd LinratAngleExpr.zero
            | "z" => Test_RG_InternVData.Znd LinratAngleExpr.zero
            | _   => raise bad_input_exp ("Unknown vertex type "^t,""))
    | input (Json.Object obj) =
        (case to_lower (get_string obj "type")
           of "h" => Test_RG_InternVData.Hnd
            | "z" => Test_RG_InternVData.Znd (get_angle obj "angle")
            | "x" => Test_RG_InternVData.Xnd (get_angle obj "angle")
            | t   => raise bad_input_exp ("Unknown vertex type "^t,"type"))
    | input _ = raise bad_input_exp ("Expected object","");
end;

structure Test_VertexDataOutputJSON : OUTPUT_JSON =
struct
  open JsonOutputUtils;
  structure L = OutputLinratJSON;
  type data = Test_RG_InternVData.data;
  fun output (Test_RG_InternVData.Znd a) =
        Json.Object (
          Json.empty_obj |> Json.update ("type",Json.String "Z")
                         |> Json.update ("angle",L.output a)
        )
    | output (Test_RG_InternVData.Xnd a) =
        Json.Object (
          Json.empty_obj |> Json.update ("type",Json.String "X")
                         |> Json.update ("angle",L.output a)
        )
    | output Test_RG_InternVData.Hnd = Json.String "H"
end;

structure Test_RG_GraphInputJSON : INPUT_JSON =
  InputGraphJSON(
    structure Graph = Test_RG_Graph
    structure InputVertexData = Test_VertexDataInputJSON
    structure InputEdgeData = InputUnitJSON
  );

structure Test_RG_GraphOutputJSON : OUTPUT_JSON =
  OutputGraphJSON(
    structure Graph = Test_RG_Graph
    structure OutputVertexData = Test_VertexDataOutputJSON
    structure OutputEdgeData = OutputUnitJSON
  );

local
  open Test_RG_MkG;
  structure L = LinratAngleExpr;
  structure C = LinratAngleExpr.Coeff;
  structure GIn = Test_RG_GraphInputJSON;
  structure GOut = Test_RG_GraphOutputJSON;
  val w1 = V.mk "w1"
  val w2 = V.mk "w2"
  val n1 = V.mk "n1"
  val n2 = V.mk "n2"
  val n3 = V.mk "n3"
  val n4 = V.mk "n4"
  val n5 = V.mk "n5"
  val e1 = E.mk "e1"
  val e2 = E.mk "e2"
  val e3 = E.mk "e3"
  val b1 = B.mk "b1"
  val b2 = B.mk "b2"
  val b3 = B.mk "b3"
  val expr_1_json = "{\"vars\":{\"a\":{\"num\":1,\"denom\":3},\"b\":{\"num\":2,\"denom\":1}}}"
  val expr_1 = L.mk C.zero [(X.mk "a",C.mk 1 3), (X.mk "b",C.mk 2 1)]
  val expr_2_json = "{\"pi\":{\"num\":1,\"denom\":1}}"
  val expr_2 = L.pi
  val g = G.empty
  val g = g |> G.add_named_vertex w1 bvert
            |> G.add_named_vertex w2 bvert
            |> G.add_named_vertex n1 mkH
            |> G.add_named_vertex n2 (mkX zero_angle)
            |> G.add_named_vertex n3 (mkZ zero_angle)
            |> G.add_named_vertex n4 (mkX expr_1)
            |> G.add_named_vertex n5 (mkZ expr_2)
            |> G.add_named_edge e1 dir_edge w1 n1
            |> G.add_named_edge e2 undir_edge n3 n4
            |> G.add_named_edge e3 dir_edge n5 n5
            |> G.add_named_bbox b1
            |> G.add_named_bbox b2
            |> G.add_named_bbox b3
            |> G.doadd_to_bbox b1 (V.NSet.of_list [w1,n1,n2])
            |> G.doadd_to_bbox b2 (V.NSet.of_list [w1,n1])
            |> G.set_bbox_parent b2 b1
  val g_json =
  "{"^
    "\"wire_vertices\":[\"w1\",\"w2\"],"^
    "\"node_vertices\":{"^
      "\"n1\":{\"data\":\"H\"},"^
      "\"n2\":{\"data\":{\"type\":\"X\",\"angle\":null}},"^
      "\"n3\":{\"data\":{\"type\":\"Z\"}},"^
      "\"n4\":{\"data\":{\"type\":\"X\",\"angle\":"^expr_1_json^"}},"^
      "\"n5\":{\"data\":{\"type\":\"Z\",\"angle\":"^expr_2_json^"}}"^
    "},"^
    "\"dir_edges\":{"^
      "\"e1\":{\"src\":\"w1\",\"tgt\":\"n1\"},"^
      "\"e3\":{\"src\":\"n5\",\"tgt\":\"n5\"}"^
    "},"^
    "\"undir_edges\":{"^
      "\"e2\":{\"src\":\"n3\",\"tgt\":\"n4\"}"^
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
    else (G.print exp_expr; G.print e; raise ERROR "Got wrong graph")
  end
  fun test_reparse expr = let
    val e = GIn.input (GOut.output expr)
  in
    if G.exact_eq expr e
    then ()
    else (G.print expr; G.print e; raise ERROR "Got wrong graph")
  end
in
  val _ = print g_json

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
