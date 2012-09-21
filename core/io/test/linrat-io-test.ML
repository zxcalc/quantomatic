local
  structure L = LinratAngleExpr;
  structure C = LinratAngleExpr.Coeff;
  structure OL = OutputLinratJSON;
  structure IL = InputLinratJSON;
  fun exprs_eq a b = L.is_zero (L.subtr_expr a b);
  val str_expr = Pretty.string_of o LinratAngleExpr.pretty
  fun test_parse (json_str,exp_expr) = let
    val e = IL.input (Json.of_string json_str)
  in
    if exprs_eq exp_expr e
    then ()
    else raise ERROR ("Expected \""^(str_expr exp_expr)^
                      "\" but got \""^(str_expr e)^"\"")
  end
  fun test_reparse expr = let
    val e = IL.input (OL.output expr)
  in
    if exprs_eq expr e
    then ()
    else raise ERROR ("Expected \""^(str_expr expr)^
                      "\" but got \""^(str_expr e)^"\"")
  end
in
  val _ = Testing.test "LinratAngleExpr I/O: parse empty"
            test_parse ("{}",L.zero);

  val _ = Testing.test "LinratAngleExpr I/O: parse pi"
            test_parse ("{\"pi\":{\"num\":1,\"denom\":1}}",L.pi);

  val _ = Testing.test "LinratAngleExpr I/O: parse 1/3 a + 2 b"
            test_parse ("{\"vars\":{\"a\":{\"num\":1,\"denom\":3},\"b\":{\"num\":2,\"denom\":1}}}",
                        L.mk C.zero [(X.mk "a",C.mk 1 3),
                                     (X.mk "b",C.mk 2 1)]);

  val _ = Testing.test "LinratAngleExpr I/O: parse 3/8 pi + 2/3 \\beta"
            test_parse ("{\"pi\":{\"num\":3,\"denom\":8},\"vars\":{\"\\\\beta\":{\"num\":2,\"denom\":3}}}",
                        L.mk (C.mk 3 8) [(X.mk "\\beta",C.mk 2 3)])

  val _ = Testing.test "LinratAngleExpr I/O: reparse zero"
            test_reparse L.zero;

  val _ = Testing.test "LinratAngleExpr I/O: reparse pi"
            test_reparse L.pi;

  val _ = Testing.test "LinratAngleExpr I/O: parse a + 2b"
            test_reparse (L.mk C.zero [(X.mk "a",C.one),
                                       (X.mk "b",C.mk 2 1)]);

  val _ = Testing.test "LinratAngleExpr I/O: parse 3/8 pi + 2/3 \\beta"
            test_reparse (L.mk (C.mk 3 8) [(X.mk "\\beta",C.mk 2 3)])

  (* NB: uses a quirk of the JSON parser (it parses JSON fragments) *)
  val _ = Testing.test "LinratAngleExpr I/O: parse as string"
            test_parse ("\"3/8 \\\\pi + 2/3 \\\\beta\"",
                        L.mk (C.mk 3 8) [(X.mk "\\beta",C.mk 2 3)])

  val _ = Testing.assert_no_failed_tests();
end
