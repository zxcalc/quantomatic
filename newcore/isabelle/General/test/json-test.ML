local
  fun parse_json_file json_f = let
    val json_stream = TextIO.openIn json_f
    val json_str = TextIO.inputAll json_stream
    val _ = TextIO.closeIn json_stream
  in
    json_str
  end;
  fun assert_parse_error json =
    (Json.of_string json; raise ERROR "Expected parse error")
    handle Json.parse_exn _ => ();
  fun assert_string_val exp json =
    case json
      of Json.String s => (if s = exp then () else
            raise ERROR ("Expected \""^exp^
                         "\" but got \""^s^"\""))
       | _ => raise ERROR "Expected String";
  fun assert_int_val exp json =
    case json
      of Json.Int i => (if i = exp then () else
            raise ERROR ("Expected "^(Int.toString exp)^
                         " but got "^(Int.toString i)))
       | _ => raise ERROR "Expected Int";
  fun assert_real_val exp json =
    case json
      of Json.Real r => (if Real.==(r,exp) then () else
            raise ERROR ("Expected "^(Real.toString exp)^
                         " but got "^(Real.toString r)))
       | _ => raise ERROR "Expected Real";
  fun assert_bool_val exp json =
    case json
      of Json.Bool true => (if exp then () else
            raise ERROR "Expected false but got true")
       | Json.Bool false => (if not exp then () else
            raise ERROR "Expected true but got false")
       | _ => raise ERROR "Expected Bool";
  fun assert_null_val json =
    case json
      of Json.Null => ()
       | _ => raise ERROR "Expected Null";
  fun test_array_singleton (json_str,val_check_fun) =
    case (Json.of_string json_str)
      of Json.Array [v] => val_check_fun v
       | Json.Array _ => raise ERROR "Array had wrong no. of els"
       | _ => raise ERROR "Expected Array"
in
val _ = Testing.test "JSON parsing: empty object" (fn _ =>
  let
    val json_str = "{}"
    val json_t = Json.of_string json_str
  in
    case json_t
      of Json.Object obj => (
            case Json.properties obj
              of [] => ()
               | _ => raise ERROR "Object was not empty"
          )
       | _ => raise ERROR "Expected Object"
  end) ();

val _ = Testing.test "JSON parsing: empty array" (fn _ =>
  let
    val json_str = "[]"
    val json_t = Json.of_string json_str
  in
    case json_t
      of Json.Array [] => ()
       | Json.Array _ => raise ERROR "Array was not empty"
       | _ => raise ERROR "Expected Array"
  end) ();

val _ = Testing.test "JSON parsing: empty array with whitespace" (fn _ =>
  let
    val json_str = " [\n]\t"
    val json_t = Json.of_string json_str
  in
    case json_t
      of Json.Array [] => ()
       | Json.Array _ => raise ERROR "Array was not empty"
       | _ => raise ERROR "Expected Array"
  end) ();

val _ = Testing.test "JSON parsing: array with empty string"
  test_array_singleton ("[\"\"]",assert_string_val "");

val _ = Testing.test "JSON parsing: array with simple string"
  test_array_singleton ("[\"foo\"]",assert_string_val "foo");

val _ = Testing.test "JSON parsing: array with escaped string (\")"
  test_array_singleton ("[\"\\\"\"]",assert_string_val "\"");

val _ = Testing.test "JSON parsing: array with escaped string (\\)"
  test_array_singleton ("[\"\\\\\"]",assert_string_val "\\");

val _ = Testing.test "JSON parsing: array with escaped string (/)"
  test_array_singleton ("[\"\\/\"]",assert_string_val "/");

val _ = Testing.test "JSON parsing: array with escaped string (backspace)"
  test_array_singleton ("[\"\\b\"]",assert_string_val "\b");

val _ = Testing.test "JSON parsing: array with escaped string (formfeed)"
  test_array_singleton ("[\"\\f\"]",assert_string_val "\f");

val _ = Testing.test "JSON parsing: array with escaped string (lf)"
  test_array_singleton ("[\"\\n\"]",assert_string_val "\n");

val _ = Testing.test "JSON parsing: array with escaped string (cr)"
  test_array_singleton ("[\"\\r\"]",assert_string_val "\r");

val _ = Testing.test "JSON parsing: array with escaped string (tab)"
  test_array_singleton ("[\"\\t\"]",assert_string_val "\t");

val _ = Testing.test "JSON parsing: array with unicode escaped string"
  test_array_singleton ("[\"\\u005B\"]",assert_string_val "[");

val _ = Testing.test "JSON parsing: array with +ve int (no exp)"
  test_array_singleton ("[27364]",assert_int_val 27364);

val _ = Testing.test "JSON parsing: array with +ve int and exp (e)"
  test_array_singleton ("[3e2]",assert_real_val 300.0);

val _ = Testing.test "JSON parsing: array with +ve int and exp (E)"
  test_array_singleton ("[3E2]",assert_real_val 300.0);

val _ = Testing.test "JSON parsing: array with +ve int and exp (e+)"
  test_array_singleton ("[3e+2]",assert_real_val 300.0);

val _ = Testing.test "JSON parsing: array with +ve int and exp (E+)"
  test_array_singleton ("[3E+2]",assert_real_val 300.0);

val _ = Testing.test "JSON parsing: array with +ve int and exp (e-)"
  test_array_singleton ("[3e-2]",assert_real_val 0.03);

val _ = Testing.test "JSON parsing: array with +ve int and exp (E-)"
  test_array_singleton ("[3E-2]",assert_real_val 0.03);

val _ = Testing.test "JSON parsing: array with -ve int (no exp)"
  test_array_singleton ("[-53]",assert_int_val ~53);

val _ = Testing.test "JSON parsing: array with -ve int and exp (e)"
  test_array_singleton ("[-3e2]",assert_real_val ~300.0);

val _ = Testing.test "JSON parsing: array with -ve int and exp (E)"
  test_array_singleton ("[-3E2]",assert_real_val ~300.0);

val _ = Testing.test "JSON parsing: array with -ve int and exp (e+)"
  test_array_singleton ("[-3e+2]",assert_real_val ~300.0);

val _ = Testing.test "JSON parsing: array with -ve int and exp (E+)"
  test_array_singleton ("[-3E+2]",assert_real_val ~300.0);

val _ = Testing.test "JSON parsing: array with -ve int and exp (e-)"
  test_array_singleton ("[-3e-2]",assert_real_val ~0.03);

val _ = Testing.test "JSON parsing: array with -ve int and exp (E-)"
  test_array_singleton ("[-3E-2]",assert_real_val ~0.03);

val _ = Testing.test "JSON parsing: array with +ve real (no exp)"
  test_array_singleton ("[3.14]",assert_real_val 3.14);

val _ = Testing.test "JSON parsing: array with +ve real and exp (e)"
  test_array_singleton ("[3.2e2]",assert_real_val 320.0);

val _ = Testing.test "JSON parsing: array with +ve real and exp (E)"
  test_array_singleton ("[3.2E2]",assert_real_val 320.0);

val _ = Testing.test "JSON parsing: array with +ve real and exp (e+)"
  test_array_singleton ("[3.2e+2]",assert_real_val 320.0);

val _ = Testing.test "JSON parsing: array with +ve real and exp (E+)"
  test_array_singleton ("[3.2E+2]",assert_real_val 320.0);

val _ = Testing.test "JSON parsing: array with +ve real and exp (e-)"
  test_array_singleton ("[3.2e-2]",assert_real_val 0.032);

val _ = Testing.test "JSON parsing: array with +ve real and exp (E-)"
  test_array_singleton ("[3.2E-2]",assert_real_val 0.032);

val _ = Testing.test "JSON parsing: array with -ve real (no exp)"
  test_array_singleton ("[-3.14]",assert_real_val ~3.14);

val _ = Testing.test "JSON parsing: array with -ve real and exp (e)"
  test_array_singleton ("[-3.2e2]",assert_real_val ~320.0);

val _ = Testing.test "JSON parsing: array with -ve real and exp (E)"
  test_array_singleton ("[-3.2E2]",assert_real_val ~320.0);

val _ = Testing.test "JSON parsing: array with -ve real and exp (e+)"
  test_array_singleton ("[-3.2e+2]",assert_real_val ~320.0);

val _ = Testing.test "JSON parsing: array with -ve real and exp (E+)"
  test_array_singleton ("[-3.2E+2]",assert_real_val ~320.0);

val _ = Testing.test "JSON parsing: array with -ve real and exp (e-)"
  test_array_singleton ("[-3.2e-2]",assert_real_val ~0.032);

val _ = Testing.test "JSON parsing: array with -ve real and exp (E-)"
  test_array_singleton ("[-3.2E-2]",assert_real_val ~0.032);

val _ = Testing.test "JSON parsing: array with true"
  test_array_singleton ("[true]",assert_bool_val true);

val _ = Testing.test "JSON parsing: array with false"
  test_array_singleton ("[false]",assert_bool_val false);

val _ = Testing.test "JSON parsing: array with null"
  test_array_singleton ("[null]",assert_null_val);

val _ = Testing.test "JSON parsing: complex object" (fn _ =>
  let
    val json_str = parse_json_file "sample1.json"
    val json_t = Json.of_string json_str
    val outer_obj = case json_t
                      of Json.Object obj => obj
                       | _ => raise ERROR "Expected Object"
    val _ = assert_int_val 219948 (Json.get outer_obj "id")
    val _ = assert_real_val 14826.049999999999 (Json.get outer_obj "longCash")
    val _ = assert_real_val ~0.16 (Json.get outer_obj "shortCash")
    val _ = assert_string_val "2008-04-01" (Json.get outer_obj "inceptionDate")
    val sum_obj = case (Json.get outer_obj "summary")
                    of Json.Object obj => obj
                     | _ => raise ERROR "Expected object"
    val _ = assert_null_val (Json.get sum_obj "totalValue")
  in () end) ();

val _ = Testing.test "JSON parsing: array with invalid escaped string"
  assert_parse_error "[\"\\x\"]";

val _ = Testing.test "JSON parsing: object notation in array"
  assert_parse_error "[\"x\":\"y\"]";

val _ = Testing.test "JSON parsing: mixed array/object notation in array"
  assert_parse_error "[\"c\",\"x\":\"y\"]";

val _ = Testing.assert_no_failed_tests ();
end;
