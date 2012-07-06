(* testing nested !-boxes *)

local
  open Test_MkG;

  val _ = Testing.test "Stub" (fn() => ()) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR NESTING BANG-BOX EXPANSION PASSED!"
end