CoreTester.registerTest "Simple test" (fn session =>
let val version = CoreTester.protocolVersion session in
  if version <> "1.0" then
    raise CoreTester.test_exp ("Wrong version (got "^version^", expected 1.0)")
  else ()
end
);

(* vi:et:sw=2:sts=2
*)
