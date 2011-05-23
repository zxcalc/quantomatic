(* do a basic test to load up a ruleset that requires some renaming of 
   rhs vertices. *)
local
val SOME ruleset = Testing.test "Read (v1): rulesets/test.rules" 
  (Test_IO_Interface_v1.Input.Ruleset.input o XMLReader.read_from_file)
  "rulesets/test.rules";

(* write out the ruleset to the new format. *)
val _ = Testing.test "Write (v2): rulesets/test.rules_v2" 
  (XMLWriter.write_to_file "rulesets/test.rules_v2" 
   o Test_IO_Interface_v2.Output.Ruleset.output)
  ruleset;

val _ = Testing.test "Read (v2): rulesets/test.rules_v2" 
  (Test_IO_Interface_v2.Input.Ruleset.input o XMLReader.read_from_file) 
  "rulesets/test.rules_v2";

val SOME ruleset = Testing.test "Read (v1): rulesets/basic.rules" 
  (Test_IO_Interface_v1.Input.Ruleset.input o XMLReader.read_from_file)
  "rulesets/basic.rules";

(* write out the ruleset to the new format. *)
val _ = Testing.test "Write (v2): rulesets/basic.rules_v2" 
  (XMLWriter.write_to_file "rulesets/basic.rules_v2" 
   o Test_IO_Interface_v2.Output.Ruleset.output)
  ruleset;

val _ = Testing.test "Read (v2): rulesets/basic.rules_v2" 
  (Test_IO_Interface_v2.Input.Ruleset.input o XMLReader.read_from_file) 
  "rulesets/basic.rules_v2";
in 
  val _ = Testing.assert_no_failed_tests();
end;
(* val _ = (RG_IO_Interface.Input.Ruleset.input o XMLReader.read_from_file) "test/rulesets/test.rules"; *)
