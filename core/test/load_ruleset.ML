(* do a basic test to load up a ruleset that requires some renaming of 
   rhs vertices. *)
val ruleset = (Test_IO_Interface_v1.Input.Ruleset.input 
               o XMLReader.read_from_file) "rulesets/test.rules";

(* write out the ruleset to the new format. *)
val _ = (XMLWriter.write_to_file "rulesets/test.rules_v2"
         o Test_IO_Interface_v2.Output.Ruleset.output) ruleset;

(* read version 2 ruleset back in *)
val ruleset2 = (Test_IO_Interface_v2.Input.Ruleset.input 
               o XMLReader.read_from_file) "rulesets/test.rules_v2";

(* val _ = (RG_IO_Interface.Input.Ruleset.input o XMLReader.read_from_file) "test/rulesets/test.rules"; *)
