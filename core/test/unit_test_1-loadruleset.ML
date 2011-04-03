
(* do a basic test to load up a ruleset that requires some renaming of rhs vertices. *)
val _ = (RG_IO_Interface.Input.Ruleset.input o XMLReader.read_from_file) "test/rulesets/test.rules";
