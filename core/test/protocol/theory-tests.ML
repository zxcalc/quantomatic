CoreTester.registerTest "Change theory (invalid_theory)" (fn session => (
  CoreTester.writeRequestByParts session ("TS","apple") "invalid_theory";
  CoreTester.demandErrorResponse session "apple" "BADTHEORY"
));

CoreTester.registerTest "Change and get theory (red_green)" (fn session => (
  CoreTester.writeRequestByParts session ("TS","pear") "red_green";
  CoreTester.demandOkResponse session "pear";
  CoreTester.writeRequestByParts session ("TG","banana") "";
  CoreTester.demandNameResponse session "banana" "red_green"
));

(* vi:et:sw=2:sts=2
*)

