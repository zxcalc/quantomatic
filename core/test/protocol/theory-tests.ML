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

CoreTester.registerTest "Change and get theory (red_green_blue)" (fn session => (
  CoreTester.writeRequestByParts session ("TS","orange") "red_green_blue";
  CoreTester.demandOkResponse session "orange";
  CoreTester.writeRequestByParts session ("TG","kiwi") "";
  CoreTester.demandNameResponse session "kiwi" "red_green_blue"
));

CoreTester.registerTest "Change and get theory (ghz_w)" (fn session => (
  CoreTester.writeRequestByParts session ("TS","plum") "ghz_w";
  CoreTester.demandOkResponse session "plum";
  CoreTester.writeRequestByParts session ("TG","mango") "";
  CoreTester.demandNameResponse session "mango" "ghz_w"
));

(* vi:et:sw=2:sts=2
*)

