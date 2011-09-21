CoreTester.registerTest "Get console commands" (fn session => (
  let
    val () = CoreTester.writeRequestByParts session ("CL","ada") ""
    val commands = CoreTester.readNameListResponse session "ada"
    fun eqTest a b = (a = b)
    fun checkContains value =
        if (List.find (eqTest value) commands) = NONE then
          raise CoreTester.test_exp (value^" was not in the list of commands")
        else ()
  in
    checkContains "list_graphs";
    checkContains "new_graph";
    checkContains "print_graph";
    checkContains "apply_rewrite"
  end
));

CoreTester.registerTest "Get console help" (fn session => (
  CoreTester.writeRequestByParts session ("CH","fortran") "untag_rule";
  CoreTester.demandConsoleHelpResponse session "fortran" ("RULE TAG", "Remove TAG from RULE");
  CoreTester.writeRequestByParts session ("CH","c++") "list_rules";
  CoreTester.demandConsoleHelpResponse session "c++" ("", "List all loaded rules")
));

CoreTester.registerTest "Run console commands" (fn session => (
  CoreTester.writeRequestByParts session ("CC","pascal")
    (CoreTester.dataChunk "change_theory \"red_green\"");
  CoreTester.demandConsoleResponse session "pascal" "ok";
  CoreTester.writeRequestByParts session ("CC","js")
    (CoreTester.dataChunk "new_graph");
  CoreTester.demandConsoleResponse session "js" "new-graph-1";
  CoreTester.writeRequestByParts session ("CC","perl")
    (CoreTester.dataChunk "rename_graph \"new-graph-1\" \"my-little-graph\"");
  CoreTester.demandConsoleResponse session "perl" "my-little-graph";
  (* check it has updated the state properly *)
  CoreTester.writeRequestByParts session ("GL", "ruby") "";
  CoreTester.demandNameListResponse session "ruby" ["my-little-graph"];

  CoreTester.writeRequestByParts session ("CC","objc")
    (CoreTester.dataChunk "rename_graph foo bar");
  CoreTester.demandConsoleResponse session "objc" "!!! No such graph \"foo\""
));

(* vi:et:sw=2:sts=2
*)
