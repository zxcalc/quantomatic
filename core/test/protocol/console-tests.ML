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

(* vi:et:sw=2:sts=2
*)
