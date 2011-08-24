use "core-tester.ML";

use "simple-test.ML";
use "theory-tests.ML";
use "console-tests.ML";

val _ = OS.Process.exit (
  if CoreTester.runRegisteredTests () then
   (TextIO.print "All tests passed\n";
    OS.Process.success)
  else
   (TextIO.print "Some tests failed\n";
    OS.Process.failure))
