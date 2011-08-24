use "test/protocol/core-tester.ML";

use "test/protocol/simple-test.ML";
use "test/protocol/theory-tests.ML";
use "test/protocol/console-tests.ML";

fun updateCoreExec [] = CoreTester.setCoreExecutable "bin/quanto-core"
  | updateCoreExec ("--core"::ce::ss) = CoreTester.setCoreExecutable ce
  | updateCoreExec (s::ss) = updateCoreExec ss;

fun updateLogFile [] = ()
  | updateLogFile ("--log"::lf::ss) = CoreTester.setLogFile lf
  | updateLogFile (s::ss) = updateCoreExec ss;

val _ = updateCoreExec (CommandLine.arguments());
val _ = updateLogFile (CommandLine.arguments());

val _ = TextIO.print "\n\n";

val _ = OS.Process.exit (
  if CoreTester.runRegisteredTests () then
   (TextIO.print "\nAll tests passed\n";
    OS.Process.success)
  else
   (TextIO.print "\nSome tests failed\n";
    OS.Process.failure))
