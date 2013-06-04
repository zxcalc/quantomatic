val _ = PolyML.Compiler.printDepth := 0;

val _ = TextIO.print "Testing the Quantomatic protocol:\n\n";

use "test/protocol/core-tester.ML";
use "test/protocol/test-utils.ML";

use "test/protocol/simple-test.ML";
use "test/protocol/theory-tests.ML";
use "test/protocol/console-tests.ML";
use "test/protocol/rg-graph-tests.ML";

fun updateCoreExec [] = setCoreExecutable "bin/quanto-core"
  | updateCoreExec ("--core"::ce::ss) = setCoreExecutable ce
  | updateCoreExec (s::ss) = updateCoreExec ss;

fun updateLogFile [] = NONE
  | updateLogFile ("--log"::lf::ss) = (setLogFile lf; SOME lf)
  | updateLogFile (s::ss) = updateLogFile ss;

val _ = updateCoreExec (CommandLine.arguments());
val logfile = updateLogFile (CommandLine.arguments());

val _ = OS.Process.exit (
  if runRegisteredTests () then
   (TextIO.print "\nAll tests passed\n";
    case logfile
      of SOME file => TextIO.print ("See "^file^" for details\n")
       | NONE => ();
    OS.Process.success)
  else
   (TextIO.print "\nSome tests failed\n";
    case logfile
      of SOME file => TextIO.print ("See "^file^" for details\n")
       | NONE => ();
    OS.Process.failure))
