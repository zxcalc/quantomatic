(* Main loop, with support for old protocol handlers *)

fun run () =
let
  val argList = CommandLine.arguments();
in
  if List.exists (fn "--version" => true | "-v" => true | _ => false) argList then
    TextIO.print "Quantomatic 2.0\n"
  else if List.exists (fn "--help" => true | "-h" => true | _ => false) argList then
    (
    TextIO.print "Usage: core [OPTIONS ...]\n";
    TextIO.print "\n";
    TextIO.print "  -v, --version             Print the version of the quantomatic core and exit\n";
    TextIO.print "  -h, --help                Print this help and exit\n";
    TextIO.print "      --protocol            Run the (old) tool communication protocol\n";
    TextIO.print "      --json-protocol       Run the (sequential) JSON protocol\n";
    TextIO.print "      --par-json-protocol   Run the parallel JSON protocol\n"
    )
  else if List.exists (fn s => s = "--par-json-protocol") argList then
    JsonControllerProtocol.parallel_run_in_stdio ()
  else if List.exists (fn s => s = "--json-protocol") argList then
    JsonControllerProtocol.run_in_stdio ()
  else if List.exists (fn s => s = "--protocol") argList then
    ProtocolInterface.run_in_stdio ()
  else
    ConsoleInterface.run_in_stdio ();
  OS.Process.exit OS.Process.success
end
