(* Main loop, with support for old protocol handlers *)

fun main () =
let
  val argList = CommandLine.arguments();
in
  if List.exists (fn "--version" => true | "-v" => true | _ => false) argList then
    TextIO.print "Quantomatic 2.0\n"
  else if List.exists (fn "--help" => true | "-h" => true | _ => false) argList then
    (
    TextIO.print "Usage: quanto-core [OPTIONS ...]\n";
    TextIO.print "\n";
    TextIO.print "  -v, --version             Print the version of the quantomatic core and exit\n";
    TextIO.print "  -h, --help                Print this help and exit\n";
    TextIO.print "      --protocol            Run the (old) tool communication protocol\n";
    TextIO.print "      --json-protocol       Run the (sequential) JSON protocol\n";
    TextIO.print "      --par-json-protocol   Run the parallel JSON protocol\n";
    TextIO.print "      --socket              Use localhost:4321 instead of stdio\n"
    )
  else if List.exists (fn s => s = "--par-json-protocol") argList then
    if List.exists (fn s => s = "--socket") argList
    then let
           val _ = TextIO.print "waiting for connection...\n"
           val s = TextSocket.local_connect 4321
           val _ = TextIO.print "connected\n"
           val (ins, outs) = TextSocket.get_io_stream s
           val _ = JsonControllerProtocol.parallel_run_in_textstreams (ins, outs)
           val _ = TextSocket.close s
          in ()
          end
    else JsonControllerProtocol.parallel_run_in_stdio ()
  else if List.exists (fn s => s = "--json-protocol") argList then
    if List.exists (fn s => s = "--socket") argList
    then let
           val s = TextSocket.local_connect 4321
           val (ins, outs) = TextSocket.get_io_stream s
           val _ = JsonControllerProtocol.run_in_textstreams (ins, outs)
           val _ = TextSocket.close s
          in ()
          end
    else JsonControllerProtocol.run_in_stdio ()
  else if List.exists (fn s => s = "--protocol") argList then
    ProtocolInterface.run_in_stdio ()
  else
    ConsoleInterface.run_in_stdio ();
  OS.Process.exit OS.Process.success
end;

(* vi:ts=2:sw=2:sts=2:et
*)
