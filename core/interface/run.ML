
(* top level run function for controller/interface protocol *)
fun run () =
let
  val argList = CommandLine.arguments();
in
  if List.exists (fn "--version" => true | "-v" => true | _ => false) argList then
    TextIO.print "Quantomatic 1.99\n"
  else if List.exists (fn "--help" => true | "-h" => true | _ => false) argList then
    (
    TextIO.print "Usage: core [OPTIONS ...]\n";
    TextIO.print "\n";
    TextIO.print "  -v, --version     Print the version of the quantomatic core and exit\n";
    TextIO.print "  -h, --help        Print this help and exit\n";
    TextIO.print "      --protocol    Run the tool communication protocol\n"
    )
  else if List.exists (fn s => s = "--protocol") argList then
    ProtocolInterface.run_in_stdio ()
  else
    ConsoleInterface.run_in_stdio ();
  OS.Process.exit OS.Process.success
end;

(* vi:ts=2:sw=2:sts=2:et
*)
