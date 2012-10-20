(* STUB *)
fun run () = (
	TextIO.print "test";
  ControllerProtocol.run_in_stdio ();
  OS.Process.exit OS.Process.success
)

