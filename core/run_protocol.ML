PolyML.SaveState.loadState "heaps/quanto.heap";
PolyML.Compiler.printDepth:=100;


fun run_protocol () =
let
   val _ = TextIO.print "waiting for connection..."
   val s = TextSocket.local_connect 4321
   val _ = TextIO.print "got connection\n"
   val (ins, outs) = TextSocket.get_io_stream s
   val _ = JsonControllerProtocol.parallel_run_in_textstreams (ins, outs)
   val _ = TextSocket.close s
in ()
end

val protocol_job = Future.fork run_protocol

(* tell the front-end that we're ready to go *)
val _ = TextIO.print ("\n<"^"<[S]0>>\n")
