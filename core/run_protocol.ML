PolyML.SaveState.loadState "heaps/quanto.heap";
PolyML.Compiler.printDepth:=100;


fun run_protocol port () =
let
   val _ = TextIO.print "waiting for connection..."
   val s = TextSocket.local_connect port
   val _ = TextIO.print "got connection\n"
   val (ins, outs) = TextSocket.get_io_stream s
   val _ = JsonControllerProtocol.parallel_run_in_textstreams (ins, outs)
   val _ = TextSocket.close s
in ()
end

val protocol_job = Future.fork (run_protocol 4321);
(*OS.Process.sleep (Time.fromMilliseconds 200);
case Future.peek protocol_job
  of SOME (Exn.Exn e) => raise e
   | NONE => ();*)
