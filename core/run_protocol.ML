PolyML.SaveState.loadState "heaps/quanto.heap";
PolyML.Compiler.printDepth:=100;

(* Futures may hang unless we allow at least 4 workers *)
if (!Multithreading.max_threads <= 4) then Multithreading.max_threads := 4 else ();

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

fun poll_future f = (
  OS.Process.sleep (Time.fromMilliseconds 100);
  case Future.peek f
    of SOME (Exn.Exn e) => raise e
     | _ => ();
  OS.Process.sleep (Time.fromMilliseconds 100);
  case Future.peek f
    of SOME (Exn.Exn e) => raise e
     | _ => ())

(*val protocol_job = Future.fork (run_protocol 4321);
poll_future protocol_job;*)

