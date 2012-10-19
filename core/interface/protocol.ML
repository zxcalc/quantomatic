signature CONTROLLER_PROTOCOL =
sig
  val run_in_textstreams : TextIO.instream * TextIO.outstream -> unit 
  val run_in_stdio : unit -> unit
end

structure ControllerProtocol =
struct
  fun run_in_textstreams _ = ()
  fun flushIn () = case TextIO.canInput (TextIO.stdIn, 1)
                     of SOME _ => (TextIO.input1 TextIO.stdIn;())
                      | NONE => ();

  fun run_in_stdio () = (flushIn(); run_in_textstreams (TextIO.stdIn,TextIO.stdOut)); 
end