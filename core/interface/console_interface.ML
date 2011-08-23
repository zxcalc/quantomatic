
signature CONTROL_INTERFACE
= sig
  val run_in_textstreams : TextIO.instream * TextIO.outstream -> unit 
  val run_in_stdio : unit -> unit
end;


(* *)
structure ConsoleInterface : CONTROL_INTERFACE = 
struct 

  structure RG_Controller = Controller(ControllerState);

  fun run_in_textstreams' cmd_eval (instream,outstream) = 
    let fun print s = (TextIO.output (outstream, s); TextIO.flushOut outstream);
				fun next_loop () = run_in_textstreams' cmd_eval (instream,outstream)
				fun change_loop svcReq = run_in_textstreams' (Console.eval_command_string svcReq) (instream,outstream)
				val _ = print ("quanto:> ")
				val input = TextIO.inputLine instream
     in
				case input
				 of NONE => (print "\n"; ())
					| SOME s =>
						(case cmd_eval (unsuffix "\n" s)
							of InterfaceResult.NORMAL output =>
									(print (output^"\n"); next_loop ())
							 | InterfaceResult.CHANGE_THEORY "red_green" =>
									(change_loop RG_Controller.serviceRequest)
							 | InterfaceResult.CHANGE_THEORY th =>
									(print ("!!! Unknown theory \""^th^"\"\n"); next_loop ())
							 | InterfaceResult.SKIP => (next_loop ())
							 | InterfaceResult.QUIT => ())
     end;

  val run_in_textstreams = run_in_textstreams' (Console.eval_command_string RG_Controller.serviceRequest)
  
  fun flushIn () = case TextIO.canInput (TextIO.stdIn, 1)
                        of SOME n => (TextIO.input1 TextIO.stdIn;())
                         | NONE => ();
  
  fun run_in_stdio () = (flushIn(); run_in_textstreams (TextIO.stdIn,TextIO.stdOut));
end;
