
signature CONSOLE_INTERFACE
= sig
  val run_in_textstreams : TextIO.instream * TextIO.outstream -> unit 
  val run_in_stdio : unit -> unit
end;


(* *)
structure ConsoleInterface : CONSOLE_INTERFACE = 
struct 

  structure RG_Controller = Controller(ControllerState);

  fun run_in_textstreams' cmd_eval (instream,outstream) = 
    let fun print s = (TextIO.output (outstream, s); TextIO.flushOut outstream);
        fun read_line () = TextIO.inputLine instream
        fun repl_loop () = 
            let
              val _ = print ("quanto:> ")
            in 
            case read_line ()
             of NONE => (print "\n"; ())
              | SOME s =>
                (case cmd_eval (unsuffix "\n" s)
                  of ConsoleResult.NORMAL output =>
                      (print (output^"\n"); repl_loop ())
                   | ConsoleResult.QUIT => ())
            end;
     in repl_loop () end;

  val run_in_textstreams = run_in_textstreams' (Console.eval_command_string RG_Controller.serviceRequest)
  
  fun flushIn () = case TextIO.canInput (TextIO.stdIn, 1)
                        of SOME n => (TextIO.input1 TextIO.stdIn;())
                         | NONE => ();
  
  fun run_in_stdio () = (flushIn(); run_in_textstreams (TextIO.stdIn,TextIO.stdOut));
end;


(* top level run function for controller/interface protocol *)
val run = ConsoleInterface.run_in_stdio;
