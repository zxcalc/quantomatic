
signature CONSOLE_INTERFACE
= sig
  val run_in_textstreams : TextIO.instream * TextIO.outstream -> unit 
  val run_in_stdio : unit -> unit
end;


(* *)
functor ConsoleInterface : CONSOLE_INTERFACE = 
struct 
  datatype consolestate = NORMAL of state | SKIP | QUIT;

  fun help1 "quit" = "\nquit\n\n    Quits the quantomatic core\n\n"
    | help1 cmd = case Symtab.lookup Commands.cmds_tab cmd
           of SOME (h,_) => "\n"^h^"\n"
            | NONE => "!!! Can't find command.\n";
  
  val sort_alpha = sort (fn x => rev_order (string_ord x));
  
  (* helper functions for commands registered below *)
  fun help [] = "Type \"help CMD\" to get help on a command or \"help all\".\n"^
            "Available commands:\n\n"^
            (fold (fn a => fn b =>a^"\n"^b)
              (sort_alpha ("quit" :: (Symtab.keys Commands.cmds_tab))) "\n")
    | help ["all"] = fold (fn a => fn b => (help1 a)^b)
                          (sort_alpha ("quit" :: (Symtab.keys Commands.cmds_tab))) "\n"
    | help [cmd] = (help1 cmd)
    | help _ = raise ControllerState.wrong_args_exp ();

  fun do_cmd ("quit",_) _ = ("Exiting...\n", QUIT)
    | do_cmd ("exit",_) _ = ("Exiting...\n", QUIT)
    | do_cmd ("NOOP",_) _ = ("", SKIP)
    | do_cmd (cmd as (cname,args)) state =
      (case Symtab.lookup Commands.cmds_tab cname
        of SOME (_,f) => (fn (a,b)=>(a, NORMAL b)) (f (args, state))
         | NONE => ("!!! Unknown command: "^
            (CommandLexer.toString cmd)^"\n", NORMAL state))
      handle ControllerState.wrong_args_exp () =>
         ("!!! Wrong number of args in " ^
          (CommandLexer.toString cmd)^".\n", NORMAL state)
       | user_exp str => ("!!! "^str^"\n", NORMAL state)
  
  fun eval_commands_string commands_string state =
      let
        val cmds = SOME (CommandLexer.parse commands_string)
                   handle Scan.FAIL => NONE 
                        | Scan.MORE => NONE 
                        | Scan.ABORT => NONE; 
        fun eval_list [] state = ([], NORMAL state)
          | eval_list (c::cs) state =
            case do_cmd c state
             of (_, SKIP) => eval_list cs state
              | (out, QUIT) => ([out], QUIT)
              | (out, NORMAL state') =>
                  let val (lst, st) = eval_list cs state'
                  in (out::lst, st)
                  end
      in case cmds of SOME cs => eval_list cs state
            | NONE => (["!!! Parse error in command: "^command^"\n"], SKIP)
      end
      
  (* Uniquely mark end of text as SPACE + BACKSPACE. This is well-behaved as
   * long as commands never send a literal BACKSPACE (0x08). *)
  val EOT = " "^(chr 8);

  (* main read-exec-print-loop *)
  fun run_in_textstreams (instream,outstream) = 
    let fun print s = (TextIO.output (outstream, s); TextIO.flushOut outstream);
        fun read_line () = TextIO.inputLine instream
        val cur_state = ref ControllerState.initState;
        fun repl_loop () = 
            let
              val _ = print ("quanto:> "^EOT)
            in 
            case read_line ()
             of NONE => (print "Exiting...\n"; print EOT; ())
              | SOME s =>
                let 
                  val (output, st) = eval_commands_string (unsuffix "\n" s) state
                in
                  print (implode output); print EOT;
                  (case st
                    of NORMAL state' => (cur_state := state'; repl_loop ())
                     | SKIP => repl_loop ()
                     | QUIT => (print EOT; ())) (* why do we print EOT again? *)
                end
            end;
     in repl_loop () end;
  
  fun flushIn () = case TextIO.canInput (TextIO.stdIn, 1)
                        of SOME n => (TextIO.input1 TextIO.stdIn;())
                         | NONE => ();
  
  fun run_in_stdio () = (flushIn(); run_in_textstreams (TextIO.stdIn,TextIO.stdOut));
end;


