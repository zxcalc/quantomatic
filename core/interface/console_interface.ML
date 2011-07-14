
signature CONSOLE_INTERFACE
= sig
  val run_in_textstreams : TextIO.instream * TextIO.outstream -> unit 
  val run_in_stdio : unit -> unit
end;


(* *)
structure ConsoleInterface : CONSOLE_INTERFACE = 
struct 
  datatype consolestate = NORMAL of ControllerState.T | SKIP | QUIT;
  structure G = ControllerState.Theory.Graph;
(* 
  fun read_block readlinef () = 
      let
        val start_parser = Scan.catch SimpleLexer.sym "---startblock:" |-- SimpleLexer.num
        val expected = Int.toString
               (case SimpleLexer.parse_with start_parser (the (readlinef ()))
           of SimpleLexer.INT n => n
            | _ => raise ControllerState.user_exp "Bad start of block")
        fun loop s = 
            let val ln = the (readlinef ())
            in if ln = ("---endblock:"^expected^"\n") 
               then s else loop (ln^s) end;
      in loop "" end
      handle Fail _ => raise ControllerState.user_exp "Bad start of block"
           | Option.Option => raise ControllerState.user_exp "Stream ended in block";

  fun echo_block [] = read_block ()
    | echo_block _ = raise wrong_args_exp ();
*)

  val commands = 
      Commands.all_commands 
      (*   |> Commands.add_command 
            ("echo_block",
             ("echo_block\n\n"^
                "    debug command for testing block input\n",
            Commands.lift_stateless echo_block)) *)

  fun help1 "quit" = "\nquit\n\n    Quits the quantomatic core\n\n"
    | help1 "help" = "help CMD\n\n    get help on CMD\n"
    | help1 cmd = case Symtab.lookup commands cmd
           of SOME (h,_) => "\n"^h^"\n"
            | NONE => "!!! No command called '"^cmd^"'\n";
  
  val sort_alpha = sort (fn x => rev_order (string_ord x));
  
  (* helper functions for commands registered below *)
  fun help [] = "Type \"help CMD\" to get help on a command or \"help all\".\n"^
            "Available commands:\n\n"^
            (fold (fn a => fn b =>a^"\n"^b)
              (sort_alpha ("quit" :: (Symtab.keys commands))) "\n")
    | help ["all"] = fold (fn a => fn b => (help1 a)^b)
                          (sort_alpha ("quit" :: (Symtab.keys commands))) "\n"
    | help [cmd] = (help1 cmd)
    | help _ = raise ControllerState.wrong_args_exp ();


  fun handle_bug_in_function f (x as (l,st)) = (f x) 
      handle G.no_such_vname_exp (s, v, g) => 
        raise ControllerState.user_exp ("BUG: G.no_such_vname_exp")
      | G.vname_already_exists_exp (v,g) =>
        raise ControllerState.user_exp ("BUG: G.vname_already_exists_exp")
      | G.no_such_ename_exp (s, e, g) =>
        raise ControllerState.user_exp ("BUG: G.no_such_ename_exp")
      | G.ename_already_exists_exp (e,g) => 
        raise ControllerState.user_exp ("BUG: G.ename_already_exists_exp")
      | G.merge_bbox_exp s => 
        raise ControllerState.user_exp ("BUG: G.merge_bbox_exp")
      | G.overlapping_bbox_exp (vs,g) =>
        raise ControllerState.user_exp ("BUG: G.overlapping_bbox_exp")
      | G.BBox.Tab.UNDEF x =>
        raise ControllerState.user_exp ("BUG: G.BBox.Tab.UNDEF")
      | G.Vertex.NSet.duplicate_exp _ => 
        raise ControllerState.user_exp ("BUG: Duplicate vertex.")
      | G.Vertex.Tab.UNDEF x =>
        raise ControllerState.user_exp ("BUG: G.Vertex.Tab.UNDEF")
      | G.Edge.NSet.duplicate_exp _ => 
        raise ControllerState.user_exp ("BUG: Duplicate Edge.")
      | G.Edge.Tab.UNDEF x =>
        raise ControllerState.user_exp ("BUG: G.Edge.Tab.UNDEF")
      | G.Vertex.Rnm.invalid_renaming_exp x =>
        raise ControllerState.user_exp ("BUG: invalid vertex renaming: "^x)
      | G.Edge.Rnm.invalid_renaming_exp x =>
        raise ControllerState.user_exp ("BUG: invalid bangbox renaming: "^x)
      | G.BBox.Rnm.invalid_renaming_exp x =>
        raise ControllerState.user_exp ("BUG: invalid edge renaming: "^x);

  fun response_string (ControllerState.OK) = "ok\n"
    | response_string (ControllerState.COUNT n) = (Int.toString n)^"\n"
    | response_string (ControllerState.NAME str) = str^"\n"
    | response_string (ControllerState.NAME_LIST xs) = (fold (suffix o (suffix "\n")) xs "")^"\n"
    | response_string (ControllerState.REWRITE_LIST rs) = (XML.string_of (RG_IO_Interface.Output.RewriteList.output rs))^"\n"
    | response_string (ControllerState.DATA str) = str^"\n"
  
  fun do_cmd ("quit", _) _ = ("", QUIT)
    | do_cmd ("exit", _) _ = ("", QUIT)
    | do_cmd ("help", args) _ = (help args, SKIP)
    | do_cmd ("NOOP", _) _ = ("", SKIP)
    | do_cmd (cmd as (cname,args)) state =
      (case Symtab.lookup commands cname
        of SOME (_,f) => (fn (a,b)=>(response_string a, NORMAL b)) (handle_bug_in_function f (args, state))
         | NONE => ("!!! Unknown command: "^
            (CommandLexer.toString cmd)^"\n", NORMAL state))
      handle ControllerState.wrong_args_exp () =>
         ("!!! Wrong number of args in " ^
          (CommandLexer.toString cmd)^".\n", NORMAL state)
       | ControllerState.user_exp str => ("!!! "^str^"\n", NORMAL state)
  
  fun eval_commands_string commands_string state =
      let
        val cmds = SOME (CommandLexer.parse commands_string)
                   handle Fail _ => NONE
		        | CommandLexer.parse_exp () => NONE;
        fun eval_list [] state = ([], NORMAL state)
          | eval_list (c::cs) state =
            case do_cmd c state
             of (out, SKIP) =>
		  let val (lst, st) = eval_list cs state
                  in (out::lst, st)
                  end
              | (out, QUIT) => ([out], QUIT)
              | (out, NORMAL state') =>
                  let val (lst, st) = eval_list cs state'
                  in (out::lst, st)
                  end
      in case cmds of SOME cs => eval_list cs state
            | NONE => (["!!! Parse error in command.\n"], SKIP)
      end
      
  (* Uniquely mark end of text as SPACE + BACKSPACE. This is well-behaved as
   * long as commands never send a literal BACKSPACE (0x08). *)
  val EOT = " "^(chr 8);

  (* main read-exec-print-loop *)
  fun run_in_textstreams (instream,outstream) = 
    let fun print s = (TextIO.output (outstream, s); TextIO.flushOut outstream);
        fun read_line () = TextIO.inputLine instream
        val cur_state = Unsynchronized.ref ControllerState.init;
        fun repl_loop () = 
            let
              val _ = print ("quanto:> "^EOT)
            in 
            case read_line ()
             of NONE => (print "\n"; print EOT; ())
              | SOME s =>
                let 
                  val (output, st) = eval_commands_string (unsuffix "\n" s) (!cur_state)
                in
                  print (String.concat output); print EOT;
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


(* top level run function for controller/interface protocol *)
val run = ConsoleInterface.run_in_stdio;
