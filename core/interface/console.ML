structure ConsoleResult =
struct
  datatype 'a result = NORMAL of (string * 'a) | QUIT;
end;

signature CONSOLE =
sig
  type state;
  val command_name_list : string list;
  val get_command_args : string -> string option;
  val get_command_help : string -> string option;
  val eval_command_string : string -> state -> (state ConsoleResult.result)
  val eval_command_string_no_quit : string -> state -> (string * state)
end;

functor Console(Commands : COMMANDS) : CONSOLE
= struct
  type state = Commands.state;
  type result = (state ConsoleResult.result)
  structure ConsoleCommands = ConsoleCommands(Commands);
  val commands = ConsoleCommands.all_commands;
  
  val sort_alpha = sort (fn x => rev_order (string_ord x));
  val command_name_list = (sort_alpha (ConsoleCommands.get_command_names commands));
  val command_name_list_with_quit = (sort_alpha ("quit" :: (ConsoleCommands.get_command_names commands)));
  val get_command_args = ConsoleCommands.get_args commands;
  val get_command_help = ConsoleCommands.get_help commands;

  fun help1 "help" = "\nhelp CMD\n\n    get help on CMD\n\n"
    | help1 cmd    =
        case (ConsoleCommands.get_help commands cmd)
          of SOME h => let val args = (the (ConsoleCommands.get_args commands cmd)) in
                         ("\n"^cmd^" "^args^"\n\n    "^h^"\n\n")
                       end
           | NONE   => ("!!! No command called '"^cmd^"'\n")

  fun help cns [] = "Type \"help CMD\" to get help on a command.\n"^
                    "Available commands:\n\n"^
                    (fold (fn a => fn b =>a^"\n"^b) cns "\n")
    | help _   [cmd] = (help1 cmd)
    | help _   _ = "!!! Wrong number of arguments";

(*
  structure G = Commands.ControllerState.Theory.Graph;
  fun handle_bug_in_function f args state = f args state
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
*)

  fun do_cmd ("help", args) state = (help command_name_list args, state)
    | do_cmd (cmd as (cname,args)) state =
      (case ConsoleCommands.get_command commands cname
        of SOME f => (f args state)
         | NONE => ("!!! Unknown command: "^
            (ConsoleLexer.toString cmd)^"\n", state))
      handle ControllerState.user_exp str => ("!!! "^str^"\n", state)

  val error_msg = "!!! Parse error in command.\n"

  fun eval_command_string command_string state =
    let
      fun do_cmd_with_quit ("quit",_) = ConsoleResult.QUIT
        | do_cmd_with_quit ("exit",_) = ConsoleResult.QUIT
        | do_cmd_with_quit ("help",["quit"]) = (ConsoleResult.NORMAL ("\nquit\n\n    Quits the quantomatic core\n\n",state))
        | do_cmd_with_quit ("help",["exit"]) = (ConsoleResult.NORMAL ("\nexit\n\n    Quits the quantomatic core\n\n",state))
        | do_cmd_with_quit ("help",args) = (ConsoleResult.NORMAL (help command_name_list_with_quit args,state))
        | do_cmd_with_quit c = (ConsoleResult.NORMAL (do_cmd c state))
    in
      (case (ConsoleLexer.parse command_string)
         of SOME c => (do_cmd_with_quit c)
          | NONE => (ConsoleResult.NORMAL ("", state)))
       handle Fail _ => (ConsoleResult.NORMAL (error_msg, state))
            | ConsoleLexer.parse_exp () => (ConsoleResult.NORMAL (error_msg, state))
    end

  fun eval_command_string_no_quit command_string state =
    (case (ConsoleLexer.parse command_string)
       of SOME c => (do_cmd c state)
        | NONE => ("", state))
     handle Fail _ => (error_msg, state)
          | ConsoleLexer.parse_exp () => (error_msg, state)
end;
