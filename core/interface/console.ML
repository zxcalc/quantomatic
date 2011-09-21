structure InterfaceResult =
struct
  datatype result = NORMAL of string | CHANGE_THEORY of string | SKIP | QUIT;
end;

signature CONSOLE =
sig
  val command_name_list : string list;
  val get_command_args : string -> string option;
  val get_command_help : string -> string option;
  val eval_command_string : (Commands.request -> Commands.response) -> string -> InterfaceResult.result;
  val eval_command_string_no_quit : (Commands.request -> Commands.response) -> string -> InterfaceResult.result;
end;

structure Console : CONSOLE
= struct
  open InterfaceResult;
  val commands = ConsoleCommands.all_commands;

  val sort_alpha = sort (fn x => rev_order (string_ord x));
  val command_name_list = (sort_alpha (ConsoleCommands.get_command_names commands));
  val command_name_list_with_quit = (sort_alpha ("quit" :: (ConsoleCommands.get_command_names commands)));

  fun get_command_args "help" = SOME "CMD"
    | get_command_args "change_theory" = SOME "THEORY"
    | get_command_args cmd    = ConsoleCommands.get_args commands cmd;

  fun get_command_help "help" = SOME "Get help on CMD"
    | get_command_help "change_theory" = SOME "Switch to theory THEORY"
    | get_command_help cmd    = ConsoleCommands.get_help commands cmd;

  fun help cns [] = "Type \"help CMD\" to get help on a command.\n"^
                    "Available commands:\n"^
                    (fold (fn a => fn b =>"\n"^a^b) cns "")
    | help _   [cmd] =
        (case (get_command_help cmd)
          of SOME h => let val args = (the (get_command_args cmd)) in
                         ("\n"^cmd^" "^args^"\n    "^h^"\n")
                       end
           | NONE   => ("!!! No command called '"^cmd))
    | help _   _ = "!!! Wrong number of arguments";

(*
  structure G = Commands.ControllerState.Theory.Graph;
  fun handle_bug_in_function f args = f args
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

  fun do_cmd _ ("help", args) = (NORMAL (help command_name_list args))
    | do_cmd _ ("change_theory", [th]) = (CHANGE_THEORY th)
    | do_cmd _ ("change_theory", _) = (NORMAL (ConsoleCommands.parse_response ConsoleCommands.wrong_number_of_args_response))
    | do_cmd serviceRequest (cmd as (cname,args)) = (NORMAL (
      (case ConsoleCommands.get_command commands cname
        of SOME f => ConsoleCommands.parse_response (serviceRequest (f args))
         | NONE => ("!!! Unknown command: "^
            (ConsoleLexer.toString cmd)))
      ))

  val error_msg = "!!! Parse error in command."

  fun eval_command_string serviceRequest command_string =
    let
      fun do_cmd_with_quit ("quit",_) = QUIT
        | do_cmd_with_quit ("exit",_) = QUIT
        | do_cmd_with_quit ("help",["quit"]) = (NORMAL ("\nquit\n    Quits the quantomatic core\n"))
        | do_cmd_with_quit ("help",["exit"]) = (NORMAL ("\nexit\n    Quits the quantomatic core\n"))
        | do_cmd_with_quit ("help",args) = (NORMAL (help command_name_list_with_quit args))
        | do_cmd_with_quit c = (do_cmd serviceRequest c)
    in
      (case (ConsoleLexer.parse command_string)
         of SOME c => (do_cmd_with_quit c)
          | NONE => SKIP)
       handle Fail _ => (NORMAL error_msg)
            | ConsoleLexer.parse_exp () => (NORMAL error_msg)
    end

  fun eval_command_string_no_quit serviceRequest command_string =
    (case (ConsoleLexer.parse command_string)
       of SOME c => (do_cmd serviceRequest c)
        | NONE => SKIP)
     handle Fail _ => (NORMAL error_msg)
          | ConsoleLexer.parse_exp () => (NORMAL error_msg)
end;
