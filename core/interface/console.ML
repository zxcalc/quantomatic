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
  val eval_command_string_for_gui : (Commands.request -> Commands.response) -> string -> InterfaceResult.result;
end;

structure Console : CONSOLE
= struct
  open InterfaceResult;
  val commands = ConsoleCommands.all_commands;

  val sort_alpha = sort (fn x => rev_order (string_ord x));
  val command_name_list = (sort_alpha ("help" :: (ConsoleCommands.get_command_names commands)));
  val command_name_list_for_console = (sort_alpha ("change_theory" :: "quit" :: command_name_list));

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
      handle G.no_such_vertex_exp (s, v, g) => 
        raise ControllerState.user_exp ("BUG: G.no_such_vertex_exp")
      | G.vname_already_exists_exp (v,g) =>
        raise ControllerState.user_exp ("BUG: G.vname_already_exists_exp")
      | G.no_such_edge_exp (s, e, g) =>
        raise ControllerState.user_exp ("BUG: G.no_such_edge_exp")
      | G.ename_already_exists_exp (e,g) => 
        raise ControllerState.user_exp ("BUG: G.ename_already_exists_exp")
      | G.merge_bbox_exp s => 
        raise ControllerState.user_exp ("BUG: G.merge_bbox_exp")
      | G.overlapping_bbox_exp (vs,g) =>
        raise ControllerState.user_exp ("BUG: G.overlapping_bbox_exp")
      | B.Tab.UNDEF x =>
        raise ControllerState.user_exp ("BUG: B.Tab.UNDEF")
      | V.NSet.duplicate_exp _ => 
        raise ControllerState.user_exp ("BUG: Duplicate vertex.")
      | V.Tab.UNDEF x =>
        raise ControllerState.user_exp ("BUG: V.Tab.UNDEF")
      | E.NSet.duplicate_exp _ => 
        raise ControllerState.user_exp ("BUG: Duplicate EData.")
      | E.Tab.UNDEF x =>
        raise ControllerState.user_exp ("BUG: E.Tab.UNDEF")
      | V.Rnm.invalid_renaming_exp x =>
        raise ControllerState.user_exp ("BUG: invalid vertex renaming: "^x)
      | E.Rnm.invalid_renaming_exp x =>
        raise ControllerState.user_exp ("BUG: invalid bangbox renaming: "^x)
      | B.Rnm.invalid_renaming_exp x =>
        raise ControllerState.user_exp ("BUG: invalid edge renaming: "^x);
*)

  fun do_cmd _ ("help", args) = (NORMAL (help command_name_list args))
    | do_cmd serviceRequest (cmd as (cname,args)) = (NORMAL (
      (case ConsoleCommands.get_command commands cname
        of SOME f => ConsoleCommands.parse_response (serviceRequest (f args))
         | NONE => ("!!! Unknown command: "^
            (ConsoleLexer.toString cmd)))
      ))

  val error_msg = "!!! Parse error in command."

  exception bang of unit;

  fun eval_command_string serviceRequest command_string =
    let
      fun do_cmd_for_console ("quit",_) = QUIT
        | do_cmd_for_console ("exit",_) = QUIT
        | do_cmd_for_console ("crashme",_) = raise bang ()
        | do_cmd_for_console ("change_theory", [th]) = (CHANGE_THEORY th)
        | do_cmd_for_console ("change_theory", _) = (NORMAL (ConsoleCommands.parse_response ConsoleCommands.wrong_number_of_args_response))
        | do_cmd_for_console ("help",["quit"]) = (NORMAL ("\nquit\n    Quits the quantomatic core\n"))
        | do_cmd_for_console ("help",["exit"]) = (NORMAL ("\nexit\n    Quits the quantomatic core\n"))
        | do_cmd_for_console ("help",args) = (NORMAL (help
            command_name_list_for_console args))
        | do_cmd_for_console c = (do_cmd serviceRequest c)
    in
      (case (ConsoleLexer.parse command_string)
         of SOME c => (do_cmd_for_console c)
          | NONE => SKIP)
       handle Fail _ => (NORMAL error_msg)
            | ConsoleLexer.parse_exp () => (NORMAL error_msg)
    end

  fun eval_command_string_for_gui serviceRequest command_string =
    (case (ConsoleLexer.parse command_string)
       of SOME ("crashme",_) => (raise bang ())
        | SOME c => (do_cmd serviceRequest c)
        | NONE => SKIP)
     handle Fail _ => (NORMAL error_msg)
          | ConsoleLexer.parse_exp () => (NORMAL error_msg)
end;
