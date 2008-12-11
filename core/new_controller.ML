
structure NewController =
struct

structure QuantoCommand =
struct
type T = string * string list
exception Parse

open SimpleLexer

fun const v = fn _ => v
fun unwrap (LIT s) = s
  | unwrap (ID s) = s

(* a term grammar with no nesting *)
fun arg x = x|>((literal || ident)>>unwrap)
fun args x = x|>(((arg -- args) >> op::)
		     || (noop >> const []));
fun expr x = x|>(
	     ((ident >> unwrap) -- args)
		 || (noop >> const ("NOOP",[]))
	     )
fun parse str = parse_with expr str
    handle _ => raise Parse
fun pretty (f,args) = Pretty.block[Pretty.str f,
				   Pretty.str_list "(" ")" args]
val print = Pretty.writeln o pretty
val toString = Pretty.string_of o pretty
end;


type graphstate = (RGGraph.T list) StrName.NTab.T
datatype consolestate = NORMAL of graphstate | SKIP | QUIT


(*
A dictionary of commands of the format:
  (help: string, cmd: args * state -> output * state)
*)
val cmds :
    ((string *
     (string list * graphstate -> string * graphstate))
	Symtab.table) ref = ref Symtab.empty

fun register_cmd name help f =
    cmds := ((!cmds) |> Symtab.update (name,(help,f)))


local
    (* wrap commands of the form ([args] * graph) -> (out * graph),
       where the graph is chosen from the first argument. *)
    fun lift_graph_mutator cmd ((graph_name::args), graph_table) =
	(case StrName.NTab.lookup graph_table graph_name
	  of SOME (g::gs) => let val (out,g') = cmd (args,g)
			     in (out,
				 graph_table |> StrName.NTab.update
					     (graph_name, g'::g::gs))
			     end
	   | _ => ("Cannot lookup graph", graph_table))
      | lift_graph_mutator _ (_, state) =
	("Graph command needs at least one argument", state)

    (* wrap commands of the form [args] -> out *)
    fun lift_stateless cmd (args, state) = (cmd args, state)

    fun help [] = "Type \"help CMD\" to get help on a command.\n"
      | help [cmd] = case Symtab.lookup (!cmds) cmd
		      of SOME (h,_) => h
		       | NONE => "Can't find command.\n"
in
val _ = register_cmd
	    "help"
	    ("\nhelp CMD\n\n"^
	     "    get help on CMD\n\n")
	    (lift_stateless help)

val _ = register_cmd
	    "test"
	    ("\ntest\n\n"^
	     "    a test command\n\n")
	    (fn ([], state) => ("Test command\n", state))
end


fun do_cmd ("quit",_) _ = ("Exiting...\n", QUIT)
  | do_cmd ("NOOP",_) _ = ("", SKIP)
  | do_cmd (cmd as (cname,args)) state =
    (case Symtab.lookup (!cmds) cname
      of SOME (_,f) => (fn (a,b)=>(a, NORMAL b)) (f (args, state))
       | NONE => ("Unknown command: "^
		  (QuantoCommand.toString cmd)^"\n", SKIP))
    handle Match =>
	   ("Wrong number of args for " ^
	    (QuantoCommand.toString cmd)^"\n", SKIP)

fun repl state = let
    val _ = TextIO.print "quanto:> "
    val cmd_str = unsuffix "\n" (the (TextIO.inputLine TextIO.stdIn))
    val cmd = QuantoCommand.parse cmd_str
	handle Parse => (TextIO.print ("Parse error in command: \""^cmd_str^"\"\n");
			 ("NOOP",[]))
    val (outpt,st) = do_cmd cmd state
in (TextIO.print outpt;
    case st
     of NORMAL state' => repl state'
      | SKIP => repl state
      | QUIT => 0)
end

fun init () = repl StrName.NTab.empty;

end (* structure NewController *)
