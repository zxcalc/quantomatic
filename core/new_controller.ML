
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

structure GraphName = 
struct
open StrName
val default_name = "Ga"
end

structure G = RGGraph
structure V = G.RGVertex
structure E = G.Edge

type graphstate = (RGGraph.T list) GraphName.NTab.T
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
    (*
     * lift_XXX are the only commands that explicitly unwrap the
     * global state. Where applicable, the first argument is
     * interpreted as a graph name and the selected graph is passed
     * to "cmd".
     *)

    (* wrap commands of the form ([args] * graph_table) -> graph option *)
    fun lift_graph_creator cmd (args, graph_table) =
	case cmd (args, graph_table)
	 of SOME gr =>
	    let
		val (name,table') =
		    graph_table |> GraphName.NTab.add
			        (GraphName.default_name,
				 [gr])
	    in
		((Pretty.str_of (GraphName.pretty_name name)) ^ "\n",
		 table')
	    end
	  | NONE => ("!!! Cannot create graph\n", graph_table)

    (* wrap commands of the form [args] * graph -> out *)
    fun lift_graph_viewer cmd ((graph_name::args), graph_table) =
	(case GraphName.NTab.lookup graph_table graph_name
	  of SOME (g::gs) => (cmd (args,g), graph_table)
	   | NONE => ("!!! Cannot lookup graph\n", graph_table))
      | lift_graph_viewer _ (_,state) =
	("!!! Graph command needs at least one argument\n", state)

    (* wrap commands of the form ([args] * graph) -> (out * graph) *)
    fun lift_graph_mutator cmd ((graph_name::args), graph_table) =
	(case GraphName.NTab.lookup graph_table graph_name
	  of SOME (g::gs) => let val (out,g') = cmd (args,g)
			     in (out,
				 graph_table |> GraphName.NTab.update
				                  (graph_name, g'::g::gs))
			     end
	   | _ => ("!!! Cannot lookup graph\n", graph_table))
      | lift_graph_mutator _ (_, state) =
	("!!! Graph command needs at least one argument\n", state)
	
    (* wrap commands of the form [args] -> out *)
    fun lift_stateless cmd (args, state) = (cmd args, state)

    (* ============================================== *)

    (* helper functions for commands registered below *)
    fun help [] = "Type \"help CMD\" to get help on a command.\n"^
		  "Available commands:\n"^
		  (foldr (fn (a,b)=>"  "^a^"\n"^b) "\n" (Symtab.keys (!cmds)))
      | help [cmd] = case Symtab.lookup (!cmds) cmd
		      of SOME (h,_) => "\n"^h^"\n"
		       | NONE => "!!! Can't find command.\n"


    fun duplicate_graph ([gr], tab) = case GraphName.NTab.lookup tab gr
				       of SOME (g::gs) => SOME g
					| _ => NONE


    fun vertex_type "red" = V.IVert (V.Xnd AngleExpr.zero)
      | vertex_type "green" =  V.IVert (V.Znd AngleExpr.zero)
      | vertex_type "hadamard" = V.IVert V.Hnd
      | vertex_type "boundary" = V.BVert ()

    fun add_vertex ([vt], graph) =
	("ok\n", #2(graph |> G.add_vertex (vertex_type vt)))
	handle Match => ("!!! Bad vertex type.\n", graph)

    (* FIX: as far as I can tell, there's no good way to get a hold
     * of the correct "version" of no_such_vname_exp. *)
    fun add_edge ([v1,v2], graph) =
	("ok\n", #2(graph |> G.add_edge () v1 v2))
	handle _ => ("!!! Error adding edge, probably a bad vertex name.\n",
		     graph)
in
val _ = (
    register_cmd
	"help"
	("help CMD\n\n"^
	 "    get help on CMD\n")
	(lift_stateless help);

    register_cmd
	"test"
	("test\n\n"^
	 "    a test command\n")
	(lift_stateless (fn [] => "Test command\n"));

    register_cmd
	"new_graph"
	("new_graph\n\n" ^
	 "    create a new graph and return its name\n")
	(lift_graph_creator (fn ([],_) => SOME RGGraph.empty));

    register_cmd
	"duplicate_graph"
	("duplicate_graph GRAPH\n\n"^
	 "    duplicate GRAPH and return the name of the new graph\n")
	(lift_graph_creator duplicate_graph);

    register_cmd
	"add_vertex"
	("add_vertex GRAPH TYPE\n\n"^
	 "    add a TYPE vertex to GRAPH\n")
	(lift_graph_mutator add_vertex);

    register_cmd
	"add_edge"
	("add_edge GRAPH V1 V2\n\n"^
	 "    add an edge in GRAPH from V1 to V2\n")
	(lift_graph_mutator add_edge);

())
end


fun do_cmd ("quit",_) _ = ("Exiting...\n", QUIT)
  | do_cmd ("NOOP",_) _ = ("", SKIP)
  | do_cmd (cmd as (cname,args)) state =
    (case Symtab.lookup (!cmds) cname
      of SOME (_,f) => (fn (a,b)=>(a, NORMAL b)) (f (args, state))
       | NONE => ("!!! Unknown command: "^
		  (QuantoCommand.toString cmd)^"\n", SKIP))
    handle Match =>
	   ("!!! Wrong number of args in " ^
	    (QuantoCommand.toString cmd)^".\n", SKIP)

fun repl state = let
    val _ = TextIO.print "quanto:> "
    val cmd_str = unsuffix "\n" (the (TextIO.inputLine TextIO.stdIn))
    val cmd = QuantoCommand.parse cmd_str
	handle Parse => (TextIO.print ("!!! Parse error in command: \""^cmd_str^"\"\n");
			 ("NOOP",[]))
    val (outpt,st) = do_cmd cmd state
in (TextIO.print outpt;
    case st
     of NORMAL state' => repl state'
      | SKIP => repl state
      | QUIT => 0)
end

fun init () = repl GraphName.NTab.empty;

end (* structure NewController *)
