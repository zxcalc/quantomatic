signature CONTROLLER =
sig
    val init : unit -> unit
end

structure Controller (*: CONTROLLER *) =
struct

structure QuantoCommand =
struct
type T = string * string list


open SimpleLexer


fun unwrap (LIT s) = s
  | unwrap (ID s) = s
  | unwrap (INT n) = Int.toString n
  | unwrap _ = raise Match

(*
a term grammar with no nesting

arg   ::= LITERAL | ID
args  ::= arg args | E
expr  ::= ID args
exprs ::= expr ; exprs | E
*)


fun arg x = x|>((literal || ident || num)>>unwrap)
fun args x = x|>(((arg -- args) >> op::)
		     || (noop >> K []))
fun expr x = x|>((ident >> unwrap) -- args)
fun exprs x = x|>(
	      ((expr --| sym ";" -- exprs) >> op::)
		  || (expr >> single)
		  || (noop >> K []))

fun parse str = parse_with exprs str
fun pretty (f,args) = Pretty.block[Pretty.str f,
				   Pretty.str_list "(" ")" args]
val print = Pretty.writeln o pretty
val toString = Pretty.string_of o pretty
end; (* structure QuantoCommand *)

structure GraphName :> STR_NAMES = 
struct
open StrName
val default_name = "Ga"
end

structure G = RGGraph
structure V = G.RGVertex
structure E = G.Edge
structure LoadSave = RGGraphLoaderSaver

(* (name, rule, fused) *)
type rewrite = string * Rule.T * RGGraph.T
type graphstate = {
     hist : (RGGraph.T list * RGGraph.T list),
     rws: rewrite list} GraphName.NTab.T
datatype consolestate = NORMAL of graphstate | SKIP | QUIT


val newState : graphstate = GraphName.NTab.empty

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
				                       {hist = ([gr],[]), rws = []})
	          in
		          ((Pretty.str_of (GraphName.pretty_name name)) ^ "\n",
		           table')
	          end
	        | NONE => ("!!! Cannot create graph\n", graph_table)
                    
    (* cmd: [args] * graphstate -> out * graphstate *)
    fun lift_graph_state_mutator cmd (args as (graph_name::_), graph_table) =
	      (case GraphName.NTab.lookup graph_table graph_name
	        of SOME state => let val (out,state') = cmd (args,state)
			                     in (out,
				                       graph_table |> GraphName.NTab.update
				                                   (graph_name, state'))
			                     end
	         | _ => ("!!! Cannot lookup graph\n", graph_table))
      | lift_graph_state_mutator _ (_, state) =
	      ("!!! Graph command needs at least one argument\n", state)

    fun lift_graph_state_viewer cmd (args, graph_table) =
	(cmd (args, graph_table), graph_table)


    (* cmd: ([args] * ([graph] * [graph])) -> (out * ([graph] * [graph])) *)
    fun lift_graph_stack_mutator cmd = let
	    fun apply_stacks cmd (args,{hist=h,rws=r}) =
	        let val (out,h') = cmd (args,h)
	        in (out,{hist=h',rws=r})
	        end
    in lift_graph_state_mutator (apply_stacks cmd)
    end
                                       
    (* cmd: ([args] * graph * [rewrites]) -> (out * graph * [rewrites]) *)
    fun lift_graph_mutator_with_rewrites cmd = let
	    fun push_mod cmd (args, {hist=((g::gs),hs),rws=r}) =
	        let val (out,g',r') = cmd (args,g,r)
	        in (out, {hist=(g'::g::gs, []), rws=r'})
	        end
	      | push_mod cmd (args, state) =
	        ("!!! Graph stack is empty\n",state)
    in lift_graph_state_mutator (push_mod cmd)
    end
                                               
    (* cmd: ([args] * graph * [rewrites]) -> (out * [rewrites]) *)
    fun lift_graph_viewer_with_rewrites cmd = let
	    fun peek_top cmd (args, {hist=(h as ((g::_),_)),rws=r}) =
	        let val (out, r') = cmd (args,g,r)
	        in (out, {hist=h, rws=r'})
	        end
	      | peek_top cmd (args, state) =
	        ("!!! Graph stack is empty\n",state)
    in lift_graph_state_mutator (peek_top cmd)
    end
                                              
    (* cmd: ([args] * graph) -> (out * graph) *)
    fun lift_graph_mutator cmd =
	      let val flat = fn ((a,b),c) => (a,b,c)
	      in lift_graph_mutator_with_rewrites (fn (a,g,_) => flat (cmd (a,g),[]))
	      end
        
    (* cmd: [args] * graph -> out *)
    fun lift_graph_viewer cmd =
	      lift_graph_viewer_with_rewrites (fn (a,g,r) => (cmd (a,g),r))
        
    (* cmd: [args] -> out *)
    fun lift_stateless cmd (args, state) = (cmd args, state)

    val stub = lift_stateless (K "STUB\n")

    (* ============================================== *)

    fun help1 cmd = case Symtab.lookup (!cmds) cmd
		     of SOME (h,_) => "\n"^h^"\n"
		      | NONE => "!!! Can't find command.\n"

    (* helper functions for commands registered below *)
    fun help [] = "Type \"help CMD\" to get help on a command or \"help all\".\n"^
		              "Available commands:\n\nquit\n"^
		              (fold (fn a => fn b =>a^"\n"^b)
				    (Symtab.keys (!cmds)) "\n")
      | help ["all"] = fold (fn a => fn b => (help1 a)^b)
			    (Symtab.keys (!cmds)) "\n"
      | help [cmd] = help1 cmd
          
    fun list_graphs ([], tab) = let
	fun pr (n, _) lst = (lst ^ n ^ "\n")
    in GraphName.NTab.fold pr tab ""
    end
           
    fun duplicate_graph ([gr], tab) =
	      (case GraphName.NTab.lookup tab gr
	        of SOME {hist=(g::gs,_),...} => SOME g
	         | _ => NONE)

    fun load_graph ([fname],_) =  SOME (LoadSave.load_graph fname)

    fun save_graph ([_, fname], graph) = 
	( LoadSave.save_graph fname graph;
	  "Saved file " ^ fname ^"\n")
	handle _ => "!!! Could not save file "^fname^"\n"

    fun vertex_type "red" = V.IVert (V.Xnd AngleExpr.zero)
      | vertex_type "green" =  V.IVert (V.Znd AngleExpr.zero)
      | vertex_type "hadamard" = V.IVert V.Hnd
      | vertex_type "boundary" = V.BVert

    fun add_vertex ([_,vt], graph) =
	      let val (n,g) = graph |> G.add_vertex (vertex_type vt)
	      in ("added vertex \"" ^ n ^ "\"\n", g)
	      end
	      handle Match => ("!!! Bad vertex type.\n", graph)

    (* TODO: as far as I can tell, there's no good way to get a hold
     * of the correct "version" of no_such_vname_exp. *)
    fun add_edge ([_,v1,v2], graph) =
	      let val (n,g) = graph |> G.add_edge () v1 v2
	      in ("added edge \"" ^ n ^ "\"\n", g)
	      end
	      handle _ => ("!!! Error adding edge, probably a bad vertex name.\n",
		                 graph)


    fun delete_vertex ([_,v], graph) =
	("deleted vertex \""^v^"\"\n", graph|>G.delete_vertex v)
	handle _ => ("!!! Cannot delete vertex\n", graph)

    fun delete_edge ([_,e], graph) =
	("deleted edge \""^e^"\"\n", graph|>G.delete_edge e)
	handle _ => ("!!! Cannot delete edge\n", graph)

    fun rename_vertex ([_,old,new], graph) =
	("renamed to \""^new^"\"\n", graph|>G.rename_vname old new)
	handle G.vname_already_exists_exp _ =>
	       ("!!! graph already "^
		"contains a vertex of this name\n", graph)
	     | G.no_such_vname_exp _ =>
	       ("!!! no such vertex\n", graph)


    fun graph_xml [nme] = (enclose ("GRAPH_XML "^nme^"\n") "\n") o
			  Pretty.string_of  o
			  RGGraphOutputXML.output

    fun print_graph [_] = (suffix "\n") o Pretty.string_of o G.pretty

    fun undo ([_], (g1::g2::gs, hs)) = ("ok\n", (g2::gs,g1::hs))
      | undo ([_], (gs, hs)) = ("!!! Can't undo.\n", (gs,hs))

    fun redo ([_],(gs, h::hs)) = ("ok\n", (h::gs, hs))
      | redo ([_], (gs, [])) = ("!!! Can't redo.\n", (gs, []))

    (* TODO: fix exception name, see above *)
    fun attach_rewrites ((_::args), graph, _) = let
	    val vset = fold V.NSet.ins_fresh args V.NSet.empty
    in
	    ("ok\n",
	     (TheoryHack.apply_all_rules_in vset graph)
         |> map (fn (n,r) => (n, r, Rule.add_rhs r graph)))
    end
	      handle _ => ("!!! Bad or duplicate vertex name.\n", [])
                    
    fun show_rewrites ([_], graph, rewrites) = 
	(Pretty.string_of (RGRewriteListOutputXML.output rewrites), rewrites)
                                               
    fun apply_rewrite ([_, n], graph, rewrites) =
	      let val (name,rule,fused) = nth rewrites (the (Int.fromString n))
	      in ("applied "^name^"\n", Rule.del_lhs rule fused, [])
	      end
	      handle Subscript => ("!!! Requested rewrite not attached.\n", graph, [])
	           | Option => ("!!! Rewrite selection must be an integer.\n", graph, [])
                         
    fun vdata a (V.IVert (V.Xnd _)) = V.IVert (V.Xnd (AngleExpr.parse a))
      | vdata a (V.IVert (V.Znd _)) = V.IVert (V.Znd (AngleExpr.parse a))
      | vdata a _ = raise Match

    fun set_angle ([_,v,a], graph) =
	      ("ok\n", graph |> G.update_vertex (vdata a) v)
	      handle Match => ("!!! Can only set angles for red and green vertices.\n",graph)
	     | AngleExpr.parse_exp => ("!!! Cannot parse angle expression.\n", graph)
                               
    fun hilb ([_, format], graph) =
	let
	    fun fmt "text" = RGHilbTerm.pretty_text
	      | fmt "mathematica" = RGHilbTerm.pretty_mathematica
	      | fmt _ = fn _ => Pretty.str "!!! Bad formatter."
	in
	    (Pretty.string_of (
	     fmt format (RGHilbTerm.from_graph graph)))^"\n"
	end

    fun add_bang ([_], graph) = graph |> G.new_box (G.BBox.default_name)
				      >> (fn a => a^"\n")

    fun drop_bang ([_,name], graph) =
	("dropped !-box \""^name^"\"\n",
	 graph |> G.drop_box name)

    fun bang_vertices ((_::box::verts), graph) = 
	("added vertices to \""^box^"\"\n",
	 graph |> G.move_to_box box (G.Vertex.NSet.of_list verts))

    fun unbang_vertices ((_::verts), graph) =
	("un-!'ed vertices\n",
	 graph |> G.move_to_unbox (G.Vertex.NSet.of_list verts))
in

(* Register commands available from the console and associate docs *)

val () = (
    register_cmd
	"help"
	("help CMD\n\n"^
	 "    get help on CMD\n")
	(lift_stateless help);
    
    register_cmd
	"HELO"
	("HELO\n\n"^
	 "    sync the console, print \"HELO\"\n")
	(lift_stateless (fn [] => "HELO\n"));
    
    register_cmd
	"list_graphs"
	("list_graphs\n\n"^
	 "    list all active graphs")
	(lift_graph_state_viewer list_graphs);

    register_cmd
	"ls"
	("ls\n\n"^
	 "    alias for \"list_graphs\"")
	(lift_graph_state_viewer list_graphs);

    register_cmd
	"new_graph"
	("new_graph\n\n" ^
	 "    create a new graph and return its name\n")
	(lift_graph_creator (fn ([],_) => SOME RGGraph.empty));

    register_cmd
	"load_graph"
	("load_graph FILENAME\n\n" ^
	 "    load a new graph from FILENAME return its name\n")
	(lift_graph_creator load_graph);
    
    register_cmd
	"save_graph"
	("save_graph GRAPH FILENAME\n\n" ^
	 "    save GRAPH in FILENAME\n")
	(lift_graph_viewer save_graph);
   
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
	"delete_vertex"
	("delete_vertex GRAPH V\n\n"^
	 "    delete vertex V from GRAPH\n")
	(lift_graph_mutator delete_vertex);

    register_cmd
	"delete_edge"
	("delete_edge GRAPH E\n\n"^
	 "    delete edge E from GRAPH\n")
	(lift_graph_mutator delete_edge);

    register_cmd
	"add_edge"
	("add_edge GRAPH V1 V2\n\n"^
	 "    add an edge in GRAPH from V1 to V2\n")
	(lift_graph_mutator add_edge);
    
    register_cmd
	"rename_vertex"
	("rename_vertex GRAPH OLD NEW\n\n"^
	 "    rename vertex in GRAPH from OLD to NEW\n")
	(lift_graph_mutator rename_vertex);

    register_cmd
	"graph_xml"
	("graph_xml GRAPH\n\n"^
	 "    dump GRAPH as XML\n")
	(lift_graph_viewer (uncurry graph_xml));
    
    register_cmd
	"print_graph"
	("print_graph GRAPH\n\n"^
	 "    print out GRAPH\n")
	(lift_graph_viewer (uncurry print_graph));

    register_cmd
	"undo"
	("undo GRAPH\n\n"^
	 "    undo last change to GRAPH\n")
	(lift_graph_stack_mutator undo);
    
    register_cmd
	"redo"
	("redo GRAPH\n\n"^
	 "    redo change on GRAPH\n")
	(lift_graph_stack_mutator redo);
    
    register_cmd
	"attach_rewrites"
	("attach_rewrites GRAPH V1 .. VN\n\n"^
	 "    calculate rewrites involving V1..VN in GRAPH and\n"^
	 "    attach to the graph state\n")
	(lift_graph_viewer_with_rewrites attach_rewrites);
    
    register_cmd
	"show_rewrites"
	("show_rewrites GRAPH\n\n"^
	 "    show rewrites attached to GRAPH\n")
	(lift_graph_viewer_with_rewrites show_rewrites);
    
    register_cmd
	"apply_rewrite"
	("apply_rewrite GRAPH N\n\n"^
	 "    apply the Nth rewrite attached to GRAPH\n")
	(lift_graph_mutator_with_rewrites apply_rewrite);
    
    register_cmd
	"hilb"
	("hilb GRAPH FORMAT\n\n"^
	 "    display the hilbert space term of GRAPH where\n"^
	 "    FORMAT is one of {text, mathematica}.\n")
	(lift_graph_viewer hilb);
    
    register_cmd
	"set_angle"
	("set_angle GRAPH V ANGLE\n\n"^
	 "    set the angle of vertex V to ANGLE\n")
	(lift_graph_mutator set_angle);

    register_cmd
	"add_bang"
	("add_bang GRAPH\n\n"^
	 "    add a new !-box to GRAPH and return its name\n")
	(lift_graph_mutator add_bang);

    register_cmd
	"drop_bang"
	("drop_bang GRAPH BANG\n\n"^
	 "    drop !-box BANG in GRAPH. Vertices inside BANG are moved\n"^
	 "    to the top-level graph.\n")
	(lift_graph_mutator drop_bang);

    register_cmd
	"bang_vertices"
	("bang_vertices GRAPH BANG V1 .. VN\n\n"^
	 "    add vertices V1..VN in graph to !-box BANG\n")
	(lift_graph_mutator bang_vertices);

    register_cmd
	"unbang_vertices"
	("unbang_vertices GRAPH BANG V1 .. VN\n\n"^
	 "    remove vertices V1..VN from !-box BANG")
	stub;

    ())

end

(* Uniquely mark end of text as SPACE + BACKSPACE. This is well-behaved as
 * long as commands never send raw binary. *)
val EOT = " "^(chr 8)

fun do_cmd ("quit",_) _ = ("Exiting...\n", QUIT)
  | do_cmd ("exit",_) _ = ("Exiting...\n", QUIT)
  | do_cmd ("NOOP",_) _ = ("", SKIP)
  | do_cmd (cmd as (cname,args)) state =
    (case Symtab.lookup (!cmds) cname
      of SOME (_,f) => (fn (a,b)=>(a, NORMAL b)) (f (args, state))
       | NONE => ("!!! Unknown command: "^
		  (QuantoCommand.toString cmd)^"\n", NORMAL state))
    handle Match =>
	   ("!!! Wrong number of args in " ^
	    (QuantoCommand.toString cmd)^".\n", NORMAL state)

fun eval command state =
    let
	    val cmds = SOME (QuantoCommand.parse command)
		handle Parse => NONE
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
    

(* main read-exec-print-loop *)
fun repl state = let
  val _ = TextIO.print ("quanto:> "^EOT)
  val cmd_str = unsuffix "\n" (the (TextIO.inputLine TextIO.stdIn))
  val (output, st) = eval cmd_str state
in TextIO.print (implode output); TextIO.print EOT;
   (case st
     of NORMAL state' => repl state'
      | SKIP => repl state
      | QUIT => (TextIO.print EOT; ()))
end

fun flushIn () = case TextIO.canInput (TextIO.stdIn, 1)
		              of SOME n => (TextIO.input1 TextIO.stdIn;())
		               | NONE => ()

fun init () = (flushIn(); repl newState);

end (* structure Controller *)
