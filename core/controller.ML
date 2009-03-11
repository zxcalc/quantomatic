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


fun unwrap (ID s) = s
  | unwrap (INT n) = Int.toString n
  | unwrap (LIT s) = s
  | unwrap _ = raise Match

(*fun unescape (LIT str) = str String.translate
               (fn c => case c of #"\\" => ""
                        | _ => Char.toString c)
               str
  | unescape _ = raise Match
*)

(*
a term grammar with no nesting

arg   ::= LITERAL | ID
args  ::= arg args | E
expr  ::= ID args
exprs ::= expr ; exprs | E
*)


fun arg x = x|>(
        (literal >> unwrap)
        || ((ident || num) >> unwrap))
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

structure GraphName :> SSTR_NAMES = 
struct
open SStrName
val default_name = SStrName.mk "Ga";
end;


exception user_exp of string


structure G = RGGraph
structure V = G.RGVertex
structure E = G.Edge
structure LoadSave = RGGraphLoaderSaver


type rewrite = string * Rule.T * RGGraph.T
type c_state = {
     graphs : G.T GraphName.NTab.T,
     rewrites : (rewrite list) GraphName.NTab.T,
     hist : (G.T list * G.T list) GraphName.NTab.T
}

(* getters and setters for the state *)
fun update_graphs f ({graphs=g,rewrites=r,hist=h}:c_state):c_state = {graphs=(f g),rewrites=r,hist=h}
fun update_rewrites f ({graphs=g,rewrites=r,hist=h}:c_state):c_state = {graphs=g,rewrites=(f r),hist=h}
fun update_hist f ({graphs=g,rewrites=r,hist=h}:c_state):c_state = {graphs=g,rewrites=r,hist=(f h)}
val (set_graphs,set_rewrites,set_hist) = (update_graphs o K, update_rewrites o K, update_hist o K)
fun get_graphs ({graphs=g,...}:c_state) = g
fun get_rewrites ({rewrites=r,...}:c_state) = r
fun get_hist ({hist=h,...}:c_state) = h

datatype consolestate = NORMAL of c_state | SKIP | QUIT


val newState : c_state = {graphs=GraphName.NTab.empty,
                                rewrites=GraphName.NTab.empty,
                                hist=GraphName.NTab.empty}

(*
A dictionary of commands of the format:
  (help: string, cmd: args * state -> output * state)
*)
val cmds :
    ((string *
     (string list * c_state -> string * c_state))
    Symtab.table) ref = ref Symtab.empty

fun register_cmd name help f =
    cmds := ((!cmds) |> Symtab.update (name,(help,f)))

(* state accessors *)
fun fetch_graph name state =
    (case GraphName.NTab.lookup (get_graphs state) name
     of SOME g => g
      | NONE => raise user_exp ("Cannot fetch graph '"^(GraphName.dest name)^"'."))


(* state mutators *)
fun push_hist gname state = 
    let
      val (back,_) = 
          the_default ([],[]) (GraphName.NTab.lookup (get_hist state) gname)
      val hist' = (get_hist state)
                        |> GraphName.NTab.update
                        (gname,((fetch_graph gname state)::back, []))
    in state |> set_hist hist' end

(* any time the graph is changed, any attached rewrites are invalid,
 * so should be cleared *)
fun set_graph_at gname graph' state =
    state |> update_graphs (GraphName.NTab.update (gname,graph'))
            |> update_rewrites (GraphName.NTab.update (gname,[]))    

(* convenience functions *)

fun graph_arg (g::args) = g
  | graph_arg _ = raise user_exp ("You must specify a graph name.")

(* Graph updating commands, cmd: ([args] * graph) -> (out * graph) *)
fun lift_graph_mutator cmd (args, state:c_state) =
    let
        val gname = GraphName.mk (graph_arg args)
        val (out,g') = cmd (args,fetch_graph gname state)
    in (out, state |> push_hist gname |> set_graph_at gname g')
    end

(* Read-only graph commands, cmd: [args] * graph -> out *)
fun lift_graph_viewer cmd (args, state) =
    let
        val gname = GraphName.mk (graph_arg args)
        val out = cmd (args,fetch_graph gname state)
    in (out, state)
    end
    
(* State-agnostic commands, cmd: [args] -> out *)
fun lift_stateless cmd (args, state:c_state) = (cmd args, state)

(* Read-only state commands, cmd: [args] * state -> out *)
fun lift_state_viewer cmd (args, state:c_state) = (cmd (args,state), state)

(* Most commands are already of the form
 *    cmd: [args] * state -> out * state *)
(* val lift_state_mutator = I *)

(* ============================================== *)
(* these functions actually implement commmands *)

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
  | help [cmd] = (help1 cmd)
         
fun list_graphs ([], state) = 
    GraphName.NTab.fold (suffix o suffix "\n" o GraphName.dest o fst) (get_graphs state) ""

fun load_ruleset args = (TheoryHack.load_rules (hd args); "Loaded ruleset\n")

fun save_ruleset args = (TheoryHack.save_rules (hd args) ; "Saved ruleset\n")

local
    fun graph_maker default_name graph state = let
    val (nm,gs) = (get_graphs state)
              |> GraphName.NTab.add (default_name, graph)
    in ((GraphName.dest nm) ^ "\n", state |> set_graphs gs)
    end
in

fun duplicate_graph ([name], state) = 
    let val gname = (GraphName.mk name) 
    in graph_maker gname (fetch_graph gname state) state end
fun new_graph ([], state) =
    graph_maker GraphName.default_name G.empty state
fun load_graph ([fname], state) = 
    graph_maker GraphName.default_name (LoadSave.load_graph fname) state
end

fun save_graph ([_, fname], graph) = 
    ( LoadSave.save_graph fname graph;
      "Saved file " ^ fname ^"\n")
    handle _ => raise user_exp ("Could not save file "^fname)

fun vertex_type "red" = V.IVert (V.Xnd AngleExpr.zero)
  | vertex_type "green" =  V.IVert (V.Znd AngleExpr.zero)
  | vertex_type "hadamard" = V.IVert V.Hnd
  | vertex_type "boundary" = V.BVert

fun add_vertex ([_,vt], graph) =
    let val (n,g) = graph |> G.add_vertex (vertex_type vt)
    in ("added vertex \"" ^ (G.Vertex.dest n) ^ "\"\n", g)
    end
    handle Match => raise user_exp "Bad vertex type."

fun add_edge ([_,v1,v2], graph) =
    let val (n,g) = graph |> G.add_edge () (V.mk v1) (V.mk v2)
    in ("added edge \"" ^ (G.Edge.dest n) ^ "\"\n", g)
    end
    handle G.no_such_vname_exp _ =>
       raise user_exp "Bad vertex name."


fun delete_vertices (_::vs, graph) =
    ("ok\n", fold G.delete_vertex (map V.mk vs) graph)
    handle G.no_such_vname_exp _ => raise user_exp "No such vertex."

fun delete_edges (_::es, graph) =
    ("ok\n", fold G.delete_edge (map E.mk es) graph)
    handle G.no_such_ename_exp _ => raise user_exp "No such edge."

fun rename_vertex ([_,old,new], graph) =
    ("renamed to \""^new^"\"\n", graph |> G.rename_vname (V.mk old) (V.mk new))
    handle G.vname_already_exists_exp _ =>
       raise user_exp "graph already contains a vertex of this name"
     | G.no_such_vname_exp _ => raise user_exp "no such vertex"


fun graph_xml [nme] = (enclose ("GRAPH_XML "^nme^"\n") "\n") o
              Pretty.string_of  o
              RGGraphOutputXML.output

fun print_graph [_] = (suffix "\n") o Pretty.string_of o G.pretty
                      
fun undo ([name], state) = 
    let
      val gname = (GraphName.mk name) 
      val current = fetch_graph gname state
    in case GraphName.NTab.lookup (get_hist state) gname
        of SOME (g::gs, hs) => ("ok\n",
                                      state |> set_graph_at gname g
                                              |> update_hist
                                              (GraphName.NTab.update
                                             (gname, (gs,current::hs)))
                                     )
         | _ => raise user_exp "Can't undo."
    end

fun redo ([name], state) = 
    let
      val gname = GraphName.mk name;
      val current = fetch_graph gname state
    in case GraphName.NTab.lookup (get_hist state) gname
        of SOME (gs, h::hs) => ("ok\n",
                                      state |> set_graph_at gname h
                                              |> update_hist
                                              (GraphName.NTab.update
                                                 (gname, (current::gs,hs)))
                                     )
         | _ => raise user_exp "Can't redo."
    end;
    
fun attach_rewrites (args as _::vs, state) = let
    val gname = GraphName.mk (graph_arg args)
    val vset = fold V.NSet.ins_fresh (map V.mk vs) V.NSet.empty
    val graph = fetch_graph gname state
    val rws = map (fn (n,r) => (n, r, Rule.add_rhs r graph))
                      (TheoryHack.apply_all_rules_in' vset graph)
in
    ("attached "^(Int.toString (length rws))^" rewrites\n",
     state |> update_rewrites (GraphName.NTab.update (gname,rws)))
end
    handle G.no_such_vname_exp _ => raise user_exp "Bad vertex name."
     | V.NSet.duplicate_exp _ => raise user_exp "Duplicate vertex."

fun show_rewrites (args,state) = let
    val gname = GraphName.mk (graph_arg args)
    val rewrites = case GraphName.NTab.lookup (get_rewrites state) gname
            of NONE => raise user_exp "No rewrites attached."
             | SOME r => r
in Pretty.string_of (RGRewriteListOutputXML.output rewrites)
end

fun apply_rewrite ([name, n], state) =
    let
      val gname = GraphName.mk name
        val rewrites = GraphName.NTab.get (get_rewrites state) gname
        val (name,rule,fused) = nth rewrites (the (Int.fromString n))
    in ("applied "^name^"\n",
          state |> push_hist gname
                |> set_graph_at gname (Rule.del_lhs rule fused))
    end
    handle UNDEF => raise user_exp "No rewrites attached." 
           | Subscript => raise user_exp "Rewrite not attached."
           | Option => raise user_exp "Rewrite selection must be an integer."
                           
fun vdata a (V.IVert (V.Xnd _)) = V.IVert (V.Xnd (AngleExpr.parse a))
  | vdata a (V.IVert (V.Znd _)) = V.IVert (V.Znd (AngleExpr.parse a))
  | vdata a _ = raise Match

fun set_angle ([_,v,a], graph) =
    ("ok\n", graph |> G.update_vertex_data (vdata a) (V.mk v))
    handle Match => raise user_exp "Can only set angles for red and green vertices."
     | AngleExpr.parse_exp => raise user_exp "Cannot parse angle expression."

fun hilb ([_, format], graph) =
    (Pretty.string_of (
     RGHilbTerm.pretty (AlgFormat.of_string format)
              (RGHilbTerm.of_graph graph)))^"\n"
    handle AlgFormat.not_supported_exp =>
       raise user_exp "Term format not supported."

fun add_bang ([_], graph) = 
    graph |> G.new_box (G.BBox.default_name)
                  >> (fn a => (G.BBox.dest a) ^ "\n")

fun drop_bang ([_,name], graph) =
    ("dropped !-box \""^name^"\"\n",
     graph |> G.drop_box (G.BBox.mk name))

fun bang_vertices ((_::box::verts), graph) = 
    ("added vertices to \""^box^"\"\n",
     graph |> G.move_to_box (G.BBox.mk box) (G.Vertex.NSet.of_list (map V.mk verts)))

fun unbang_vertices ((_::verts), graph) =
    ("un-!'ed vertices\n",
     graph |> G.move_to_unbox (G.Vertex.NSet.of_list (map V.mk verts)))

fun copy_subgraph (args as (_::target::vs), state) = let
    val graph = fetch_graph (GraphName.mk (graph_arg args)) state
    val clip = G.pull_subgraph_within_selection (G.Vertex.NSet.of_list (map V.mk vs))
                                                            graph
in ("ok\n", state |> set_graph_at (GraphName.mk target) clip)
end

fun insert_graph ([name1,name2], state) = 
    let
      val tname = (GraphName.mk name1)
      val sname = (GraphName.mk name2)
      val targ = fetch_graph tname state
      val source = fetch_graph sname state
      val (_, source) = G.rename_apart targ (fetch_graph sname state)
    in ("ok\n", state |> push_hist tname
                          |> set_graph_at tname (G.merge_by_vertices targ source))
    end

fun flip_vertices (_::vs, graph) = let
    fun flip_data (V.IVert (V.Xnd e)) = V.IVert (V.Znd e)
      | flip_data (V.IVert (V.Znd e)) = V.IVert (V.Xnd e)
      | flip_data d = d
in ("ok\n", fold (G.update_vertex_data flip_data) (map V.mk vs) graph)
end


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
    (lift_stateless (fn[] => "HELO\n"));

   register_cmd
    "load_ruleset"
    ("load_ruleset FILENAME\n\n" ^
     "    load a new rule set from FILENAME\n")
    (lift_stateless load_ruleset);
    
    register_cmd
    "save_ruleset"
    ("save_ruleset FILENAME\n\n" ^
     "    save the current rule set in FILENAME\n")
    (lift_stateless save_ruleset);

    register_cmd
    "list_graphs"
    ("list_graphs\n\n"^
     "    list all active graphs")
    (lift_state_viewer list_graphs);

    register_cmd
    "ls"
    ("ls\n\n"^
     "    alias for \"list_graphs\"")
    (lift_state_viewer list_graphs);

    register_cmd
    "new_graph"
    ("new_graph\n\n" ^
     "    create a new graph and return its name\n")
    (new_graph);

    register_cmd
    "load_graph"
    ("load_graph FILENAME\n\n" ^
     "    load a new graph from FILENAME return its name\n")
    (load_graph);
    
    register_cmd
    "save_graph"
    ("save_graph GRAPH FILENAME\n\n" ^
     "    save GRAPH in FILENAME\n")
    (lift_graph_viewer save_graph);
 
    register_cmd
    "duplicate_graph"
    ("duplicate_graph GRAPH\n\n"^
     "    duplicate GRAPH and return the name of the new graph\n")
    (duplicate_graph);

    register_cmd
    "add_vertex"
    ("add_vertex GRAPH TYPE\n\n"^
     "    add a TYPE vertex to GRAPH\n")
    (lift_graph_mutator add_vertex);
    
    register_cmd
    "delete_vertices"
    ("delete_vertices GRAPH V1 .. VN\n\n"^
     "    delete vertices V1..VN from GRAPH\n")
    (lift_graph_mutator delete_vertices);

    register_cmd
    "delete_edges"
    ("delete_edges GRAPH E1 .. EN\n\n"^
     "    delete edges E1 .. EN from GRAPH\n")
    (lift_graph_mutator delete_edges);

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
    (undo);
    
    register_cmd
    "redo"
    ("redo GRAPH\n\n"^
     "    redo change on GRAPH\n")
    (redo);
    
    register_cmd
    "attach_rewrites"
    ("attach_rewrites GRAPH V1 .. VN\n\n"^
     "    calculate rewrites involving V1..VN in GRAPH and\n"^
     "    attach to the graph state\n")
    (attach_rewrites);

    register_cmd
    "show_rewrites"
    ("show_rewrites GRAPH\n\n"^
     "    show rewrites attached to GRAPH\n")
    (lift_state_viewer show_rewrites);

    register_cmd
    "apply_rewrite"
    ("apply_rewrite GRAPH N\n\n"^
     "    apply the Nth rewrite attached to GRAPH\n")
    (apply_rewrite);

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
    ("unbang_vertices GRAPH V1 .. VN\n\n"^
     "    remove vertices V1..VN from their current !-box\n")
    (lift_graph_mutator unbang_vertices);

    register_cmd
    "copy_subgraph"
    ("copy_subgraph SOURCE TARGET V1 .. VN\n\n"^
     "    copy the subgraph of SOURCE spanned by V1..VN to\n"^
     "    the graph TARGET\n")
    (copy_subgraph);

    register_cmd
    "insert_graph"
    ("insert_graph TARGET SOURCE\n\n"^
     "    insert of copy of SOURCE into TARGET\n")
    (insert_graph);

    register_cmd
    "flip_vertices"
    ("flip_vertices G V1 .. VN\n\n"^
     "    reverse the colour of vertices V1..VN, ingoring\n"^
     "    boundary vertices.")
    (lift_graph_mutator flip_vertices);



    ())



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
     | user_exp str => ("!!! "^str^"\n", NORMAL state)

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

fun init () = (flushIn(); repl newState)

end (* structure Controller *)
