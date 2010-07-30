signature CONTROLLER =
sig
    val init : unit -> unit
end

structure Controller : CONTROLLER = 
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
val default_name = SStrName.mk "new-graph-1"
end

exception user_exp of string

structure G = RGGraph
structure V = G.RGVertex
structure E = G.Edge
structure LoadSave = RGGraphLoaderSaver


type rewrite = SStrName.name * Rule.T * RGGraph.T
datatype c_state = CState of {
     graphs : G.T GraphName.NTab.T,
     rewrites : (rewrite list) GraphName.NTab.T,
     hist : (G.T list * G.T list) GraphName.NTab.T,
     rulesets : (RuleSet.T * bool) RuleSet.NTab.T
}

(* getters and setters for the state *)
fun update_graphs f (CState {graphs=g,rewrites=r,hist=h,rulesets=t}) = 
  CState {graphs=(f g),rewrites=r,hist=h,rulesets=t}
fun update_rewrites f (CState {graphs=g,rewrites=r,hist=h,rulesets=t}) =
  CState {graphs=g,rewrites=(f r),hist=h,rulesets=t}
fun update_hist f (CState {graphs=g,rewrites=r,hist=h,rulesets=t}) = 
  CState {graphs=g,rewrites=r,hist=(f h),rulesets=t}
fun update_rulesets f (CState {graphs=g,rewrites=r,hist=h,rulesets=t}) = 
  CState {graphs=g,rewrites=r,hist=h,rulesets=(f t)}
val (set_graphs,set_rewrites,set_hist,set_rulesets) = 
  (update_graphs o K, update_rewrites o K, update_hist o K, update_rulesets o K)
fun get_graphs (CState {graphs=g,...}) = g
fun get_rewrites (CState {rewrites=r,...}) = r
fun get_hist (CState {hist=h,...}) = h
fun get_rulesets (CState {rulesets=t,...}) = t

datatype consolestate = NORMAL of c_state | SKIP | QUIT

(*val default_rulesets = RuleSet.NTab.empty
                           |> RuleSet.NTab.update (RuleSet.mk "default",
                                                  (!RuleSetHack.rules,true))
*)

val newState = CState {graphs=GraphName.NTab.empty,
                       rewrites=GraphName.NTab.empty,
                       hist=GraphName.NTab.empty,
                       rulesets=RuleSet.NTab.empty}

(*
A dictionary of commands of the format:
  (help: string, cmd: args * state -> output * state)
*)
val cmds :
    ((string *
     (string list * c_state -> string * c_state))
    Symtab.table) ref = ref Symtab.empty


fun handle_bug_in_function name f (x as (l,st)) = 
    (f x) 
    handle G.no_such_vname_exp (s, v, g) => 
      raise user_exp ("BUG: G.no_such_vname_exp")
    | G.vname_already_exists_exp (v,g) =>
      raise user_exp ("BUG: G.vname_already_exists_exp")
    | G.no_such_ename_exp (s, e, g) =>
      raise user_exp ("BUG: G.no_such_ename_exp")
    | G.ename_already_exists_exp (e,g) => 
      raise user_exp ("BUG: G.ename_already_exists_exp")
    | G.merge_bbox_exp s => 
      raise user_exp ("BUG: G.merge_bbox_exp")
    | G.overlapping_bbox_exp (vs,g) =>
      raise user_exp ("BUG: G.overlapping_bbox_exp")
	  | E.NSet.duplicate_exp _ => 
      raise user_exp ("BUG: Duplicate Edge.")
	  | V.NSet.duplicate_exp _ => 
      raise user_exp ("BUG: Duplicate vertex.")
    | G.BBox.Tab.UNDEF x =>
      raise user_exp ("BUG: G.BBox.Tab.UNDEF")
    | G.Vertex.Tab.UNDEF x =>
      raise user_exp ("BUG: G.Vertex.Tab.UNDEF")
    | G.Vertex.Tab.UNDEF x =>
      raise user_exp ("BUG: G.Edge.Tab.UNDEF");

(* add a command; wrap it's execution in a generic bug-handler to stop quanto 
   from dieing when internal errors happen. Better to print out the error to the 
   user. *)
fun register_cmd name help f =
    cmds := 
      ((!cmds) |> Symtab.update 
       (name,(help, handle_bug_in_function name f)))

(* state accessors *)
fun fetch_graph name state =
    (case GraphName.NTab.lookup (get_graphs state) name
     of SOME g => g
      | NONE => raise user_exp ("Cannot fetch graph '"^
                (GraphName.dest name)^"'."))


(* state mutators *)
fun push_hist gname state = 
    let
      val (back,_) = 
          the_default ([],[]) (GraphName.NTab.lookup (get_hist state) gname)
      val hist' = (get_hist state)
                        |> GraphName.NTab.update
                        (gname,((fetch_graph gname state)::back, []))
    in state |> set_hist hist'
    end

(* any time the graph is changed, any attached rewrites are invalid,
 * so should be cleared *)
fun set_graph_at gname graph' state =
    state |> update_graphs (GraphName.NTab.update (gname,graph'))
            |> update_rewrites (GraphName.NTab.update (gname,[]))    

(* convenience functions *)

fun graph_arg (g::args) = g
  | graph_arg _ = raise user_exp ("You must specify a graph name.")

(* Graph updating commands, cmd: ([args] * graph) -> (out * graph) *)
fun lift_graph_mutator cmd (args, state) =
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


fun read_block () = let
    val start_parser = SimpleLexer.sym "---startblock:" |-- SimpleLexer.num
    val line = fn () => the (TextIO.inputLine TextIO.stdIn)
    val expected = Int.toString
		       (case SimpleLexer.parse_with start_parser (line ())
			 of SimpleLexer.INT n => n
			  | _ => raise user_exp "Bad start of block")
    fun loop () = let val ln = line ()
		  in if ln = ("---endblock:"^expected^"\n") then ""
		     else ln^(loop ())
		  end
in loop ()
end
    handle Parse => raise user_exp "Bad start of block"


(* Most commands are already of the form
 *    cmd: [args] * state -> out * state *)
(* val lift_state_mutator = I *)

(* ============================================== *)
(* these functions actually implement commmands *)

exception wrong_args_exp of unit

fun echo_block [] = read_block ()
  | echo_block _ = raise wrong_args_exp ()

fun help1 "quit" = "\nquit\n\n    Quits the quantomatic core\n\n"
  | help1 cmd = case Symtab.lookup (!cmds) cmd
         of SOME (h,_) => "\n"^h^"\n"
          | NONE => "!!! Can't find command.\n"

val sort_alpha = sort (fn x => rev_order (string_ord x))

(* helper functions for commands registered below *)
fun help [] = "Type \"help CMD\" to get help on a command or \"help all\".\n"^
          "Available commands:\n\n"^
          (fold (fn a => fn b =>a^"\n"^b)
            (sort_alpha ("quit" :: (Symtab.keys (!cmds)))) "\n")
  | help ["all"] = fold (fn a => fn b => (help1 a)^b)
                        (sort_alpha ("quit" :: (Symtab.keys (!cmds)))) "\n"
  | help [cmd] = (help1 cmd)
  | help _ = raise wrong_args_exp ()
         
fun list_graphs ([], state) = 
    GraphName.NTab.fold (suffix o suffix "\n" o GraphName.dest o fst)
			(get_graphs state) ""
  | list_graphs _ = raise wrong_args_exp ()


fun list_rulesets ([], state) =
    (Pretty.string_of (Pretty.chunks
                       (map RuleSet.pretty_name
                            (RuleSet.get_all_rulesets (get_rulesets state))
    ))) ^ "\n"
  | list_rulesets _ = raise wrong_args_exp ()

fun list_active_rulesets ([], state) =
    (Pretty.string_of (Pretty.chunks
                       (map RuleSet.pretty_name
                            (RuleSet.get_active_rulesets (get_rulesets state))
    ))) ^ "\n"
  | list_active_rulesets _ = raise wrong_args_exp ()

fun list_rules ([rset], state) = let
  val rs = fst (RuleSet.NTab.get (get_rulesets state) (RuleSet.mk rset))
in
  (suffix "\n" o Pretty.string_of)
    (Pretty.chunks (map RuleSet.pretty_name (RuleSet.list_of_rules rs)))
  handle RuleSet.Tab.UNDEF _ => raise user_exp ("No such ruleset '" ^ rset ^ "'")
end
  | list_rules _ = raise wrong_args_exp ()

local
    fun set_active active nm thys = let
        val thy = RuleSet.mk nm
        val (data, _) = RuleSet.NTab.get thys thy
    in thys |> RuleSet.NTab.update (thy, (data, active))
    end

    fun activate_ruleset' active ([thy], state) =
        (("ok\n",
          update_rulesets (set_active active thy) state)
         handle RuleSet.Tab.UNDEF _ => raise user_exp ("No such ruleset '" ^ thy ^ "'"))
      | activate_ruleset' active _ = raise wrong_args_exp ()
in
val deactivate_ruleset  = activate_ruleset' false
val activate_ruleset = activate_ruleset' true
end  

local
    fun update_rule updater rulesetname rulename rulesets = 
        let
          val rs = RuleSet.mk rulesetname
          val rl = Rule.Name.mk rulename
          val (rset, active) = RuleSet.NTab.get rulesets rs
        in 
          rulesets |> RuleSet.NTab.update (rs, ((updater rl rset), active))
        end

    fun update_rule' updater ([rule,ruleset], state) =
        (("ok\n",
          update_rulesets (update_rule updater ruleset rule) state)
         handle RuleSet.Tab.UNDEF _ => raise user_exp ("No such ruleset '" ^ ruleset ^ "'"))
      | update_rule' updater _ = raise wrong_args_exp ()
      

in
val deactivate_rule  = update_rule' RuleSet.deactivate_rule
val activate_rule = update_rule' RuleSet.activate_rule
val delete_rule = update_rule' RuleSet.delete_rule

fun replace_rule ([rset,rule,lhs,rhs], state) =
  let
    val graphs = get_graphs state
    
    (* build graphs for the LHS and RHS *)
    val lgr = GraphName.NTab.get graphs (GraphName.mk lhs)
      handle GraphName.Tab.UNDEF _ => raise user_exp "LHS doesn't exist."
    val rgr = GraphName.NTab.get graphs (GraphName.mk rhs)
      handle GraphName.Tab.UNDEF _ => raise user_exp "RHS doesn't exist."
      
    (* replace the given rule with a new one, preserving "active" *)
    fun new_rule (_,active) = (Rule.mk (lgr, rgr), active)
    
    (* find ruleset/rule and apply the above fn to it *)
    val update_rsets = RuleSet.NTab.map_entry
          (fn (rs,act) => (Rule.Name.NTab.map_entry
            new_rule (Rule.Name.mk rule) rs, act))
          (RuleSet.mk rset)
      handle RuleSet.Tab.UNDEF _ => raise user_exp "Can't find ruleset."
           | Rule.Name.Tab.UNDEF _ => raise user_exp "Can't find rule."
  in ("ok\n", state |> update_rulesets update_rsets)
      handle Rule.bad_rule_exp _ =>
        raise user_exp "Boundaries and !-boxes on LHS must match RHS."
end
  | replace_rule _ = raise wrong_args_exp ()

(* create a new identity rule from the current graph *)
fun new_rule ([rset,graph], state) =
let
  val rset_n = RuleSet.mk rset
  
  (* select graph and ruleset *)
  val (rs,active) = RuleSet.NTab.get (get_rulesets state) rset_n
      handle RuleSet.Tab.UNDEF _ => raise user_exp "Can't find ruleset."
  val graph = GraphName.NTab.get (get_graphs state) (GraphName.mk graph)
      handle GraphName.Tab.UNDEF _ => raise user_exp "Graph doesn't exist."
      
  (* add a new rule and make it active *)
  val rule = (Rule.mk (graph, graph), true)
  val (nm, rs') = rs |> Rule.Name.NTab.add (Rule.Name.default_name, rule)
in ((Pretty.string_of o Rule.Name.pretty_name) nm ^ "\n",
      state |> update_rulesets (RuleSet.NTab.update (rset_n, (rs',active))))
end
  | new_rule _ = raise wrong_args_exp ()
  
  
end  



fun load_ruleset ([nm,file], state) =
    (let val loader = RGRuleSetInputXML.input o XMLReader.read_from_file
         val rsets = get_rulesets state
         val (nm', rsets') = rsets |> RuleSet.NTab.add (RuleSet.mk nm, (loader file, true))
     in ((Pretty.string_of (RuleSet.pretty_name nm')) ^ "\n", set_rulesets rsets' state)
     end
     handle IO.Io _ => raise user_exp "Can't open ruleset.")
  | load_ruleset _ = raise wrong_args_exp ()

fun save_ruleset ([nm,file], state) =
    (let val save = (PrettyWriter.write_to_file file) o RGRuleSetOutputXML.output
         val nm' = RuleSet.mk nm
         val (rset,b) = RuleSet.NTab.get (get_rulesets state) nm'
     in (save (nm',rset,b); "Saved ruleset in file " ^file)
     end
     handle RuleSet.NTab.Tab.UNDEF _ => raise user_exp ("Bad ruleset name: "^nm)
     handle IO.Io _ => raise user_exp ("Can't write ruleset file "^file^"."))
  | save_ruleset _ = raise wrong_args_exp ()

  
fun unload_ruleset ([nm], state) =
    let fun del nm thys = thys |> RuleSet.NTab.delete (RuleSet.mk nm)
            handle RuleSet.Tab.UNDEF _ => raise user_exp ("No such ruleset: " ^ nm)
    in ("ok\n", update_rulesets (del nm) state)
    end
  | unload_ruleset _ = raise wrong_args_exp ()

local
    fun graph_maker default_name graph state = let
    val (nm,gs) = (get_graphs state)
              |> GraphName.NTab.add (default_name, graph)
    in ((GraphName.dest nm) ^ "\n", state |> set_graphs gs)
    end
in

fun duplicate_graph ([name], state) = 
    let val gname = (GraphName.mk name) 
    in graph_maker gname (fetch_graph gname state) state
    end
  | duplicate_graph _ = raise wrong_args_exp ()
fun new_graph ([], state) =
    graph_maker GraphName.default_name G.empty state
  | new_graph _ = raise wrong_args_exp ()
fun load_graph ([fname], state) = 
    graph_maker GraphName.default_name (LoadSave.load_graph fname) state
  | load_graph _ = raise wrong_args_exp ()
fun input_graph_xml ([], state) = let
    val gr_block = read_block ()
    val gr = RGGraphInputXML.input (XMLReader.read_from_string gr_block)
in graph_maker GraphName.default_name gr state
end
  | input_graph_xml _ = raise wrong_args_exp ()
end


fun kill_graph ([name], state) =
    (("ok\n", update_graphs
                (fn gs => gs |> GraphName.NTab.delete (GraphName.mk name)) state)
     handle GraphName.NTab.Tab.UNDEF _ => raise user_exp ("no such graph: "^name))
  | kill_graph _ = raise wrong_args_exp ()

fun rename_graph ([old,new], state) = (let
        val graphs = get_graphs state
        val oldname = GraphName.mk old
        val gr = GraphName.NTab.get graphs oldname
        val (nm,graphs') = graphs
                  |> GraphName.NTab.delete oldname
                  |> GraphName.NTab.add (GraphName.mk new, gr)
    in
      (Pretty.string_of (GraphName.pretty_name nm) ^ "\n",
       set_graphs graphs' state)
    end
    handle GraphName.NTab.Tab.UNDEF _ => raise user_exp ("no such graph: "^old))
  | rename_graph _ = raise wrong_args_exp ()


fun save_graph ([_, fname], graph) = 
    ((LoadSave.save_graph fname graph; "Saved file " ^ fname ^"\n")
     handle _ => raise user_exp ("Could not save file "^fname))
  | save_graph _ = raise wrong_args_exp ()

fun vertex_type "red" = V.IVert (V.Xnd AngleExpr.zero)
  | vertex_type "green" =  V.IVert (V.Znd AngleExpr.zero)
  | vertex_type "hadamard" = V.IVert V.Hnd
  | vertex_type "boundary" = V.BVert
  | vertex_type _ = raise user_exp "Bad vertex type."

fun add_vertex ([_,vt], graph) =
    let val (n,g) = graph |> G.add_vertex (vertex_type vt)
    in ("added vertex \"" ^ (G.Vertex.dest n) ^ "\"\n", g)
    end
  | add_vertex _ = raise wrong_args_exp ()

fun add_edge ([_,v1,v2], graph) =
    (let val (n,g) = graph |> G.add_edge () (V.mk v1) (V.mk v2)
     in ("added edge \"" ^ (G.Edge.dest n) ^ "\"\n", g)
     end
     handle G.no_such_vname_exp _ =>
            raise user_exp "Bad vertex name.")
  | add_edge _ = raise wrong_args_exp ()


fun delete_vertices (_::vs, graph) =
    (("ok\n", fold G.delete_vertex (map V.mk vs) graph)
     handle G.no_such_vname_exp _ => raise user_exp "No such vertex.")
  | delete_vertices _ = raise wrong_args_exp ()

fun delete_edges (_::es, graph) =
    (("ok\n", fold G.delete_edge (map E.mk es) graph)
     handle G.no_such_ename_exp _ => raise user_exp "No such edge.")
  | delete_edges _ = raise wrong_args_exp ()

fun rename_vertex ([_,old,new], graph) =
    (("renamed to \""^new^"\"\n", graph |> G.rename_vname (V.mk old) (V.mk new))
     handle G.vname_already_exists_exp _ =>
            raise user_exp "graph already contains a vertex of this name"
          | G.no_such_vname_exp _ => raise user_exp "no such vertex")
  | rename_vertex _ = raise wrong_args_exp ()


fun graph_xml [nme] = (suffix "\n") o
              Pretty.string_of  o
              RGGraphOutputXML.output
  | graph_xml _ = raise wrong_args_exp ()

fun print_graph [_] = (suffix "\n") o Pretty.string_of o G.pretty
  | print_graph _ = raise wrong_args_exp ()
                      
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
  | undo _ = raise wrong_args_exp ()

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
    end
  | redo _ = raise wrong_args_exp ()

fun apply_rewrite ([name, n], state) =
    (let val gname = GraphName.mk name
         val rewrites = GraphName.NTab.get (get_rewrites state) gname
         val (name,rule,fused) = nth rewrites (the (Int.fromString n))
     in ("applied "^(SStrName.dest name)^"\n",
         state |> push_hist gname
               |> set_graph_at gname (Rule.del_lhs rule fused))
     end
     handle GraphName.Tab.UNDEF _ => raise user_exp "No rewrites attached." 
          | Subscript => raise user_exp "Rewrite not attached."
          | Option => raise user_exp "Rewrite selection must be an integer.")
  | apply_rewrite _ = raise wrong_args_exp ()

local
    fun attach_with' attacher (args as _::vs, state) =
        (let val gname = GraphName.mk (graph_arg args)
	           val vset = fold V.NSet.ins_fresh (map V.mk vs) V.NSet.empty
	           val graph = fetch_graph gname state
             val rules = RuleSet.get_active_rules (get_rulesets state)
	           val rws = map (fn (n,r) => (n, r, Rule.add_rhs r graph))
                           (Seq.list_of (attacher rules vset graph))
         in
	           (length rws,
	            state |> update_rewrites (GraphName.NTab.update (gname,rws)))
         end)
    | attach_with' _ _ = raise wrong_args_exp ()

    fun attach_with attacher args = let
        val (n,st) = attach_with' attacher args
    in ("attached "^(Int.toString n)^" rewrites\n",st)
    end
in
val attach_rewrites = attach_with RuleSet.apply_rules_in
val attach_one_rewrite = attach_with RuleSet.apply_a_rule_in

fun apply_first_rewrite (args as [_], state) = let
    val gname = GraphName.mk (graph_arg args)
    val graph = fetch_graph gname state
    val args' = (GraphName.pretty_name gname)::
                (map V.pretty_name (V.NSet.list_of (G.get_vnames graph)))
    val (n,state') = attach_with' RuleSet.apply_a_rule_in (map Pretty.string_of args', state)
in if n = 0 then raise user_exp "No more rewrites."
   else apply_rewrite (args @ ["0"],state')
end
  | apply_first_rewrite _ = raise wrong_args_exp ()

end

fun show_rewrites (args,state) = let
    val gname = GraphName.mk (graph_arg args)
    val rewrites = case GraphName.NTab.lookup (get_rewrites state) gname
            of NONE => raise user_exp "No rewrites attached."
             | SOME r => r
in Pretty.string_of (RGRewriteListOutputXML.output rewrites)
end
                           
fun vdata a (V.IVert (V.Xnd _)) = V.IVert (V.Xnd (AngleExpr.parse a))
  | vdata a (V.IVert (V.Znd _)) = V.IVert (V.Znd (AngleExpr.parse a))
  | vdata a _ = raise user_exp "Can only set angles for red and green vertices."

fun set_angle ([_,v,a], graph) =
    (("ok\n", graph |> G.update_vertex_data (vdata a) (V.mk v))
     handle AngleExpr.parse_exp => raise user_exp "Cannot parse angle expression.")
  | set_angle _ = raise wrong_args_exp ()

fun hilb ([_, format], graph) =
    (Pretty.string_of (
     RGHilbTerm.pretty (AlgFormat.of_string format)
              (RGHilbTerm.of_graph graph)) ^ "\n"
    handle AlgFormat.not_supported_exp =>
       raise user_exp "Term format not supported.")
  | hilb _ = raise wrong_args_exp ()

fun add_bang ([_], graph) = 
    graph |> G.new_box (G.BBox.default_name)
          >> (fn a => (G.BBox.dest a) ^ "\n")
  | add_bang _ = raise wrong_args_exp ()

fun drop_bang ((_::names), graph) =
  let  
    val namestr = foldl (fn (x,y) => x^" "^y) "." names
  in
    ("dropped !-box \""^namestr^"\"\n",
      graph |> fold G.drop_box (map G.BBox.mk names))
  end  
  | drop_bang _ = raise wrong_args_exp ()

fun kill_bang ((_::names), graph) =
  let  
    val namestr = foldl (fn (x,y) => x^" "^y) "." names
  in
    ("kill !-box \""^namestr^"\"\n",
     graph |> fold G.kill_box (map G.BBox.mk names))
  end
  | kill_bang _ = raise wrong_args_exp ()
  
fun merge_bang ((_::names), graph) =
  let 
    val namesstr = foldl (fn (x,y) => x^" "^y) "." names
    val nameset = G.BBox.NSet.of_list (map G.BBox.mk names)
  in  
    ("merge !-boxes \""^namesstr^"\n",
     graph |> G.merge_boxes nameset |> snd )
  end 
  | merge_bang _ = raise wrong_args_exp ()
  
fun duplicate_bang ([_,name], graph) =
    ("duplicate !-box \""^name^"\n",
     graph |> G.copy1_box (G.get_vnames graph, 
                           G.get_enames graph,
                           G.get_bnames graph) (G.BBox.mk name) 
           |> snd)
  | duplicate_bang _ = raise wrong_args_exp ()
  
fun bang_vertices ((_::box::verts), graph) = 
    ("added vertices to \""^box^"\"\n",
     graph |> G.move_to_box (G.BBox.mk box) (G.Vertex.NSet.of_list (map V.mk verts)))
  | bang_vertices _ = raise wrong_args_exp ()

fun unbang_vertices ((_::verts), graph) =
    ("un-!'ed vertices\n",
     graph |> G.move_to_unbox (G.Vertex.NSet.of_list (map V.mk verts)))
  | unbang_vertices _ = raise wrong_args_exp ()

fun copy_subgraph (args as (_::target::vs), state) = let
    val graph = fetch_graph (GraphName.mk (graph_arg args)) state
    val clip = G.pull_subgraph_within_selection (G.Vertex.NSet.of_list (map V.mk vs))
                                                            graph
in ("ok\n", state |> set_graph_at (GraphName.mk target) clip)
end
  | copy_subgraph _ = raise wrong_args_exp ()

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
  | insert_graph _ = raise wrong_args_exp ()

fun flip_vertices (_::vs, graph) = let
    fun flip_data (V.IVert (V.Xnd e)) = V.IVert (V.Znd e)
      | flip_data (V.IVert (V.Znd e)) = V.IVert (V.Xnd e)
      | flip_data d = d
in ("ok\n", fold (G.update_vertex_data flip_data) (map V.mk vs) graph)
end
  | flip_vertices _ = raise wrong_args_exp ()
  

fun export_rule getter label ([ruleset,rule],state) =
    let
      val rulesets = get_rulesets state
      val rset = case RuleSet.NTab.lookup rulesets (RuleSet.mk ruleset)
                 of SOME v => v | NONE => raise user_exp "Can't find ruleset."
      val rl = case Rule.Name.NTab.lookup (fst rset) (Rule.Name.mk rule)
                   of SOME v => v | NONE => raise user_exp "Can't find rule."
      val (name, graphs) = GraphName.NTab.add (GraphName.mk (rule^"_"^label), getter (fst rl)) (get_graphs state)
    in
      (((suffix "\n") o Pretty.string_of o GraphName.pretty_name) name,
       state |> set_graphs graphs)
    end
  | export_rule _ _ _ = raise wrong_args_exp ()

val open_rule_lhs = export_rule Rule.get_lhs "lhs"
val open_rule_rhs = export_rule Rule.get_rhs "rhs"

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
    (lift_stateless (K "HELO\n"));

    register_cmd
    "echo_block"
    ("echo_block\n\n"^
     "    debug command for testing block input\n")
    (lift_stateless echo_block);

   register_cmd
    "load_ruleset"
    ("load_ruleset NAME FILE\n\n" ^
     "    load a ruleset from FILE and call it NAME\n")
    load_ruleset;

    register_cmd
    "unload_ruleset"
    ("unload_ruleset RULESET\n\n" ^
     "    unload RULESET\n")
    unload_ruleset;
    
   register_cmd
    "save_ruleset"
    ("save_ruleset RULESET FILENAME\n\n" ^
     "    save RULESET in FILENAME\n")
    (lift_state_viewer save_ruleset);

    register_cmd
    "list_graphs"
    ("list_graphs\n\n"^
     "    list all active graphs\n")
    (lift_state_viewer list_graphs);

    register_cmd
    "ls"
    ("ls\n\n"^
     "    alias for \"list_graphs\"\n")
    (lift_state_viewer list_graphs);

    register_cmd
    "list_rulesets"
    ("list_rulesets\n\n"^
     "    list all loaded rulesets\n")
    (lift_state_viewer list_rulesets);

    register_cmd
    "list_active_rulesets"
    ("list_active_rulesets\n\n"^
     "    list all active rulesets\n")
    (lift_state_viewer list_active_rulesets);

    register_cmd
    "list_rules"
    ("list_rules RULESET\n\n"^
     "    list the rules in RULESET\n")
    (lift_state_viewer list_rules);

    register_cmd
    "activate_ruleset"
    ("activate_ruleset RULESET\n\n"^
     "    enable the rules in RULESET for rewriting\n")
    (activate_ruleset);

    register_cmd
    "deactivate_ruleset"
    ("deactivate_ruleset RULESET\n\n"^
     "    disable the rules in RULESET for rewriting\n")
    (deactivate_ruleset);

    register_cmd
    "activate_rule"
    ("activate_rule RULESET RULE\n\n"^
     "    enable RULE in RULESET for rewriting\n")
    (activate_rule);

    register_cmd
    "deactivate_rule"
    ("deactivate_ruleset RULESET RULE\n\n"^
     "    disable RULE in RULESET for rewriting\n")
    (deactivate_rule);

    register_cmd
    "delete_rule"
    ("delete_ruleset RULESET RULE\n\n"^
     "    remove RULE from RULESET for rewriting\n")
    (delete_rule);

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
    "input_graph_xml"
    ("input_graph_xml\n\n" ^
     "    create a new graph from the XML data given as block input\n")
    (input_graph_xml);
    
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
    "rename_graph"
    ("rename_graph OLD NEW\n\n"^
     "    rename graph OLD to NEW, return the real new\n"^
     "    name.\n")
    (rename_graph);

    register_cmd
    "kill_graph"
    ("kill_graph GRAPH\n\n"^
     "    remove GRAPH from the workspace\n")
    (kill_graph);

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
    "attach_one_rewrite"
    ("attach_one_rewrite GRAPH V1 .. VN\n\n"^
     "    calculate at most one rewrite involving V1..VN in"^
     "    GRAPH and attach to the graph state\n")
    (attach_one_rewrite);

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
    "apply_first_rewrite"
    ("apply_first_rewrite GRAPH N\n\n"^
     "    attempt to attach one rewrite to GRAPH and apply it\n")
    (apply_first_rewrite);

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
    "bbox_drop"
    ("bbox_drop GRAPH BANGS\n\n"^
     "    drop !-boxes BANGS in GRAPH. Vertices inside BANGS are moved\n"^
     "    to the top-level graph.\n")
    (lift_graph_mutator drop_bang);

    register_cmd
    "bbox_kill"
    ("bbox_kill GRAPH BANGS\n\n"^
     "    kill !-boxes BANGS in GRAPH. Vertices inside BANGS are deleted\n"^
     "    to the top-level graph.\n")
    (lift_graph_mutator kill_bang);
    
    register_cmd
    "bbox_merge"
    ("bbox_kill GRAPH BANGS\n\n"^
     "    merges !-boxes BANGS in GRAPH.\n")
    (lift_graph_mutator merge_bang);
    
    register_cmd
    "bbox_duplicate"
    ("bbox_duplicate GRAPH BANGS\n\n"^
     "    duplicates !-boxes BANGS in GRAPH.\n")
    (lift_graph_mutator duplicate_bang);
    
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
     "    boundary vertices.\n")
    (lift_graph_mutator flip_vertices);

    register_cmd
    "open_rule_lhs"
    ("open_rule_lhs RULESET RULE\n\n"^
     "    Open the LHS of the given rule and return the name of\n"^
     "    the new graph buffer.\n")
    (open_rule_lhs);

    register_cmd
    "open_rule_rhs"
    ("open_rule_rhs RULESET RULE\n\n"^
     "    Open the RHS of the given rule and return the name of\n"^
     "    the new graph buffer.\n")
    (open_rule_rhs);

    
    register_cmd
    "replace_rule"
    ("replace_rule RULESET RULE LHS RHS\n\n"^
     "    Replace the given rule with graphs LHS and RHS in the\n"^
     "    workspace.\n")
    (replace_rule);


    register_cmd
    "new_rule"
    ("new_rule RULESET GRAPH\n\n"^
     "    Create a new identity rule from GRAPH.\n")
    (new_rule);
    
    ())



(* Uniquely mark end of text as SPACE + BACKSPACE. This is well-behaved as
 * long as commands never send a literal BACKSPACE (0x08). *)
val EOT = " "^(chr 8)

fun do_cmd ("quit",_) _ = ("Exiting...\n", QUIT)
  | do_cmd ("exit",_) _ = ("Exiting...\n", QUIT)
  | do_cmd ("NOOP",_) _ = ("", SKIP)
  | do_cmd (cmd as (cname,args)) state =
    (case Symtab.lookup (!cmds) cname
      of SOME (_,f) => (fn (a,b)=>(a, NORMAL b)) (f (args, state))
       | NONE => ("!!! Unknown command: "^
          (QuantoCommand.toString cmd)^"\n", NORMAL state))
    handle wrong_args_exp () =>
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
in 
  case TextIO.inputLine TextIO.stdIn 
   of NONE => (TextIO.print "Exiting...\n"; TextIO.print EOT; ())
    | SOME s =>
      let 
        val cmd_str = unsuffix "\n" (s)
        val (output, st) = eval cmd_str state
      in                             
        TextIO.print (implode output); TextIO.print EOT;
        (case st
          of NORMAL state' => repl state'
           | SKIP => repl state
           | QUIT => (TextIO.print EOT; ()))
      end
end;

fun flushIn () = case TextIO.canInput (TextIO.stdIn, 1)
                      of SOME n => (TextIO.input1 TextIO.stdIn;())
                       | NONE => ()

fun init () = (flushIn(); repl newState)

end (* structure Controller *)
