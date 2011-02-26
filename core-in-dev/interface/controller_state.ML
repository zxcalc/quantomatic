(* controller state *)

structure GraphName :> SSTR_NAMES = 
struct open SStrName val default_name = SStrName.mk "new-graph-1" end;

signature CONTROLLER_STATE =
sig
  type T;
end;

(* controls the state of Quantomatic *)
structure DB_ControllerState = 
struct
  
  exception user_exp of string
  
  structure RuleSet = RGRuleSet;
  structure R = RGRule;
  structure G = RGGraph;
  structure V = G.RGVertex;
  structure E = G.Edge;
  structure LoadSave = RGGraphLoaderSaver;
  
  type rewrite = (RuleName.name * R.T) * G.T (* rule, and graph to be rewritten to *)

  (* data for controller; top level data with which user interacts *)
  datatype state = CState of {
       graphs : G.T GraphName.NTab.T, (* the known graphs *)
       rewrites : (rewrite list) GraphName.NTab.T, (* cached possible rewrites *)
       hist : (G.T list * G.T list) GraphName.NTab.T, (* past and future *)
       rulesets : RuleSet.T (* the known rules *)
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
  
  (*val default_rulesets = RuleSet.NTab.empty
                             |> RuleSet.NTab.update (RuleSet.mk "default",
                                                    (!RuleSetHack.rules,true))
  *)
  
  val newState = CState {graphs=GraphName.NTab.empty,
                         rewrites=GraphName.NTab.empty,
                         hist=GraphName.NTab.empty,
                         rulesets=RuleSet.empty};
  
  fun handle_bug_in_function f (x as (l,st)) = (f x) 
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
      | G.BBox.Tab.UNDEF x =>
        raise user_exp ("BUG: G.BBox.Tab.UNDEF")
      | E.NSet.duplicate_exp _ => 
        raise user_exp ("BUG: Duplicate Edge.")
      | V.NSet.duplicate_exp _ => 
        raise user_exp ("BUG: Duplicate vertex.")
      | V.Tab.UNDEF x =>
        raise user_exp ("BUG: G.Vertex.Tab.UNDEF")
      | E.Tab.UNDEF x =>
        raise user_exp ("BUG: G.Edge.Tab.UNDEF");
  
  
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
  fun lift_stateless cmd (args, state:state) = (cmd args, state)
  
  (* Read-only state commands, cmd: [args] * state -> out *)
  fun lift_state_viewer cmd (args, state:state) = (cmd (args,state), state)
  
  
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
  in loop () end
  handle SimpleLexer.Parse => raise user_exp "Bad start of block"
  
  
  (* Most commands are already of the form
   *    cmd: [args] * state -> out * state *)
  (* val lift_state_mutator = I *)
  
  (* ============================================== *)
  (* these functions actually implement commmands *)
  
  exception wrong_args_exp of unit
  
  fun echo_block [] = read_block ()
    | echo_block _ = raise wrong_args_exp ()
  
  fun list_graphs ([], state) = 
      GraphName.NTab.fold (suffix o suffix "\n" o GraphName.dest o fst)
        (get_graphs state) ""
    | list_graphs _ = raise wrong_args_exp ()
  
  (* common things we want to assert are true *)
  fun assert_tag_exists state tag_name = 
      if RuleSet.tag_exists (get_rulesets state) tag_name then ()
      else raise user_exp ("No such ruleset named '" 
                           ^ (TagName.string_of_name tag_name) ^ "'");

  fun assert_rule_exists state rule_name = 
      if RuleSet.rule_exists (get_rulesets state) rule_name then ()
      else raise user_exp ("No such rule named '" 
                           ^ (RuleName.string_of_name rule_name) ^ "'");

  (* basic stuff on rule tags *)
  fun list_allrules ([], state) =
      (Pretty.string_of (RuleName.NSet.pretty (RuleName.NTab.get_nameset (RuleSet.get_allrules (get_rulesets state)))))
      ^ "\n"
    | list_allrules _ = raise wrong_args_exp ()
  
  fun list_active_rules ([], state) =
      (Pretty.string_of (RuleName.NSet.pretty (RuleSet.get_active (get_rulesets state))))
      ^ "\n"
    | list_active_rules _ = raise wrong_args_exp ()
  
  fun list_rules_with_tag ([tag], state) = 
      let 
        val tag_name = TagName.mk tag;
        val () = assert_tag_exists state tag_name;
        val rule_name_set = TagRel.codf (RuleSet.get_tagrel (get_rulesets state)) tag_name
      in (suffix "\n" o Pretty.string_of) (RuleName.NSet.pretty rule_name_set) end
    | list_rules_with_tag _ = raise wrong_args_exp ();
      
  fun tag_name_action f ([tag], state) =
      let val tag_name = TagName.mk tag
          val () = assert_tag_exists state tag_name
      in ("ok\n", update_rulesets (f tag_name) state) end
    | tag_name_action f (_, state) = raise wrong_args_exp ();
  val deactivate_ruleset = tag_name_action RuleSet.deactivate_tag; 
  val activate_ruleset = tag_name_action RuleSet.deactivate_tag
  val delete_ruleset = tag_name_action RuleSet.delete_tag


  fun rule_name_action f ([rule], state) = 
      let val rule_name = TagName.mk rule
          val () = assert_rule_exists state rule_name
      in ("ok\n", update_rulesets (f rule_name) state) end
    | rule_name_action f (_, state) = raise wrong_args_exp ();
  val deactivate_rule  = rule_name_action RuleSet.deactivate_rule
  val activate_rule = rule_name_action RuleSet.activate_rule
  val delete_rule = rule_name_action RuleSet.delete_rule

  (* creates/replaces rule *)
  fun update_rule ([rule,lhs,rhs], state) =
      let
        val rule_name = TagName.mk rule
        val () = assert_rule_exists state rule_name
  
        val graphs = get_graphs state
        
        (* build graphs for the LHS and RHS *)
        val lgr = GraphName.NTab.get graphs (GraphName.mk lhs)
          handle GraphName.Tab.UNDEF _ => raise user_exp ("LHS graph '" ^ lhs ^"' doesn't exist.")
        val rgr = GraphName.NTab.get graphs (GraphName.mk rhs)
          handle GraphName.Tab.UNDEF _ => raise user_exp ("RHS graph '" ^ rhs ^"' doesn't exist.")
          
      in ("ok\n", state |> update_rulesets (RuleSet.update_rule (rule_name, RuleSet.Rule.mk (lgr, rgr))))
          handle RuleSet.Rule.bad_rule_exp _ =>
            raise user_exp "Boundaries and !-boxes on LHS must match RHS."
      end
    | update_rule _ = raise wrong_args_exp ()
  
  (* create a new identity rule from the current graph *)
  fun new_rule ([rule,graph], state) = update_rule ([rule,graph,graph], state)  
    | new_rule (_, state) = raise wrong_args_exp ();
  
  fun load_ruleset ([file], state) =
      (let val loader = InputRGRuleSetXML.input o XMLReader.read_from_file
       in ("ok\n", state |> update_rulesets (RuleSet.merge (loader file))) end
       handle IO.Io _ => raise user_exp "Can't open ruleset.")
    | load_ruleset _ = raise wrong_args_exp ();
  
  fun save_ruleset ([file], state) =
      (let val save = (PrettyWriter.write_to_file file) o OutputRGRuleSetXML.output
       in (save (get_rulesets state); "Saved ruleset in file " ^ file)
       end
       handle IO.Io _ => raise user_exp ("Can't write ruleset file "^file^"."))
    | save_ruleset _ = raise wrong_args_exp ();
    
  fun delete_tag ([tag], state) =
      let val () = assert_tag_exists state tag
      in ("ok\n", update_rulesets (RuleSet.delete_tag tag) state) end
    | delete_tag _ = raise wrong_args_exp ();

  fun delete_rules_in_tag ([tag], state) =
      let val () = assert_tag_exists state tag
      in ("ok\n", update_rulesets (RuleSet.delete_rules_in_tag tag) state) end
    | delete_rules_in_tag _ = raise wrong_args_exp ();

  fun delete_tag_and_its_rules ([tag], state) =
      let val () = assert_tag_exists state tag
      in ("ok\n", update_rulesets (RuleSet.delete_tag tag 
                                   o RuleSet.delete_rules_in_tag tag) state) end
    | delete_tag_and_its_rules _ = raise wrong_args_exp ();
      
  local
    fun graph_maker default_name graph state = let
        val (nm,gs) = (get_graphs state)
                  |> GraphName.NTab.add (default_name, graph)
        in ((GraphName.dest nm) ^ "\n", state |> set_graphs gs) end
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
    fun input_graph_xml ([], state) = 
        let val gr_block = read_block ()
            val gr = InputRGGraphXML.input (XMLReader.read_from_string gr_block)
        in graph_maker GraphName.default_name gr state end
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
    | vertex_type "boundary" = V.EVert
    | vertex_type _ = raise user_exp "Bad vertex type."
  
  fun add_vertex ([_,vt], graph) =
      let val (n,g) = graph |> G.add_vertex (vertex_type vt)
      in ("added vertex \"" ^ (G.Vertex.dest n) ^ "\"\n", g)
      end
    | add_vertex _ = raise wrong_args_exp ()
  
  fun add_edge ([_,v1,v2], graph) =
      (let val (n,g) = graph |> G.add_edge (G.UnDirected,()) (V.mk v1) (V.mk v2)
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
                OutputRGGraphXML.output
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
            | General.Subscript => raise user_exp "Rewrite not attached."
            | Option.Option => raise user_exp "Rewrite selection must be an integer.")
    | apply_rewrite _ = raise wrong_args_exp ()
  
  local
      fun attach_with' attacher (args as _::vs, state) =
          (let val gname = GraphName.mk (graph_arg args)
               val vset = fold V.NSet.ins_fresh (map V.mk vs) V.NSet.empty
               val graph = fetch_graph gname state
               val rules = RuleSet.get_active (get_rulesets state)
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
                             G.get_bbnames graph) (G.BBox.mk name) 
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
  
end; (* structure Controller *)

(* *)
structure ControllerState : CONTROLLER = DB_Controller;
