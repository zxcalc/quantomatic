(* controller state *)

structure GraphName :> SSTR_NAMES = 
struct open SStrName val default_name = SStrName.mk "new-graph-1" end;

signature CONTROLLER_STATE =
sig
  type T;
  structure Theory : GRAPHICAL_THEORY;
  type rewrite = (RuleName.name * Theory.Rule.T) * Theory.Graph.T
  type vertex = (Theory.Vertex.name * (Theory.Vertex.data * (Theory.Edge.NSet.T * Theory.Edge.NSet.T)));
  type edge = (Theory.Edge.name * ((Theory.Graph.dir_or_undir * Theory.Edge.data) * (Theory.Vertex.name * Theory.Vertex.name)));

  (* FIXME: ditch user_exp *)
  exception user_exp of string; (* for errors sent to user console *)
  exception unknown_graph_exp of string;
  exception unknown_vertex_exp of string;
  exception unknown_edge_exp of string;
  exception unknown_bbox_exp of string;

  val init : T (* initial empty state, no graphs *)

  (* get/set*)
  val get_rset : T -> Theory.Ruleset.T
  val get_rewrites : T -> rewrite list GraphName.NTab.T
  val get_hist : T -> (Theory.Graph.T list * Theory.Graph.T list) GraphName.NTab.T
  val get_graphs : T -> Theory.Graph.T GraphName.NTab.T
  val get_graph : T -> GraphName.name -> Theory.Graph.T
  val get_graph_names : T -> GraphName.name list

  val update_rset : (Theory.Ruleset.T -> Theory.Ruleset.T) -> T -> T
  val update_rewrites :
   (rewrite list GraphName.NTab.T -> rewrite list GraphName.NTab.T) ->
     T -> T
  val update_hist :
   ((Theory.Graph.T list * Theory.Graph.T list) GraphName.NTab.T ->
     (Theory.Graph.T list * Theory.Graph.T list) GraphName.NTab.T) -> T -> T
  val update_graphs :
   (Theory.Graph.T GraphName.NTab.T -> Theory.Graph.T GraphName.NTab.T) -> T -> T

  val set_rset : Theory.Ruleset.T -> T -> T
  val set_rewrites : rewrite list GraphName.NTab.T -> T -> T
  val set_hist : (Theory.Graph.T list * Theory.Graph.T list) GraphName.NTab.T -> T -> T
  val set_graphs : Theory.Graph.T GraphName.NTab.T -> T -> T
  val set_graph : Theory.Graph.T -> GraphName.name -> T -> T



  (* history *)
  val undo : GraphName.name -> T -> T
  val redo : GraphName.name -> T -> T
  val push_hist : GraphName.name -> T -> T

  (* printing/showing info (for console) *)
  val get_attached_rewrites : GraphName.name -> T -> rewrite list
  val list_rules_with_tag : TagName.name -> T -> TagName.name list
  val list_allrules : T -> RuleName.name list
  val list_tags : T -> TagName.name list
  val list_active_rules : T -> RuleName.name list

  (* rewriting *)
  val attach_rewrites : GraphName.name -> Theory.Vertex.name list -> T -> int * T
  val attach_first_rewrite : GraphName.name -> Theory.Vertex.name list -> T -> int * T
  val apply_rewrite : GraphName.name -> int -> T -> T
  val apply_first_rewrite : GraphName.name -> T -> T

  (* rules/rulsesets *)
  val set_rule : RuleName.name -> GraphName.name -> GraphName.name -> T -> T
  val open_rule_lhs : T -> RuleName.name -> GraphName.name * T
  val open_rule_rhs : T -> RuleName.name -> GraphName.name * T
  val delete_tag_and_its_rules : TagName.name -> T -> T
  val delete_tag : TagName.name -> T -> T
  val delete_rules_in_tag : TagName.name -> T -> T
  val delete_rule : RuleName.name -> T -> T

  val activate_rules_with_tag : TagName.name -> T -> T
  val activate_rule : RuleName.name -> T -> T
  val deactivate_rules_with_tag : TagName.name -> T -> T
  val deactivate_rule : RuleName.name -> T -> T

  val tag_rule : RuleName.name -> TagName.name -> T -> T
  val untag_rule : RuleName.name -> TagName.name -> T -> T

  (* graphs and their manipulations *)
  val kill_graph : GraphName.name -> T -> T
  val insert_graph : GraphName.name -> GraphName.name -> T -> T
  val add_graph : GraphName.name -> Theory.Graph.T -> T -> GraphName.name * T
  val copy_subgraph : GraphName.name -> GraphName.name -> Theory.Vertex.name list -> T -> T

  val add_vertex : string -> GraphName.name -> T -> vertex * T
  val add_edge : string -> Theory.Graph.dir_or_undir  -> Theory.Vertex.name -> Theory.Vertex.name -> GraphName.name -> T -> edge * T
  val add_bang : GraphName.name -> T -> Theory.Graph.BBox.name * T

  val update_vertex_data : (Theory.Vertex.data -> Theory.Vertex.data) -> Theory.Vertex.name -> GraphName.name -> T -> T
  val update_edge_data : (Theory.Edge.data -> Theory.Edge.data) -> Theory.Edge.name -> GraphName.name -> T -> T
  val rename_vertex : Theory.Vertex.name -> Theory.Vertex.name -> GraphName.name -> T -> T
  val rename_graph : GraphName.name -> GraphName.name -> T -> GraphName.name * T
  val delete_vertices : Theory.Vertex.name list -> GraphName.name -> T -> T
  val delete_edges : Theory.Edge.name list -> GraphName.name -> T -> T

  val bang_vertices : Theory.Graph.BBox.name -> Theory.Vertex.name list -> GraphName.name -> T -> T
  val kill_bang : Theory.Graph.BBox.name list -> GraphName.name -> T -> T
  val unbang_vertices : Theory.Vertex.name list -> GraphName.name -> T -> T
  val merge_bang : Theory.Graph.BBox.name list -> GraphName.name -> T -> Theory.Graph.BBox.name * T
  val duplicate_bang : Theory.Graph.BBox.name -> GraphName.name -> T -> Theory.Graph.BBox.name * T
  val drop_bang : Theory.Graph.BBox.name list -> GraphName.name -> T -> T

end;


(* controls the state of Quantomatic *)
functor ControllerState(
  structure GraphicalTheory : GRAPHICAL_THEORY
) 
= struct


  exception user_exp of string
  exception unknown_graph_exp of string;
  exception unknown_vertex_exp of string;
  exception unknown_edge_exp of string;
  exception unknown_bbox_exp of string;

  structure Theory = GraphicalTheory;
  structure RulesetRewrites = Theory.RulesetRewrites
  structure Ruleset = Theory.Ruleset;
  structure R = Theory.Rule;
  structure G = Theory.Graph;
  structure V = Theory.Vertex;
  structure E = Theory.Edge;

  type rewrite = (RuleName.name * R.T) * G.T (* rule, and graph to be rewritten to *)
  type vertex = (V.name * (V.data * (E.NSet.T * E.NSet.T)));
  type edge = (E.name * ((G.dir_or_undir * E.data) * (V.name * V.name)));

  (* data for controller; top level data with which user interacts *)
  datatype T = CState of {
       graphs : G.T GraphName.NTab.T, (* the known graphs *)
       rewrites : (rewrite list) GraphName.NTab.T, (* cached possible rewrites *)
       hist : (G.T list * G.T list) GraphName.NTab.T, (* past and future *)
       rset : Ruleset.T (* the known rules *)
  }

  (* getters and setters for the state *)
  fun update_graphs f (CState {graphs=g,rewrites=r,hist=h,rset=t}) = 
    CState {graphs=(f g),rewrites=r,hist=h,rset=t}
  fun update_rewrites f (CState {graphs=g,rewrites=r,hist=h,rset=t}) =
    CState {graphs=g,rewrites=(f r),hist=h,rset=t}
  fun update_hist f (CState {graphs=g,rewrites=r,hist=h,rset=t}) = 
    CState {graphs=g,rewrites=r,hist=(f h),rset=t}
  fun update_rset f (CState {graphs=g,rewrites=r,hist=h,rset=t}) = 
    CState {graphs=g,rewrites=r,hist=h,rset=(f t)}
  val (set_graphs,set_rewrites,set_hist,set_rset) = 
    (update_graphs o K, update_rewrites o K, update_hist o K, update_rset o K)
  fun get_graphs (CState {graphs=g,...}) = g
  fun get_rewrites (CState {rewrites=r,...}) = r
  fun get_hist (CState {hist=h,...}) = h
  fun get_rset (CState {rset=t,...}) = t

  val init = CState {graphs=GraphName.NTab.empty,
                         rewrites=GraphName.NTab.empty,
                         hist=GraphName.NTab.empty,
                         rset=Ruleset.empty};

  (* state accessors *)
  fun get_graph state name =
      (case GraphName.NTab.lookup (get_graphs state) name
       of SOME g => g
        | NONE => raise unknown_graph_exp (GraphName.dest name))

  val get_graph_names = GraphName.NSet.list_of o GraphName.NTab.get_nameset o get_graphs

  (* state mutators *)
  fun push_hist gname state = 
      let
        val (back,_) = 
            the_default ([],[]) (GraphName.NTab.lookup (get_hist state) gname)
        val hist' = (get_hist state)
                          |> GraphName.NTab.update
                          (gname,((get_graph state gname)::back, []))
      in state |> set_hist hist'
      end

  (* any time the graph is changed, any attached rewrites are invalid,
   * so should be cleared *)
  fun set_graph graph' gname state =
      state |> update_graphs (GraphName.NTab.update (gname,graph'))
              |> update_rewrites (GraphName.NTab.update (gname,[]))    


  (* ============================================== *)
  (* these functions actually implement commmands *)

  (* common things we want to assert are true *)
  fun assert_tag_exists state tag_name = 
      if Ruleset.tag_exists (get_rset state) tag_name then ()
      else raise user_exp ("No such tag named '" 
                           ^ (TagName.string_of_name tag_name) ^ "'");

  fun assert_rule_exists state rule_name = 
      if Ruleset.rule_exists (get_rset state) rule_name then ()
      else raise user_exp ("No such rule named '" 
                           ^ (RuleName.string_of_name rule_name) ^ "'");

  fun assert_v_exists graph vname =
        if G.has_vname graph vname then ()
        else raise user_exp ("No such vertex named '" ^ (V.string_of_name vname) ^ "'")

  fun assert_e_exists graph ename =
        if G.has_ename graph ename then ()
        else raise user_exp ("No such edge named '" ^ (E.string_of_name ename) ^ "'")

  fun assert_bb_exists graph bname =
        if G.has_bname graph bname then ()
        else raise user_exp ("No such !-box named '" ^ (G.BBox.string_of_name bname) ^ "'")


  (* basic stuff on rule tags *)
  val list_allrules =
      RuleName.NSet.list_of o RuleName.NTab.get_nameset o Ruleset.get_allrules o get_rset

  val list_active_rules =
      RuleName.NSet.list_of o Ruleset.get_active o get_rset

  val list_tags =
      TagName.NSet.list_of o Ruleset.get_tags o get_rset

  fun list_rules_with_tag tag_name state = 
      let 
        val () = assert_tag_exists state tag_name;
        val rule_name_set = TagRel.codf (Ruleset.get_tagrel (get_rset state)) tag_name
      in RuleName.NSet.list_of rule_name_set end

  fun tag_name_action f tag_name state =
      let val () = assert_tag_exists state tag_name
      in update_rset (f tag_name) state end
  val deactivate_rules_with_tag = tag_name_action Ruleset.deactivate_tag; 
  val activate_rules_with_tag = tag_name_action Ruleset.activate_tag


  fun rule_name_action f rule state = 
      let val () = assert_rule_exists state rule
      in update_rset (f rule) state end
  val deactivate_rule  = rule_name_action Ruleset.deactivate_rule
  val activate_rule = rule_name_action Ruleset.activate_rule
  val delete_rule = rule_name_action Ruleset.delete_rule

  (* creates/replaces rule *)
  fun set_rule rule_name lhs rhs state =
      let
        val graphs = get_graphs state

        (* build graphs for the LHS and RHS *)
        val lgr = GraphName.NTab.get graphs lhs
          handle GraphName.Tab.UNDEF _ => raise user_exp ("LHS graph '" ^ (GraphName.dest lhs) ^"' doesn't exist.")
        val rgr = GraphName.NTab.get graphs rhs
          handle GraphName.Tab.UNDEF _ => raise user_exp ("RHS graph '" ^ (GraphName.dest rhs) ^"' doesn't exist.")

      in state |> update_rset (Ruleset.update_rule (rule_name, Ruleset.Rule.mk (lgr, rgr)))
          handle Ruleset.Rule.bad_rule_exp _ =>
            raise user_exp "Boundaries and !-boxes on LHS must match RHS."
      end

  fun delete_tag tag_name state =
      let val () = assert_tag_exists state tag_name
      in update_rset (Ruleset.delete_tag tag_name) state end

  fun delete_rules_in_tag tag_name state =
      let val () = assert_tag_exists state tag_name
      in update_rset (Ruleset.delete_rules_in_tag tag_name) state end

  fun delete_tag_and_its_rules tag_name state =
      let val () = assert_tag_exists state tag_name
      in update_rset (Ruleset.delete_tag tag_name
                                   o Ruleset.delete_rules_in_tag tag_name) state end

  fun tag_rule rule_name tag_name state = 
      let val () = assert_rule_exists state rule_name
      in update_rset (Ruleset.tag_rule rule_name tag_name) state end

  fun untag_rule rule_name tag_name state = 
      let val () = assert_rule_exists state rule_name
          val () = assert_tag_exists state tag_name
      in update_rset (Ruleset.untag_rule rule_name tag_name) state end


    fun add_graph default_name graph state = let
        val (nm,gs) = (get_graphs state)
                  |> GraphName.NTab.add (default_name, graph)
        in (nm, state |> set_graphs gs) end

  fun kill_graph name state =
      (update_graphs (fn gs => gs |> GraphName.NTab.delete name) state)
       handle GraphName.NTab.Tab.UNDEF _ => raise user_exp ("no such graph: "^(GraphName.dest name))

  fun rename_graph oldname newname state = (let
          val graphs = get_graphs state
          val gr = GraphName.NTab.get graphs oldname
          val (nm,graphs') = graphs
                    |> GraphName.NTab.delete oldname
                    |> GraphName.NTab.add (newname, gr)
      in
        (nm, set_graphs graphs' state)
      end
      handle GraphName.NTab.Tab.UNDEF _ => raise user_exp ("no such graph: "^(GraphName.dest oldname)))

  fun lift_graph_mutator cmdf gname state =
      let val (out,g') = cmdf (get_graph state gname)
      in (out, state |> push_hist gname 
                     |> set_graph g' gname) end;
  fun lift_graph_mutator_no_result cmdf gname state =
      let val g' = cmdf (get_graph state gname)
      in state |> push_hist gname 
               |> set_graph g' gname end;

  fun add_vertex_internal vt graph =
      (let val (n,g) = graph |> G.add_vertex (Theory.VertexComponentData.default_data vt) in
          ((n,G.get_vertex g n), g)
       end
       handle G.unknown_type_exp _ => raise user_exp ("Unknown vertex type "^vt))
  fun add_vertex vt gname state = lift_graph_mutator (add_vertex_internal vt) gname state

  fun add_edge_internal et dir_or_undir v1 v2 graph =
      (let val (n,g) = graph |> G.add_edge (dir_or_undir,Theory.EdgeComponentData.default_data et) v1 v2
       in ((n,G.get_edge g n), g)
       end
       handle G.no_such_vname_exp _ =>
              raise user_exp "Bad vertex name.")

  fun add_edge et dir_or_undir v1 v2 gname state = lift_graph_mutator
  (add_edge_internal et dir_or_undir v1 v2) gname state


  fun delete_vertices_internal vs graph =
      (fold G.delete_vertex vs graph
       handle G.no_such_vname_exp _ => raise user_exp "No such vertex.")
  fun delete_vertices vs = lift_graph_mutator_no_result (delete_vertices_internal vs)

  fun delete_edges_internal es graph =
      (fold G.delete_edge es graph
       handle G.no_such_ename_exp _ => raise user_exp "No such edge.")
  fun delete_edges es = lift_graph_mutator_no_result (delete_edges_internal es)

  fun rename_vertex_internal old new graph =
      (graph |> G.rename_vname old new
       handle G.vname_already_exists_exp _ =>
              raise user_exp "graph already contains a vertex of this name"
            | G.no_such_vname_exp _ => raise user_exp "no such vertex")
  fun rename_vertex old new graph state = lift_graph_mutator_no_result (rename_vertex_internal old new) graph state


  fun undo gname state = 
      let
        val current = get_graph state gname
      in case GraphName.NTab.lookup (get_hist state) gname
          of SOME (g::gs, hs) =>
              state |> set_graph g gname
                    |> update_hist (GraphName.NTab.update
                                   (gname, (gs,current::hs)))
           | _ => raise user_exp "Can't undo, no more undo history."
      end

  fun redo gname state = 
      let
        val current = get_graph state gname
      in case GraphName.NTab.lookup (get_hist state) gname
          of SOME (gs, h::hs) =>
              state |> set_graph h gname
                    |> update_hist (GraphName.NTab.update
                                   (gname, (current::gs,hs)))
           | _ => raise user_exp "Can't redo, nothing to redo."
      end

  fun apply_rewrite gname n state =
      (let val rewrites = GraphName.NTab.get (get_rewrites state) gname
           val (_,rewritten_graph) = nth rewrites n
       in state |> push_hist gname
                |> set_graph rewritten_graph gname
       end
       handle GraphName.Tab.UNDEF _ => raise user_exp "No rewrites attached." 
            | General.Subscript => raise user_exp "Rewrite not attached."
            | Option.Option => raise user_exp "Rewrite selection must be an integer.")

  local
      fun attach_with attacher gname vs state =
          (let val vset = fold V.NSet.ins_fresh vs V.NSet.empty
               val graph = get_graph state gname
               val rws = (Seq.list_of (attacher (get_rset state) graph vset))
           in
               (length rws,
                state |> update_rewrites (GraphName.NTab.update (gname,rws)))
           end)

      fun apply_first_in rset vs g = 
          case RulesetRewrites.apply_first_in rset vs g of NONE => Seq.empty
          | SOME x => Seq.single x;
  in
  val attach_rewrites = attach_with RulesetRewrites.apply_in;
  val attach_first_rewrite = attach_with apply_first_in; 

  fun apply_first_rewrite gname state = 
      let
        val graph = get_graph state gname 
        val (n,state') = attach_with apply_first_in gname (V.NSet.list_of (G.get_vnames graph)) state
      in if n = 0 then raise user_exp "No more rewrites."
         else apply_rewrite gname 0 state'
      end
  end

  fun get_attached_rewrites gname state = let
      val rewrites = case GraphName.NTab.lookup (get_rewrites state) gname
              of NONE => raise user_exp "No rewrites attached."
               | SOME rs => rs
  in rewrites end;

  fun update_vertex_data_internal vd_upd vn graph =
      let val () = assert_v_exists graph vn in
        graph |> G.update_vertex_data vd_upd vn
      end
  fun update_vertex_data vd_upd vn = lift_graph_mutator_no_result (update_vertex_data_internal vd_upd vn)

  fun update_edge_data_internal ed_upd en graph =
      let val () = assert_e_exists graph en in
        graph |> G.update_edge_data ed_upd en
      end
  fun update_edge_data ed_upd en = lift_graph_mutator_no_result (update_edge_data_internal ed_upd en)

  fun add_bang_internal graph = 
      graph |> G.new_box (G.BBox.default_name)
  val add_bang = lift_graph_mutator add_bang_internal

  fun drop_bang_internal bs graph =
      let val _ = map (assert_bb_exists graph) bs in
        graph |> fold G.drop_box bs
      end
  fun drop_bang bs = lift_graph_mutator_no_result (drop_bang_internal bs)

  fun kill_bang_internal bs graph =
      let val _ = map (assert_bb_exists graph) bs in
        graph |> fold G.kill_box bs
      end
  fun kill_bang bs = lift_graph_mutator_no_result (kill_bang_internal bs)

  fun merge_bang_internal bs graph =
      let val _ = map (assert_bb_exists graph) bs in
        G.merge_boxes (G.BBox.NSet.of_list bs) graph
      end 
  fun merge_bang bs = lift_graph_mutator (merge_bang_internal bs)

  fun duplicate_bang_internal bb graph =
      let val _ = assert_bb_exists graph bb in
        G.fresh_copy1_box bb graph
      end 
  fun duplicate_bang bb = lift_graph_mutator (duplicate_bang_internal bb)

  fun bang_vertices_internal box verts graph = 
      let val _ = assert_bb_exists graph box
          val _ = map (assert_v_exists graph) verts
      in
        graph |> G.move_to_box box (G.Vertex.NSet.of_list verts)
      end
  fun bang_vertices box verts = lift_graph_mutator_no_result (bang_vertices_internal box verts)

  fun unbang_vertices_internal verts graph =
      let val _ = map (assert_v_exists graph) verts in
        graph |> G.move_to_unbox (G.Vertex.NSet.of_list verts)
      end
  fun unbang_vertices verts = lift_graph_mutator_no_result (unbang_vertices_internal verts)

  fun copy_subgraph source target vs state = let
      val graph = get_graph state source
      val _ = map (assert_v_exists graph) vs
      val clip = G.delete_subgraph_outside_vset 
                   (G.Vertex.NSet.of_list vs)
                   graph
  in state |> set_graph clip target
  end

  fun insert_graph sname tname state = 
      let
        val sarg = get_graph state sname
        val targ = get_graph state tname
        val (_, source) = G.rename_apart targ sarg
      in state |> push_hist tname
                            |> set_graph (G.merge_by_vertices targ source) tname
      end

  fun graph_of_rule getter label state rulename =
      let
        val rset = get_rset state
        val r = case RuleName.NTab.lookup (Ruleset.get_allrules rset) rulename
                     of SOME v => v | NONE => raise user_exp "Can't find rule."
        val (name, graphs) = GraphName.NTab.add (GraphName.mk ((RuleName.dest rulename)^"_"^label), getter r) (get_graphs state)
      in
        (name, state |> set_graphs graphs)
      end

  val open_rule_lhs = graph_of_rule R.get_lhs "lhs"
  val open_rule_rhs = graph_of_rule R.get_rhs "rhs"

end; (* structure Controller *)

(* *)
structure DB_ControllerState = ControllerState(
  structure GraphicalTheory = RG_Theory
);
structure ControllerState : CONTROLLER_STATE = DB_ControllerState; 

(* vim:et:sts=2:sw=2
*)
