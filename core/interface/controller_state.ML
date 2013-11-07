(* controller state *)
signature CONTROLLER_STATE =
sig
  type T;
  structure Theory : GRAPHICAL_THEORY;
  structure GraphUserData : GRAPH_STRING_TABLE_ANNOTATIONS;
  structure RulesetUserData : RULESET_ANNOTATIONS
  sharing type RulesetUserData.GraphAnnotations.T = GraphUserData.T;
  structure MetController : METRICS;

  type rewrite = (R.name * Theory.Rule.T) * Theory.Graph.T
  type edge = ((dir_or_undir * Theory.Graph.edata) * (V.name * V.name));
  datatype undo_block = RWR of (Theory.Graph.T * GraphUserData.T)
                      | UA of (Theory.Graph.T * GraphUserData.T)
  type history = ((undo_block) list * 
                    (undo_block) list ) GraphName.NTab.T;
  type ud_unit = string Symtab.table

  exception no_such_graph_exp of GraphName.name;
  exception no_such_graph_user_data_exp of (GraphName.name * string);
  exception no_such_vertex_user_data_exp of (V.name * string);
  exception no_such_edge_user_data_exp of (E.name * string);
  exception no_such_bbox_user_data_exp of (B.name * string);
  exception no_such_rule_user_data_exp of (R.name * string);
  exception no_such_vertex_exp of (V.name * GraphName.name);
  exception no_such_vertex_type_exp of string;
  exception no_such_edge_exp of (E.name * GraphName.name);
  exception no_such_edge_type_exp of string;
  exception no_such_bbox_exp of (B.name * GraphName.name);
  exception no_such_rule_exp of R.name;
  exception no_such_tag_exp of TagName.name;
  exception vertex_name_exists_exp of (V.name * GraphName.name);
  exception edge_name_exists_exp of (E.name * GraphName.name);
  exception bbox_name_exists_exp of (B.name * GraphName.name);
  exception inconsistent_bbox_exp of ((V.name * V.name) * B.name * GraphName.name);
  exception wire_vertex_exp of (string * GraphName.name * V.name);
  exception rule_name_exists_exp of R.name;
  exception bad_rule_exp of string;
  exception bad_state_exp of string;
  exception out_of_bounds_exp of int;
  exception unsupported_metric_exp of string;
  exception no_such_metric_exp of string;
  exception not_an_int_exp of string;
  exception duplicate_name_exp of unit;

  val init : T (* initial empty state, no graphs *)

  (* get/set*)
  val get_rset : T -> Theory.Ruleset.T
  val get_rewrites : T -> rewrite list GraphName.NTab.T;
  val get_hist : T -> history;
  val get_graphs : T -> Theory.Graph.T GraphName.NTab.T;
  val get_graph : T -> GraphName.name -> Theory.Graph.T;
  val get_graph_ud : T -> GraphName.name -> GraphUserData.T
  val get_graph_names : T -> GraphName.name list;
  val get_rset_ud : T -> RulesetUserData.T;
  val update_rset : (Theory.Ruleset.T -> Theory.Ruleset.T) -> T -> T
  val update_rset_ud : (RulesetUserData.T -> RulesetUserData.T) -> T -> T;
  val update_rewrites :
   (rewrite list GraphName.NTab.T -> rewrite list GraphName.NTab.T) ->
     T -> T
  val update_hist : (history -> history) -> T -> T;
  val update_graphs :
   (Theory.Graph.T GraphName.NTab.T -> Theory.Graph.T GraphName.NTab.T) -> T -> T

  val set_rset : Theory.Ruleset.T -> T -> T
  val set_rset_ud : RulesetUserData.T -> T -> T;
  val set_rewrites : rewrite list GraphName.NTab.T -> T -> T
  val set_hist : history -> T -> T
  val set_graphs : Theory.Graph.T GraphName.NTab.T -> T -> T
  val set_graph : Theory.Graph.T -> GraphName.name -> T -> T
  val set_graph_ud : GraphUserData.T -> GraphName.name -> T -> T


  (* history *)
  val undo : GraphName.name -> T -> T
  val redo : GraphName.name -> T -> T
  val undo_rewrite : GraphName.name -> T -> T
  val redo_rewrite : GraphName.name -> T -> T
  val start_undo_group : GraphName.name -> T -> T (* nothing is pushed on the
  undo stack  *)
  val end_undo_group : GraphName.name -> T -> T (* push_hist actually pushes
  the changes creating a new undo point  *)
  (* printing/showing info (for console) *)
  val get_attached_rewrites : GraphName.name -> T -> rewrite list
  val list_rules_with_tag : TagName.name -> T -> R.name list
  val list_allrules : T -> R.name list
  val list_tags : T -> TagName.name list
  val list_active_rules : T -> R.name list

  (* rewriting *)
  val attach_rewrites : GraphName.name -> V.name list -> T -> int * T
  val attach_first_rewrite : GraphName.name -> V.name list -> T -> int * T
  val apply_rewrite : GraphName.name -> int -> T -> T
  val apply_first_rewrite : GraphName.name -> T -> T

  (* rules/rulsesets *)
  val set_rule : R.name -> GraphName.name -> GraphName.name -> T -> T
  val open_rule_lhs : T -> R.name -> GraphName.name * T
  val open_rule_rhs : T -> R.name -> GraphName.name * T
  val delete_tag_and_its_rules : TagName.name -> T -> T
  val delete_tag : TagName.name -> T -> T
  val delete_rules_in_tag : TagName.name -> T -> T
  val delete_rule : R.name -> T -> T
  val rename_rule : R.name -> R.name -> T -> T

  val activate_rules_with_tag : TagName.name -> T -> T
  val activate_rule : R.name -> T -> T
  val deactivate_rules_with_tag : TagName.name -> T -> T
  val deactivate_rule : R.name -> T -> T

  val tag_rule : R.name -> TagName.name -> T -> T
  val untag_rule : R.name -> TagName.name -> T -> T

  (* graphs and their manipulations *)
  val kill_graph : GraphName.name -> T -> T
  val insert_graph : GraphName.name -> GraphName.name -> T -> T
  val add_graph : GraphName.name -> Theory.Graph.T -> T -> GraphName.name * T
  val copy_subgraph : GraphName.name -> GraphName.name -> V.name list -> T -> T

  val add_vertex : string -> GraphName.name -> T -> (V.name * (Theory.Graph.vdata * (E.NSet.T * E.NSet.T))) * T
  val add_edge : string -> dir_or_undir  -> V.name -> V.name -> GraphName.name -> T -> (E.name * edge) * T
  val add_bang : GraphName.name -> T -> B.name * T

  val update_vertex_data : (Theory.Graph.vdata -> Theory.Graph.vdata) -> V.name -> GraphName.name -> T -> T
  val update_edge_data : (Theory.Graph.edata -> Theory.Graph.edata) -> E.name -> GraphName.name -> T -> T
  val rename_vertex : V.name -> V.name -> GraphName.name -> T -> (V.name*(V.name option)) * T
  val rename_edge : E.name -> E.name -> GraphName.name -> T -> T
  val rename_bang : B.name -> B.name -> GraphName.name -> T -> T
  val rename_graph : GraphName.name -> GraphName.name -> T -> GraphName.name * T
  val delete_vertices : V.name list -> GraphName.name -> T -> T
  val delete_edges : E.name list -> GraphName.name -> T -> T

  val bang_vertices : B.name -> V.name list -> GraphName.name -> T -> (V.NSet.T*T)
  val kill_bang : B.name list -> GraphName.name -> T -> T
  val completely_unbang_vertices : V.name list -> GraphName.name -> T -> T
  val unbang_vertices : B.name -> V.name list -> GraphName.name -> T -> (V.NSet.T*T)
  val merge_bang : B.name list -> GraphName.name -> T -> B.name * T
  val duplicate_bang : B.name -> GraphName.name -> T -> B.name * T
  val drop_bang : B.name list -> GraphName.name -> T -> T

  (* UI Data getters/setters *)
  (* rules *)
  val get_rule_ud_property_opt : T -> R.name -> string -> string option
  val set_rule_ud_property : string -> string -> R.name -> T -> T
  val delete_rule_ud_property : string -> R.name -> T -> T
  (* Graph userdata *)
  val get_graph_ud_property_opt : T -> GraphName.name -> string -> string option
  val set_graph_ud_property : string -> string ->  GraphName.name -> T -> T
  val delete_graph_ud_property : string -> GraphName.name -> T -> T
  (* Vertex userdata *)
  val get_vertex_ud_property_opt : T -> GraphName.name -> V.name -> string -> string option
  val set_vertex_ud_property : string -> string -> V.name -> GraphName.name -> T -> T
  val delete_vertex_ud_property : string -> V.name -> GraphName.name -> T -> T
  (* Edge userdata *)
  val get_edge_ud_property_opt : T -> GraphName.name -> E.name -> string -> string option
  val set_edge_ud_property : string -> string -> E.name -> GraphName.name -> T -> T
  val delete_edge_ud_property : string -> E.name -> GraphName.name -> T -> T
  (* !-box userdata *)
  val get_bbox_ud_property_opt : T -> GraphName.name -> B.name -> string -> string option
  val set_bbox_ud_property : string -> string -> B.name -> GraphName.name -> T -> T
  val delete_bbox_ud_property : string -> B.name -> GraphName.name -> T -> T

  (* Functions for metrics *)
  val set_metric : string -> T -> T;
  val compute_metric : GraphName.name -> T -> MetController.foreign_type; 
  val ord_graph_metric : GraphName.name -> GraphName.name -> T -> int;
  val ord_metric : string list -> string list -> T -> int;
  val bottom_metric : T -> MetController.foreign_type;

  val get_names_metrics : T -> string list;
  val get_descs_metrics : T -> string list;
  val get_current_name_metric : T -> string;
end;


(* controls the state of Quantomatic *)
functor ControllerState(GraphicalTheory : GRAPHICAL_THEORY) : CONTROLLER_STATE
= struct


  structure Theory = GraphicalTheory;
  structure RulesetRewriter = Theory.RulesetRewriter
  structure Ruleset = Theory.Ruleset;
  structure Rule = Theory.Rule;
  structure Graph = Theory.Graph;
  structure GraphUserData = GraphStringTableAnnotations;
  structure RulesetUserData = RulesetStringTableAnnotations;
  structure MetController = MetricsController(structure Graph = Graph);

  exception no_such_graph_exp of GraphName.name;
  exception no_such_graph_user_data_exp of (GraphName.name * string);
  exception no_such_vertex_user_data_exp of (V.name * string);
  exception no_such_edge_user_data_exp of (E.name * string);
  exception no_such_bbox_user_data_exp of (B.name * string);
  exception no_such_rule_user_data_exp of (R.name * string);
  exception no_such_vertex_exp of (V.name * GraphName.name);
  exception no_such_vertex_type_exp of string;
  exception no_such_edge_exp of (E.name * GraphName.name);
  exception no_such_edge_type_exp of string;
  exception no_such_bbox_exp of (B.name * GraphName.name);
  exception no_such_rule_exp of R.name;
  exception no_such_tag_exp of TagName.name;
  exception vertex_name_exists_exp of (V.name * GraphName.name);
  exception edge_name_exists_exp of (E.name * GraphName.name);
  exception bbox_name_exists_exp of (B.name * GraphName.name);
  exception inconsistent_bbox_exp of ((V.name * V.name) * B.name * GraphName.name);
  exception wire_vertex_exp of (string * GraphName.name * V.name);
  exception rule_name_exists_exp of R.name;
  exception bad_rule_exp of string;
  exception bad_state_exp of string;
  exception out_of_bounds_exp of int;
  exception unsupported_metric_exp of string;
  exception no_such_metric_exp of string;
  exception not_an_int_exp of string;
  exception duplicate_name_exp of unit;
  type rewrite = (R.name * Rule.T) * Graph.T (* rule, and graph to be rewritten to *)
  type edge = ((dir_or_undir * Graph.edata) * (V.name * V.name));
  datatype undo_block = RWR of (Theory.Graph.T * GraphUserData.T)
                      | UA of (Theory.Graph.T * GraphUserData.T)
  type history = ((undo_block) list * 
                    (undo_block) list ) GraphName.NTab.T;
  type ud_unit = string Symtab.table

  (* data for controller; top level data with which user interacts *)
  datatype T = CState of {
       graphs : Graph.T GraphName.NTab.T, (* the known graphs *)
       rewrites : (rewrite list) GraphName.NTab.T, (* cached possible rewrites *)
       ud : GraphUserData.T GraphName.NTab.T, (* User Data *)
       hist : history, (* past and future *)
       merge_changes: bool GraphName.NTab.T, (* whether we create a new undo
       point when the state is modified  *)
       rset : Ruleset.T, (* the known rules *)
       rset_uidata : RulesetUserData.T, (* lhs and rhs uidata *)
       met_cont : MetController.T (* Metrics for graphs *)
  }

  (* getters and setters for the state *)
  fun update_graphs f (CState
    {graphs=g,rewrites=r,ud=u,hist=h,merge_changes=mg,rset=t,rset_uidata=tu,
      met_cont=mt}) = 
    CState {graphs=(f g),rewrites=r,ud=u,hist=h,merge_changes=mg,rset=t,rset_uidata=tu,met_cont=mt} 
  fun update_rewrites f (CState
    {graphs=g,rewrites=r,ud=u,hist=h,merge_changes=mg,rset=t,rset_uidata=tu,
      met_cont=mt}) = 
    CState {graphs=g,rewrites=(f r),ud=u,hist=h,merge_changes=mg,rset=t,rset_uidata=tu,met_cont=mt} 
  fun update_ud f (CState 
    {graphs=g,rewrites=r,ud=u,hist=h,merge_changes=mg,rset=t,rset_uidata=tu,
      met_cont=mt}) = 
    CState {graphs=g,rewrites=r,ud=(f u),hist=h,merge_changes=mg,rset=t,rset_uidata=tu,met_cont=mt} 
  fun update_hist f (CState 
    {graphs=g,rewrites=r,ud=u,hist=h,merge_changes=mg,rset=t,rset_uidata=tu,
      met_cont=mt}) = 
    CState {graphs=g,rewrites=r,ud=u,hist=(f h),merge_changes=mg,rset=t,rset_uidata=tu,met_cont=mt} 
  fun update_merge_changes f (CState 
    {graphs=g,rewrites=r,ud=u,hist=h,merge_changes=mg,rset=t,rset_uidata=tu,
      met_cont=mt}) = 
    CState {graphs=g,rewrites=r,ud=u,hist=h,merge_changes=(f mg),rset=t,rset_uidata=tu,met_cont=mt} 
  fun update_rset f (CState 
    {graphs=g,rewrites=r,ud=u,hist=h,merge_changes=mg,rset=t,rset_uidata=tu,
      met_cont=mt}) = 
    CState {graphs=g,rewrites=r,ud=u,hist=h,merge_changes=mg,rset=(f t),rset_uidata=tu,met_cont=mt} 
  fun update_rset_ud f (CState
    {graphs=g,rewrites=r,ud=u,hist=h,merge_changes=mg,rset=t,rset_uidata=tu,
      met_cont=mt}) = 
    CState {graphs=g,rewrites=r,ud=u,hist=h,merge_changes=mg,rset=t,rset_uidata=(f tu),met_cont=mt} 
  fun update_met_cont f (CState
    {graphs=g,rewrites=r,ud=u,hist=h,merge_changes=mg,rset=t,rset_uidata=tu,
      met_cont=mt}) = 
    CState {graphs=g,rewrites=r,ud=u,hist=h,merge_changes=mg,rset=t,rset_uidata=tu,met_cont=(f mt)} 

  val (set_graphs,set_rewrites,set_ud,set_hist,set_rset,set_rset_ud,
    set_met_cont) = 
    (update_graphs o K, update_rewrites o K, update_ud o K,update_hist o K,
    update_rset o K, update_rset_ud o K, update_met_cont o K)

  fun get_graphs (CState {graphs=g,...}) = g
  fun get_rewrites (CState {rewrites=r,...}) = r
  fun get_ud (CState {ud=u,...}) = u
  fun get_hist (CState {hist=h,...}) = h
  fun get_merge_changes (CState {merge_changes=mg,...}) = mg
  fun get_rset (CState {rset=t,...}) = t
  fun get_rset_ud (CState {rset_uidata=tu,...}) = tu
  fun get_met_cont (CState {met_cont=mt,...}) = mt

  val init = CState {graphs=GraphName.NTab.empty,
                         rewrites=GraphName.NTab.empty,
                         ud=GraphName.NTab.empty,
                         hist=GraphName.NTab.empty,
                         merge_changes=GraphName.NTab.empty,
                         rset=Ruleset.empty,
                         rset_uidata=RulesetUserData.init,
                         met_cont = MetController.init};

  (* state accessors *)
  fun get_graph state name =
      (case GraphName.NTab.lookup (get_graphs state) name
       of SOME g => g
        | NONE => raise no_such_graph_exp name)

  val get_graph_names = GraphName.NSet.list_of o GraphName.NTab.get_nameset o get_graphs

  fun get_graph_ud state name =
      (case GraphName.NTab.lookup (get_ud state) name
       of SOME u => u
        | NONE => raise no_such_graph_exp name)

  (* state mutators *)
  fun push_hist gname tp state = 
      let
        val (back,_) = 
            the_default ([],[]) (GraphName.NTab.lookup (get_hist state) gname)
        val mg = the_default false (GraphName.NTab.lookup (get_merge_changes
                                    state) gname) 
        val hist =  (get_hist state) 
                      |> GraphName.NTab.update (gname, 
                        (tp (get_graph state gname, get_graph_ud state gname)::back, []))
      in if mg then state else state |> set_hist hist
      end

  (* any time the graph is changed, any attached rewrites are invalid,
   * so should be cleared *)
  fun set_graph graph' gname state =
      state |> update_graphs (GraphName.NTab.update (gname,graph'))
            |> update_rewrites (GraphName.NTab.update (gname,[]))    

  fun set_graph_ud ud' gname state =
      state |> update_ud (GraphName.NTab.update (gname,ud'))

  (* ============================================== *)
  (* these functions actually implement commmands *)

  (* common things we want to assert are true *)
  fun assert_tag_exists state tag_name = 
      if Ruleset.tag_exists (get_rset state) tag_name then ()
      else raise no_such_tag_exp tag_name;

  fun assert_rule_exists state rule_name = 
      if Ruleset.rule_exists (get_rset state) rule_name then ()
      else raise no_such_rule_exp rule_name;

  fun assert_rule_does_not_exist state rule_name = 
      if Ruleset.rule_exists (get_rset state) rule_name
      then raise rule_name_exists_exp rule_name
      else ();

  fun assert_vertex_exists state gname vname =
        if Graph.has_vertex (get_graph state gname) vname then ()
        else raise no_such_vertex_exp (vname, gname);

  fun assert_vertex_exists_gr graph vname =
        if Graph.has_vertex graph vname then ()
        else raise Graph.no_such_vertex_exp ("ASSERT", vname, graph);

  fun assert_vertices_exist state gname vs =
        fold (fn v => fn () => assert_vertex_exists state gname v) vs ();

  fun assert_edge_exists state gname ename =
        if Graph.has_edge (get_graph state gname) ename then ()
        else raise no_such_edge_exp (ename, gname);

  fun assert_edge_exists_gr graph ename =
        if Graph.has_edge graph ename then ()
        else raise Graph.no_such_edge_exp ("ASSERT", ename, graph);

  fun assert_bbox_exists state gname bname =
        if Graph.has_bbox (get_graph state gname) bname then ()
        else raise no_such_bbox_exp (bname, gname);


  (* Internal function: takes the state and updates the rset_uidata
   * against the current ruleset: removes dead user data  *)
  fun update_rset_uidata_against state = let
    val rulenames = (RTab.get_dom_set o Ruleset.get_allrules o get_rset) state
  in
    update_rset_ud (RulesetUserData.cleanup rulenames) state
  end;

  (* basic stuff on rule tags *)
  val list_allrules =
      R.NSet.list_of o RTab.get_dom_set o Ruleset.get_allrules o get_rset

  val list_active_rules =
      R.NSet.list_of o Ruleset.get_active o get_rset

  val list_tags =
      TagName.NSet.list_of o Ruleset.get_tags o get_rset

  fun list_rules_with_tag tag_name state =
    if Ruleset.tag_exists (get_rset state) tag_name
    then
      let
        val rule_name_set = RTagRel.inv_img (Ruleset.get_tagrel (get_rset state)) tag_name
      in
        R.NSet.list_of rule_name_set
      end
    else
      []

  fun tag_name_action f tag_name state =
      let val () = assert_tag_exists state tag_name
      in update_rset (f tag_name) state end
  val deactivate_rules_with_tag = tag_name_action Ruleset.deactivate_tag; 
  val activate_rules_with_tag = tag_name_action Ruleset.activate_tag


  fun rule_name_action f rule state = 
      let val () = assert_rule_exists state rule
      in state |> update_rset (f rule)
               |> update_rset_uidata_against
      end
  val deactivate_rule  = rule_name_action Ruleset.deactivate_rule
  val activate_rule = rule_name_action Ruleset.activate_rule
  val delete_rule = rule_name_action Ruleset.delete_rule
  fun rename_rule oldName newName state =
      let val () = assert_rule_exists state oldName
          val () = assert_rule_does_not_exist state newName
      in state |> update_rset (Ruleset.rename_rule oldName newName)
               |> update_rset_ud (RulesetUserData.rename_rule oldName newName)
      end

  (* creates/replaces rule *)
  fun set_rule rule_name lhs rhs state =
      let
        (* build graphs for the LHS and RHS *)
        val lgr = get_graph state lhs
        val rgr = get_graph state rhs
        val (rule,(vrn,ern)) = Ruleset.Rule.mk_with_renamings (lgr, rgr)
        val lgr_ud = get_graph_ud state lhs
        val rgr_ud = GraphUserData.apply_renaming
                      (vrn,ern,BSub.empty)
                      (get_graph_ud state rhs)
        val upd_ud = (RulesetUserData.set_rule_lhs_annotation rule_name lgr_ud)
                     o (RulesetUserData.set_rule_rhs_annotation rule_name rgr_ud)
      in
        state |> update_rset (Ruleset.update_rule (rule_name, rule))
              |> update_rset_ud upd_ud
      end
      handle Ruleset.Rule.bad_rule_exp _ =>
            raise bad_rule_exp "Boundaries and !-boxes on LHS must match RHS. !-boxes with the same name must bang boundary vertices with the same name."

  fun delete_tag tag_name state =
      let val () = assert_tag_exists state tag_name
      in update_rset (Ruleset.delete_tag tag_name) state end

  fun delete_rules_in_tag tag_name state =
      let val () = assert_tag_exists state tag_name
      in state |> update_rset (Ruleset.delete_rules_in_tag tag_name)
               |> update_rset_uidata_against
      end

  fun delete_tag_and_its_rules tag_name state =
      let val () = assert_tag_exists state tag_name
      in state |> update_rset (Ruleset.delete_tag tag_name
                                   o Ruleset.delete_rules_in_tag tag_name)
               |> update_rset_uidata_against
      end

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
        in (nm, state |> set_graphs gs
                      |> set_graph_ud GraphUserData.init nm) end

  fun kill_graph name state =
      (update_graphs (fn gs => gs |> GraphName.NTab.delete name) state)
       handle GraphName.NTab.Tab.UNDEF _ => raise no_such_graph_exp name

  fun rename_graph oldname newname state =
     (let
          (* When a graph is renamed there is no reason to loose its
           * history and its user data.  *)
          val graphs = get_graphs state
          val gr = GraphName.NTab.get graphs oldname
          val hist = the_default ([],[]) (GraphName.NTab.lookup (get_hist state)
                                                                    oldname)
          val ud = GraphName.NTab.get (get_ud state) oldname
          val (nm,graphs') = graphs
                    |> GraphName.NTab.delete oldname
                    |> GraphName.NTab.add (newname, gr)
          val hist' = GraphName.NTab.update (nm, hist) (get_hist state)
          val ud' = GraphName.NTab.update (nm, ud) (get_ud state) 
      in
        (nm, state |> set_graphs graphs'
                   |> set_hist hist'
                   |> set_ud ud')
      end
      handle GraphName.NTab.Tab.UNDEF _ => raise no_such_graph_exp oldname)

  fun cleanup_graph_userdata gname newg state = let
    val vs = Graph.get_vertices newg
    val es = Graph.get_edges newg
    val bs = Graph.get_bboxes newg
    val cleanup = GraphUserData.cleanup (vs,es,bs)
    val old_ud = get_graph_ud state gname
  in
    set_graph_ud (cleanup old_ud) gname state
  end

  fun lift_graph_mutator cmdf gname state =
      let
          val (out,g') = cmdf (get_graph state gname)
      in
        (out, state |> push_hist gname UA
                    |> set_graph g' gname
                    |> cleanup_graph_userdata gname g')
      end
      handle Graph.no_such_vertex_exp (_,vname,_) =>
               raise no_such_vertex_exp (vname,gname)
           | Graph.no_such_edge_exp (_,ename,_) =>
               raise no_such_edge_exp (ename,gname)
           | Graph.duplicate_vertex_exp (vname,_) =>
                raise vertex_name_exists_exp (vname,gname)
           | Graph.duplicate_edge_exp (ename,_) =>
                raise edge_name_exists_exp (ename,gname)
           | Graph.duplicate_bbox_exp (bname,_) =>
                raise bbox_name_exists_exp (bname,gname)
           | Graph.wire_vertex_exp (msg,_,v) =>
                raise wire_vertex_exp (msg,gname,v)
           | Graph.bbox_not_open_exp (vs,b,_) =>
                raise inconsistent_bbox_exp (vs,b,gname);

  fun lift_graph_mutator_no_result cmdf gname state =
      let 
          val g' = cmdf (get_graph state gname)
      in
        state |> push_hist gname UA
              |> set_graph g' gname
              |> cleanup_graph_userdata gname g'
      end
      handle Graph.no_such_vertex_exp (_,vname,_) =>
               raise no_such_vertex_exp (vname,gname)
           | Graph.no_such_edge_exp (_,ename,_) =>
               raise no_such_edge_exp (ename,gname)
           | Graph.duplicate_vertex_exp (vname,_) =>
                raise vertex_name_exists_exp (vname,gname)
           | Graph.duplicate_edge_exp (ename,_) =>
                raise edge_name_exists_exp (ename,gname)
           | Graph.duplicate_bbox_exp (bname,_) =>
                raise bbox_name_exists_exp (bname,gname);

  fun lift_ud_mutator_no_result cmdf gname state =
      let 
         (* Make sure the graph exists *)
          val _ = get_graph state gname
          val u' = cmdf (get_graph_ud state gname)
      in
        state |> push_hist gname UA
              |> set_graph_ud u' gname
      end
      handle Graph.no_such_vertex_exp (_,vname,_) =>
               raise no_such_vertex_exp (vname,gname)
           | Graph.no_such_edge_exp (_,ename,_) =>
               raise no_such_edge_exp (ename,gname);


  fun get_vertex_type "wire-vertex" = Graph.WVert
    | get_vertex_type "edge-point" = Graph.WVert
    | get_vertex_type s = Graph.default_nv_vdata_of_typestring s
  fun add_vertex_internal vt graph =
      (let val (n,g) = graph |> Graph.add_vertex (get_vertex_type vt) in
          ((n,(Graph.get_vertex_data g n, (Graph.get_in_edges g n, Graph.get_out_edges g n))), g)
       end
       handle unknown_typestring_exp _ => raise no_such_vertex_type_exp vt)
  fun add_vertex vt gname state = lift_graph_mutator (add_vertex_internal vt) gname state

  fun add_edge_internal et dir_or_undir v1 v2 graph = let
    val data = Theory.Graph.default_edata_of_typestring et
               handle unknown_typestring_exp _
                        => raise no_such_edge_type_exp et
    (*val _ = case Graph.tryget_bbox_extended_by_edge v1 v2 graph
              of NONE => ()
               | SOME b => raise Graph.bbox_not_open_exp ((v1,v2),b,graph)*)
    (* TODO: add_edge should throw exception if it breaks open-ness. handle this appropriately *)
    val (n,g) = graph |> Graph.add_edge (dir_or_undir,data) v1 v2
  in
    ((n,Graph.get_edge_info g n), g)
  end

  fun add_edge et dir_or_undir v1 v2 gname state =
   (lift_graph_mutator (add_edge_internal et dir_or_undir v1 v2) gname state
    handle Graph.wire_vertex_exp (msg,_,v)
             => raise wire_vertex_exp (msg,gname,v)
         | Graph.bbox_not_open_exp (vs,b,_)
             => raise inconsistent_bbox_exp (vs,b,gname)
   )


  fun delete_vertices vs =
    lift_graph_mutator_no_result (fold Graph.delete_vertex vs)

  fun delete_edges es =
    lift_graph_mutator_no_result (fold Graph.delete_edge es)

  fun rename_vertex_internal old new graph =
    ((new,NONE),graph |> Graph.rename_vertex old new)
    handle Graph.duplicate_vertex_exp _ =>
      (* Rename apart the old vertex *)
      let
        (*Ideally, we would just need to call the suc function in order
         * to generate a fresh name, but how?*)
        val ((frshN,_),_) = add_vertex_internal "edge-point" graph
      in
        ((new,SOME frshN),graph |> Graph.rename_vertex new (frshN)
                           |> Graph.rename_vertex old new)
      end;
 
  (* There was no conflict *) 
  fun rename_vertex_uidata old (new,NONE) graphName state =
    let
      val ud = get_graph_ud state graphName
    in
      state |> set_graph_ud (GraphUserData.rename_vertex old new ud) graphName
    end
    (* We had a conflict, therefore we have a freshName for 
     * the node vertex that we need to rename as well *)
    | rename_vertex_uidata old (new,SOME fresh) graphName state =
    let
      val ud = get_graph_ud state graphName
      val ud' = ud |> GraphUserData.rename_vertex new fresh
                   |> GraphUserData.rename_vertex old new
    in
      state |> set_graph_ud ud' graphName
    end;
        
  fun rename_vertex old new graph state = 
    let
      val (out, state') = lift_graph_mutator (rename_vertex_internal old new) graph state
    in
    (* When a vertex is renamed, the ui_data as well *)
     (out, state' |> rename_vertex_uidata old out graph)
    end;

  (* Internal semi-generic function used to rename uidata
   * on edges and !-boxes  *)
  fun rename_edge_or_bb_uidata renamer old new graphName state =
    let
      val ud = get_graph_ud state graphName
      val ud' = ud |> renamer old new 
    in
      state |> set_graph_ud ud' graphName
    end;

  fun rename_edge_internal old new graph = (graph |> Graph.rename_edge old new)
  fun rename_edge old new graph state = 
      let
        val state' = lift_graph_mutator_no_result (rename_edge_internal old new) graph state
      in
        state' |> rename_edge_or_bb_uidata GraphUserData.rename_edge old new graph 
      end;

  fun rename_bang_internal old new graph = (graph |> Graph.rename_bbox old new)
  fun rename_bang old new graph state = 
    let
      val state' = lift_graph_mutator_no_result (rename_bang_internal old new) graph state
    in
      state' |> rename_edge_or_bb_uidata GraphUserData.rename_bbox old new graph
    end;

  fun undo_rewrite gname state = 
      let
        val current_graph = get_graph state gname
        val current_ud = get_graph_ud state gname
        val current = (current_graph, current_ud)
        fun set_state g u ps hs tp = 
              state |> set_graph g gname
                    |> set_graph_ud u gname
                    |> update_hist (GraphName.NTab.update
                                   (gname, (ps,tp current::hs)))
      in case GraphName.NTab.lookup (get_hist state) gname
          of SOME (UA  (g,u)::ps, hs) => undo_rewrite gname (set_state g u ps hs UA)
           | SOME (RWR (g,u)::ps, hs) => set_state g u ps hs RWR
           | _ => raise bad_state_exp "Can't undo rewrite. No more rewrites"
      end
   
  fun redo_rewrite gname state = 
      let
        val current_graph = get_graph state gname
        val current_ud = get_graph_ud state gname
        val current = (current_graph, current_ud)
        fun set_state gs h u ps tp = 
              state |> set_graph h gname
                    |> set_graph_ud u gname
                    |> update_hist (GraphName.NTab.update
                                   (gname, (tp current::gs,ps)))
      in case GraphName.NTab.lookup (get_hist state) gname
          of SOME (gs,UA  (h,u)::ps) => redo_rewrite gname (set_state gs h u ps UA)
          |  SOME (gs,RWR (h,u)::ps) => set_state gs h u ps RWR
          | _ => raise bad_state_exp "Can't redo rewrite, nothing to redo."
      end

  fun undo gname state =
      let
        val current_graph = get_graph state gname
        val current_ud = get_graph_ud state gname
        val current = (current_graph, current_ud)
        fun set_state g u ps hs tp = 
              state |> set_graph g gname
                    |> set_graph_ud u gname
                    |> update_hist (GraphName.NTab.update
                                   (gname, (ps,tp current::hs)))
      in case GraphName.NTab.lookup (get_hist state) gname
          of SOME (UA  (g,u)::ps, hs) => set_state g u ps hs UA
           | SOME (RWR (g,u)::ps, hs) => set_state g u ps hs RWR
           | _ => raise bad_state_exp "Can't undo, no more undo history."
      end

  fun redo gname state = 
      let
        val current_graph = get_graph state gname
        val current_ud = get_graph_ud state gname
        val current = (current_graph, current_ud)
        fun set_state gs h u ps tp = 
              state |> set_graph h gname
                    |> set_graph_ud u gname
                    |> update_hist (GraphName.NTab.update
                                   (gname, (tp current::gs,ps)))
      in case GraphName.NTab.lookup (get_hist state) gname
          of SOME (gs,UA  (h,u)::ps) => set_state gs h u ps UA
          |  SOME (gs,RWR (h,u)::ps) => set_state gs h u ps RWR
          | _ => raise bad_state_exp "Can't redo, nothing to redo."
      end

  fun apply_rewrite gname n state =
      (let val rewrites = GraphName.NTab.get (get_rewrites state) gname
           val (_,rewritten_graph) = nth rewrites n
       in state |> push_hist gname RWR
                |> set_graph rewritten_graph gname
       end
       handle GraphName.Tab.UNDEF _ => raise bad_state_exp "No rewrites attached." 
            | General.Subscript => raise out_of_bounds_exp n)

  local
      fun attach_with attacher gname vs state =
          (let val graph = get_graph state gname
               val _ = assert_vertices_exist state gname vs
               val vset = fold V.NSet.ins_fresh vs V.NSet.empty
                          handle V.NSet.duplicate_exp _ => raise duplicate_name_exp ()
               val rws = (Seq.list_of (attacher (get_rset state) graph vset))
           in
               (length rws,
                state |> update_rewrites (GraphName.NTab.update (gname,rws)))
           end)

      fun apply_first_in rset vs g = 
          case RulesetRewriter.apply_first_in rset vs g of NONE => Seq.empty
          | SOME x => Seq.single x;
  in
  val attach_rewrites = attach_with RulesetRewriter.apply_in;
  val attach_first_rewrite = attach_with apply_first_in; 

  fun apply_first_rewrite gname state = 
      let
        val graph = get_graph state gname 
        val (n,state') = attach_with apply_first_in gname (V.NSet.list_of (Graph.get_vertices graph)) state
      in if n = 0 then raise bad_state_exp "No more rewrites."
         else apply_rewrite gname 0 state'
      end
  end

  fun get_attached_rewrites gname state = let
      val rewrites = case GraphName.NTab.lookup (get_rewrites state) gname
              of NONE => raise bad_state_exp "No rewrites attached."
               | SOME rs => rs
  in rewrites end;

  fun update_vertex_data_internal vd_upd vn graph =
      let val () = assert_vertex_exists_gr graph vn in
        graph |> Graph.update_vertex_data vd_upd vn
      end
  fun update_vertex_data vd_upd vn = lift_graph_mutator_no_result (update_vertex_data_internal vd_upd vn)
 

  fun update_edge_data_internal ed_upd en graph =
      let val () = assert_edge_exists_gr graph en in
        graph |> Graph.update_edge_data ed_upd en
      end
  fun update_edge_data ed_upd en = lift_graph_mutator_no_result (update_edge_data_internal ed_upd en)

  fun add_bang_internal graph = 
      graph |> Graph.add_bbox
  val add_bang = lift_graph_mutator add_bang_internal

  fun drop_bang bs gname state =
      let val _ = map (assert_bbox_exists state gname) bs in
        lift_graph_mutator_no_result (fold Graph.drop_bbox bs) gname state
      end

  fun kill_bang bs gname state =
      let val _ = map (assert_bbox_exists state gname) bs in
        lift_graph_mutator_no_result (fold Graph.kill_bbox bs) gname state
      end

  fun merge_bang bs gname state =
      let
        val _ = map (assert_bbox_exists state gname) bs
        val bset = B.NSet.of_list bs
                   handle B.NSet.duplicate_exp _ => raise duplicate_name_exp ()
      in
        lift_graph_mutator (Graph.merge_bboxes bset) gname state
      end

  fun duplicate_bang bb gname state =
      let
        val _ = assert_bbox_exists state gname bb
        fun cp_bbox g = let
          val ((_,_,bsub),g') = Graph.fresh_copy_bbox bb g
          val bb' = BSub.get bsub bb
        in (bb',g')
        end
      in
        lift_graph_mutator cp_bbox gname state
      end

  fun bang_vertices box verts gname state =
      let
        val _ = assert_bbox_exists state gname box
        val _ = map (assert_vertex_exists state gname) verts
        val vset = V.NSet.of_list verts
                   handle V.NSet.duplicate_exp _ => raise duplicate_name_exp ()
      in
        lift_graph_mutator (Graph.add_to_bbox box vset) gname state
        handle Graph.bbox_not_open_exp (vs,b,_)
             => raise inconsistent_bbox_exp (vs,b,gname)
      end

  fun completely_unbang_vertices verts gname state =
      let
        val _ = map (assert_vertex_exists state gname) verts
        val vset = V.NSet.of_list verts
                   handle V.NSet.duplicate_exp _ => raise duplicate_name_exp ()
      in
        lift_graph_mutator_no_result (Graph.remove_from_all_bboxes vset) gname state
        handle Graph.bbox_not_open_exp (vs,b,_)
             => raise inconsistent_bbox_exp (vs,b,gname)
      end

  fun unbang_vertices box verts gname state =
      let
        val _ = assert_bbox_exists state gname box
        val _ = map (assert_vertex_exists state gname) verts
        val vset = V.NSet.of_list verts
                   handle V.NSet.duplicate_exp _ => raise duplicate_name_exp ()
      in
        lift_graph_mutator (Graph.remove_from_bbox box vset) gname state
        handle Graph.bbox_not_open_exp (vs,b,_)
             => raise inconsistent_bbox_exp (vs,b,gname)
      end

  (* Copy the subgraph as well as it user data *)
  (* Here is the problem: when a graph is copied it's 
   * usually in order to paste it later. However, once it's
   * pasted (especially if it's in the same graph), 
   * and renamed it may useful to know
   * where our vertices come from, i.e. what they are the copy of
   * A solution is to store that as ui_data on the vertices, edges, etc...*)
  fun copy_subgraph source target vs state = let
      val _ = map (assert_vertex_exists state source) vs
      val vset = V.NSet.of_list vs
                 handle V.NSet.duplicate_exp _ => raise duplicate_name_exp ()
      val source_graph = get_graph state source
      val keep_vertex = V.NSet.contains vset
      fun keep_bbox b = not (V.NSet.is_empty (V.NSet.intersect
          (Graph.get_vertices_in_bbox source_graph b)
          vset
        ))
      val clip = source_graph
        |> V.NSet.fold (fn v => if keep_vertex v then I else Graph.delete_vertex v)
                       (Graph.get_vertices source_graph)
        |> B.NSet.fold (fn b => if keep_bbox b then I else Graph.delete_bbox b)
                       (Graph.get_bboxes source_graph)
                   
      fun add_v_ud ud n =
        let
          val v_ud = GraphUserData.get_vertex_annotation ud n
          val v_ud_with_src = Symtab.update ("copy_of", V.dest n) v_ud
        in
          GraphUserData.set_vertex_annotation n v_ud_with_src
        end
      fun add_e_ud ud n =
        let
          val e_ud = GraphUserData.get_edge_annotation ud n
          val e_ud_with_src = Symtab.update ("copy_of", E.dest n) e_ud
        in
          GraphUserData.set_edge_annotation n e_ud_with_src
        end
      fun add_b_ud ud n =
        let
          val b_ud = GraphUserData.get_bbox_annotation ud n
          val b_ud_with_src = Symtab.update ("copy_of", B.dest n) b_ud
        in
          GraphUserData.set_bbox_annotation n b_ud_with_src
        end
      val ud  = get_graph_ud state source
      val ud' = GraphUserData.init
           |> fold (add_v_ud ud) (V.NSet.list_of (Graph.get_vertices clip))
           |> fold (add_e_ud ud) (E.NSet.list_of (Graph.get_edges clip))
           |> fold (add_b_ud ud) (B.NSet.list_of (Graph.get_bboxes clip))
           |> GraphUserData.set_graph_annotation (GraphUserData.get_graph_annotation ud)
  in
    state |> set_graph clip target
          |> set_graph_ud ud' target
  end

  (* Rename the graph apart, insert it and insert its user data as well*)
  fun insert_graph sname tname state =
      let
        val sarg = get_graph state sname
        val targ = get_graph state tname
        val vrn = VSub.extend_fresh (Graph.get_vertices sarg)
                                    (VSub.mk_from_avoids (Graph.get_vertices targ))
        val ern = ESub.extend_fresh (Graph.get_edges sarg)
                                    (ESub.mk_from_avoids (Graph.get_edges targ))
        val brn = BSub.extend_fresh (Graph.get_bboxes sarg)
                                    (BSub.mk_from_avoids (Graph.get_bboxes targ))
        val ((vrn,ern,_), source) = Graph.rename_bang_graph (vrn,ern,brn) sarg
        (* Rename the ud and insert it on the new graph  *)
        val ud_s = get_graph_ud state sname
        val ud_t = get_graph_ud state tname
        val ud_s' = GraphUserData.apply_renaming (vrn,ern,BSub.empty) ud_s
        val ud' = ud_t |> VTab.fold
                          (fn (k,v) => GraphUserData.set_vertex_annotation k v)
                          (GraphUserData.get_vertex_annotation_tab ud_s')
                       |> ETab.fold
                          (fn (k,v) => GraphUserData.set_edge_annotation k v)
                          (GraphUserData.get_edge_annotation_tab ud_s')
                       |> BTab.fold
                          (fn (k,v) => GraphUserData.set_bbox_annotation k v)
                          (GraphUserData.get_bbox_annotation_tab ud_s')
      in state |> push_hist tname UA
               |> set_graph (Graph.merge targ source) tname
               |> set_graph_ud ud' tname
      end

  fun graph_of_rule getter ann_getter label state rulename =
      let
        val rset = get_rset state
        val r = case RTab.get_opt (Ruleset.get_allrules rset) rulename
                     of SOME v => v
                    | NONE => raise no_such_rule_exp rulename
        val (name, graphs) = GraphName.NTab.add (GraphName.mk ((R.dest rulename)^"_"^label), getter r) (get_graphs state)
      in
        (name, state
                     |> set_graphs graphs
                     |> set_graph_ud (ann_getter (get_rset_ud state) rulename) name)
      end

  val open_rule_lhs = graph_of_rule Rule.get_lhs RulesetUserData.get_rule_lhs_annotation "lhs"
  val open_rule_rhs = graph_of_rule Rule.get_rhs RulesetUserData.get_rule_rhs_annotation "rhs"

  (* Set, delete and get the user data  *)

  (* rules *)
  fun get_rule_ud_property_opt state rn prop =
    let val () = assert_rule_exists state rn in
      RulesetUserData.get_rule_property_opt (get_rset_ud state) rn prop
    end
  fun set_rule_ud_property dn ud rn =
    update_rset_ud (RulesetUserData.set_rule_property rn (dn,ud))
  fun delete_rule_ud_property dn rn =
    update_rset_ud (RulesetUserData.remove_rule_property rn dn)

  (* On Graphs:  *)
  fun get_graph_ud_property_opt state gn prop =
    GraphUserData.get_graph_property_opt (get_graph_ud state gn) prop
  fun set_graph_ud_property dn ud = lift_ud_mutator_no_result
    (GraphUserData.set_graph_property (dn,ud))
  fun delete_graph_ud_property dn = lift_ud_mutator_no_result
    (GraphUserData.remove_graph_property dn)

  (* On Vertices:  *)
  fun get_vertex_ud_property_opt state gn vn prop =
    let val () = assert_vertex_exists state gn vn in
      GraphUserData.get_vertex_property_opt (get_graph_ud state gn) vn prop
    end
  fun set_vertex_ud_property dn ud vn = lift_ud_mutator_no_result
    (GraphUserData.set_vertex_property vn (dn,ud))
  fun delete_vertex_ud_property dn vn = lift_ud_mutator_no_result
    (GraphUserData.remove_vertex_property vn dn)

  (* On Edges:  *)
  fun get_edge_ud_property_opt state gn en prop =
    let val () = assert_edge_exists state gn en in
      GraphUserData.get_edge_property_opt (get_graph_ud state gn) en prop
    end
  fun set_edge_ud_property dn ud en = lift_ud_mutator_no_result
    (GraphUserData.set_edge_property en (dn,ud))
  fun delete_edge_ud_property dn en = lift_ud_mutator_no_result
    (GraphUserData.remove_edge_property en dn)

  (* On Bang Boxes:  *)
  fun get_bbox_ud_property_opt state gn bn prop =
    let val () = assert_bbox_exists state gn bn in
      GraphUserData.get_bbox_property_opt (get_graph_ud state gn) bn prop
    end
  fun set_bbox_ud_property dn ud bn = lift_ud_mutator_no_result
    (GraphUserData.set_bbox_property bn (dn,ud))
  fun delete_bbox_ud_property dn bn = lift_ud_mutator_no_result
    (GraphUserData.remove_bbox_property bn dn)

  fun start_undo_group gname state =
    state |> update_merge_changes (GraphName.NTab.update (gname,true))
  fun end_undo_group gname state =
    state |> update_merge_changes (GraphName.NTab.update (gname,false))

  (* Functions for metrics *)
  fun set_metric nm state = 
    let
      val met_cont = get_met_cont state
    in
      set_met_cont (MetController.set_current_metric nm met_cont) state
    end
      handle MetController.no_such_metric_exp n => 
        raise no_such_metric_exp n;

  fun compute_metric gn state =
    let
      val g = get_graph state gn
      val met_cont = get_met_cont state
    in
      case (MetController.get_current_compute met_cont) g of
        SOME a => a
        | NONE => raise unsupported_metric_exp
            (MetController.get_current_metric_name met_cont)
    end;    

  (* Tiny function to convert order to an int: 0 1 2 *)
  fun convert_order_to_int (SOME ord) _ =
    (case ord of
        LESS => 0
      | GREATER => 2
      | _   => 1)   
    | convert_order_to_int NONE state =
      raise unsupported_metric_exp  
            (MetController.get_current_metric_name (get_met_cont state))
  (* ** *)

  fun ord_graph_metric gn1 gn2 state =
    let
      val g1 = get_graph state gn1
      val g2 = get_graph state gn2
      val met_cont = get_met_cont state
    in
      case (MetController.get_current_ord_graph met_cont) (g1,g2) of
      SOME a => convert_order_to_int (SOME a) state
    | NONE => raise unsupported_metric_exp
            (MetController.get_current_metric_name met_cont)
    end;

  fun ord_metric ms1 ms2 state = 
    let
      fun parseInt s = case Int.fromString s of
        SOME i => i
        | NONE => raise not_an_int_exp s
      val m1 = List.map (parseInt) ms1
      val m2 = List.map (parseInt) ms2
    in
      state |> convert_order_to_int 
                (MetController.get_current_ord (get_met_cont state) (m1,m2))
    end;
  fun bottom_metric state =
    MetController.get_current_bottom (get_met_cont state);

  fun get_names_metrics state =
    MetController.get_metrics_names (get_met_cont state); 
  fun get_descs_metrics state = 
    MetController.get_metrics_descs (get_met_cont state);

  fun get_current_name_metric state =
    MetController.get_current_metric_name (get_met_cont state)
    handle MetController.no_such_metric_exp n =>
      raise no_such_metric_exp n;
end; (* structure Controller *)
(* vim:et:sts=2:sw=2
*)
