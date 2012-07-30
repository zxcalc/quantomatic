(* controller state *)
signature CONTROLLER_STATE =
sig
  type T;
  structure Theory : GRAPHICAL_THEORY;
  structure UIData : USER_DATA;
  structure MetController : METRICS;

  type rewrite = (R.name * Theory.Rule.T) * Theory.Graph.T
  type vertex = (Theory.OVData.data * (E.NSet.T * E.NSet.T));
  type edge = ((Theory.Graph.dir_or_undir * Theory.EData.data) * (V.name * V.name));
  datatype undo_block = RWR of (Theory.Graph.T * UIData.T)
                      | UA of (Theory.Graph.T * UIData.T)
  type history = ((undo_block) list * 
                    (undo_block) list ) GraphName.NTab.T;
  type ud_unit = string Symtab.table

  exception no_such_graph_exp of GraphName.name;
  exception no_such_vertex_exp of (V.name * GraphName.name);
  exception no_such_vertex_type_exp of string;
  exception no_such_vertex_user_data_exp of (V.name * string);
  exception no_such_rule_user_data_exp of (R.name * string);
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
  exception bad_rule_exp of string;
  exception bad_state_exp of string;
  exception out_of_bounds_exp of int;
  exception unsupported_metric_exp of string;
  exception no_such_metric_exp of string;
  exception not_an_int_exp of string;

  val init : T (* initial empty state, no graphs *)

  (* get/set*)
  val get_rset : T -> Theory.Ruleset.T
  val get_rewrites : T -> rewrite list GraphName.NTab.T;
  val get_hist : T -> history;
  val get_graphs : T -> Theory.Graph.T GraphName.NTab.T;
  val get_ud : T -> UIData.T;
  val get_graph : T -> GraphName.name -> Theory.Graph.T;
  val get_graph_names : T -> GraphName.name list;
  val get_rset_uidata : T -> (ud_unit * (UIData.T * UIData.T)) R.NTab.T;
  val update_rset : (Theory.Ruleset.T -> Theory.Ruleset.T) -> T -> T
  val update_rewrites :
   (rewrite list GraphName.NTab.T -> rewrite list GraphName.NTab.T) ->
     T -> T
  val update_hist : (history -> history) -> T -> T;
  val update_graphs :
   (Theory.Graph.T GraphName.NTab.T -> Theory.Graph.T GraphName.NTab.T) -> T -> T

  val set_rset : Theory.Ruleset.T -> T -> T
  val set_rewrites : rewrite list GraphName.NTab.T -> T -> T
  val set_hist : history -> T -> T
  val set_graphs : Theory.Graph.T GraphName.NTab.T -> T -> T
  val set_graph : Theory.Graph.T -> GraphName.name -> T -> T
  val set_ud : UIData.T -> GraphName.name -> T -> T


  (* history *)
  val undo : GraphName.name -> T -> T
  val redo : GraphName.name -> T -> T
  val undo_rewrite : GraphName.name -> T -> T
  val redo_rewrite : GraphName.name -> T -> T
  val push_hist : GraphName.name -> T -> T
  val start_undo_group : GraphName.name -> T -> T (* nothing is pushed on the
  undo stack  *)
  val end_undo_group : GraphName.name -> T -> T (* push_hist actually pushes
  the changes creating a new undo point  *)
  (* printing/showing info (for console) *)
  val get_attached_rewrites : GraphName.name -> T -> rewrite list
  val list_rules_with_tag : TagName.name -> T -> TagName.name list
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

  val add_vertex : string -> GraphName.name -> T -> (V.name * vertex) * T
  val add_edge : string -> Theory.Graph.dir_or_undir  -> V.name -> V.name -> GraphName.name -> T -> (E.name * edge) * T
  val add_bang : GraphName.name -> T -> B.name * T

  val update_vertex_data : (Theory.OVData.data -> Theory.OVData.data) -> V.name -> GraphName.name -> T -> T
  val update_edge_data : (Theory.EData.data -> Theory.EData.data) -> E.name -> GraphName.name -> T -> T
  val rename_vertex : V.name -> V.name -> GraphName.name -> T -> (string list) * T
  val rename_edge : E.name -> E.name -> GraphName.name -> T -> T
  val rename_bang : B.name -> B.name -> GraphName.name -> T -> T
  val rename_graph : GraphName.name -> GraphName.name -> T -> GraphName.name * T
  val delete_vertices : V.name list -> GraphName.name -> T -> T
  val delete_edges : E.name list -> GraphName.name -> T -> T

  val bang_vertices : B.name -> V.name list -> GraphName.name -> T -> T
  val kill_bang : B.name list -> GraphName.name -> T -> T
  val completely_unbang_vertices : V.name list -> GraphName.name -> T -> T
  val unbang_vertices : B.name -> V.name list -> GraphName.name -> T -> T
  val merge_bang : B.name list -> GraphName.name -> T -> B.name * T
  val duplicate_bang : B.name -> GraphName.name -> T -> B.name * T
  val drop_bang : B.name list -> GraphName.name -> T -> T

  (* UI Data getters/setters *)
  (* ... For Graphs *)

  (* get_graph_ud actually returns the UD.T structure, with everything about the
   * vertices, etc...   ......*)
  val get_graph_ud : T -> GraphName.name -> UIData.T
  val update_graph_user_data : string -> string ->  GraphName.name -> T -> T
  (* ...... whereas get_graph_user_data returns the data attached to the graph *)
  val get_graph_user_data : GraphName.name -> T -> string Symtab.table
  val delete_graph_user_data : string -> GraphName.name -> T -> T
  (* ... For Vertices *)
  val update_vertex_user_data : string -> string -> V.name -> GraphName.name -> T -> T
  val get_vertex_user_data : GraphName.name -> V.name -> T -> (string
    Symtab.table option)
  val delete_vertex_user_data : string -> V.name -> GraphName.name -> T -> T
  (* ... For Edges *)
  val update_edge_user_data : string -> string -> E.name -> GraphName.name -> T -> T
  val get_edge_user_data : GraphName.name -> E.name -> T -> (string
    Symtab.table option)
  val delete_edge_user_data : string -> E.name -> GraphName.name -> T -> T
  (* ... For !-Boxes *)
  val update_bangbox_user_data : string -> string -> B.name -> GraphName.name -> T -> T
  val get_bangbox_user_data : GraphName.name -> B.name -> T -> (string
    Symtab.table option)
  val delete_bangbox_user_data : string -> B.name -> GraphName.name -> T -> T

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
functor ControllerState(GraphicalTheory : GRAPHICAL_THEORY)
= struct


  structure Theory = GraphicalTheory;
  structure RulesetRewriter = Theory.RulesetRewriter
  structure Ruleset = Theory.Ruleset;
  structure Rule = Theory.Rule;
  structure Graph = Theory.Graph;
  structure OVData = Graph.OVData;
  structure EData = Graph.EData;
  structure UIData = UserData(Graph):USER_DATA;
  structure MetController = MetricsController(structure Graph = Graph);

  exception no_such_graph_exp of GraphName.name;
  exception no_such_graph_user_data_exp of (GraphName.name * string);
  exception no_such_vertex_user_data_exp of (V.name * string);
  exception no_such_edge_user_data_exp of (E.name * string);
  exception no_such_bangbox_user_data_exp of (B.name * string);
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
  type rewrite = (R.name * Rule.T) * Graph.T (* rule, and graph to be rewritten to *)
  type vertex =  (OVData.data * (E.NSet.T * E.NSet.T));
  type edge = ((Graph.dir_or_undir * EData.data) * (V.name * V.name));
  datatype undo_block = RWR of (Theory.Graph.T * UIData.T)
                      | UA of (Theory.Graph.T * UIData.T)
  type history = ((undo_block) list * 
                    (undo_block) list ) GraphName.NTab.T;
  type ud_unit = string Symtab.table

  (* data for controller; top level data with which user interacts *)
  datatype T = CState of {
       graphs : Graph.T GraphName.NTab.T, (* the known graphs *)
       rewrites : (rewrite list) GraphName.NTab.T, (* cached possible rewrites *)
       ud : UIData.T GraphName.NTab.T, (* User Data *)
       hist : history, (* past and future *)
       merge_changes: bool GraphName.NTab.T, (* whether we create a new undo
       point when the state is modified  *)
       rset : Ruleset.T, (* the known rules *)
       rset_uidata : (ud_unit * (UIData.T * UIData.T)) R.NTab.T, (* lhs and rhs uidata *)
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
  fun update_rset_uidata f (CState
    {graphs=g,rewrites=r,ud=u,hist=h,merge_changes=mg,rset=t,rset_uidata=tu,
      met_cont=mt}) = 
    CState {graphs=g,rewrites=r,ud=u,hist=h,merge_changes=mg,rset=t,rset_uidata=(f tu),met_cont=mt} 
  fun update_met_cont f (CState
    {graphs=g,rewrites=r,ud=u,hist=h,merge_changes=mg,rset=t,rset_uidata=tu,
      met_cont=mt}) = 
    CState {graphs=g,rewrites=r,ud=u,hist=h,merge_changes=mg,rset=t,rset_uidata=tu,met_cont=(f mt)} 

  val (set_graphs,set_rewrites,set_ud,set_hist,set_merge_changes,set_rset,set_rset_uidata,
    set_met_cont) = 
    (update_graphs o K, update_rewrites o K, update_ud o K,update_hist o K,
    update_merge_changes o K, update_rset o K, update_rset_uidata o K,
    update_met_cont o K)

  fun get_graphs (CState {graphs=g,...}) = g
  fun get_rewrites (CState {rewrites=r,...}) = r
  fun get_ud (CState {ud=u,...}) = u
  fun get_hist (CState {hist=h,...}) = h
  fun get_merge_changes (CState {merge_changes=mg,...}) = mg
  fun get_rset (CState {rset=t,...}) = t
  fun get_rset_uidata (CState {rset_uidata=tu,...}) = tu
  fun get_met_cont (CState {met_cont=mt,...}) = mt

  val init = CState {graphs=GraphName.NTab.empty,
                         rewrites=GraphName.NTab.empty,
                         ud=GraphName.NTab.empty,
                         hist=GraphName.NTab.empty,
                         merge_changes=GraphName.NTab.empty,
                         rset=Ruleset.empty,
                         rset_uidata=R.NTab.empty,
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

  fun get_rule_ud state name =
      (case R.NTab.lookup (get_rset_uidata state) name
      of SOME rud => rud
      | NONE => (Symtab.empty, (UIData.init, UIData.init)))

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

  fun set_rule_ud ud' rulename state =
      state |> update_rset_uidata (R.NTab.update (rulename, ud'))
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
        if Graph.has_vname (get_graph state gname) vname then ()
        else raise no_such_vertex_exp (vname, gname);

  fun assert_vertex_exists_gr graph vname =
        if Graph.has_vname graph vname then ()
        else raise Graph.no_such_vname_exp ("ASSERT", vname, graph);

  fun assert_edge_exists state gname ename =
        if Graph.has_ename (get_graph state gname) ename then ()
        else raise no_such_edge_exp (ename, gname);

  fun assert_edge_exists_gr graph ename =
        if Graph.has_ename graph ename then ()
        else raise Graph.no_such_ename_exp ("ASSERT", ename, graph);

  fun assert_bbox_exists state gname bname =
        if Graph.has_bbox (get_graph state gname) bname then ()
        else raise no_such_bbox_exp (bname, gname);


  (* Internal function: takes the state and updates the rset_uidata
   * against the current ruleset: removes dead user data  *)
  fun update_rset_uidata_against state =
    let
      val rulenames = (R.NTab.get_nameset o Ruleset.get_allrules o get_rset) state
      val ruleuidatanames = (R.NTab.get_nameset o get_rset_uidata) state
      val diff = R.NSet.remove_set rulenames ruleuidatanames
      val rset_uidata' = (R.NSet.fold (R.NTab.delete) diff) 
                                (get_rset_uidata state)
    in
      state |> set_rset_uidata rset_uidata'
    end;

  (* basic stuff on rule tags *)
  val list_allrules =
      R.NSet.list_of o R.NTab.get_nameset o Ruleset.get_allrules o get_rset

  val list_active_rules =
      R.NSet.list_of o Ruleset.get_active o get_rset

  val list_tags =
      TagName.NSet.list_of o Ruleset.get_tags o get_rset

  fun list_rules_with_tag tag_name state =
    if Ruleset.tag_exists (get_rset state) tag_name
    then
      let
        val rule_name_set = TagRel.codf (Ruleset.get_tagrel (get_rset state)) tag_name
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
               |> update_rset_uidata (snd o (R.NTab.rename1 oldName newName)) 
      end

  (* creates/replaces rule *)
  fun set_rule rule_name lhs rhs state =
      let
        (* build graphs for the LHS and RHS *)
        val lgr = get_graph state lhs
        val rgr = get_graph state rhs
        val rule = Ruleset.Rule.mk (lgr, rgr)
        val lgr_ud = get_graph_ud state lhs
        val rgr_ud = get_graph_ud state rhs

        (* Problem: When Rule.mk is called it renames the RHS apart.
         * Therefore, the names in rhs_ud become obsolete and we need to 
         * rename them as well. It does not seem to be a good idea to make
         * Rule.mk return the renamings and it involves a lot changes everywhere
         * in the code: We rename them by hand *)
        val lhs_boundary = Graph.get_boundary lgr
        val ign_vnames =
            V.NSet.union_merge
              lhs_boundary
              (V.NSet.subtract (Graph.get_vnames rgr)
                                  (Graph.get_vnames lgr))
        val ign_enames = E.NSet.subtract (Graph.get_enames rgr)
                          (Graph.get_enames lgr)
        val vrn = V.mk_renaming ign_vnames
                      (V.NSet.union_merge ign_vnames
                        (Graph.get_vnames lgr)) V.NTab.empty
        val ern = E.mk_renaming ign_enames 
                    (E.NSet.union_merge ign_enames (Graph.get_enames lgr))
                    E.NTab.empty
        (* Now we actually do the renaming *)
        val (_,newvtab) = (V.NTab.rename vrn (UIData.get_vs_ud rgr_ud)) 
        val (_,newetab) = (E.NTab.rename ern (UIData.get_es_ud rgr_ud)) 
        val rgr_ud' = rgr_ud |> UIData.set_v_ud newvtab
                             |> UIData.set_e_ud newetab
        (* We also need to take care of the uidata attached to the rule itself *)
        val rule_ud = fst (get_rule_ud state rule_name)
      in
        state |> update_rset (Ruleset.update_rule (rule_name, rule))
              |> update_rset_uidata 
                  (R.NTab.update (rule_name, (rule_ud, (lgr_ud, rgr_ud'))))
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
                      |> set_graph_ud UIData.init nm) end

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

  fun lift_graph_mutator cmdf gname state =
      let 
          val (out,g') = cmdf (get_graph state gname)
          val ud' = UIData.update_against_graph (get_graph_ud state gname) g'
      in
        (out, state |> push_hist gname UA
                    |> set_graph g' gname
                    |> set_graph_ud ud' gname)
      end
      handle Graph.no_such_vname_exp (_,vname,_) =>
               raise no_such_vertex_exp (vname,gname)
           | Graph.no_such_ename_exp (_,ename,_) =>
               raise no_such_edge_exp (ename,gname)
           | Graph.vname_already_exists_exp (vname,_) =>
                raise vertex_name_exists_exp (vname,gname)
           | Graph.ename_already_exists_exp (ename,_) =>
                raise edge_name_exists_exp (ename,gname)
           | Graph.bbox_already_exists_exp (bname,_) =>
                raise bbox_name_exists_exp (bname,gname)
           | Graph.wire_vertex_exp (msg,_,v) =>
                raise wire_vertex_exp (msg,gname,v)
           | Graph.bbox_not_open_exp (vs,b,_) =>
                raise inconsistent_bbox_exp (vs,b,gname);

  fun lift_graph_mutator_no_result cmdf gname state =
      let 
          val g' = cmdf (get_graph state gname)
          val ud' = UIData.update_against_graph (get_graph_ud state gname) g'
      in
        state |> push_hist gname UA
              |> set_graph g' gname
              |> set_graph_ud ud' gname
      end
      handle Graph.no_such_vname_exp (_,vname,_) =>
               raise no_such_vertex_exp (vname,gname)
           | Graph.no_such_ename_exp (_,ename,_) =>
               raise no_such_edge_exp (ename,gname)
           | Graph.vname_already_exists_exp (vname,_) =>
                raise vertex_name_exists_exp (vname,gname)
           | Graph.ename_already_exists_exp (ename,_) =>
                raise edge_name_exists_exp (ename,gname)
           | Graph.bbox_already_exists_exp (bname,_) =>
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
      handle Graph.no_such_vname_exp (_,vname,_) =>
               raise no_such_vertex_exp (vname,gname)
           | Graph.no_such_ename_exp (_,ename,_) =>
               raise no_such_edge_exp (ename,gname);


  fun add_vertex_internal vt graph =
      (let val (n,g) = graph |> Graph.add_vertex (Theory.VertexComponentData.default_data vt) in
          ((n,Graph.get_vertex g n), g)
       end
       handle Theory.VertexComponentData.unknown_type_exp _ => raise no_such_vertex_type_exp vt)
  fun add_vertex vt gname state = lift_graph_mutator (add_vertex_internal vt) gname state

  fun add_edge_internal et dir_or_undir v1 v2 graph = let
    val data = Theory.EdgeComponentData.default_data et
               handle Theory.EdgeComponentData.unknown_type_exp _
                        => raise no_such_edge_type_exp et
    val _ = case Graph.tryget_bbox_extended_by_edge v1 v2 graph
              of NONE => ()
               | SOME b => raise Graph.bbox_not_open_exp ((v1,v2),b,graph)
    val (n,g) = graph |> Graph.add_edge (dir_or_undir,data) v1 v2
  in
    ((n,Graph.get_edge g n), g)
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
    ([new],graph |> Graph.rename_vname old new)
    handle Graph.vname_already_exists_exp _ =>
      (* Rename apart the old vertex *)
      let
        (*Ideally, we would just need to call the suc function in order
         * to generate a fresh name, but how?*)
        val ((frshN,_),_) = add_vertex_internal "edge-point" graph
      in
        ([new,frshN],graph |> Graph.rename_vname new (frshN)
                           |> Graph.rename_vname old new)
      end;
 
  (* There was no conflict *) 
  fun rename_vertex_uidata old (new::[]) graphName state =
    let
      val ud = get_graph_ud state graphName
    in
      state |> set_graph_ud (UIData.rename_vertex old new ud) graphName
    end
    (* We had a conflict, therefore we have a freshName for 
     * the node vertex that we need to rename as well *)
    | rename_vertex_uidata old (new::fresh::[]) graphName state =
    let
      val ud = get_graph_ud state graphName
      val ud' = ud |> UIData.rename_vertex new fresh
                   |> UIData.rename_vertex old new
    in
      state |> set_graph_ud ud' graphName
    end;
        
  fun rename_vertex old new graph state = 
    let
      val (out, state') = lift_graph_mutator (rename_vertex_internal old new) graph state
    in
    (* When a vertex is renamed, the ui_data as well *)
     (out, state' 
                |> rename_vertex_uidata old out graph)
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

  fun rename_edge_internal old new graph = (graph |> Graph.rename_ename old new)
  fun rename_edge old new graph state = 
      let
        val state' = lift_graph_mutator_no_result (rename_edge_internal old new) graph state
      in
        state' |> rename_edge_or_bb_uidata UIData.rename_edge old new graph 
      end;

  fun rename_bang_internal old new graph = (graph |> Graph.rename_bbox old new)
  fun rename_bang old new graph state = 
    let
      val state' = lift_graph_mutator_no_result (rename_bang_internal old new) graph state
    in
      state' |> rename_edge_or_bb_uidata UIData.rename_bangbox old new graph
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
          (let val vset = fold V.NSet.ins_fresh vs V.NSet.empty
               val graph = get_graph state gname
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
        val (n,state') = attach_with apply_first_in gname (V.NSet.list_of (Graph.get_vnames graph)) state
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
      let val _ = map (assert_bbox_exists state gname) bs in
        lift_graph_mutator (Graph.merge_bboxes (B.NSet.of_list bs)) gname state
      end

  fun duplicate_bang bb gname state =
      let val _ = assert_bbox_exists state gname bb in
        lift_graph_mutator (Graph.fresh_copy1_bbox bb) gname state
      end

  fun bang_vertices box verts gname state =
      let
        val _ = assert_bbox_exists state gname box
        val _ = map (assert_vertex_exists state gname) verts
        val vset = V.NSet.of_list verts
      in
        lift_graph_mutator_no_result (Graph.doadd_to_bbox box vset) gname state
        handle Graph.bbox_not_open_exp (vs,b,_)
             => raise inconsistent_bbox_exp (vs,b,gname)
      end

  fun completely_unbang_vertices verts gname state =
      let
        val _ = map (assert_vertex_exists state gname) verts
        val vset = V.NSet.of_list verts
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
        val mutator = Graph.doremove_from_bbox box vset
      in
        lift_graph_mutator_no_result mutator gname state
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
      val clip = Graph.delete_subgraph_outside_vset 
                   (V.NSet.of_list vs)
                   (get_graph state source)
      fun add_v_ud ud n ud' =
        let
          val v_ud = case (UIData.get_v_ud ud n) of
            SOME a => a
            | NONE => Symtab.empty 
        in
          ud' |> UIData.update_v_ud (fn _ => v_ud) n
              |> UIData.update_v_ud (Symtab.update ("copy_of", V.dest n)) n
        end
      fun add_e_ud ud n ud' =
        let
          val e_ud = case (UIData.get_e_ud ud n) of
            SOME a => a
            | NONE => Symtab.empty 
        in
          ud' |> UIData.update_e_ud (fn _ => e_ud) n
              |> UIData.update_e_ud (Symtab.update ("copy_of", E.dest n)) n
        end
      fun add_b_ud ud n ud' =
        let
          val b_ud = case (UIData.get_bb_ud ud n) of
            SOME a => a
            | NONE => Symtab.empty 
        in
          ud' |> UIData.update_bb_ud (fn _ => b_ud) n
              |> UIData.update_bb_ud (Symtab.update ("copy_of", B.dest n)) n
        end
      val ud  = get_graph_ud state source
      val ud' = UIData.init |> fold (add_v_ud ud)
                      (V.NSet.list_of (Graph.get_vnames clip))
                            |> fold (add_e_ud ud)
                      (E.NSet.list_of (Graph.get_enames clip))
                            |> fold (add_b_ud ud)
                      (Graph.get_bbox_list clip)
                            |> UIData.update_g_ud 
                      (fn _ => (UIData.get_g_ud ud))
  in state |> set_graph clip target
           |> set_graph_ud ud' target 
  end

  (* Rename the graph apart, insert it and insert its user data as well*)
  fun insert_graph sname tname state = 
      let
        val sarg = get_graph state sname
        val targ = get_graph state tname
        val ((_,vrn,ern), source) = Graph.rename_apart targ sarg
        (* Rename the ud and insert it on the new graph  *)
        val ud_s = get_graph_ud state sname
        val ud_t = get_graph_ud state tname
        val (_,newvtab) = (V.NTab.rename vrn (UIData.get_vs_ud ud_s)) 
        val (_,newetab) = (E.NTab.rename ern (UIData.get_es_ud ud_s)) 
        val ud' = ud_t |> UIData.set_v_ud newvtab
                       |> UIData.set_e_ud newetab
                       |> UIData.set_b_ud (UIData.get_bs_ud ud_s) 
      in state |> push_hist tname UA
               |> set_graph (Graph.merge_by_vertices targ source) tname
               |> set_graph_ud ud' tname
      end

  fun graph_of_rule getter pos label state rulename =
      let
        val rset = get_rset state
        val r = case R.NTab.lookup (Ruleset.get_allrules rset) rulename
                     of SOME v => v 
                    | NONE => raise no_such_rule_exp rulename
        val (name, graphs) = GraphName.NTab.add (GraphName.mk ((R.dest rulename)^"_"^label), getter r) (get_graphs state)
      in
        (name, state
                     |> set_graphs graphs
                     |> set_graph_ud (pos (get_rule_ud state rulename)) name)
      end

  val open_rule_lhs = graph_of_rule Rule.get_lhs (fst o snd) "lhs"
  val open_rule_rhs = graph_of_rule Rule.get_rhs (snd o snd) "rhs"

  (* Set, delete and get the user data  *)
  (* On Graphs:  *)
  fun update_graph_user_data_internal dn ud user_data =
    let
      val updater = Symtab.update (dn, ud)
    in
      UIData.update_g_ud updater user_data
    end

  fun update_graph_user_data dn ud = lift_ud_mutator_no_result
    (update_graph_user_data_internal dn ud)

  fun delete_graph_user_data_internal dn user_data = 
    let
      val updater = Symtab.delete dn
    in
      UIData.update_g_ud updater user_data
    end

  fun delete_graph_user_data dn = lift_ud_mutator_no_result
    (delete_graph_user_data_internal dn)

  fun get_graph_user_data gn state =
    UIData.get_g_ud (get_graph_ud state gn)

  (* On Vertices:  *)
  fun update_vertex_user_data_internal dn ud vn user_data =
    let
      val updater = Symtab.update (dn, ud)
    in
      UIData.update_v_ud updater vn user_data
    end

  fun update_vertex_user_data dn ud vn = lift_ud_mutator_no_result
    (update_vertex_user_data_internal dn ud vn)

  fun get_vertex_user_data gn vn state =
    let
      val () = assert_vertex_exists state gn vn
    in
      UIData.get_v_ud (get_graph_ud state gn) vn
    end

  fun delete_vertex_user_data_internal dn vn user_data =
    let
      val updater = Symtab.delete dn
    in
      UIData.update_v_ud updater vn user_data
    end
  fun delete_vertex_user_data dn vn = lift_ud_mutator_no_result
    (delete_vertex_user_data_internal dn vn)

  (* On Edges:  *)
  fun update_edge_user_data_internal dn ud en user_data =
    let
      val updater = Symtab.update (dn, ud)
    in
      UIData.update_e_ud updater en user_data
    end

  fun update_edge_user_data dn ud en = lift_ud_mutator_no_result
    (update_edge_user_data_internal dn ud en)

  fun get_edge_user_data gn en state =
    let
      val () = assert_edge_exists state gn en
    in
      UIData.get_e_ud (get_graph_ud state gn) en
    end

  fun delete_edge_user_data_internal dn en user_data =
    let
      val updater = Symtab.delete_safe dn
    in
      UIData.update_e_ud updater en user_data
    end
  fun delete_edge_user_data dn en = lift_ud_mutator_no_result
    (delete_edge_user_data_internal dn en)

  (* On Bang Boxes:  *)
  fun update_bangbox_user_data_internal dn ud bn user_data =
    let
      val updater = Symtab.update (dn, ud)
    in
      UIData.update_bb_ud updater bn user_data
    end

  fun update_bangbox_user_data dn ud bn = lift_ud_mutator_no_result
    (update_bangbox_user_data_internal dn ud bn)

  fun get_bangbox_user_data gn bn state =
    let
      val () = assert_bbox_exists state gn bn
    in
      UIData.get_bb_ud (get_graph_ud state gn) bn
    end

  fun delete_bangbox_user_data_internal dn bn user_data =
    let
      val updater = Symtab.delete_safe dn
    in
      UIData.update_bb_ud updater bn user_data
    end
  fun delete_bangbox_user_data dn bn = lift_ud_mutator_no_result
    (delete_bangbox_user_data_internal dn bn)

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
