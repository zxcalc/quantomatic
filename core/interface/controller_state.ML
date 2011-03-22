(* controller state *)

structure GraphName :> SSTR_NAMES = 
struct open SStrName val default_name = SStrName.mk "new-graph-1" end;

signature CONTROLLER_STATE =
sig
  type T;
  type rewrite = (RuleName.name * RGRule.T) * RGGraph.T
  datatype response = OK | COUNT of int | NAME of string | NAME_LIST of (string list) | REWRITE_LIST of (rewrite list) | DATA of string;

  exception wrong_args_exp of unit (* when wrong arguments are given to a command *)
  exception user_exp of string; (* for errors sent to user console *) 

  val init : T (* initial empty state, no graphs *)

  val as_name : string -> response
  val as_data : string -> response

  (* get/set*)
  val get_rset : T -> RGRuleset.T
  val get_rewrites : T -> rewrite list GraphName.NTab.T
  val get_hist : T -> (RGGraph.T list * RGGraph.T list) GraphName.NTab.T
  val get_graphs : T -> RGGraph.T GraphName.NTab.T
  val get_graph : T -> GraphName.name -> RGGraph.T

  val update_rule : string list * T -> response * T
  val update_rset : (RGRuleset.T -> RGRuleset.T) -> T -> T
  val update_rewrites :
   (rewrite list GraphName.NTab.T -> rewrite list GraphName.NTab.T) ->
     T -> T
  val update_hist :
   ((RGGraph.T list * RGGraph.T list) GraphName.NTab.T ->
     (RGGraph.T list * RGGraph.T list) GraphName.NTab.T) -> T -> T
  val update_graphs :
   (RGGraph.T GraphName.NTab.T -> RGGraph.T GraphName.NTab.T) -> T -> T

  val set_rset : RGRuleset.T -> T -> T
  val set_rewrites : rewrite list GraphName.NTab.T -> T -> T
  val set_hist : (RGGraph.T list * RGGraph.T list) GraphName.NTab.T -> T -> T
  val set_graphs : RGGraph.T GraphName.NTab.T -> T -> T
  val set_graph : RGGraph.T -> GraphName.name -> T -> T


  
  (* history *)
  val undo : string list * T -> response * T (* response: OK (FIXME: return what changed?) *)
  val redo : string list * T -> response * T (* response: OK (FIXME: return what changed?) *)
  val push_hist : GraphName.name -> T -> T

  (* input/output *)
  val save_ruleset : string list * T -> response (* response: OK *)
  val save_graph : string list * RGGraph.T -> response (* response: OK *)
  val load_ruleset : string list * T -> response * T (* response: OK *)
  val load_graph : string list * T -> response * T (* response: NAME graphname *)

  (* printing/showing info (for console) *)
  val show_rewrites : string list * T -> response (* response: OUTPUT rewrite-list-xml *)
  val print_graph : 'a list -> RGGraph.T -> response (* response: DATA pretty-graph-structure *)
  val list_graphs : 'a list * T -> response (* response: NAME_LIST pretty-graph-list *)
  val list_rules_with_tag : string list * T -> response (* response: NAME_LIST pretty-rule-list *)
  val list_allrules : 'a list * T -> response (* response: NAME_LIST pretty-rule-list *)
  val list_tags : 'a list * T -> response (* response: NAME_LIST pretty-tag-list *)
  val list_active_rules : 'a list * T -> response (* response: NAME_LIST pretty-rule-list *)

  (* rewriting *)
  val attach_rewrites : string list * T -> response * T (* response: COUNT num-of-rewrites *)
  val attach_first_rewrite : string list * T -> response * T (* response: COUNT num-of-rewrites *)
  val apply_rewrite : string list * T -> response * T (* response: OK *)
  val apply_first_rewrite : string list * T -> response * T (* response: OK *)

  (* rules/rulsesets *)
  val new_rule : string list * T -> response * T (* response: OK *)
  val open_rule_rhs : string list * T -> response * T (* response: NAME graph-name *)
  val open_rule_lhs : string list * T -> response * T (* response: NAME graph-name *)
  val delete_tag_and_its_rules : string list * T -> response * T (* response: OK *)
  val delete_tag : string list * T -> response * T (* response: OK *)
  val delete_rules_in_tag : string list * T -> response * T (* response: OK *)
  val delete_rule : string list * T -> response * T (* response: OK *)

  val activate_rules_with_tag : string list * T -> response * T (* response: OK *)
  val activate_rule : string list * T -> response * T (* response: OK *)
  val deactivate_rules_with_tag : string list * T -> response * T (* response: OK *)
  val deactivate_rule : string list * T -> response * T (* response: OK *)

  val tag_rule : string list * T -> response * T (* response: OK *)
  val untag_rule : string list * T -> response * T (* response: OK *)

  (* graphs and their manipulations *)
  val new_graph : 'a list * T -> response * T (* response: NAME graph-name *)
  val kill_graph : string list * T -> response * T (* response: OK *)
  val insert_graph : string list * T -> response * T (* response: OK *)
  val duplicate_graph : string list * T -> response * T (* response: NAME graph-name *)
  val create_graph : GraphName.name -> RGGraph.T -> T -> response * T (* response: NAME graph-name *)
  val copy_subgraph : string list * T -> response * T (* response: OK *)

  val add_vertex : string list * RGGraph.T -> response * RGGraph.T (* response: NAME vertex-name *)
  val add_edge : string list * RGGraph.T -> response * RGGraph.T (* response: NAME edge-name *)
  val add_bang : 'a list * RGGraph.T -> response * RGGraph.T (* response: NAME bb-name *)

  val set_angle : string list * RGGraph.T -> response * RGGraph.T (* response: OK *)
  val rename_vertex : string list * RGGraph.T -> response * RGGraph.T (* response: OK *)
  val rename_graph : string list * T -> response * T (* response: NAME new-graph-name *)
  val delete_vertices : string list * RGGraph.T -> response * RGGraph.T (* response: OK *)
  val delete_edges : string list * RGGraph.T -> response * RGGraph.T (* response: OK *)

  val bang_vertices : string list * RGGraph.T -> response * RGGraph.T (* response: OK *)
  val kill_bang : string list * RGGraph.T -> response * RGGraph.T (* response: OK *)
  val unbang_vertices : string list * RGGraph.T -> response * RGGraph.T (* response: OK *)
  val merge_bang : string list * RGGraph.T -> response * RGGraph.T (* response: OK *)
  val duplicate_bang : string list * RGGraph.T -> response * RGGraph.T (* response: OK *)
  val drop_bang : string list * RGGraph.T -> response * RGGraph.T (* response: OK *)

  val flip_vertices : string list * RGGraph.T -> response * RGGraph.T (* response: OK *)

  (* other *)
  val hilb : string list * RGGraph.T -> response (* response: DATA hilb-term *)

  (* internal *)
  val assert_tag_exists : T -> TagName.name -> unit
  val assert_rule_exists : T -> RuleName.name -> unit
  val graph_xml : 'a list -> RGGraph.T -> response (* response: DATA graph-xml *)
  val graph_arg : string list -> GraphName.name (* first argument as a graph name *)

  val tag_name_action :
   (TagName.name -> RGRuleset.T -> RGRuleset.T) -> string list * T -> response * T
  val rule_name_action :
   (TagName.name -> RGRuleset.T -> RGRuleset.T) ->
     string list * T -> response * T
  val graph_of_rule :
   (RGRule.T -> RGGraph.T) -> string -> string list * T -> response * T

end;


(* controls the state of Quantomatic *)
functor ControllerState(
  structure RulesetRewrites : RULESET_REWRITES
  (* FIXME: HACK: interface cannot be generic while we depend on RG data - need to generalise protocol and GUI for arbitrary kinds of data *)
  where type Rule.Graph.Vertex.data = RGVertex.data
    and type Rule.Graph.Edge.data = unit 
  structure IO_Interface : IO_INTERFACE
  sharing type IO_Interface.Input.Graph.data = RulesetRewrites.Ruleset.Rule.Graph.T
  sharing type IO_Interface.Input.Rule.data = RulesetRewrites.Ruleset.Rule.T
  sharing type IO_Interface.Input.Ruleset.data = RulesetRewrites.Ruleset.T
) 
= struct
  

  exception user_exp of string
  
  structure Input = IO_Interface.Input;
  structure Output = IO_Interface.Output;
  structure Ruleset = RulesetRewrites.Ruleset;
  structure R = Ruleset.Rule;
  structure G = R.Graph;
  structure V = G.Vertex;
  structure E = G.Edge;
  
  type rewrite = (RuleName.name * R.T) * G.T (* rule, and graph to be rewritten to *)
  datatype response = OK | COUNT of int | NAME of string | NAME_LIST of (string list) | REWRITE_LIST of (rewrite list) | DATA of string;

  (* data for controller; top level data with which user interacts *)
  datatype T = CState of {
       graphs : G.T GraphName.NTab.T, (* the known graphs *)
       rewrites : (rewrite list) GraphName.NTab.T, (* cached possible rewrites *)
       hist : (G.T list * G.T list) GraphName.NTab.T, (* past and future *)
       rset : Ruleset.T (* the known rules *)
  }
  
  fun as_name str = NAME str
  fun as_data str = DATA str
  
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
  
  (*val default_rset = Ruleset.NTab.empty
                             |> Ruleset.NTab.update (Ruleset.mk "default",
                                                    (!RulesetHack.rules,true))
  *)
  
  val init = CState {graphs=GraphName.NTab.empty,
                         rewrites=GraphName.NTab.empty,
                         hist=GraphName.NTab.empty,
                         rset=Ruleset.empty};
  
  (* state accessors *)
  fun get_graph state name  =
      (case GraphName.NTab.lookup (get_graphs state) name
       of SOME g => g
        | NONE => raise user_exp ("No such graph '"^
                  (GraphName.dest name)^"'."))
  
  
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
  
  (* convenience functions *)
  
  fun graph_arg (g::args) = (GraphName.mk g)
    | graph_arg _ = raise user_exp ("You must specify a graph name.")
  

  (* Most commands are already of the form
   *    cmd: [args] * state -> out * state *)
  (* val lift_state_mutator = I *)
  
  (* ============================================== *)
  (* these functions actually implement commmands *)
  
  exception wrong_args_exp of unit
  
  fun list_graphs ([], state) = 
      NAME_LIST (map GraphName.dest (GraphName.NSet.list_of (GraphName.NTab.get_nameset (get_graphs state))))
    | list_graphs _ = raise wrong_args_exp ()
  
  (* common things we want to assert are true *)
  fun assert_tag_exists state tag_name = 
      if Ruleset.tag_exists (get_rset state) tag_name then ()
      else raise user_exp ("No such tag named '" 
                           ^ (TagName.string_of_name tag_name) ^ "'");

  fun assert_rule_exists state rule_name = 
      if Ruleset.rule_exists (get_rset state) rule_name then ()
      else raise user_exp ("No such rule named '" 
                           ^ (RuleName.string_of_name rule_name) ^ "'");

  fun assert_not_rule_exists state rule_name = 
      if not (Ruleset.rule_exists (get_rset state) rule_name) then ()
      else raise user_exp ("Rule '" ^ (RuleName.string_of_name rule_name)
                                    ^ "' already exists");

  fun output_rulenames nset = NAME_LIST (map RuleName.string_of_name (RuleName.NSet.list_of nset))

  (* basic stuff on rule tags *)
  fun list_allrules ([], state) =
      output_rulenames (RuleName.NTab.get_nameset (Ruleset.get_allrules (get_rset state)))
    | list_allrules _ = raise wrong_args_exp ()
  
  fun list_active_rules ([], state) =
      output_rulenames (Ruleset.get_active (get_rset state))
    | list_active_rules _ = raise wrong_args_exp ()
  
  fun list_tags ([], state) =
      NAME_LIST (map TagName.string_of_name (TagName.NSet.list_of (Ruleset.get_tags (get_rset state))))
    | list_tags _ = raise wrong_args_exp ()
  
  fun list_rules_with_tag ([tag], state) = 
      let 
        val tag_name = TagName.mk tag;
        val () = assert_tag_exists state tag_name;
        val rule_name_set = TagRel.codf (Ruleset.get_tagrel (get_rset state)) tag_name
      in output_rulenames rule_name_set end
    | list_rules_with_tag _ = raise wrong_args_exp ();
      
  fun tag_name_action f ([tag], state) =
      let val tag_name = TagName.mk tag
          val () = assert_tag_exists state tag_name
      in (OK, update_rset (f tag_name) state) end
    | tag_name_action f (_, state) = raise wrong_args_exp ();
  val deactivate_rules_with_tag = tag_name_action Ruleset.deactivate_tag; 
  val activate_rules_with_tag = tag_name_action Ruleset.activate_tag


  fun rule_name_action f ([rule], state) = 
      let val rule_name = RuleName.mk rule
          val () = assert_rule_exists state rule_name
      in (OK, update_rset (f rule_name) state) end
    | rule_name_action f (_, state) = raise wrong_args_exp ();
  val deactivate_rule  = rule_name_action Ruleset.deactivate_rule
  val activate_rule = rule_name_action Ruleset.activate_rule
  val delete_rule = rule_name_action Ruleset.delete_rule

  (* creates/replaces rule *)
  fun force_update_rule rule_name lhs rhs state =
      let
        val graphs = get_graphs state
        
        (* build graphs for the LHS and RHS *)
        val lgr = GraphName.NTab.get graphs (GraphName.mk lhs)
          handle GraphName.Tab.UNDEF _ => raise user_exp ("LHS graph '" ^ lhs ^"' doesn't exist.")
        val rgr = GraphName.NTab.get graphs (GraphName.mk rhs)
          handle GraphName.Tab.UNDEF _ => raise user_exp ("RHS graph '" ^ rhs ^"' doesn't exist.")
          
      in (OK, state |> update_rset (Ruleset.update_rule (rule_name, Ruleset.Rule.mk (lgr, rgr))))
          handle Ruleset.Rule.bad_rule_exp _ =>
            raise user_exp "Boundaries and !-boxes on LHS must match RHS."
      end
  
  (* updates an existing rule *)
  fun update_rule ([rule,lhs,rhs], state) =
      let val rule_name = RuleName.mk rule
          val () = assert_rule_exists state rule_name
      in force_update_rule rule_name lhs rhs state end
    | update_rule (_, state) = raise wrong_args_exp ();
  
  (* create a new rule from two graphs *)
  fun new_rule ([rule,lhs,rhs], state) =
      let val rule_name = RuleName.mk rule
          val () = assert_not_rule_exists state rule_name
      in force_update_rule rule_name lhs rhs state end
    | new_rule (_, state) = raise wrong_args_exp ();
  
  fun load_ruleset ([file], state) =
      (let val loader = Input.Ruleset.input o XMLReader.read_from_file
       in (OK, state |> update_rset (Ruleset.merge (loader file))) end
       handle IO.Io _ => raise user_exp "Can't open rules file."
            | Input.Ruleset.bad_input_exp (s,p) => raise user_exp ("Malformed rules file: "^s^" at "^p))
    | load_ruleset _ = raise wrong_args_exp ();
  
  fun save_ruleset ([file], state) =
      (let val save = (XMLWriter.write_to_file file) o Output.Ruleset.output
       in (save (get_rset state); OK)
       end
       handle IO.Io _ => raise user_exp ("Can't write rules file \""^file^"\"."))
    | save_ruleset _ = raise wrong_args_exp ();
    
  fun delete_tag ([tag], state) =
      let val tag_name = TagName.mk tag
          val () = assert_tag_exists state tag_name
      in (OK, update_rset (Ruleset.delete_tag tag_name) state) end
    | delete_tag _ = raise wrong_args_exp ();

  fun delete_rules_in_tag ([tag], state) =
      let val tag_name = TagName.mk tag
          val () = assert_tag_exists state tag_name
      in (OK, update_rset (Ruleset.delete_rules_in_tag tag_name) state) end
    | delete_rules_in_tag _ = raise wrong_args_exp ();

  fun delete_tag_and_its_rules ([tag], state) =
      let val tag_name = TagName.mk tag
	  val () = assert_tag_exists state tag_name
      in (OK, update_rset (Ruleset.delete_tag tag_name
                                   o Ruleset.delete_rules_in_tag tag_name) state) end
    | delete_tag_and_its_rules _ = raise wrong_args_exp ();

  fun tag_rule ([rule,tag], state) = 
      let val rule_name = RuleName.mk rule
          val tag_name = TagName.mk tag
          val () = assert_rule_exists state rule_name
      in (OK, update_rset (Ruleset.tag_rule rule_name tag_name) state) end
    | tag_rule (_, state) = raise wrong_args_exp ();

  fun untag_rule ([rule,tag], state) = 
      let val rule_name = RuleName.mk rule
          val tag_name = TagName.mk tag
          val () = assert_rule_exists state rule_name
          val () = assert_tag_exists state tag_name
      in (OK, update_rset (Ruleset.untag_rule rule_name tag_name) state) end
    | untag_rule (_, state) = raise wrong_args_exp ();
      
    
    fun create_graph default_name graph state = let
        val (nm,gs) = (get_graphs state)
                  |> GraphName.NTab.add (default_name, graph)
        in (NAME (GraphName.dest nm), state |> set_graphs gs) end
  
    fun duplicate_graph ([name], state) = 
        let val gname = (GraphName.mk name) 
        in create_graph gname (get_graph state gname) state
        end
      | duplicate_graph _ = raise wrong_args_exp ()

    fun new_graph ([], state) =
        create_graph GraphName.default_name G.empty state
      | new_graph _ = raise wrong_args_exp ();

    fun load_graph ([fname], state) = 
        create_graph GraphName.default_name 
                     (Input.Graph.input (XMLReader.read_from_file fname)) state
      | load_graph _ = raise wrong_args_exp ()
    (* fun input_graph_xml ([], state) = 
        let val gr_block = read_block ()
            val gr = InputRGGraphXML.input (XMLReader.read_from_string gr_block)
        in create_graph GraphName.default_name gr state end
      | input_graph_xml _ = raise wrong_args_exp () *)
  
  fun kill_graph ([name], state) =
      ((OK, update_graphs
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
        (NAME (Pretty.string_of (GraphName.pretty_name nm)),
         set_graphs graphs' state)
      end
      handle GraphName.NTab.Tab.UNDEF _ => raise user_exp ("no such graph: "^old))
    | rename_graph _ = raise wrong_args_exp ()
  
  
  fun save_graph ([_, fname], graph) = 
      ((XMLWriter.write_to_file fname (Output.Graph.output graph); OK)
       handle _ => raise user_exp ("Could not save file "^fname))
    | save_graph _ = raise wrong_args_exp ()
  
  (* FIXME: make generic over vertex data *)
  fun vertex_type "red" = RGVertex.IVert (RGVertex.Xnd AngleExpr.zero)
    | vertex_type "green" =  RGVertex.IVert (RGVertex.Znd AngleExpr.zero)
    | vertex_type "hadamard" = RGVertex.IVert RGVertex.Hnd
    | vertex_type "boundary" = RGVertex.EVert
    | vertex_type _ = raise user_exp "Bad vertex type."
  
  fun add_vertex ([_,vt], graph) =
      let val (n,g) = graph |> G.add_vertex (vertex_type vt)
      in (NAME (G.Vertex.dest n), g)
      end
    | add_vertex _ = raise wrong_args_exp ()
  
  (* FIXME: make generic over edge data *)
  fun add_edge ([_,v1,v2], graph) =
      (let val (n,g) = graph |> G.add_edge (G.UnDirected,()) (V.mk v1) (V.mk v2)
       in (NAME (G.Edge.dest n), g)
       end
       handle G.no_such_vname_exp _ =>
              raise user_exp "Bad vertex name.")
    | add_edge _ = raise wrong_args_exp ()
  
  
  fun delete_vertices (_::vs, graph) =
      ((OK, fold G.delete_vertex (map V.mk vs) graph)
       handle G.no_such_vname_exp _ => raise user_exp "No such vertex.")
    | delete_vertices _ = raise wrong_args_exp ()
  
  fun delete_edges (_::es, graph) =
      ((OK, fold G.delete_edge (map E.mk es) graph)
       handle G.no_such_ename_exp _ => raise user_exp "No such edge.")
    | delete_edges _ = raise wrong_args_exp ()
  
  fun rename_vertex ([_,old,new], graph) =
      ((OK, graph |> G.rename_vname (V.mk old) (V.mk new))
       handle G.vname_already_exists_exp _ =>
              raise user_exp "graph already contains a vertex of this name"
            | G.no_such_vname_exp _ => raise user_exp "no such vertex")
    | rename_vertex _ = raise wrong_args_exp ()
  
  fun graph_xml [nme] = as_data o XML.string_of o Output.Graph.output
    | graph_xml _ = raise wrong_args_exp ()
  
  fun print_graph [_] = as_data o Pretty.string_of o G.pretty
    | print_graph _ = raise wrong_args_exp ()
                        
  fun undo ([name], state) = 
      let
        val gname = (GraphName.mk name) 
        val current = get_graph state gname
      in case GraphName.NTab.lookup (get_hist state) gname
          of SOME (g::gs, hs) => (OK,
              state |> set_graph g gname
                    |> update_hist (GraphName.NTab.update
                                   (gname, (gs,current::hs))))
           | _ => raise user_exp "Can't undo, no more undo history."
      end
    | undo _ = raise wrong_args_exp ()
  
  fun redo ([name], state) = 
      let
        val gname = GraphName.mk name;
        val current = get_graph state gname
      in case GraphName.NTab.lookup (get_hist state) gname
          of SOME (gs, h::hs) => (OK,
              state |> set_graph h gname
                    |> update_hist (GraphName.NTab.update
                                   (gname, (current::gs,hs))))
           | _ => raise user_exp "Can't redo, nothing to redo."
      end
    | redo _ = raise wrong_args_exp ()
  
  fun apply_rewrite ([name, n], state) =
      (let val gname = GraphName.mk name
           val rewrites = GraphName.NTab.get (get_rewrites state) gname
           val ((rulename,rule),rewritten_graph) = nth rewrites (the (Int.fromString n))
       in (OK, state |> push_hist gname
                     |> set_graph rewritten_graph gname)
       end
       handle GraphName.Tab.UNDEF _ => raise user_exp "No rewrites attached." 
            | General.Subscript => raise user_exp "Rewrite not attached."
            | Option.Option => raise user_exp "Rewrite selection must be an integer.")
    | apply_rewrite _ = raise wrong_args_exp ()
  
  local
      fun attach_with' attacher (args as _::vs, state) =
          (let val gname = graph_arg args
               val vset = fold V.NSet.ins_fresh (map V.mk vs) V.NSet.empty
               val graph = get_graph state gname
               val rws = (Seq.list_of (attacher (get_rset state) graph vset))
           in
               (length rws,
                state |> update_rewrites (GraphName.NTab.update (gname,rws)))
           end)
      | attach_with' _ _ = raise wrong_args_exp ();

      fun attach_with attacher args = 
          let val (n,st) = attach_with' attacher args
          in (COUNT n,st) end;

      fun apply_first_in rset vs g = 
          case RulesetRewrites.apply_first_in rset vs g of NONE => Seq.empty
          | SOME x => Seq.single x;
  in
  val attach_rewrites = attach_with RulesetRewrites.apply_in;
  val attach_first_rewrite = attach_with apply_first_in; 
  
  fun apply_first_rewrite (args as [_], state) = 
      let
        val gname = graph_arg args
        val graph = get_graph state gname 
        val args' = (GraphName.pretty_name gname)::
                    (map V.pretty_name (V.NSet.list_of (G.get_vnames graph)))
        val (n,state') = attach_with' apply_first_in (map Pretty.string_of args', state)
      in if n = 0 then raise user_exp "No more rewrites."
         else apply_rewrite (args @ ["0"],state')
      end
    | apply_first_rewrite _ = raise wrong_args_exp ()
  
  end
  
  fun show_rewrites (args,state) = let
      val gname = graph_arg args
      val rewrites = case GraphName.NTab.lookup (get_rewrites state) gname
              of NONE => raise user_exp "No rewrites attached."
               | SOME rs => rs
  in REWRITE_LIST rewrites  end;
  
  (* FIXME: make generic, currently depends on RGVertex *)
  fun vdata a (RGVertex.IVert (RGVertex.Xnd _)) = RGVertex.IVert (RGVertex.Xnd (AngleExpr.parse a))
    | vdata a (RGVertex.IVert (RGVertex.Znd _)) = RGVertex.IVert (RGVertex.Znd (AngleExpr.parse a))
    | vdata a _ = raise user_exp "Can only set angles for red and green vertices."
  
  fun set_angle ([_,v,a], graph) =
      ((OK, graph |> G.update_vertex_data (vdata a) (V.mk v))
       handle AngleExpr.parse_exp => raise user_exp "Cannot parse angle expression.")
    | set_angle _ = raise wrong_args_exp ()
  
  (* FIXME: make generic => i.e. depend only on RG Hilb term *)
  fun hilb ([_, format], graph) =
      (DATA (Pretty.string_of (
       RGHilbTerm.pretty (AlgFormat.of_string format)
                (RGHilbTerm.of_graph graph)))
      handle AlgFormat.not_supported_exp =>
         raise user_exp "Term format not supported.")
    | hilb _ = raise wrong_args_exp ()
  
  fun add_bang ([_], graph) = 
      graph |> G.new_box (G.BBox.default_name)
            >> (fn a => NAME (G.BBox.dest a))
    | add_bang _ = raise wrong_args_exp ()
  
  fun drop_bang ((_::names), graph) =
    let val namestr = implode ((separate " " names) @ ["."]);
    in
      (OK,
        graph |> fold G.drop_box (map G.BBox.mk names))
    end  
    | drop_bang _ = raise wrong_args_exp ()
  
  fun kill_bang ((_::names), graph) =
    let val namestr = implode ((separate " " names) @ ["."]);
    in
      (OK,
       graph |> fold G.kill_box (map G.BBox.mk names))
    end
    | kill_bang _ = raise wrong_args_exp ()
    
  fun merge_bang ((_::names), graph) =
    let val namestr = implode ((separate " " names) @ ["."]);
      val nameset = G.BBox.NSet.of_list (map G.BBox.mk names)
    in  
      (OK,
       graph |> G.merge_boxes nameset |> snd )
    end 
    | merge_bang _ = raise wrong_args_exp ()
    
  fun duplicate_bang ([_,name], graph) =
      (OK,
       G.fresh_copy1_box (G.BBox.mk name) graph)
    | duplicate_bang _ = raise wrong_args_exp ()
    
  fun bang_vertices ((_::box::verts), graph) = 
      (OK,
       graph |> G.move_to_box (G.BBox.mk box) (G.Vertex.NSet.of_list (map V.mk verts)))
    | bang_vertices _ = raise wrong_args_exp ()
  
  fun unbang_vertices ((_::verts), graph) =
      (OK,
       graph |> G.move_to_unbox (G.Vertex.NSet.of_list (map V.mk verts)))
    | unbang_vertices _ = raise wrong_args_exp ()
  
  fun copy_subgraph (args as (_::target::vs), state) = let
      val graph = get_graph state (graph_arg args) 
      val clip = G.delete_subgraph_outside_vset 
                   (G.Vertex.NSet.of_list (map V.mk vs))
                   graph
  in (OK, state |> set_graph clip (GraphName.mk target))
  end
    | copy_subgraph _ = raise wrong_args_exp ()
  
  fun insert_graph ([name1,name2], state) = 
      let
        val tname = (GraphName.mk name1)
        val sname = (GraphName.mk name2)
        val targ = get_graph state tname 
        val source = get_graph state sname 
        val (_, source) = G.rename_apart targ (get_graph state sname)
      in (OK, state |> push_hist tname
                            |> set_graph (G.merge_by_vertices targ source) tname)
      end
    | insert_graph _ = raise wrong_args_exp ()
  
  (* FIXME: make generic; currently depends on RGVertex *)
  fun flip_vertices (_::vs, graph) = let
      fun flip_data (RGVertex.IVert (RGVertex.Xnd e)) = RGVertex.IVert (RGVertex.Znd e)
        | flip_data (RGVertex.IVert (RGVertex.Znd e)) = RGVertex.IVert (RGVertex.Xnd e)
        | flip_data d = d
  in (OK, fold (G.update_vertex_data flip_data) (map V.mk vs) graph)
  end
    | flip_vertices _ = raise wrong_args_exp ()
    
  
  fun graph_of_rule getter label ([rulename],state) =
      let
        val rset = get_rset state
        val r = case RuleName.NTab.lookup (Ruleset.get_allrules rset) (RuleName.mk rulename)
                     of SOME v => v | NONE => raise user_exp "Can't find rule."
        val (name, graphs) = GraphName.NTab.add (GraphName.mk (rulename^"_"^label), getter r) (get_graphs state)
      in
        ((as_name o Pretty.string_of o GraphName.pretty_name) name,
         state |> set_graphs graphs)
      end
    | graph_of_rule _ _ _ = raise wrong_args_exp ()
  
  val open_rule_lhs = graph_of_rule R.get_lhs "lhs"
  val open_rule_rhs = graph_of_rule R.get_rhs "rhs"
  
end; (* structure Controller *)

(* *)
structure DB_ControllerState = ControllerState(
  structure RulesetRewrites = RGRulesetRewrites
  structure IO_Interface = RG_IO_Interface
);
structure ControllerState : CONTROLLER_STATE = DB_ControllerState; 

(* vim:et:sts=2:sw=2
*)
