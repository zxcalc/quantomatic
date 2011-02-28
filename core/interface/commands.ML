(* Commands available from the console and associate help stuff *)
signature COMMANDS = 
sig
  type command = string * (* help info *)
                    (string list * ControllerState.T (* arguments and state given to command *) 
                     -> string * ControllerState.T (* result string and new state *)
                    )

  type T

  val empty : T;
  val all_commands : T;

  val add_command : command -> T -> T;
  val add_simple_command : command -> T -> T;

  (* lift various functions *)
  val lift_stateless : (string list -> string) -> command;
  val lift_state_viewer : (string list * ControllerState.T -> string) -> command;
  val lift_graph_viewer : (string list * RGGraph.T -> string) -> command;
  val lift_graph_mutator : (string list * RGGraph.T -> string * RGGraph.T) -> command;
end;

(* *)
structure Commands 
= struct 
  type command = string * (* help info *)
                    (string list * ControllerState.T (* arguments and state given to command *) 
                    -> string * ControllerState.T (* result string and new state *)
                    );

  type T = command Symtab.table;

  (* State-agnostic commands, cmd: [args] -> out *)
  fun lift_stateless cmdf (args, state:ControllerState.T) = 
      (cmdf args, state)
  
  (* Read-only state commands, cmd: [args] * state -> out *)
  fun lift_state_viewer cmdf (args, state:ControllerState.T) = 
      (cmdf (args,state), state)
  
  (* Graph updating commands, cmd: ([args] * graph) -> (out * graph) *)
  fun lift_graph_mutator cmdf (args, state) =
      let val gname = ControllerState.graph_arg args
          val (out,g') = cmdf (args,ControllerState.get_graph state gname)
      in (out, state |> ControllerState.push_hist gname 
                     |> ControllerState.set_graph g' gname) end;
  
  (* Read-only graph commands, cmd: [args] * graph -> out *)
  fun lift_graph_viewer cmdf (args, state) =
      let val gname = ControllerState.graph_arg args
          val out = cmdf (args, ControllerState.get_graph state gname)
      in (out, state) end;
  
  (* *)
  val add_command = Symtab.update; 

  val empty = Symtab.empty;

local open ControllerState in
  val all_commands = 
  fold add_command [
    
    ("HELO",
     ("HELO\n\n"^
        "    sync the console, print \"HELO\"\n",
     lift_stateless (K "HELO\n"))),

    ("load_rules",
     ("load_rules FILE\n\n" ^
        "    load the rules contained in FILE\n",
    load_ruleset)),

    ("save_rules",
     ("save_rules FILENAME\n\n" ^
        "    save all loaded rules to FILENAME\n",
    lift_state_viewer save_ruleset)),

    ("list_graphs",
     ("list_graphs\n\n"^
        "    list all active graphs\n",
    lift_state_viewer list_graphs)),

    ("ls",
     ("ls\n\n"^
        "    alias for \"list_graphs\"\n",
    lift_state_viewer list_graphs)),

    ("list_rules",
     ("list_rules\n\n"^
        "    list all loaded rules\n",
    lift_state_viewer list_allrules)),

    ("list_tags",
     ("list_tags\n\n"^
        "    list all known tags for rules\n",
    lift_state_viewer list_tags)),

    ("list_rules_with_tag",
     ("list_rules_with_tag TAG\n\n"^
        "    list all loaded rules that are tagged with TAG\n",
    lift_state_viewer list_rules_with_tag)),

    ("list_active_rules",
     ("list_active_rules\n\n"^
        "    list all active rulesets\n",
    lift_state_viewer list_active_rules)),

    ("activate_rules_with_tag",
     ("activate_rules_with_tag TAG\n\n"^
        "    allow the rules tagged with TAG to be used for rewriting\n",
    activate_rules_with_tag)),

    ("deactivate_rules_with_tag",
     ("deactivate_rules_with_tag TAG\n\n"^
        "    prevent the rules tagged with TAG from being used for rewriting\n",
    deactivate_rules_with_tag)),

    ("delete_rules_with_tag",
     ("delete_rules_with_tag TAG\n\n"^
        "    delete all rules tagged with TAG\n",
    delete_tag_and_its_rules)),

    ("delete_tag",
     ("delete_tag TAG\n\n"^
        "    remove TAG from all rules and forget about TAG entirely\n",
    delete_tag)),

    ("activate_rule",
     ("activate_rule RULE\n\n"^
        "    allow RULE to be used for rewriting\n",
    activate_rule)),

    ("deactivate_rule",
     ("deactivate_rule RULE\n\n"^
        "    prevent RULE from being used for rewriting\n",
    deactivate_rule)),

    ("delete_rule",
     ("delete_rule RULE\n\n"^
        "    forget about RULE entirely\n",
    delete_rule)),

    ("tag_rule",
     ("tag_rule RULE TAG\n\n"^
        "    tag RULE with TAG\n",
    tag_rule)),

    ("untag_rule",
     ("untag_rule RULE TAG\n\n"^
        "    remove TAG from RULE\n",
    untag_rule)),

    ("new_graph",
     ("new_graph\n\n" ^
        "    create a new graph and return its name\n",
    new_graph)),

    ("load_graph",
     ("load_graph FILENAME\n\n" ^
        "    load a new graph from FILENAME return its name\n",
    load_graph)),
(* 
    ("input_graph_xml",
     ("input_graph_xml\n\n" ^
        "    create a new graph from the XML data given as block input\n",
    input_graph_xml)),
   *) 
    ("save_graph",
     ("save_graph GRAPH FILENAME\n\n" ^
        "    save GRAPH in FILENAME\n",
    lift_graph_viewer save_graph)),
 
    ("duplicate_graph",
     ("duplicate_graph GRAPH\n\n"^
        "    duplicate GRAPH and return the name of the new graph\n",
    duplicate_graph)),

    ("rename_graph",
     ("rename_graph OLD NEW\n\n"^
        "    rename graph OLD to NEW, return the real new\n"^
        "    name.\n",
    rename_graph)),

    ("kill_graph",
     ("kill_graph GRAPH\n\n"^
        "    remove GRAPH from the workspace\n",
    kill_graph)),

    ("add_vertex",
     ("add_vertex GRAPH TYPE\n\n"^
        "    add a TYPE vertex to GRAPH\n",
    lift_graph_mutator add_vertex)),
    
    ("delete_vertices",
     ("delete_vertices GRAPH V1 .. VN\n\n"^
        "    delete vertices V1..VN from GRAPH\n",
    lift_graph_mutator delete_vertices)),

    ("delete_edges",
     ("delete_edges GRAPH E1 .. EN\n\n"^
        "    delete edges E1 .. EN from GRAPH\n",
    lift_graph_mutator delete_edges)),

    ("add_edge",
     ("add_edge GRAPH V1 V2\n\n"^
        "    add an edge in GRAPH from V1 to V2\n",
    lift_graph_mutator add_edge)),
  
    ("rename_vertex",
     ("rename_vertex GRAPH OLD NEW\n\n"^
        "    rename vertex in GRAPH from OLD to NEW\n",
    lift_graph_mutator rename_vertex)),

    ("graph_xml",
     ("graph_xml GRAPH\n\n"^
        "    dump GRAPH as XML\n",
    lift_graph_viewer (uncurry graph_xml))),
    
    ("print_graph",
     ("print_graph GRAPH\n\n"^
        "    print out GRAPH\n",
    lift_graph_viewer (uncurry print_graph))),

    ("undo",
     ("undo GRAPH\n\n"^
        "    undo last change to GRAPH\n",
    undo)),
    
    ("redo",
     ("redo GRAPH\n\n"^
        "    redo change on GRAPH\n",
    redo)),
    
    ("attach_rewrites",
     ("attach_rewrites GRAPH V1 .. VN\n\n"^
        "    calculate rewrites involving V1..VN in GRAPH and\n"^
        "    attach to the graph state\n",
    attach_rewrites)),

    ("attach_one_rewrite",
     ("attach_one_rewrite GRAPH V1 .. VN\n\n"^
        "    calculate at most one rewrite involving V1..VN in"^
        "    GRAPH and attach to the graph state\n",
    attach_first_rewrite)),

    ("show_rewrites",
     ("show_rewrites GRAPH\n\n"^
        "    show rewrites attached to GRAPH\n",
    lift_state_viewer show_rewrites)),

    ("apply_rewrite",
     ("apply_rewrite GRAPH N\n\n"^
        "    apply the Nth rewrite attached to GRAPH\n",
    apply_rewrite)),

    ("apply_first_rewrite",
     ("apply_first_rewrite GRAPH N\n\n"^
        "    attempt to attach one rewrite to GRAPH and apply it\n",
    apply_first_rewrite)),

    ("hilb",
     ("hilb GRAPH FORMAT\n\n"^
        "    display the hilbert space term of GRAPH where\n"^
        "    FORMAT is one of {text, mathematica}.\n",
    lift_graph_viewer hilb)),
    
    ("set_angle",
     ("set_angle GRAPH V ANGLE\n\n"^
        "    set the angle of vertex V to ANGLE\n",
    lift_graph_mutator set_angle)),

    ("add_bang",
     ("add_bang GRAPH\n\n"^
        "    add a new !-box to GRAPH and return its name\n",
    lift_graph_mutator add_bang)),

    ("bbox_drop",
     ("bbox_drop GRAPH BANGS\n\n"^
        "    drop !-boxes BANGS in GRAPH. Vertices inside BANGS are moved\n"^
        "    to the top-level graph.\n",
    lift_graph_mutator drop_bang)),

    ("bbox_kill",
     ("bbox_kill GRAPH BANGS\n\n"^
        "    kill !-boxes BANGS in GRAPH. Vertices inside BANGS are deleted\n"^
        "    to the top-level graph.\n",
    lift_graph_mutator kill_bang)),
    
    ("bbox_merge",
     ("bbox_kill GRAPH BANGS\n\n"^
        "    merges !-boxes BANGS in GRAPH.\n",
    lift_graph_mutator merge_bang)),
    
    ("bbox_duplicate",
     ("bbox_duplicate GRAPH BANGS\n\n"^
        "    duplicates !-boxes BANGS in GRAPH.\n",
    lift_graph_mutator duplicate_bang)),
    
    ("bang_vertices",
     ("bang_vertices GRAPH BANG V1 .. VN\n\n"^
        "    add vertices V1..VN in graph to !-box BANG\n",
    lift_graph_mutator bang_vertices)),

    ("unbang_vertices",
     ("unbang_vertices GRAPH V1 .. VN\n\n"^
        "    remove vertices V1..VN from their current !-box\n",
    lift_graph_mutator unbang_vertices)),

    ("copy_subgraph",
     ("copy_subgraph SOURCE TARGET V1 .. VN\n\n"^
        "    copy the subgraph of SOURCE spanned by V1..VN to\n"^
        "    the graph TARGET\n",
    copy_subgraph)),

    ("insert_graph",
     ("insert_graph TARGET SOURCE\n\n"^
        "    insert of copy of SOURCE into TARGET\n",
    insert_graph)),

    ("flip_vertices",
     ("flip_vertices G V1 .. VN\n\n"^
        "    reverse the colour of vertices V1..VN, ingoring\n"^
        "    boundary vertices.\n",
    lift_graph_mutator flip_vertices)),

    ("open_rule_lhs",
     ("open_rule_lhs RULE\n\n"^
        "    Open the LHS of the given rule and return the name of\n"^
        "    the new graph buffer.\n",
    open_rule_lhs)),

    ("open_rule_rhs",
     ("open_rule_rhs RULE\n\n"^
        "    Open the RHS of the given rule and return the name of\n"^
        "    the new graph buffer.\n",
    open_rule_rhs)),

    ("update_rule",
     ("update_rule RULE LHS RHS\n\n"^
        "    Replace the given rule with graphs LHS and RHS in the\n"^
        "    workspace.\n",
    update_rule)),

    ("new_rule",
     ("new_rule GRAPH\n\n"^
        "    Create a new identity rule from GRAPH.\n",
    new_rule))
    ]
    empty;
end; (* local *)
 
end;
