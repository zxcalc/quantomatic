(* Commands available from the console and associate help stuff *)
structure Commands 
= struct 
local open Controller in

  type command = (string, (* help info *)
                  string list * state (* arguments and state given to command *) 
                  -> string * state (* result string and new state *)
                  );

  val cmds_tab = 
  fold Symtab.update [
    ("help",
     ("help CMD\n\n"^
        "    get help on CMD\n",
    lift_stateless help)),
    
    ("HELO",
     ("HELO\n\n"^
        "    sync the console, print \"HELO\"\n",
     lift_stateless (K "HELO\n"))),

    ("echo_block",
     ("echo_block\n\n"^
        "    debug command for testing block input\n",
    lift_stateless echo_block)),

    ("load_ruleset",
     ("load_ruleset NAME FILE\n\n" ^
        "    load a ruleset from FILE and call it NAME\n",
    load_ruleset)),

    ("unload_ruleset",
     ("unload_ruleset RULESET\n\n" ^
        "    unload RULESET\n",
    unload_ruleset)),
    
    ("save_ruleset",
     ("save_ruleset RULESET FILENAME\n\n" ^
        "    save RULESET in FILENAME\n",
    lift_state_viewer save_ruleset)),

    ("list_graphs",
     ("list_graphs\n\n"^
        "    list all active graphs\n",
    lift_state_viewer list_graphs)),

    ("ls",
     ("ls\n\n"^
        "    alias for \"list_graphs\"\n",
    lift_state_viewer list_graphs)),

    ("list_rulesets",
     ("list_rulesets\n\n"^
        "    list all loaded rulesets\n",
    lift_state_viewer list_rulesets)),

    ("list_active_rulesets",
     ("list_active_rulesets\n\n"^
        "    list all active rulesets\n",
    lift_state_viewer list_active_rulesets)),

    ("list_rules",
     ("list_rules RULESET\n\n"^
        "    list the rules in RULESET\n",
    lift_state_viewer list_rules)),

    ("activate_ruleset",
     ("activate_ruleset RULESET\n\n"^
        "    enable the rules in RULESET for rewriting\n",
    activate_ruleset)),

    ("deactivate_ruleset",
     ("deactivate_ruleset RULESET\n\n"^
        "    disable the rules in RULESET for rewriting\n",
    deactivate_ruleset)),

    ("activate_rule",
     ("activate_rule RULESET RULE\n\n"^
        "    enable RULE in RULESET for rewriting\n",
    activate_rule)),

    ("deactivate_rule",
     ("deactivate_ruleset RULESET RULE\n\n"^
        "    disable RULE in RULESET for rewriting\n",
    deactivate_rule)),

    ("delete_rule",
     ("delete_ruleset RULESET RULE\n\n"^
        "    remove RULE from RULESET for rewriting\n",
    delete_rule)),

    ("new_graph",
     ("new_graph\n\n" ^
        "    create a new graph and return its name\n",
    new_graph)),

    ("load_graph",
     ("load_graph FILENAME\n\n" ^
        "    load a new graph from FILENAME return its name\n",
    load_graph)),

    ("input_graph_xml",
     ("input_graph_xml\n\n" ^
        "    create a new graph from the XML data given as block input\n",
    input_graph_xml)),
    
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
    attach_one_rewrite)),

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
     ("open_rule_lhs RULESET RULE\n\n"^
        "    Open the LHS of the given rule and return the name of\n"^
        "    the new graph buffer.\n",
    open_rule_lhs)),

    ("open_rule_rhs",
     ("open_rule_rhs RULESET RULE\n\n"^
        "    Open the RHS of the given rule and return the name of\n"^
        "    the new graph buffer.\n",
    open_rule_rhs)),

    ("replace_rule",
     ("replace_rule RULESET RULE LHS RHS\n\n"^
        "    Replace the given rule with graphs LHS and RHS in the\n"^
        "    workspace.\n",
    replace_rule)),

    ("new_rule",
     ("new_rule RULESET GRAPH\n\n"^
        "    Create a new identity rule from GRAPH.\n",
    new_rule))
    ]
    Symtab.empty;

end; (* local *) 
end;
