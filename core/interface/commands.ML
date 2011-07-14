(* Commands available from the console and associate help stuff *)
signature COMMANDS = 
sig

  datatype response = OK
                    | COUNT of int
                    | NAME of string
                    | NAME_LIST of (string list)
                    | REWRITE_LIST of (ControllerState.rewrite list)
                    | DATA of string;
  type command = string * (* help info *)
                    (string list * ControllerState.T (* arguments and state given to command *) 
                     -> response * ControllerState.T (* result and new state *)
                    )

  type T

  exception wrong_args_exp of unit (* when wrong arguments are given to a command *)

  val empty : T;
  val all_commands : T;

  val add_command : command -> T -> T;
  val add_simple_command : command -> T -> T;
end;

(* *)
structure Commands 
= struct 
  datatype response = OK
                    | COUNT of int
                    | NAME of string
                    | NAME_LIST of (string list)
                    | REWRITE_LIST of (ControllerState.rewrite list)
                    | DATA of string;

  type command = string * (* help info *)
                    (string list * ControllerState.T (* arguments and state given to command *) 
                    -> response * ControllerState.T (* result and new state *)
                    );

  type T = command Symtab.table;
  structure R = ControllerState.Theory.Rule;
  structure G = ControllerState.Theory.Graph;
  structure V = ControllerState.Theory.Vertex;
  structure E = ControllerState.Theory.Edge;

  exception wrong_args_exp of unit

  fun as_name str = NAME str
  fun as_name_list l = NAME_LIST l
  fun as_data str = DATA str

  (* State-agnostic commands, cmd: [args] -> out *)
  fun lift_stateless cmdf (args, state:ControllerState.T) = 
      (cmdf args, state)

  (* Commands with no output, cmd: [args] * state -> state *)
  fun lift_procedure cmdf (args, state:ControllerState.T) = 
      (OK, cmdf (args,state))

  (* Read-only state commands with no output, cmd: [args] * state -> unit *)
  (* eg: saving *)
  fun lift_stateless_procedure cmdf (args, state:ControllerState.T) = 
      (cmdf (args,state); (OK, state))

  (* Read-only state commands, cmd: [args] * state -> out *)
  fun lift_state_viewer cmdf (args, state:ControllerState.T) = 
      (cmdf (args,state), state)

  (* Graph updating commands, cmd: ([args] * graph) -> (out * graph) *)
  fun lift_graph_mutator cmdf (gname::args, state) =
      let val (out,g') = cmdf (args,ControllerState.get_graph state gname)
      in (out, state |> ControllerState.push_hist gname 
                     |> ControllerState.set_graph g' gname) end;

  (* Read-only graph commands, cmd: graph -> out *)
  fun lift_graph_viewer cmdf (name::args, state) =
      let val gname = GraphName.mk name
          val out = cmdf (args, ControllerState.get_graph state gname)
      in (out, state) end;

  fun lift_no_args f ([], state) = f state
    | lift_no_args _ (_, state) = raise wrong_args_exp ()
  fun lift_string_arg f ([arg:string], state) = f arg state
    | lift_string_arg _ (_, state) = raise wrong_args_exp ()
  fun lift_rulename_arg f ([arg:string], state) = f (RuleName.mk arg) state
    | lift_rulename_arg _ (_, state) = raise wrong_args_exp ()
  fun lift_tagname_arg f ([arg:string], state) = f (TagName.mk arg) state
    | lift_tagname_arg _ (_, state) = raise wrong_args_exp ()
  fun lift_graphname_arg f ([arg:string], state) = f (GraphName.mk arg) state
    | lift_graphname_arg _ (_, state) = raise wrong_args_exp ()

  fun name_list_no_args namemapper f = 
      (lift_state_viewer o lift_no_args) (as_name_list o (map namemapper) o f)

  (* *)
  val add_command = Symtab.update; 

  val empty = Symtab.empty;

  fun apply_to_first f (a,b) = (f a, b)
  val first_as_graph_name = apply_to_first (as_name o GraphName.dest)

  structure Output = RG_IO_Interface.Output;
  val vertex_xml = as_data o XML.string_of o Output.Vertex.output
  val edge_xml = as_data o XML.string_of o Output.Edge.output
  val bbox_xml = as_data o XML.string_of o Output.BBox.output

  fun set_rule ([rule,lhs,rhs], state) =
      (OK, ControllerState.set_rule (RuleName.mk rule) (GraphName.mk lhs) (GraphName.mk rhs) state)
    | set_rule (_, state) = raise wrong_args_exp ();
  fun open_rule_lhs ([rule], state) =
      first_as_graph_name (ControllerState.open_rule_lhs state (RuleName.mk rule))
    | open_rule_lhs (_, state) = raise wrong_args_exp ();
  fun open_rule_rhs ([rule], state) =
      first_as_graph_name (ControllerState.open_rule_rhs state (RuleName.mk rule))
    | open_rule_rhs (_, state) = raise wrong_args_exp ();
  fun tag_op f ([rule,tag], state) = 
      let val rule_name = RuleName.mk rule
          val tag_name = TagName.mk tag
      in (OK, f rule_name tag_name state) end
    | tag_op _ (_, state) = raise wrong_args_exp ();
  fun save_graph ([gname,fname], state) =
      (ControllerState.save_graph (GraphName.mk gname) fname state; (OK,state))
    | save_graph (_, state) = raise wrong_args_exp ();
  fun rename_graph ([old,new], state) =
      first_as_graph_name (ControllerState.rename_graph (GraphName.mk old) (GraphName.mk new) state)
    | rename_graph (_, state) = raise wrong_args_exp ();
  fun hilb ([format], graph) =
      (DATA (Pretty.string_of (
       RGHilbTerm.pretty (AlgFormat.of_string format)
                (RGHilbTerm.of_graph graph)))
      handle AlgFormat.not_supported_exp =>
         raise ControllerState.user_exp "Term format not supported.")
    | hilb (_, graph) = raise wrong_args_exp ()
  fun add_vertex ([gname,vt], state) =
      apply_to_first vertex_xml (ControllerState.add_vertex vt (GraphName.mk gname) state)
    | add_vertex (_, state) = raise wrong_args_exp ()
  fun rename_vertex ([gname,old,new], state) =
      (OK, (ControllerState.rename_vertex (V.mk old) (V.mk new) (GraphName.mk gname) state))
    | rename_vertex (_, state) = raise wrong_args_exp ()
  fun set_angle ([gname,v,angle], state) =
      (OK, (ControllerState.set_angle (V.mk v) angle (GraphName.mk gname) state))
    | set_angle (_, state) = raise wrong_args_exp ()
  fun delete_vertices (gname::vs, state) =
      (OK, (ControllerState.delete_vertices (map V.mk vs) (GraphName.mk gname) state))
    | delete_vertices (_, state) = raise wrong_args_exp ()
  fun add_edge ([gname,v1,v2], state) =
      apply_to_first edge_xml (ControllerState.add_edge (V.mk v1) (V.mk v2) (GraphName.mk gname) state)
    | add_edge (_, state) = raise wrong_args_exp ()
  fun delete_edges (gname::es, state) =
      (OK, (ControllerState.delete_edges (map E.mk es) (GraphName.mk gname) state))
    | delete_edges (_, state) = raise wrong_args_exp ()
  fun bb_list_op f (gname::names, state) =
      (*let val names_tr = implode ((separate " " names) @ ["."]);
      in*)
        f (map G.BBox.mk names) (GraphName.mk gname) state
      (*end*)
    | bb_list_op _ (_, state) = raise wrong_args_exp ()
  fun duplicate_bang ([gname,bname], state) =
      apply_to_first (as_name o G.BBox.dest) (ControllerState.duplicate_bang (G.BBox.mk bname) (GraphName.mk gname) state)
    | duplicate_bang (_, state) = raise wrong_args_exp ()
  fun bang_vertices (gname::bname::vnames, state) =
        ControllerState.bang_vertices (G.BBox.mk bname) (map V.mk vnames) (GraphName.mk gname) state
    | bang_vertices (_, state) = raise wrong_args_exp ()
  fun unbang_vertices (gname::vnames, state) =
        ControllerState.unbang_vertices (map V.mk vnames) (GraphName.mk gname) state
    | unbang_vertices (_, state) = raise wrong_args_exp ()
  fun copy_subgraph (sname::tname::vs, state) =
        ControllerState.copy_subgraph (GraphName.mk sname) (GraphName.mk tname) (map V.mk vs) state
    | copy_subgraph (_, state) = raise wrong_args_exp ()
  fun insert_graph ([sname,tname], state) =
        ControllerState.insert_graph (GraphName.mk sname) (GraphName.mk tname) state
    | insert_graph (_, state) = raise wrong_args_exp ()
  fun attach_op f (gname::vnames, state) =
        let val (n,state) = f (GraphName.mk gname) (map V.mk vnames) state in
          (COUNT n, state)
        end
    | attach_op _ (_, state) = raise wrong_args_exp ()
  fun apply_rewrite ([gname,n], state) =
      ControllerState.apply_rewrite (GraphName.mk gname) (the (Int.fromString n)) state
    | apply_rewrite (_, state) = raise wrong_args_exp ()

  val all_commands = 
  fold add_command [

    ("HELO",
     ("HELO\n\n"^
        "    sync the console, print \"HELO\"\n",
      lift_stateless (K (DATA "HELO")))),

    ("load_ruleset",
     ("load_ruleset FILE\n\n" ^
        "    load the rules contained in FILE\n",
      (lift_procedure o lift_string_arg) ControllerState.load_ruleset)),

    ("save_ruleset",
     ("save_ruleset FILENAME\n\n" ^
        "    save all loaded rules to FILENAME\n",
      (lift_stateless_procedure o lift_string_arg) ControllerState.save_ruleset)),

    ("set_rule",
     ("set_rule RULE LHS RHS\n\n"^
        "    Create or update a rule from the graphs LHS and RHS (which\n"^
        "    may be the same).  If a rule with the same name exists,\n"^
        "    it will be replaced.\n",
      set_rule)),

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

    ("list_graphs",
     ("list_graphs\n\n"^
        "    list all active graphs\n",
      name_list_no_args GraphName.dest ControllerState.list_graphs)),

    ("ls",
     ("ls\n\n"^
        "    alias for \"list_graphs\"\n",
      name_list_no_args GraphName.dest ControllerState.list_graphs)),

    ("list_rules",
     ("list_rules\n\n"^
        "    list all loaded rules\n",
      name_list_no_args RuleName.dest ControllerState.list_allrules)),

    ("list_tags",
     ("list_tags\n\n"^
        "    list all known tags for rules\n",
      name_list_no_args TagName.dest ControllerState.list_tags)),

    ("list_rules_with_tag",
     ("list_rules_with_tag TAG\n\n"^
        "    list all loaded rules that are tagged with TAG\n",
      (apply_to_first (as_name_list o (map TagName.dest)))
         o ((lift_state_viewer o lift_tagname_arg) ControllerState.list_rules_with_tag)
      )),

    ("list_active_rules",
     ("list_active_rules\n\n"^
        "    list all active rulesets\n",
      name_list_no_args RuleName.dest ControllerState.list_active_rules)),

    ("activate_rules_with_tag",
     ("activate_rules_with_tag TAG\n\n"^
        "    allow the rules tagged with TAG to be used for rewriting\n",
      (lift_procedure o lift_tagname_arg) ControllerState.activate_rules_with_tag)),

    ("deactivate_rules_with_tag",
     ("deactivate_rules_with_tag TAG\n\n"^
        "    prevent the rules tagged with TAG from being used for rewriting\n",
      (lift_procedure o lift_tagname_arg) ControllerState.deactivate_rules_with_tag)),

    ("delete_rules_with_tag",
     ("delete_rules_with_tag TAG\n\n"^
        "    delete all rules tagged with TAG\n",
      (lift_procedure o lift_tagname_arg) ControllerState.delete_tag_and_its_rules)),

    ("delete_tag",
     ("delete_tag TAG\n\n"^
        "    remove TAG from all rules and forget about TAG entirely\n",
      (lift_procedure o lift_tagname_arg) ControllerState.delete_tag)),

    ("activate_rule",
     ("activate_rule RULE\n\n"^
        "    allow RULE to be used for rewriting\n",
      (lift_procedure o lift_rulename_arg) ControllerState.activate_rule)),

    ("deactivate_rule",
     ("deactivate_rule RULE\n\n"^
        "    prevent RULE from being used for rewriting\n",
      (lift_procedure o lift_rulename_arg) ControllerState.deactivate_rule)),

    ("delete_rule",
     ("delete_rule RULE\n\n"^
        "    forget about RULE entirely\n",
      (lift_procedure o lift_rulename_arg) ControllerState.delete_rule)),

    ("tag_rule",
     ("tag_rule RULE TAG\n\n"^
        "    tag RULE with TAG\n",
      tag_op ControllerState.tag_rule)),

    ("untag_rule",
     ("untag_rule RULE TAG\n\n"^
        "    remove TAG from RULE\n",
      tag_op ControllerState.untag_rule)),

    ("new_graph",
     ("new_graph\n\n" ^
        "    create a new graph and return its name\n",
      (apply_to_first (as_name o GraphName.dest)) o (lift_no_args ControllerState.new_graph)
     )),

    ("load_graph",
     ("load_graph FILENAME\n\n" ^
        "    load a new graph from FILENAME return its name\n",
      first_as_graph_name o (lift_string_arg ControllerState.load_graph))),

    ("save_graph",
     ("save_graph GRAPH FILENAME\n\n" ^
        "    save GRAPH in FILENAME\n",
      save_graph)),

    ("duplicate_graph",
     ("duplicate_graph GRAPH\n\n"^
        "    duplicate GRAPH and return the name of the new graph\n",
      (apply_to_first (as_name o GraphName.dest)) o (lift_graphname_arg ControllerState.duplicate_graph)
     )),

    ("rename_graph",
     ("rename_graph OLD NEW\n\n"^
        "    rename graph OLD to NEW, return the real new\n"^
        "    name.\n",
      rename_graph)),

    ("kill_graph",
     ("kill_graph GRAPH\n\n"^
        "    remove GRAPH from the workspace\n",
      (lift_procedure o lift_graphname_arg) ControllerState.kill_graph)),

    ("graph_xml",
     ("graph_xml GRAPH\n\n"^
        "    dump GRAPH as XML\n",
      (lift_graph_viewer o lift_no_args) (as_data o XML.string_of o Output.Graph.output)
     )),

    ("print_graph",
     ("print_graph GRAPH\n\n"^
        "    print out GRAPH\n",
      (lift_graph_viewer o lift_no_args) (as_data o Pretty.string_of o G.pretty)
     )),

    ("print_theory_name",
     ("print_theory_name GRAPH\n\n"^
       "     print out the name of the theory implemented by graph\n",
      (lift_graph_viewer o lift_no_args) (as_data o Pretty.string_of o G.pretty_theory_name)
     )),


    ("hilb",
     ("hilb GRAPH FORMAT\n\n"^
        "    display the hilbert space term of GRAPH where\n"^
        "    FORMAT is one of {plain, latex, mathematica, matlab}.\n",
      lift_graph_viewer hilb)),

    ("undo",
     ("undo GRAPH\n\n"^
        "    undo last change to GRAPH\n",
      (lift_procedure o lift_graphname_arg) ControllerState.undo)),

    ("redo",
     ("redo GRAPH\n\n"^
        "    redo change on GRAPH\n",
      (lift_procedure o lift_graphname_arg) ControllerState.redo)),

    ("add_vertex",
     ("add_vertex GRAPH TYPE\n\n"^
        "    add a TYPE vertex to GRAPH\n",
      add_vertex)),

    ("rename_vertex",
     ("rename_vertex GRAPH OLD NEW\n\n"^
        "    rename vertex in GRAPH from OLD to NEW\n",
      rename_vertex)),

    ("set_angle",
     ("set_angle GRAPH V ANGLE\n\n"^
        "    set the angle of vertex V to ANGLE\n",
      set_angle)),

    ("delete_vertices",
     ("delete_vertices GRAPH V1 .. VN\n\n"^
        "    delete vertices V1..VN from GRAPH\n",
      delete_vertices)),

    ("add_edge",
     ("add_edge GRAPH V1 V2\n\n"^
        "    add an edge in GRAPH from V1 to V2\n",
      add_edge)),

    ("delete_edges",
     ("delete_edges GRAPH E1 .. EN\n\n"^
        "    delete edges E1 .. EN from GRAPH\n",
      delete_edges)),

    ("add_bang",
     ("add_bang GRAPH\n\n"^
        "    add a new !-box to GRAPH and return its name\n",
      (apply_to_first (as_name o G.BBox.dest)) o (lift_graphname_arg ControllerState.add_bang)
     )),

    ("bbox_drop",
     ("bbox_drop GRAPH B1 .. BN\n\n"^
        "    drop !-boxes B1 .. BN in GRAPH. Vertices inside the !-boxes are moved\n"^
        "    to the top-level graph.\n",
      lift_procedure (bb_list_op ControllerState.drop_bang))),

    ("bbox_kill",
     ("bbox_kill GRAPH B1 .. BN\n\n"^
        "    kill !-boxes B1 .. BN in GRAPH. Vertices inside BANGS are deleted\n"^
        "    to the top-level graph.\n",
      lift_procedure (bb_list_op ControllerState.kill_bang))),

    ("bbox_merge",
     ("bbox_kill GRAPH B1 .. BN\n\n"^
        "    merges !-boxes B1 .. BN in GRAPH.\n",
      (apply_to_first (as_name o G.BBox.dest)) o (bb_list_op ControllerState.merge_bang))),

    ("bbox_duplicate",
     ("bbox_duplicate GRAPH BB\n\n"^
        "    duplicates the !-box BB in GRAPH.\n",
      duplicate_bang)),

    ("bang_vertices",
     ("bang_vertices GRAPH BANG V1 .. VN\n\n"^
        "    add vertices V1..VN in graph to !-box BANG\n",
      lift_procedure bang_vertices)),

    ("unbang_vertices",
     ("unbang_vertices GRAPH V1 .. VN\n\n"^
        "    remove vertices V1..VN from their current !-box\n",
      lift_procedure unbang_vertices)),

    ("copy_subgraph",
     ("copy_subgraph SOURCE TARGET V1 .. VN\n\n"^
        "    copy the subgraph of SOURCE spanned by V1..VN to\n"^
        "    the graph TARGET\n",
      lift_procedure copy_subgraph)),

    ("insert_graph",
     ("insert_graph SOURCE TARGET\n\n"^
        "    insert of copy of SOURCE into TARGET\n",
      lift_procedure insert_graph)),

    ("attach_rewrites",
     ("attach_rewrites GRAPH V1 .. VN\n\n"^
        "    calculate rewrites involving V1..VN in GRAPH and\n"^
        "    attach to the graph state\n",
      attach_op ControllerState.attach_rewrites)),

    ("attach_one_rewrite",
     ("attach_one_rewrite GRAPH V1 .. VN\n\n"^
        "    calculate at most one rewrite involving V1..VN in"^
        "    GRAPH and attach to the graph state\n",
      attach_op ControllerState.attach_first_rewrite)),

    ("show_rewrites",
     ("show_rewrites GRAPH\n\n"^
        "    show rewrites attached to GRAPH\n",
      (apply_to_first (fn rs => REWRITE_LIST rs)) o ((lift_state_viewer o lift_graphname_arg) ControllerState.get_attached_rewrites)
     )),

    ("apply_rewrite",
     ("apply_rewrite GRAPH N\n\n"^
        "    apply the Nth rewrite attached to GRAPH\n",
      lift_procedure apply_rewrite)),

    ("apply_first_rewrite",
     ("apply_first_rewrite GRAPH\n\n"^
        "    attempt to attach one rewrite to GRAPH and apply it\n",
      (lift_procedure o lift_graphname_arg) ControllerState.apply_first_rewrite))
    ]
    empty;
end; (* local *)

(* vim:et:sts=2:sw=2
*)
