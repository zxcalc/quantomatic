(* Commands available from the console and associate help stuff *)
signature CONSOLE_COMMANDS = 
sig
  type T;
  type command = string list -> Commands.request;

  exception wrong_args_exp;

  val parse_response : Commands.response -> string;
  (* An ErrorResponse for use when a wrong_args_exp is raised *)
  val bad_args_response : Commands.response;

  val empty : T;
  val all_commands : T;

  val get_command_names : T -> string list;

  val get_command : T -> string -> command option;
  val get_args : T -> string -> string option;
  val get_help : T -> string -> string option;

  (* name, (args, help, command) *)
  val add_command : (string * (string * string * command)) -> T -> T;
end;

(* *)
structure ConsoleCommands : CONSOLE_COMMANDS
= struct 
  (* args, help, command *)
  type command = string list -> Commands.request;
  type annotated_command = string * string * command;

  exception wrong_args_exp;

  type T = annotated_command Symtab.table;

  exception unexpected_response_exp;

  fun args (SOME (args,_,_)) = SOME args
    | args  NONE             = NONE
  fun help (SOME (_,help,_)) = SOME help
    | help  NONE             = NONE
  fun cmd  (SOME (_,_,cmd))  = SOME cmd
    | cmd   NONE             = NONE

  val get_command_names = Symtab.keys;
  fun get_command cs    = cmd o (Symtab.lookup cs);
  fun get_args cs       = args o (Symtab.lookup cs);
  fun get_help cs       = help o (Symtab.lookup cs);

  val add_command = Symtab.update; 

  val empty = Symtab.empty;

  fun parse_response (Commands.OkResponse { requestId }) = "OK"
    | parse_response (Commands.ErrorResponse { requestId, errorCode, message }) = "!!! "^message
    | parse_response (Commands.DataResponse { requestId, data }) = data
    | parse_response (Commands.PrettyResponse { requestId, data }) = Pretty.string_of data
    | parse_response (Commands.NameResponse { requestId, name }) = name
    | parse_response (Commands.NameListResponse { requestId, names }) = (String.concatWith "\n" names)
    | parse_response (Commands.ComponentDataResponse { requestId, data as (ComponentData.Unit) }) = ""
    | parse_response (Commands.ComponentDataResponse { requestId, data as (ComponentData.String s) }) = s
    | parse_response (Commands.ComponentDataResponse { requestId, data as (ComponentData.LinratExpr a) }) = Pretty.string_of (LinratExpr.pretty a)
    | parse_response (Commands.UserDataResponse { requestId, data }) = data
    | parse_response _ = raise unexpected_response_exp;

  val bad_args_response = Commands.ErrorResponse { requestId = "", errorCode = "BADARGS", message = "Wrong number of arguments" }

  val all_commands = 
  fold add_command [

    ("list_graphs",
     ("", "List all loaded graphs",
      (fn [] => (Commands.ListGraphsRequest { requestId = "" })
        | _  => raise wrong_args_exp))),

    ("ls",
     ("", "List all loaded graphs (alias for list_graphs)",
      (fn [] => (Commands.ListGraphsRequest { requestId = "" })
        | _  => raise wrong_args_exp))),

    ("new_graph",
     ("", "Create a new graph and return its name\n",
      (fn [] => (Commands.LoadGraphRequest { requestId = "",
                        details = Commands.LoadEmptyGraphRequest })
        | _  => raise wrong_args_exp))),

    ("load_graph",
     ("FILENAME", "Load a new graph from FILENAME return its name",
      (fn [filename] => (Commands.LoadGraphRequest { requestId = "",
                        details = Commands.LoadGraphFromFileRequest { fileName = filename } })
        | _          => raise wrong_args_exp))),

    ("duplicate_graph",
     ("GRAPH", "Duplicate GRAPH and return the name of the new graph",
      (fn [graph] => (Commands.LoadGraphRequest { requestId = "",
                     details = Commands.CopyGraphRequest { graphName = graph } })
        | _       => raise wrong_args_exp))),

    ("copy_subgraph",
     ("SOURCE TARGET V1 .. VN", "Copy the subgraph of SOURCE spanned by V1..VN to the graph TARGET",
      (fn (src::tgt::vs) => (Commands.LoadGraphRequest { requestId = "",
                            details = Commands.CopySubgraphRequest { source = src, graphName = tgt, vertexNames = vs } })
        | _              => raise wrong_args_exp))),

    ("save_graph",
     ("GRAPH FILENAME", "Save GRAPH to FILENAME",
      (fn [graph,fnm] => (Commands.SaveGraphToFileRequest
                           { requestId = "", graphName = graph, fileName = fnm })
        | _           => raise wrong_args_exp))),

    ("rename_graph",
     ("OLD NEW", "Rename graph OLD to NEW, return the real new name.",
      (fn [old,new] => (Commands.RenameGraphRequest
                         { requestId = "", oldName = old, newName = new })
        | _         => raise wrong_args_exp))),

    ("kill_graph",
     ("GRAPH", "Remove GRAPH from the workspace",
      (fn [graph] => (Commands.DiscardGraphRequest
                       { requestId = "", graphName = graph })
        | _       => raise wrong_args_exp))),

    ("print_graph",
     ("GRAPH", "Print out GRAPH",
      (fn [graph] => (Commands.ExportGraphRequest
                       { requestId = "", graphName = graph, format = Commands.PrettyGraphFormat })
        | _       => raise wrong_args_exp))),

    ("hilb",
     ("GRAPH FORMAT", "Display the hilbert space term of GRAPH where FORMAT is one of {plain, latex, mathematica, matlab}.",
      (fn [graph,"plain"]       => (Commands.ExportGraphRequest
                       { requestId = "", graphName = graph, format = Commands.HilbertSpaceGraphFormat })
        | [graph,"mathematica"] => (Commands.ExportGraphRequest
                       { requestId = "", graphName = graph, format = Commands.MathematicaGraphFormat })
        | [graph,"matlab"]      => (Commands.ExportGraphRequest
                       { requestId = "", graphName = graph, format = Commands.MatlabGraphFormat })
        | [graph,"latex"]       => (Commands.ExportGraphRequest
                       { requestId = "", graphName = graph, format = Commands.TikzGraphFormat })
        | [_,fmt]               => raise wrong_args_exp (* FIXME: better exp *)
        | _                     => raise wrong_args_exp))),

    ("undo",
     ("GRAPH", "Undo the last change to GRAPH",
      (fn [graph] => (Commands.EditGraphRequest { requestId = "",
                     graphName = graph, action = Commands.UndoAction })
        | _       => raise wrong_args_exp))),

    ("redo",
     ("GRAPH", "Redo the last undone change to GRAPH",
      (fn [graph] => (Commands.EditGraphRequest { requestId = "",
                     graphName = graph, action = Commands.RedoAction })
        | _       => raise wrong_args_exp))),

    ("insert_graph",
     ("SOURCE TARGET", "Insert a copy of SOURCE into TARGET",
      (fn [src,tgt] => (Commands.EditGraphRequest { requestId = "",
                       graphName = tgt, action = Commands.InsertGraphAction { sourceGraphName = src } })
        | _         => raise wrong_args_exp))),

    ("add_vertex",
     ("GRAPH TYPE", "Add a TYPE vertex to GRAPH",
      (fn [graph,vt] => (Commands.EditGraphRequest { requestId = "",
                        graphName = graph, action = Commands.AddVertexAction { vertexType = vt, resultFormat = Commands.NameFormat } })
        | _          => raise wrong_args_exp))),

    ("rename_vertex",
     ("GRAPH OLD NEW", "Rename vertex in GRAPH from OLD to NEW",
      (fn [graph,old,new] => (Commands.EditGraphRequest { requestId = "",
                              graphName = graph,
                              action = Commands.RenameVertexAction { oldName = old, newName = new } })
        | _               => raise wrong_args_exp))),

    ("delete_vertices",
     ("GRAPH V1 .. VN", "Delete vertices V1..VN from GRAPH",
      (fn (graph::vs) => (Commands.EditGraphRequest { requestId = "",
                         graphName = graph, action = Commands.DeleteVerticesAction { vertexNames = vs } })
        | _           => raise wrong_args_exp))),

    ("set_vertex_data",
     ("GRAPH V DATA", "Set the data of vertex V to DATA",
      (fn [graph,v,data] => (Commands.EditGraphRequest { requestId = "",
                              graphName = graph,
                              action = Commands.SetVertexDataAction
                                { vertexName = v, vertexData = (Commands.StringCData data) } })
        | _               => raise wrong_args_exp))),

    ("add_edge",
     ("GRAPH TYPE DIR_OR_UNDIR V1 V2", "Add a TYPE edge in GRAPH from V1 to V2.  DIR_OR_UNDIR should be \"dir\" or \"undir\" for directed and undirected edges respectively.",
      (fn [graph,et,"dir",v1,v2] => (Commands.EditGraphRequest { requestId = "",
                        graphName = graph, action = Commands.AddEdgeAction
                          { edgeType = et, directed = true, source = v1, target = v2, resultFormat = Commands.NameFormat } })
      (fn [graph,et,"undir",v1,v2] => (Commands.EditGraphRequest { requestId = "",
                        graphName = graph, action = Commands.AddEdgeAction
                          { edgeType = et, directed = false, source = v1, target = v2, resultFormat = Commands.NameFormat } })
        | _          => raise wrong_args_exp))),

    ("set_edge_data",
     ("GRAPH E DATA", "Set the data of edge E to DATA",
      (fn [graph,e,data] => (Commands.EditGraphRequest { requestId = "",
                              graphName = graph,
                              action = Commands.SetEdgeDataAction
                                { edgeName = e, edgeData = (Commands.StringCData data) } })
        | _               => raise wrong_args_exp))),

    ("rename_edge",
     ("GRAPH OLD NEW", "Rename edge in GRAPH from OLD to NEW",
      (fn [graph,old,new] => (Commands.EditGraphRequest { requestId = "",
                              graphName = graph,
                              action = Commands.RenameEdgeAction { oldName = old, newName = new } })
        | _               => raise wrong_args_exp))),

    ("delete_edges",
     ("GRAPH E1 .. EN", "Delete edges E1 .. EN from GRAPH",
      (fn (graph::es) => (Commands.EditGraphRequest { requestId = "",
                         graphName = graph, action = Commands.DeleteEdgesAction { edgeNames = es } })
        | _           => raise wrong_args_exp))),

    (* FIXME: set_edge_data *)

    ("add_bbox",
     ("GRAPH", "Add a new !-box to GRAPH and return its name",
      (fn [graph] => (Commands.EditGraphRequest { requestId = "",
                     graphName = graph, action = Commands.AddBangBoxAction { vertexNames = [], resultFormat = Commands.NameFormat } })
        | _       => raise wrong_args_exp))),

    ("bbox_drop",
     ("GRAPH B1 .. BN", "Drop !-boxes B1 .. BN in GRAPH. Vertices inside the !-boxes are moved to the top-level graph.",
      (fn (graph::bs) => (Commands.EditGraphRequest { requestId = "",
                         graphName = graph, action = Commands.DropBangBoxesAction { bangBoxNames = bs } })
        | _           => raise wrong_args_exp))),

    ("bbox_kill",
     ("GRAPH B1 .. BN", "Kill !-boxes B1 .. BN in GRAPH. Vertices inside BANGS are deleted to the top-level graph.",
      (fn (graph::bs) => (Commands.EditGraphRequest { requestId = "",
                         graphName = graph, action = Commands.KillBangBoxesAction { bangBoxNames = bs } })
        | _           => raise wrong_args_exp))),

    ("bbox_duplicate",
     ("GRAPH BB", "Duplicates the !-box BB in GRAPH.",
      (fn [graph,bb] => (Commands.EditGraphRequest { requestId = "",
                         graphName = graph, action = Commands.DuplicateBangBoxAction { bangBoxName = bb } })
        | _           => raise wrong_args_exp))),

    ("bbox_merge",
     ("GRAPH B1 .. BN", "Merges !-boxes B1 .. BN in GRAPH, and return the name of the new !-box",
      (fn (graph::bs) => (Commands.EditGraphRequest { requestId = "",
                         graphName = graph, action = Commands.MergeBangBoxesAction { bangBoxNames = bs } })
        | _           => raise wrong_args_exp))),

    ("bang_vertices",
     ("GRAPH BB V1 .. VN", "Add vertices V1..VN in graph to !-box BB\n",
      (fn (graph::bb::vs) => (Commands.EditGraphRequest { requestId = "",
                             graphName = graph, action = Commands.BangVerticesAction { bangBoxName = bb, vertexNames = vs } })
        | _               => raise wrong_args_exp))),

    ("unbang_vertices",
     ("GRAPH V1 .. VN", "Remove vertices V1..VN from their current !-box",
      (fn (graph::vs) => (Commands.EditGraphRequest { requestId = "",
                         graphName = graph, action = Commands.UnbangVerticesAction { vertexNames = vs } })
        | _           => raise wrong_args_exp))),

    ("import_ruleset",
     ("FILENAME", "Add the rules contained in FILENAME to the existing ruleset.  "^
              "Tags will also be imported, and will overwrite existing tags of the same name.",
      (fn [filename] => (Commands.ImportRulesetFromFileRequest
                          { requestId = "", fileName = filename })
        | _          => raise wrong_args_exp))),

    ("export_ruleset",
     ("FILENAME", "Save all loaded rules to FILENAME, along with their tags and active status.",
      (fn [filename] => (Commands.ExportRulesetToFileRequest
                          { requestId = "", fileName = filename })
        | _          => raise wrong_args_exp))),

    ("list_rules",
     ("", "List all loaded rules",
      (fn [] => (Commands.ListRulesRequest { requestId = "" })
        | _  => raise wrong_args_exp))),

    ("list_active_rules",
     ("", "List all active rules",
      (fn [] => (Commands.ListActiveRulesRequest { requestId = "" })
        | _  => raise wrong_args_exp))),

    ("list_rules_with_tag",
     ("TAG", "List all loaded rules that are tagged with TAG",
      (fn [tag] => (Commands.ListRulesByTagRequest { requestId = "", tagName = tag })
        | _     => raise wrong_args_exp))),

    ("open_rule_lhs",
     ("RULE", "Open the LHS of the given rule and return the name of the new graph buffer.",
      (fn [rule] => (Commands.OpenRuleLhsRequest { requestId = "", ruleName = rule })
        | _      => raise wrong_args_exp))),

    ("open_rule_rhs",
     ("RULE", "Open the RHS of the given rule and return the name of the new graph buffer.",
      (fn [rule] => (Commands.OpenRuleRhsRequest { requestId = "", ruleName = rule })
        | _      => raise wrong_args_exp))),

    ("set_rule",
     ("RULE LHS RHS", "Create or update a rule from the graphs LHS and RHS (which "^
                      "may be the same).  If a rule with the same name exists, "^
                      "it will be replaced.",
      (fn [rule,lhs,rhs] => (Commands.SetRuleRequest { requestId = "",
                            ruleName = rule, lhsGraphName = lhs, rhsGraphName = rhs })
        | _              => raise wrong_args_exp))),

    ("delete_rule",
     ("RULE", "Forget about RULE entirely",
      (fn [rule] => (Commands.DeleteRuleRequest { requestId = "", ruleName = rule })
        | _      => raise wrong_args_exp))),

    ("activate_rule",
     ("RULE", "Allow RULE to be used for rewriting",
      (fn [rule] => (Commands.ActivateRuleRequest { requestId = "", ruleName = rule })
        | _      => raise wrong_args_exp))),

    ("deactivate_rule",
     ("RULE", "Prevent RULE from being used for rewriting",
      (fn [rule] => (Commands.DeactivateRuleRequest { requestId = "", ruleName = rule })
        | _      => raise wrong_args_exp))),

    ("list_tags",
     ("", "List all known tags for rules",
      (fn [] => (Commands.ListTagsRequest { requestId = "" })
        | _  => raise wrong_args_exp))),

    ("tag_rule",
     ("RULE TAG", "Tag RULE with TAG",
      (fn [rule,tag] => (Commands.TagRuleRequest { requestId = "",
                        ruleName = rule, tagName = tag })
        | _          => raise wrong_args_exp))),

    ("untag_rule",
     ("RULE TAG", "Remove TAG from RULE",
      (fn [rule,tag] => (Commands.UntagRuleRequest { requestId = "",
                        ruleName = rule, tagName = tag })
        | _          => raise wrong_args_exp))),

    ("delete_tag",
     ("TAG", "Remove TAG from all rules and forget about TAG entirely",
      (fn [tag] => (Commands.ForgetTagRequest { requestId = "", tagName = tag })
        | _     => raise wrong_args_exp))),

    ("delete_rules_with_tag",
     ("TAG", "Delete all rules tagged with TAG",
      (fn [tag] => (Commands.DeleteRulesByTagRequest { requestId = "", tagName = tag })
        | _     => raise wrong_args_exp))),

    ("activate_rules_with_tag",
     ("TAG", "Allow the rules tagged with TAG to be used for rewriting",
      (fn [tag] => (Commands.ActivateRulesByTagRequest { requestId = "", tagName = tag })
        | _     => raise wrong_args_exp))),

    ("deactivate_rules_with_tag",
     ("TAG", "Prevent the rules tagged with TAG from being used for rewriting",
      (fn [tag] => (Commands.DeactivateRulesByTagRequest { requestId = "", tagName = tag })
        | _     => raise wrong_args_exp))),

    ("attach_rewrites",
     ("GRAPH V1 .. VN", "Calculate rewrites involving V1..VN in GRAPH and attach to the graph state",
      (fn (graph::vs) => (Commands.AttachRewritesRequest { requestId = "",
                         graphName = graph, vertexNames = vs })
        | _           => raise wrong_args_exp))),

    ("attach_one_rewrite",
     ("GRAPH V1 .. VN", "Calculate at most one rewrite involving V1..VN in GRAPH and attach to the graph state",
      (fn (graph::vs) => (Commands.AttachOneRewriteRequest { requestId = "",
                         graphName = graph, vertexNames = vs })
        | _           => raise wrong_args_exp))),

    ("show_rewrites",
     ("GRAPH", "Show rewrites attached to GRAPH",
      (fn [graph] => (Commands.ListAttachedRewritesRequest { requestId = "",
                     graphName = graph })
        | _       => raise wrong_args_exp))),

    ("apply_rewrite",
     ("GRAPH N", "Apply the Nth rewrite attached to GRAPH",
      (fn [graph,nstr] => case Int.fromString nstr of
                            SOME i => (Commands.ApplyAttachedRewriteRequest { requestId = "",
                                      graphName = graph, index = i })
                          | NONE => raise wrong_args_exp
        | _         => raise wrong_args_exp)))

(*
    ("apply_first_rewrite",
     ("apply_first_rewrite GRAPH\n\n"^
        "    attempt to attach one rewrite to GRAPH and apply it\n",
      (lift_procedure o lift_graphname_arg) ControllerState.apply_first_rewrite))

    ("print_theory_name",
     ("print_theory_name GRAPH\n\n"^
       "     print out the name of the theory implemented by graph\n",
      (lift_graph_viewer o lift_no_args) (as_data o Pretty.string_of o G.pretty_theory_name)
     )),
*)
    ]
    empty;
end; (* local *)

(* vim:et:sts=2:sw=2
*)
