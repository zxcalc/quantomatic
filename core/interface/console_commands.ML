(* Commands available from the console and associate help stuff *)
signature CONSOLE_COMMANDS = 
sig
  type T;
  type state;
  type command = (string list -> state -> (string * state));

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
functor ConsoleCommands(Commands : COMMANDS) : CONSOLE_COMMANDS
= struct 
  (* args, help, command *)
  type state = Commands.state;
  type command = (string list -> state -> (string * state));
  type annotated_command = string * string * command;

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

  fun liftStringResultResponse f (input:string list) (state:state) =
  let
    val (response:Commands.response,state':state) = f input state
    fun parseResponse (Commands.OkResponse { requestId }) = "OK"
      | parseResponse (Commands.ErrorResponse { requestId, errorCode, message }) = "!!! "^message
      | parseResponse (Commands.DataResponse { requestId, data }) = data
      | parseResponse (Commands.PrettyResponse { requestId, data }) = Pretty.string_of data
      | parseResponse (Commands.NameResponse { requestId, name }) = name
      | parseResponse (Commands.NameListResponse { requestId, names }) = (String.concatWith "\n" names)
      | parseResponse (Commands.ComponentDataResponse { requestId, data as (ComponentData.Unit) }) = ""
      | parseResponse (Commands.ComponentDataResponse { requestId, data as (ComponentData.String s) }) = s
      | parseResponse (Commands.ComponentDataResponse { requestId, data as (ComponentData.LinratExpr a) }) = Pretty.string_of (LinratExpr.pretty a)
      | parseResponse (Commands.UserDataResponse { requestId, data }) = data
      | parseResponse _ = raise unexpected_response_exp;
  in
    (parseResponse response, state')
  end;

  fun bad_args_response st = (Commands.ErrorResponse { requestId = "", errorCode = "BADARGS", message = "Wrong number of arguments" }, st)

  val all_commands = 
  fold add_command [

    ("list_graphs",
     ("", "List all loaded graphs",
      liftStringResultResponse
      (fn [] => Commands.serviceRequest (Commands.ListGraphsRequest { requestId = "" })
        | _  => bad_args_response))),

    ("ls",
     ("", "List all loaded graphs (alias for list_graphs)",
      liftStringResultResponse
      (fn [] => Commands.serviceRequest (Commands.ListGraphsRequest { requestId = "" })
        | _  => bad_args_response))),

    ("new_graph",
     ("", "Create a new graph and return its name\n",
      liftStringResultResponse
      (fn [] => Commands.serviceRequest (Commands.LoadGraphRequest { requestId = "",
                        details = Commands.LoadEmptyGraphRequest })
        | _  => bad_args_response))),

    ("load_graph",
     ("FILENAME", "Load a new graph from FILENAME return its name",
      liftStringResultResponse
      (fn [filename] => Commands.serviceRequest (Commands.LoadGraphRequest { requestId = "",
                        details = Commands.LoadGraphFromFileRequest { fileName = filename } })
        | _          => bad_args_response))),

    ("duplicate_graph",
     ("GRAPH", "Duplicate GRAPH and return the name of the new graph",
      liftStringResultResponse
      (fn [graph] => Commands.serviceRequest (Commands.LoadGraphRequest { requestId = "",
                     details = Commands.CopyGraphRequest { graphName = graph } })
        | _       => bad_args_response))),

    ("copy_subgraph",
     ("SOURCE TARGET V1 .. VN", "Copy the subgraph of SOURCE spanned by V1..VN to the graph TARGET",
      liftStringResultResponse
      (fn (src::tgt::vs) => Commands.serviceRequest (Commands.LoadGraphRequest { requestId = "",
                            details = Commands.CopySubgraphRequest { source = src, graphName = tgt, vertexNames = vs } })
        | _              => bad_args_response))),

    ("save_graph",
     ("GRAPH FILENAME", "Save GRAPH to FILENAME",
      liftStringResultResponse
      (fn [graph,fnm] => Commands.serviceRequest (Commands.SaveGraphToFileRequest
                           { requestId = "", graphName = graph, fileName = fnm })
        | _           => bad_args_response))),

    ("rename_graph",
     ("OLD NEW", "Rename graph OLD to NEW, return the real new name.",
      liftStringResultResponse
      (fn [old,new] => Commands.serviceRequest (Commands.RenameGraphRequest
                         { requestId = "", oldName = old, newName = new })
        | _         => bad_args_response))),

    ("kill_graph",
     ("GRAPH", "Remove GRAPH from the workspace",
      liftStringResultResponse
      (fn [graph] => Commands.serviceRequest (Commands.DiscardGraphRequest
                       { requestId = "", graphName = graph })
        | _       => bad_args_response))),

    ("print_graph",
     ("GRAPH", "Print out GRAPH",
      liftStringResultResponse
      (fn [graph] => Commands.serviceRequest (Commands.ExportGraphRequest
                       { requestId = "", graphName = graph, format = Commands.PrettyGraphFormat })
        | _       => bad_args_response))),

    ("hilb",
     ("GRAPH FORMAT", "Display the hilbert space term of GRAPH where FORMAT is one of {plain, latex, mathematica, matlab}.",
      liftStringResultResponse
      (fn [graph,"plain"]       => Commands.serviceRequest (Commands.ExportGraphRequest
                       { requestId = "", graphName = graph, format = Commands.HilbertSpaceGraphFormat })
        | [graph,"mathematica"] => Commands.serviceRequest (Commands.ExportGraphRequest
                       { requestId = "", graphName = graph, format = Commands.MathematicaGraphFormat })
        | [graph,"matlab"]      => Commands.serviceRequest (Commands.ExportGraphRequest
                       { requestId = "", graphName = graph, format = Commands.MatlabGraphFormat })
        | [graph,"latex"]       => Commands.serviceRequest (Commands.ExportGraphRequest
                       { requestId = "", graphName = graph, format = Commands.TikzGraphFormat })
        | [_,fmt]       => (fn st => (Commands.ErrorResponse { requestId = "", errorCode = "BADARG", message = "Unknown format "^fmt }, st))
        | _               => bad_args_response))),

    ("undo",
     ("GRAPH", "Undo the last change to GRAPH",
      liftStringResultResponse
      (fn [graph] => Commands.serviceRequest (Commands.EditGraphRequest { requestId = "",
                     graphName = graph, action = Commands.UndoAction })
        | _       => bad_args_response))),

    ("redo",
     ("GRAPH", "Redo the last undone change to GRAPH",
      liftStringResultResponse
      (fn [graph] => Commands.serviceRequest (Commands.EditGraphRequest { requestId = "",
                     graphName = graph, action = Commands.RedoAction })
        | _       => bad_args_response))),

    ("insert_graph",
     ("SOURCE TARGET", "Insert a copy of SOURCE into TARGET",
      liftStringResultResponse
      (fn [src,tgt] => Commands.serviceRequest (Commands.EditGraphRequest { requestId = "",
                       graphName = tgt, action = Commands.InsertGraphAction { sourceGraphName = src } })
        | _         => bad_args_response))),

    ("add_vertex",
     ("GRAPH TYPE", "Add a TYPE vertex to GRAPH",
      liftStringResultResponse
      (fn [graph,vt] => Commands.serviceRequest (Commands.EditGraphRequest { requestId = "",
                        graphName = graph, action = Commands.AddVertexAction { vertexType = vt, resultFormat = Commands.NameFormat } })
        | _          => bad_args_response))),

    ("rename_vertex",
     ("GRAPH OLD NEW", "Rename vertex in GRAPH from OLD to NEW",
      liftStringResultResponse
      (fn [graph,old,new] => Commands.serviceRequest (Commands.EditGraphRequest { requestId = "",
                              graphName = graph,
                              action = Commands.RenameVertexAction { oldName = old, newName = new } })
        | _               => bad_args_response))),

    ("delete_vertices",
     ("GRAPH V1 .. VN", "Delete vertices V1..VN from GRAPH",
      liftStringResultResponse
      (fn (graph::vs) => Commands.serviceRequest (Commands.EditGraphRequest { requestId = "",
                         graphName = graph, action = Commands.DeleteVerticesAction { vertexNames = vs } })
        | _           => bad_args_response))),

    (* FIXME: linrat-only *)
    ("set_vertex_data",
     ("GRAPH V DATA", "Set the data of vertex V to ANGLE",
      liftStringResultResponse
      (fn [graph,v,data] => Commands.serviceRequest (Commands.EditGraphRequest { requestId = "",
                              graphName = graph,
                              action = Commands.SetVertexDataAction { vertexName = v, vertexData = (ComponentData.LinratExpr (LinratExpr.parse data)) } })
        | _               => bad_args_response))),

    ("add_edge",
     ("GRAPH TYPE DIR_OR_UNDIR V1 V2", "Add a TYPE edge in GRAPH from V1 to V2.  DIR_OR_UNDIR should be \"dir\" or \"undir\" for directed and undirected edges respectively.",
      liftStringResultResponse
      (fn [graph,et,v1,v2] => Commands.serviceRequest (Commands.EditGraphRequest { requestId = "",
                        graphName = graph, action = Commands.AddEdgeAction
                          { edgeType = et, source = v1, target = v2, resultFormat = Commands.NameFormat } })
        | _          => bad_args_response))),

    ("rename_edge",
     ("GRAPH OLD NEW", "Rename edge in GRAPH from OLD to NEW",
      liftStringResultResponse
      (fn [graph,old,new] => Commands.serviceRequest (Commands.EditGraphRequest { requestId = "",
                              graphName = graph,
                              action = Commands.RenameEdgeAction { oldName = old, newName = new } })
        | _               => bad_args_response))),

    ("delete_edges",
     ("GRAPH E1 .. EN", "Delete edges E1 .. EN from GRAPH",
      liftStringResultResponse
      (fn (graph::es) => Commands.serviceRequest (Commands.EditGraphRequest { requestId = "",
                         graphName = graph, action = Commands.DeleteEdgesAction { edgeNames = es } })
        | _           => bad_args_response))),

    (* FIXME: set_edge_data *)

    ("add_bbox",
     ("GRAPH", "Add a new !-box to GRAPH and return its name",
      liftStringResultResponse
      (fn [graph] => Commands.serviceRequest (Commands.EditGraphRequest { requestId = "",
                     graphName = graph, action = Commands.AddBangBoxAction })
        | _       => bad_args_response))),

    ("bbox_drop",
     ("GRAPH B1 .. BN", "Drop !-boxes B1 .. BN in GRAPH. Vertices inside the !-boxes are moved to the top-level graph.",
      liftStringResultResponse
      (fn (graph::bs) => Commands.serviceRequest (Commands.EditGraphRequest { requestId = "",
                         graphName = graph, action = Commands.DropBangBoxesAction { bangBoxNames = bs } })
        | _           => bad_args_response))),

    ("bbox_kill",
     ("GRAPH B1 .. BN", "Kill !-boxes B1 .. BN in GRAPH. Vertices inside BANGS are deleted to the top-level graph.",
      liftStringResultResponse
      (fn (graph::bs) => Commands.serviceRequest (Commands.EditGraphRequest { requestId = "",
                         graphName = graph, action = Commands.KillBangBoxesAction { bangBoxNames = bs } })
        | _           => bad_args_response))),

    ("bbox_duplicate",
     ("GRAPH BB", "Duplicates the !-box BB in GRAPH.",
      liftStringResultResponse
      (fn [graph,bb] => Commands.serviceRequest (Commands.EditGraphRequest { requestId = "",
                         graphName = graph, action = Commands.DuplicateBangBoxAction { bangBoxName = bb } })
        | _           => bad_args_response))),

    ("bbox_merge",
     ("GRAPH B1 .. BN", "Merges !-boxes B1 .. BN in GRAPH, and return the name of the new !-box",
      liftStringResultResponse
      (fn (graph::bs) => Commands.serviceRequest (Commands.EditGraphRequest { requestId = "",
                         graphName = graph, action = Commands.MergeBangBoxesAction { bangBoxNames = bs } })
        | _           => bad_args_response))),

    ("bang_vertices",
     ("GRAPH BB V1 .. VN", "Add vertices V1..VN in graph to !-box BB\n",
      liftStringResultResponse
      (fn (graph::bb::vs) => Commands.serviceRequest (Commands.EditGraphRequest { requestId = "",
                             graphName = graph, action = Commands.BangVerticesAction { bangBoxName = bb, vertexNames = vs } })
        | _               => bad_args_response))),

    ("unbang_vertices",
     ("GRAPH V1 .. VN", "Remove vertices V1..VN from their current !-box",
      liftStringResultResponse
      (fn (graph::vs) => Commands.serviceRequest (Commands.EditGraphRequest { requestId = "",
                         graphName = graph, action = Commands.UnbangVerticesAction { vertexNames = vs } })
        | _           => bad_args_response))),

    ("import_ruleset",
     ("FILENAME", "Add the rules contained in FILENAME to the existing ruleset.  "^
              "Tags will also be imported, and will overwrite existing tags of the same name.",
      liftStringResultResponse
      (fn [filename] => Commands.serviceRequest (Commands.ImportRulesetFromFileRequest
                          { requestId = "", fileName = filename })
        | _          => bad_args_response))),

    ("export_ruleset",
     ("FILENAME", "Save all loaded rules to FILENAME, along with their tags and active status.",
      liftStringResultResponse
      (fn [filename] => Commands.serviceRequest (Commands.ExportRulesetToFileRequest
                          { requestId = "", fileName = filename })
        | _          => bad_args_response))),

    ("list_rules",
     ("", "List all loaded rules",
      liftStringResultResponse
      (fn [] => Commands.serviceRequest (Commands.ListRulesRequest { requestId = "" })
        | _  => bad_args_response))),

    ("list_active_rules",
     ("", "List all active rules",
      liftStringResultResponse
      (fn [] => Commands.serviceRequest (Commands.ListActiveRulesRequest { requestId = "" })
        | _  => bad_args_response))),

    ("list_rules_with_tag",
     ("TAG", "List all loaded rules that are tagged with TAG",
      liftStringResultResponse
      (fn [tag] => Commands.serviceRequest (Commands.ListRulesByTagRequest { requestId = "", tagName = tag })
        | _     => bad_args_response))),

    ("open_rule_lhs",
     ("RULE", "Open the LHS of the given rule and return the name of the new graph buffer.",
      liftStringResultResponse
      (fn [rule] => Commands.serviceRequest (Commands.OpenRuleLhsRequest { requestId = "", ruleName = rule })
        | _      => bad_args_response))),

    ("open_rule_rhs",
     ("RULE", "Open the RHS of the given rule and return the name of the new graph buffer.",
      liftStringResultResponse
      (fn [rule] => Commands.serviceRequest (Commands.OpenRuleRhsRequest { requestId = "", ruleName = rule })
        | _      => bad_args_response))),

    ("set_rule",
     ("RULE LHS RHS", "Create or update a rule from the graphs LHS and RHS (which "^
                      "may be the same).  If a rule with the same name exists, "^
                      "it will be replaced.",
      liftStringResultResponse
      (fn [rule,lhs,rhs] => Commands.serviceRequest (Commands.SetRuleRequest { requestId = "",
                            ruleName = rule, lhsGraphName = lhs, rhsGraphName = rhs })
        | _              => bad_args_response))),

    ("delete_rule",
     ("RULE", "Forget about RULE entirely",
      liftStringResultResponse
      (fn [rule] => Commands.serviceRequest (Commands.DeleteRuleRequest { requestId = "", ruleName = rule })
        | _      => bad_args_response))),

    ("activate_rule",
     ("RULE", "Allow RULE to be used for rewriting",
      liftStringResultResponse
      (fn [rule] => Commands.serviceRequest (Commands.ActivateRuleRequest { requestId = "", ruleName = rule })
        | _      => bad_args_response))),

    ("deactivate_rule",
     ("RULE", "Prevent RULE from being used for rewriting",
      liftStringResultResponse
      (fn [rule] => Commands.serviceRequest (Commands.DeactivateRuleRequest { requestId = "", ruleName = rule })
        | _      => bad_args_response))),

    ("list_tags",
     ("", "List all known tags for rules",
      liftStringResultResponse
      (fn [] => Commands.serviceRequest (Commands.ListTagsRequest { requestId = "" })
        | _  => bad_args_response))),

    ("tag_rule",
     ("RULE TAG", "Tag RULE with TAG",
      liftStringResultResponse
      (fn [rule,tag] => Commands.serviceRequest (Commands.TagRuleRequest { requestId = "",
                        ruleName = rule, tagName = tag })
        | _          => bad_args_response))),

    ("untag_rule",
     ("RULE TAG", "Remove TAG from RULE",
      liftStringResultResponse
      (fn [rule,tag] => Commands.serviceRequest (Commands.UntagRuleRequest { requestId = "",
                        ruleName = rule, tagName = tag })
        | _          => bad_args_response))),

    ("delete_tag",
     ("TAG", "Remove TAG from all rules and forget about TAG entirely",
      liftStringResultResponse
      (fn [tag] => Commands.serviceRequest (Commands.ForgetTagRequest { requestId = "", tagName = tag })
        | _     => bad_args_response))),

    ("delete_rules_with_tag",
     ("TAG", "Delete all rules tagged with TAG",
      liftStringResultResponse
      (fn [tag] => Commands.serviceRequest (Commands.DeleteRulesByTagRequest { requestId = "", tagName = tag })
        | _     => bad_args_response))),

    ("activate_rules_with_tag",
     ("TAG", "Allow the rules tagged with TAG to be used for rewriting",
      liftStringResultResponse
      (fn [tag] => Commands.serviceRequest (Commands.ActivateRulesByTagRequest { requestId = "", tagName = tag })
        | _     => bad_args_response))),

    ("deactivate_rules_with_tag",
     ("TAG", "Prevent the rules tagged with TAG from being used for rewriting",
      liftStringResultResponse
      (fn [tag] => Commands.serviceRequest (Commands.DeactivateRulesByTagRequest { requestId = "", tagName = tag })
        | _     => bad_args_response))),

    ("attach_rewrites",
     ("GRAPH V1 .. VN", "Calculate rewrites involving V1..VN in GRAPH and attach to the graph state",
      liftStringResultResponse
      (fn (graph::vs) => Commands.serviceRequest (Commands.AttachRewritesRequest { requestId = "",
                         graphName = graph, vertexNames = vs })
        | _           => bad_args_response))),

    ("attach_one_rewrite",
     ("GRAPH V1 .. VN", "Calculate at most one rewrite involving V1..VN in GRAPH and attach to the graph state",
      liftStringResultResponse
      (fn (graph::vs) => Commands.serviceRequest (Commands.AttachOneRewriteRequest { requestId = "",
                         graphName = graph, vertexNames = vs })
        | _           => bad_args_response))),

    ("show_rewrites",
     ("GRAPH", "Show rewrites attached to GRAPH",
      liftStringResultResponse
      (fn [graph] => Commands.serviceRequest (Commands.ListAttachedRewritesRequest { requestId = "",
                     graphName = graph })
        | _       => bad_args_response))),

    ("apply_rewrite",
     ("GRAPH N", "Apply the Nth rewrite attached to GRAPH",
      liftStringResultResponse
      (fn [graph,nstr] => case Int.fromString nstr of
                            SOME i => Commands.serviceRequest (Commands.ApplyAttachedRewriteRequest { requestId = "",
                                      graphName = graph, index = i })
                          | NONE => bad_args_response
        | _         => bad_args_response)))

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
