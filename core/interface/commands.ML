structure Commands =
struct
  type vertexDesc = { name: string, data: ComponentData.T, userData: string };
  type edgeDesc = { name: string, sourceName: string, targetName: string, directed: bool, data: ComponentData.T, userData: string };
  type bangBoxDesc = { name: string, vertexNames: string list };
  type graphDesc = { name: string, graphName: string, vertices: vertexDesc list, edges: edgeDesc list, bangBoxen: bangBoxDesc list };
  type rewriteDesc = { ruleName: string, result: graphDesc, lhs: graphDesc, rhs: graphDesc };
  datatype resultFormat = NameFormat | PrettyFormat | XmlFormat | DescriptionFormat;
  datatype componentData = StringCData of string | XmlCData of (XML.tree list) | DescriptionCData of ComponentData.T;

                     (* A dummy request does not change the state, and simply produces the given response *)
  datatype request = DummyRequest of { response: response }

                   (* Graph commands: G *)
                     (* NameListResponse *)
                   | ListGraphsRequest of { requestId: string }                                                  (* GL *)
                     (* NameResponse | ErrorResponse *)
                   | LoadGraphRequest of { requestId: string, details: graphLoadRequest }                        (* GO *)
                     (* OkResponse | ErrorResponse *)
                   | SaveGraphToFileRequest of { requestId: string, graphName: string, fileName: string }        (* GS *)
                     (* NameResponse | ErrorResponse *)
                   | RenameGraphRequest of { requestId: string, oldName: string, newName: string }               (* GR *)
                     (* OkResponse | ErrorResponse *)
                   | DiscardGraphRequest of { requestId: string, graphName: string }                             (* GD *)
                     (* DataResponse | ErrorResponse *)
                   | ExportGraphRequest of { requestId: string, graphName: string, format: graphFormat }         (* GE *)
                   | DescribeGraphRequest of { requestId: string, graphName: string, desc: graphDescribeAction } (* GV *)
                   | EditGraphRequest of { requestId: string, graphName: string, action: graphEditAction }       (* GM *)

                   (* Ruleset commands: RS *)
                     (* OkResponse | ErrorResponse *)
                   | ImportRulesetFromFileRequest of { requestId: string, fileName: string } (* RSO *)
                     (* OkResponse | ErrorResponse *)
                   | ImportRulesetFromDataRequest of { requestId: string, data: string }     (* RSI *)
                     (* OkResponse | ErrorResponse *)
                   | ExportRulesetToFileRequest of { requestId: string, fileName: string }   (* RSS *)
                     (* OkResponse | ErrorResponse *)
                   | ExportRulesetAsDataRequest of { requestId: string }                     (* RSE *)

                   (* Rule commands: RR *)
                     (* NameListResponse *)
                   | ListRulesRequest of { requestId: string }                        (* RRL *)
                     (* NameListResponse *)
                   | ListActiveRulesRequest of { requestId: string }                  (* RRA *)
                     (* NameListResponse | ErrorResponse *)
                   | ListRulesByTagRequest of { requestId: string, tagName: string }  (* RRT *)
                   | OpenRuleLhsRequest of { requestId: string, ruleName: string }    (* RRP *)
                   | OpenRuleRhsRequest of { requestId: string, ruleName: string }    (* RRQ *)
                     (* OkResponse | ErrorResponse *)
                   | SetRuleRequest of { requestId: string, ruleName: string, lhsGraphName: string, rhsGraphName: string } (* RRU *)
                     (* OkResponse | ErrorResponse *)
                   | DeleteRuleRequest of { requestId: string, ruleName: string }     (* RRD *)
                     (* OkResponse | ErrorResponse *)
                   | ActivateRuleRequest of { requestId: string, ruleName: string }   (* RRY *)
                     (* OkResponse | ErrorResponse *)
                   | DeactivateRuleRequest of { requestId: string, ruleName: string } (* RRN *)

                   (* Tag commands: RT *)
                     (* NameListResponse *)
                   | ListTagsRequest of { requestId: string }                                     (* RTL *)
                     (* OkResponse | ErrorResponse *)
                   | TagRuleRequest of { requestId: string, ruleName: string, tagName: string }   (* RTT *)
                     (* OkResponse | ErrorResponse *)
                   | UntagRuleRequest of { requestId: string, ruleName: string, tagName: string } (* RTU *)
                     (* OkResponse | ErrorResponse *)
                   | ForgetTagRequest of { requestId: string, tagName: string }                   (* RTF *)
                     (* OkResponse | ErrorResponse *)
                   | DeleteRulesByTagRequest of { requestId: string, tagName: string }            (* RTD *)
                     (* OkResponse | ErrorResponse *)
                   | ActivateRulesByTagRequest of { requestId: string, tagName: string }          (* RTA *)
                     (* OkResponse | ErrorResponse *)
                   | DeactivateRulesByTagRequest of { requestId: string, tagName: string }        (* RTA *)

                   (* Rewrite commands: W *)
                   | AttachRewritesRequest of { requestId: string, graphName: string, vertexNames: string list }   (* WA *)
                   | AttachOneRewriteRequest of { requestId: string, graphName: string, vertexNames: string list } (* WO *)
                   | ListAttachedRewritesRequest of { requestId: string, graphName: string }                       (* WL *)
                   | ApplyAttachedRewriteRequest of { requestId: string, graphName: string, index: int }           (* WW *)

                   | UnknownRequest of { requestId: string, code: string }

       (* GO *)
       and graphLoadRequest = LoadEmptyGraphRequest                                                         (* GOE *)
                            | LoadGraphFromFileRequest of { fileName: string }                              (* GOF *)
                            | LoadGraphFromDataRequest of { data: string }                                  (* GOD *)
                            | CopyGraphRequest of { graphName: string }                                     (* GOG *)
                            | CopySubgraphRequest of { source: string, graphName: string, vertexNames: string list } (* GOS *)

                       (* Native format: suitable for saving to a file and loading again *)
       and graphFormat = NativeGraphFormat
                       | XmlGraphFormat
                       | PrettyGraphFormat
                       | HilbertSpaceGraphFormat
                       | MathematicaGraphFormat
                       | MatlabGraphFormat
                       | TikzGraphFormat

                                 (* GraphDescriptionResponse | ErrorResponse *)
       and graphDescribeAction = DescribeFullGraph                          (* GVA *)
                                 (* NameListResponse | ErrorResponse *)
                               | ListVertices                               (* GVV *)
                                 (* NameListResponse | ErrorResponse *)
                               | ListEdges                                  (* GVE *)
                                 (* NameListResponse | ErrorResponse *)
                               | ListBangBoxes                              (* GVB *)
                                 (* ComponentDataResponse | ErrorResponse *)
                               | VertexData of { vertexName: string }       (* GVW *)
                                 (* UserDataResponse | ErrorResponse *)
                               | VertexUserData of { vertexName: string }   (* GVX *)
                                 (* ComponentDataResponse | ErrorResponse *)
                               | EdgeData of { edgeName: string }           (* GVF *)
                                 (* UserDataResponse | ErrorResponse *)
                               | EdgeUserData of { edgeName: string }       (* GVG *)
                                 (* NameListResponse | ErrorResponse *)
                               | BangBoxVertices of { bangBoxName: string } (* GVC *)
                                 (* UserDataResponse | ErrorResponse *)
                               | BangBoxUserData of { bangBoxName: string } (* GVD *)

                             (* OkResponse | ErrorResponse *)
       and graphEditAction = UndoAction                                                                (* GMU *)
                             (* OkResponse | ErrorResponse *)
                           | RedoAction                                                                (* GMR *)
                             (* OkResponse | ErrorResponse *)
                           | InsertGraphAction of { sourceGraphName: string }                          (* GMI *)
                             (* VertexDescriptionAction | ErrorResponse *)
                           | AddVertexAction of { vertexType: string, resultFormat: resultFormat }     (* GMVA *)
                             (* FIXME: do we need this? *)
                             (* NameResponse | ErrorResponse *)
                           | RenameVertexAction of { oldName: string, newName: string }                (* GMVR *)
                             (* OkResponse | ErrorResponse *)
                           | DeleteVerticesAction of { vertexNames: string list }                      (* GMVD *)
                             (* OkResponse | ErrorResponse *)
                           | SetVertexDataAction of { vertexName: string, vertexData: componentData }  (* GMVS *)
                             (* OkResponse | ErrorResponse *)
                           | SetVertexUserDataAction of { vertexName: string, userData: string }       (* GMVU *)
                             (* EdgeDescriptionAction | ErrorResponse *)
                           | AddEdgeAction of { edgeType: string, directed: bool, source: string, target: string, resultFormat: resultFormat }(* GMEA *)
                             (* FIXME: do we need this? *)
                             (* NameResponse | ErrorResponse *)
                           | RenameEdgeAction of { oldName: string, newName: string }                  (* GMER *)
                             (* OkResponse | ErrorResponse *)
                           | DeleteEdgesAction of { edgeNames: string list }                           (* GMED *)
                             (* OkResponse | ErrorResponse *)
                           | SetEdgeDataAction of { edgeName: string, edgeData: componentData }        (* GMES *)
                             (* OkResponse | ErrorResponse *)
                           | SetEdgeUserDataAction of { edgeName: string, userData: string }           (* GMEU *)
                             (* NameResponse | ErrorResponse *)
                           | AddBangBoxAction of { vertexNames: string list, resultFormat: resultFormat }(* GMBA *)
                             (* OkResponse | ErrorResponse *)
                           | RenameBangBoxAction of { oldName: string, newName: string }               (* GMBR *)
                             (* OkResponse | ErrorResponse *)
                           | DropBangBoxesAction of { bangBoxNames: string list }                      (* GMBD *)
                             (* OkResponse | ErrorResponse *)
                           | KillBangBoxesAction of { bangBoxNames: string list }                      (* GMBK *)
                             (* NameResponse | ErrorResponse *)
                           | DuplicateBangBoxAction of { bangBoxName: string }                         (* GMBC *)
                             (* NameResponse | ErrorResponse *)
                           | MergeBangBoxesAction of { bangBoxNames: string list }                     (* GMBM *)
                             (* OkResponse | ErrorResponse *)
                           | BangVerticesAction of { bangBoxName: string, vertexNames: string list }   (* GMBB *)
                             (* OkResponse | ErrorResponse *)
                           | UnbangVerticesAction of { vertexNames: string list }                      (* GMBL *)
                             (* OkResponse | ErrorResponse *)
                           | SetBangBoxUserDataAction of { bangBoxName: string, userData: string }     (* GMBU *)

       and response = OkResponse of { requestId: string }                                         (* O *)
                    | ErrorResponse of { requestId: string, errorCode: string, message: string }  (* X *)
                    | DataResponse of { requestId: string, data: string }                         (* R *)
                    | PrettyResponse of { requestId: string, data: Pretty.T }
                    | XmlResponse of { requestId: string, data: XML.tree }

                    | NameResponse of { requestId: string, name: string }                      (* N *)
                    | NameListResponse of { requestId: string, names: string list }            (* M *)
                    | GraphDescriptionResponse of { requestId: string, graph: graphDesc }      (* G *)
                    | VertexDescriptionResponse of { requestId: string, vertex: vertexDesc }   (* V *)
                    | EdgeDescriptionResponse of { requestId: string, edge: edgeDesc }         (* E *)
                    | ComponentDataResponse of { requestId: string, data: ComponentData.T }    (* D *)
                    | UserDataResponse of { requestId: string, data: string }                  (* U *)
                    | RewriteListResponse of { requestId: string, rewrites: rewriteDesc list } (* W *)

                    | UnknownResponse of { requestId: string, code: string }                   (* Z *)
                    ;
end;

signature CONTROLLER =
sig
  val serviceRequest : Commands.request -> Commands.response;
end;


functor Controller(State : CONTROLLER_STATE) : CONTROLLER =
struct
  val cur_state = Unsynchronized.ref State.init;
  open Commands;

  structure Ruleset = State.Theory.Ruleset;
  structure R = State.Theory.Rule;
  structure G = State.Theory.Graph;
  structure V = State.Theory.Vertex;
  structure E = State.Theory.Edge;
  structure B = State.Theory.Graph.BBox;
  structure VCD = State.Theory.VertexComponentData;
  structure ECD = State.Theory.EdgeComponentData;
  structure Input = State.Theory.IO_Xml.Input;
  structure Output = State.Theory.IO_Xml.Output;

  fun unknown_graph_resp requestId graphName = 
             ErrorResponse { requestId = requestId, errorCode = "BADARG",
                message = ("No such graph \""^graphName^"\"") }

  fun get_graph (name:string) (state:State.T) : G.T = State.get_graph state (GraphName.mk name)

  fun load_ruleset requestId data state =
     ((OkResponse { requestId = requestId },
       State.update_rset (Ruleset.merge (Input.Ruleset.input data)) state)
      handle XML_Parse_Utils.bad_input_exp (s1,s2) =>
             (ErrorResponse { requestId = requestId, errorCode = "BADDATA",
                message = ("The file is not a valid ruleset for the active theory ("^s1^" at "^s2^")") }, state)
           | Input.Ruleset.bad_input_exp (s1,s2) => 
             (ErrorResponse { requestId = requestId, errorCode = "BADDATA",
                message = ("The file is not a valid ruleset for the active theory ("^s1^" at "^s2^")") }, state))

  fun pretty_vert (n, (nd,(ins,outs))) =
      Pretty.block [V.pretty_name n,
                    Pretty.str "; ",
                    Pretty.list "i{" "}" (map E.pretty_name (E.NSet.list_of ins)),
                    Pretty.list "; o{" "}" (map E.pretty_name (E.NSet.list_of outs)),
                    Pretty.str "; ",
                    V.pretty_data nd
                   ]

  fun pretty_edge_dir_or_undir G.Directed = Pretty.str "dir"
    | pretty_edge_dir_or_undir G.UnDirected = Pretty.str "bidir";

  fun pretty_edge (en, ((bd,e), (n1, n2))) =
      Pretty.block [E.pretty_name en,
                    Pretty.str ": ",
                    V.pretty_name n1,
                    Pretty.str " - ",
                    V.pretty_name n2,
                    Pretty.str "; (",
                    pretty_edge_dir_or_undir bd,
                    Pretty.str ") ",
                    E.pretty_data e
                   ]


  fun serviceRequest' (DummyRequest { response }) (state:State.T) = (response, state)

    | serviceRequest' (ListGraphsRequest { requestId }) (state:State.T) =
        let val graphNames = map GraphName.dest (State.get_graph_names state) in
          (NameListResponse { requestId = requestId, names = graphNames }, state)
        end

    | serviceRequest' (LoadGraphRequest { requestId, details }) state =
        let fun load_graph data =
                (let
                   val graph = (Input.Graph.input data)
                   val (nm,state') = State.add_graph GraphName.default_name graph state
                 in
                   (NameResponse { requestId = requestId, name = GraphName.dest nm },state')
                 end
                 handle XML_Parse_Utils.bad_input_exp (s1,s2) =>
                        (ErrorResponse { requestId = requestId, errorCode = "BADDATA",
                           message = ("The file is not a valid graph for the active theory ("^s1^" at "^s2^")") }, state)
                      | Input.Graph.bad_input_exp (s1,s2) => 
                        (ErrorResponse { requestId = requestId, errorCode = "BADDATA",
                           message = ("The file is not a valid graph for the active theory ("^s1^" at "^s2^")") }, state))
        in
          case details of
            LoadEmptyGraphRequest =>
              let val (nm,state') = State.add_graph GraphName.default_name G.empty state in
                (NameResponse { requestId = requestId, name = GraphName.dest nm },state')
              end
          | LoadGraphFromFileRequest { fileName } =>
             (load_graph (XMLReader.read_from_file fileName)
              handle IO.Io {name : string, function : string, cause as (OS.SysErr (msg:string,_)) : exn} => 
                     (ErrorResponse { requestId = requestId, errorCode = "FILEACCESS",
                                      message = ("The file could not be read ("^msg^")") }, state)
                   | IO.Io {name : string, function : string, cause : exn} => 
                     (ErrorResponse { requestId = requestId, errorCode = "FILEACCESS",
                                      message = ("The file could not be read (unknown error in "^function^" of "^name^")") }, state))
          | LoadGraphFromDataRequest { data } =>
             load_graph (XMLReader.read_from_string data)
          | CopyGraphRequest { graphName } =>
             (let
                val gname = GraphName.mk graphName
                val otherGraph = State.get_graph state gname
                val (nm,state') = State.add_graph gname otherGraph state
              in
                (NameResponse { requestId = requestId, name = GraphName.dest nm },state')
              end
              handle State.unknown_graph_exp gr => (unknown_graph_resp requestId gr,state))
          | CopySubgraphRequest { source, graphName, vertexNames } =>
             (OkResponse { requestId = requestId },
              State.copy_subgraph (GraphName.mk source) (GraphName.mk graphName) (map V.mk vertexNames) state)
        end

    | serviceRequest' (SaveGraphToFileRequest { requestId, graphName, fileName }) state =
       (let
          val graph = get_graph graphName state
          val () = ((XMLWriter.write_to_file fileName) o Output.Graph.output) graph
        in
          (OkResponse { requestId = requestId }, state)
        end
        handle IO.Io {name : string, function : string, cause as (OS.SysErr (msg:string,_)) : exn} => 
               (ErrorResponse { requestId = requestId, errorCode = "FILEACCESS",
                                message = ("The file could not be written to ("^msg^")") }, state)
             | IO.Io {name : string, function : string, cause : exn} => 
               (ErrorResponse { requestId = requestId, errorCode = "FILEACCESS",
                                message = ("The file could not be written to (unknown error in "^function^" of "^name^")") }, state))

    | serviceRequest' (RenameGraphRequest { requestId, oldName, newName }) state =
        let val (nm,state') = State.rename_graph (GraphName.mk oldName) (GraphName.mk newName) state in
          (NameResponse { requestId = requestId, name = GraphName.dest nm },state')
        end

    | serviceRequest' (DiscardGraphRequest { requestId, graphName }) state =
        (OkResponse { requestId = requestId }, State.kill_graph (GraphName.mk graphName) state)

    | serviceRequest' (ExportGraphRequest { requestId, graphName, format }) state =
        let val graph = get_graph graphName state in
          if format = NativeGraphFormat then
              let val graph_data = (XMLWriter.write_to_string o Output.Graph.output) graph in
                (DataResponse { requestId = requestId, data = graph_data}, state)
              end
          else if format = XmlGraphFormat then
              (XmlResponse { requestId = requestId, data = Output.Graph.output graph}, state)
          else if format = PrettyGraphFormat then
              (PrettyResponse { requestId = requestId, data = G.pretty graph}, state)
          else (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)
        end
(*
fun hilb ([format], graph) =
    (DATA (Pretty.string_of (
     RGHilbTerm.pretty (AlgFormat.of_string format)
              (RGHilbTerm.of_graph graph)))
    handle AlgFormat.not_supported_exp =>
       raise State.user_exp "Term format not supported.")
  | hilb (_, graph) = raise wrong_args_exp ()
*)

    | serviceRequest' (DescribeGraphRequest { requestId, graphName, desc }) state =
       (case desc of
          DescribeFullGraph =>
            (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)
        | ListVertices =>
            (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)
        | ListEdges =>
            (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)
        | ListBangBoxes =>
            (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)
        | VertexData { vertexName } =>
            (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)
        | VertexUserData { vertexName } =>
            (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)
        | EdgeData { edgeName } =>
            (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)
        | EdgeUserData { edgeName } =>
            (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)
        | BangBoxVertices { bangBoxName } =>
            (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)
        | BangBoxUserData { bangBoxName } =>
            (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)
        )

    | serviceRequest' (EditGraphRequest { requestId, graphName, action }) state =
       (case action of
          UndoAction =>
            (OkResponse { requestId = requestId },
             State.undo (GraphName.mk graphName) state)
        | RedoAction =>
            (OkResponse { requestId = requestId },
             State.redo (GraphName.mk graphName) state)
        | InsertGraphAction { sourceGraphName } =>
            (OkResponse { requestId = requestId },
             State.insert_graph (GraphName.mk sourceGraphName) (GraphName.mk graphName) state)
        | AddVertexAction { vertexType, resultFormat } =>
            let
              val (v as (vn,_),state') = State.add_vertex vertexType (GraphName.mk graphName) state
            in
              case resultFormat
                of NameFormat => (NameResponse { requestId = requestId, name = V.dest vn }, state')
                 | XmlFormat => (XmlResponse { requestId = requestId, data = Output.Vertex.output v }, state')
                 | PrettyFormat => (PrettyResponse { requestId = requestId, data = pretty_vert v }, state')
                 | DescriptionFormat => 
              (* VertexDescriptionResponse *)
              (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state')
            end
        | RenameVertexAction { oldName, newName } =>
            (OkResponse { requestId = requestId },
             State.rename_vertex (V.mk oldName) (V.mk newName) (GraphName.mk graphName) state)
        | DeleteVerticesAction { vertexNames } =>
            (OkResponse { requestId = requestId },
             State.delete_vertices (map V.mk vertexNames) (GraphName.mk graphName) state)
        | SetVertexDataAction { vertexName, vertexData } =>
           (let
              val updater =
                case vertexData
                  of (StringCData s)      => VCD.update_from_string s
                   | (XmlCData xml)       => VCD.update_from_xml xml
                   | (DescriptionCData d) => VCD.update (K d)
            in
              (OkResponse { requestId = requestId },
               State.update_vertex_data updater (V.mk vertexName) (GraphName.mk graphName) state)
            end
            handle (ComponentData.bad_format_exp s) =>
                      (ErrorResponse { requestId = requestId, errorCode = "BADFORMAT", message = s }, state))
        | SetVertexUserDataAction { vertexName, userData } =>
            (* OkResponse *)
            (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)
        | AddEdgeAction { edgeType, directed, source, target, resultFormat } =>
            let
              val dir_or_undir = if directed then G.Directed else G.UnDirected
              val (e as (en,_),state') = State.add_edge edgeType dir_or_undir (V.mk source) (V.mk target) (GraphName.mk graphName) state
            in
              case resultFormat
                of NameFormat => (NameResponse { requestId = requestId, name = E.dest en }, state')
                 | XmlFormat => (XmlResponse { requestId = requestId, data = Output.Edge.output e }, state')
                 | PrettyFormat => (PrettyResponse { requestId = requestId, data = pretty_edge e }, state')
                 | DescriptionFormat => 
              (* EdgeDescriptionResponse *)
              (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state')
            end
        | RenameEdgeAction { oldName, newName } =>
            (* OkResponse *)
            (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)
        | DeleteEdgesAction { edgeNames } =>
            (OkResponse { requestId = requestId },
             State.delete_edges (map E.mk edgeNames) (GraphName.mk graphName) state)
        | SetEdgeDataAction { edgeName, edgeData } =>
           (let
              val updater =
                case edgeData
                  of (StringCData s)      => ECD.update_from_string s
                   | (XmlCData xml)       => ECD.update_from_xml xml
                   | (DescriptionCData d) => ECD.update (K d)
            in
              (OkResponse { requestId = requestId },
               State.update_edge_data updater (E.mk edgeName) (GraphName.mk graphName) state)
            end
            handle (ComponentData.bad_format_exp s) =>
                      (ErrorResponse { requestId = requestId, errorCode = "BADFORMAT", message = s }, state))
        | SetEdgeUserDataAction { edgeName, userData } =>
            (* OkResponse *)
            (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)
        | AddBangBoxAction { vertexNames, resultFormat } =>
            let
              val gname = (GraphName.mk graphName)
              val (nm,state') = State.add_bang gname state
              val state'' = if vertexNames = [] then state' else
                               State.bang_vertices nm (map V.mk vertexNames) gname state
            in
              case resultFormat
                of NameFormat => (NameResponse { requestId = requestId, name = B.dest nm }, state'')
                 | _ => (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state'')
            end
        | RenameBangBoxAction { oldName, newName } =>
            (* OkResponse *)
            (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)
        | DropBangBoxesAction { bangBoxNames } =>
            (OkResponse { requestId = requestId },
             State.drop_bang (map B.mk bangBoxNames) (GraphName.mk graphName) state)
        | KillBangBoxesAction { bangBoxNames } =>
            (OkResponse { requestId = requestId },
             State.kill_bang (map B.mk bangBoxNames) (GraphName.mk graphName) state)
        | DuplicateBangBoxAction { bangBoxName } =>
            let val (nm,state') = State.duplicate_bang (B.mk bangBoxName) (GraphName.mk graphName) state in
              (NameResponse { requestId = requestId, name = B.dest nm }, state')
            end
        | MergeBangBoxesAction { bangBoxNames } =>
            let val (nm,state') = State.merge_bang (map B.mk bangBoxNames) (GraphName.mk graphName) state in
              (NameResponse { requestId = requestId, name = B.dest nm }, state')
            end
        | BangVerticesAction { bangBoxName, vertexNames } =>
            (OkResponse { requestId = requestId },
             State.bang_vertices (B.mk bangBoxName) (map V.mk vertexNames) (GraphName.mk graphName) state)
        | UnbangVerticesAction { vertexNames } =>
            (OkResponse { requestId = requestId },
             State.unbang_vertices (map V.mk vertexNames) (GraphName.mk graphName) state)
        | SetBangBoxUserDataAction { bangBoxName, userData } =>
            (* OkResponse *)
            (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)
        )


    (* Rulesets *)
    | serviceRequest' (ImportRulesetFromFileRequest { requestId, fileName }) state =
       (load_ruleset requestId (XMLReader.read_from_file fileName) state
        handle IO.Io {name : string, function : string, cause as (OS.SysErr (msg:string,_)) : exn} => 
               (ErrorResponse { requestId = requestId, errorCode = "FILEACCESS",
                                message = ("The file could not be read ("^msg^")") }, state)
             | IO.Io {name : string, function : string, cause : exn} => 
               (ErrorResponse { requestId = requestId, errorCode = "FILEACCESS",
                                message = ("The file could not be read (unknown error in "^function^" of "^name^")") }, state))

    | serviceRequest' (ImportRulesetFromDataRequest { requestId, data }) state =
        load_ruleset requestId (XMLReader.read_from_string data) state

    | serviceRequest' (ExportRulesetToFileRequest { requestId, fileName }) state =
       (let
          val () = ((XMLWriter.write_to_file fileName) o Output.Ruleset.output o State.get_rset) state
        in
          (OkResponse { requestId = requestId }, state)
        end
        handle IO.Io {name : string, function : string, cause as (OS.SysErr (msg:string,_)) : exn} => 
               (ErrorResponse { requestId = requestId, errorCode = "FILEACCESS",
                                message = ("The file could not be written to ("^msg^")") }, state)
             | IO.Io {name : string, function : string, cause : exn} => 
               (ErrorResponse { requestId = requestId, errorCode = "FILEACCESS",
                                message = ("The file could not be written to (unknown error in "^function^" of "^name^")") }, state))

    | serviceRequest' (ExportRulesetAsDataRequest { requestId }) state =
        let val get_rset_data = XMLWriter.write_to_string o Output.Ruleset.output o State.get_rset in
          (DataResponse { requestId = requestId, data = get_rset_data state }, state)
        end


    (* Rules *)
    | serviceRequest' (ListRulesRequest { requestId }) state =
        let val rules = (State.list_allrules  state) in
          (NameListResponse { requestId = requestId, names = map RuleName.dest rules }, state)
        end

    | serviceRequest' (ListActiveRulesRequest { requestId }) state =
        let val rules = (State.list_active_rules  state) in
          (NameListResponse { requestId = requestId, names = map RuleName.dest rules }, state)
        end

    | serviceRequest' (ListRulesByTagRequest { requestId, tagName }) state =
        let val rules = (State.list_rules_with_tag (TagName.mk tagName) state) in
          (NameListResponse { requestId = requestId, names = map RuleName.dest rules }, state)
        end

    | serviceRequest' (OpenRuleLhsRequest { requestId, ruleName }) state =
        let val (nm,state') = (State.open_rule_lhs state (RuleName.mk ruleName)) in
          (NameResponse { requestId = requestId, name = GraphName.dest nm }, state')
        end

    | serviceRequest' (OpenRuleRhsRequest { requestId, ruleName }) state =
        let val (nm,state') = (State.open_rule_rhs state (RuleName.mk ruleName)) in
          (NameResponse { requestId = requestId, name = GraphName.dest nm }, state')
        end

    | serviceRequest' (SetRuleRequest { requestId, ruleName, lhsGraphName, rhsGraphName }) state =
        (OkResponse { requestId = requestId },
         State.set_rule (RuleName.mk ruleName) (GraphName.mk lhsGraphName) (GraphName.mk rhsGraphName) state)

    | serviceRequest' (DeleteRuleRequest { requestId, ruleName }) state =
        (OkResponse { requestId = requestId },
         State.delete_rule (RuleName.mk ruleName) state)

    | serviceRequest' (ActivateRuleRequest { requestId, ruleName }) state =
        (OkResponse { requestId = requestId },
         State.activate_rule (RuleName.mk ruleName) state)

    | serviceRequest' (DeactivateRuleRequest { requestId, ruleName }) state =
        (OkResponse { requestId = requestId },
         State.deactivate_rule (RuleName.mk ruleName) state)


    (* Tags *)
    | serviceRequest' (ListTagsRequest { requestId }) state =
        let val tags = (State.list_tags  state) in
          (NameListResponse { requestId = requestId, names = map TagName.dest tags }, state)
        end

    | serviceRequest' (TagRuleRequest { requestId, ruleName, tagName }) state =
        (OkResponse { requestId = requestId },
         State.tag_rule (RuleName.mk ruleName) (TagName.mk tagName) state)

    | serviceRequest' (UntagRuleRequest { requestId, ruleName, tagName }) state =
        (OkResponse { requestId = requestId },
         State.untag_rule (RuleName.mk ruleName) (TagName.mk tagName) state)

    | serviceRequest' (ForgetTagRequest { requestId, tagName }) state =
        (OkResponse { requestId = requestId },
         State.delete_tag (TagName.mk tagName) state)

    | serviceRequest' (DeleteRulesByTagRequest { requestId, tagName }) state =
        (OkResponse { requestId = requestId },
         State.delete_tag_and_its_rules (TagName.mk tagName) state)

    | serviceRequest' (ActivateRulesByTagRequest { requestId, tagName }) state =
        (OkResponse { requestId = requestId },
         State.activate_rules_with_tag (TagName.mk tagName) state)

    | serviceRequest' (DeactivateRulesByTagRequest { requestId, tagName }) state =
        (OkResponse { requestId = requestId },
         State.deactivate_rules_with_tag (TagName.mk tagName) state)


    (* Rewriting *)
    | serviceRequest' (AttachRewritesRequest { requestId, graphName, vertexNames }) state =
        (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)

    | serviceRequest' (AttachOneRewriteRequest { requestId, graphName, vertexNames }) state =
        (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)

    | serviceRequest' (ListAttachedRewritesRequest { requestId, graphName }) state =
        (ErrorResponse { requestId = requestId, errorCode = "NOTIMP", message = "Not implemented" }, state)

    | serviceRequest' (ApplyAttachedRewriteRequest { requestId, graphName, index: int }) state =
        (OkResponse { requestId = requestId },
         State.apply_rewrite (GraphName.mk graphName) index state)

    | serviceRequest' (UnknownRequest { requestId, code }) state =
        (UnknownResponse { requestId = requestId, code = code },state)
    ;

    fun serviceRequest req =
      let
        val (resp,newstate) = serviceRequest' req (!cur_state)
        val () = cur_state := newstate
      in
        resp
      end
end;

(* vi:ts=2:sw=2:sts=2:et
*)
