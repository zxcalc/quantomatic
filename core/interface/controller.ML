(**********************************************************************)
(* Provides commands for interfaces to the core                       *)
(*                                                                    *)
(* Commands defines those commands.                                   *)
(* Controller implements those commands, using a CONTROLLER_STATE     *)
(*   to provide the running state.  Each Controller implements a      *)
(*   different theory.                                                *)
(* ControllerRegistry provides a registry of all known Controllers.   *)
(*   The Controller functor automatically registers its               *)
(*   serviceRequest function with the ControllerRegistry.             *)
(*                                                                    *)
(**********************************************************************)

structure Commands =
struct
  type vertexDesc = { name: string, vType: string, data: ComponentData.T, userData: string };
  type edgeDesc = { name: string, eType: string, sourceName: string, targetName: string, directed: bool, data: ComponentData.T, userData: string };
  type bangBoxDesc = { name: string, vertexNames: string list };
  type graphDesc = { name: string, vertices: vertexDesc list, edges: edgeDesc list, bangBoxen: bangBoxDesc list };
  type rewriteDesc = { ruleName: string, result: graphDesc, lhs: graphDesc, rhs: graphDesc };
  (* BasicFormat is the name of a graph or component, and the string value of
   * component data *)
  datatype resultFormat = BasicFormat | PrettyFormat | XmlFormat | DescriptionFormat;
  datatype componentData = StringCData of string
                         | XmlCData of (XML.tree list)
                         | DescriptionCData of ComponentData.T;
  datatype graphData = PrettyGData of Pretty.T
                     | XmlGData of XML.tree
                     | DescriptionGData of graphDesc;

                     (* A dummy request does not change the state, and simply produces the given response *)
  datatype request = DummyRequest of { response: response }

                   | TheoryNameRequest
                     (* StringListResponse *)

                   (*** Graph Commands ***)
                   | ListGraphsRequest
                     (* StringListResponse *)
                   | LoadGraphRequest of { details: graphLoadRequest }
                     (* StringResponse | ErrorResponse *)
                   | SaveGraphToFileRequest of { graphName: string, fileName: string }
                     (* OkResponse | ErrorResponse *)
                   | RenameGraphRequest of { oldName: string, newName: string }
                     (* StringResponse | ErrorResponse *)
                   | DiscardGraphRequest of { graphName: string }
                     (* OkResponse | ErrorResponse *)
                   | ExportGraphRequest of { graphName: string, format: graphFormat }
                     (* DataResponse | ErrorResponse *)
                   | DescribeGraphRequest of { graphName: string, desc: graphDescribeAction }
                   | EditGraphRequest of { graphName: string, action: graphEditAction }

                   (*** Ruleset commands ***)
                   | ImportRulesetFromFileRequest of { fileName: string }
                     (* OkResponse | ErrorResponse *)
                   | ImportRulesetFromDataRequest of { data: string }
                     (* OkResponse | ErrorResponse *)
                   | ExportRulesetToFileRequest of { fileName: string }
                     (* OkResponse | ErrorResponse *)
                   | ExportRulesetAsDataRequest
                     (* DataResponse | ErrorResponse *)

                   (*** Rule commands ***)
                   | ListRulesRequest
                     (* StringListResponse *)
                   | ListActiveRulesRequest
                     (* StringListResponse *)
                   | ListRulesByTagRequest of { tagName: string }
                     (* StringListResponse | ErrorResponse *)
                   | OpenRuleLhsRequest of { ruleName: string }
                     (* StringResponse | ErrorResponse *)
                   | OpenRuleRhsRequest of { ruleName: string }
                     (* StringResponse | ErrorResponse *)
                   | SetRuleRequest of { ruleName: string, lhsGraphName: string, rhsGraphName: string }
                     (* OkResponse | ErrorResponse *)
                   | DeleteRuleRequest of { ruleName: string }
                     (* OkResponse | ErrorResponse *)
                   | ActivateRuleRequest of { ruleName: string }
                     (* OkResponse | ErrorResponse *)
                   | DeactivateRuleRequest of { ruleName: string }
                     (* OkResponse | ErrorResponse *)

                   (*** Tag commands ***)
                   | ListTagsRequest
                     (* StringListResponse *)
                   | TagRuleRequest of { ruleName: string, tagName: string }
                     (* OkResponse | ErrorResponse *)
                   | UntagRuleRequest of { ruleName: string, tagName: string }
                     (* OkResponse | ErrorResponse *)
                   | ForgetTagRequest of { tagName: string }
                     (* OkResponse | ErrorResponse *)
                   | DeleteRulesByTagRequest of { tagName: string }
                     (* OkResponse | ErrorResponse *)
                   | ActivateRulesByTagRequest of { tagName: string }
                     (* OkResponse | ErrorResponse *)
                   | DeactivateRulesByTagRequest of { tagName: string }
                     (* OkResponse | ErrorResponse *)

                   (*** Rewrite commands ***)
                   | AttachRewritesRequest of { graphName: string, vertexNames: string list }
                   | AttachOneRewriteRequest of { graphName: string, vertexNames: string list }
                   | ListAttachedRewritesRequest of { graphName: string, format: resultFormat }
                   | ApplyAttachedRewriteRequest of { graphName: string, index: int }

                   | UnknownRequest of { code: string }

       and graphLoadRequest = LoadEmptyGraphRequest of { requestedName: string }
                            | LoadGraphFromFileRequest of { fileName: string }
                            | LoadGraphFromDataRequest of { data: string, requestedName: string }
                            | CopyGraphRequest of { graphName: string }
                            | CopySubgraphRequest of { source: string, graphName: string, vertexNames: string list }

                       (* Native format: suitable for saving to a file and loading again *)
       and graphFormat = NativeGraphFormat
                       | XmlGraphFormat
                       | PrettyGraphFormat
                       | HilbertSpaceGraphFormat
                       | MathematicaGraphFormat
                       | MatlabGraphFormat
                       | TikzGraphFormat

       and graphDescribeAction = DescribeFullGraph
                                 (* GraphDescriptionResponse | ErrorResponse *)
                               | ListVertices
                                 (* StringListResponse | ErrorResponse *)
                               | ListEdges
                                 (* StringListResponse | ErrorResponse *)
                               | ListBangBoxes
                                 (* StringListResponse | ErrorResponse *)
                               | VertexData of { vertexName: string, format: resultFormat }
                                 (* ComponentDataResponse | ErrorResponse *)
                               | VertexUserData of { vertexName: string }
                                 (* UserDataResponse | ErrorResponse *)
                               | EdgeData of { edgeName: string, format: resultFormat }
                                 (* ComponentDataResponse | ErrorResponse *)
                               | EdgeUserData of { edgeName: string }
                                 (* UserDataResponse | ErrorResponse *)
                               | BangBoxVertices of { bangBoxName: string }
                                 (* StringListResponse | ErrorResponse *)
                               | BangBoxUserData of { bangBoxName: string }
                                 (* UserDataResponse | ErrorResponse *)

       and graphEditAction = UndoAction
                             (* OkResponse | ErrorResponse *)
                           | RedoAction
                             (* OkResponse | ErrorResponse *)
                           | InsertGraphAction of { sourceGraphName: string }
                             (* OkResponse | ErrorResponse *)
                           | AddVertexAction of { vertexType: string, resultFormat: resultFormat }
                             (* VertexDescriptionAction | ErrorResponse *)
                           | RenameVertexAction of { oldName: string, newName: string }
                             (* StringResponse | ErrorResponse *)
                           | DeleteVerticesAction of { vertexNames: string list }
                             (* OkResponse | ErrorResponse *)
                           | SetVertexDataAction of { vertexName: string, vertexData: componentData }
                             (* OkResponse | ErrorResponse *)
                           | SetVertexUserDataAction of { vertexName: string, userData: string }
                             (* OkResponse | ErrorResponse *)
                           | AddEdgeAction of { edgeType: string, directed: bool, source: string, target: string, resultFormat: resultFormat }
                             (* EdgeDescriptionAction | ErrorResponse *)
                           | RenameEdgeAction of { oldName: string, newName: string }
                             (* StringResponse | ErrorResponse *)
                           | DeleteEdgesAction of { edgeNames: string list }
                             (* OkResponse | ErrorResponse *)
                           | SetEdgeDataAction of { edgeName: string, edgeData: componentData }
                             (* OkResponse | ErrorResponse *)
                           | SetEdgeUserDataAction of { edgeName: string, userData: string }
                             (* OkResponse | ErrorResponse *)
                           | AddBangBoxAction of { vertexNames: string list, resultFormat: resultFormat }
                             (* StringResponse | ErrorResponse *)
                           | RenameBangBoxAction of { oldName: string, newName: string }
                             (* OkResponse | ErrorResponse *)
                           | DropBangBoxesAction of { bangBoxNames: string list }
                             (* OkResponse | ErrorResponse *)
                           | KillBangBoxesAction of { bangBoxNames: string list }
                             (* OkResponse | ErrorResponse *)
                           | DuplicateBangBoxAction of { bangBoxName: string }
                             (* StringResponse | ErrorResponse *)
                           | MergeBangBoxesAction of { bangBoxNames: string list }
                             (* StringResponse | ErrorResponse *)
                           | BangVerticesAction of { bangBoxName: string, vertexNames: string list }
                             (* OkResponse | ErrorResponse *)
                           | UnbangVerticesAction of { vertexNames: string list }
                             (* OkResponse | ErrorResponse *)
                           | SetBangBoxUserDataAction of { bangBoxName: string, userData: string }
                             (* OkResponse | ErrorResponse *)

       and response = OkResponse
                    | ErrorResponse of { errorCode: string, message: string }

                    | StringResponse of { data: string }
                    | StringListResponse of { data: string list }

                    | DataResponse of { data: string }
                    | PrettyResponse of { data: Pretty.T }
                    | XmlResponse of { data: XML.tree }
                    | CountResponse of { count: int }
                    | UserDataResponse of { data: string }

                    | GraphDescriptionResponse of { graph: graphDesc }
                    | VertexDescriptionResponse of { vertex: vertexDesc }
                    | EdgeDescriptionResponse of { edge: edgeDesc }
                    | BangBoxDescriptionResponse of { bangBox: bangBoxDesc }
                    | ComponentDataResponse of { data: ComponentData.T }
                    | RewriteListResponse of { rewrites: rewriteDesc list }

                    | UnknownResponse of { code: string }
                    ;
  type requestMessage = (string * request);
  type responseMessage = (string * response);

  fun serviceRequestMessage
      (serviceRequest : request -> response)
      ((requestId,req) : requestMessage)
      : responseMessage
    = (requestId,serviceRequest req);
end;


signature CONTROLLER_REGISTRY
= sig
  val register_eval_func : string -> (Commands.request -> Commands.response) -> unit;
  val get_eval_func : string -> (Commands.request -> Commands.response) option;
end;

structure ControllerRegistry : CONTROLLER_REGISTRY
= struct
  val eval_funcs = Unsynchronized.ref Symtab.empty;

  fun register_eval_func theory_name func =
        eval_funcs := Symtab.update_new (theory_name,func) (!eval_funcs)

  fun get_eval_func name = Symtab.lookup (!eval_funcs) name
end;


signature CONTROLLER =
sig
  val serviceRequest : Commands.request -> Commands.response;
  val theoryName : string;
end;


functor Controller(Theory : GRAPHICAL_THEORY) : CONTROLLER =
struct
  structure State = ControllerState(Theory);
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

  val theoryName = State.Theory.theory_name;

  fun get_graph (name:string) (state:State.T) : G.T = State.get_graph state (GraphName.mk name)

  fun load_ruleset reader source state =
     ((OkResponse,
       State.update_rset (Ruleset.merge (Input.Ruleset.input (reader source))) state)
      handle XML_Parse_Utils.bad_input_exp (s1,s2) =>
             (ErrorResponse { errorCode = "BADDATA",
                message = ("The file is not a valid ruleset for the active theory ("^s1^" at "^s2^")") }, state)
           | Input.Ruleset.bad_input_exp (s1,s2) =>
             (ErrorResponse { errorCode = "BADDATA",
                message = ("The file is not a valid ruleset for the active theory ("^s1^" at "^s2^")") }, state)
           | XMLReader.bad_input_exp msg =>
             (ErrorResponse { errorCode = "BADDATA",
                message = ("The file is not a valid ruleset for the active theory ("^msg^")") }, state))

  fun describe_vertex (n, (nd,_)) =
    { name = V.dest n, vType = VCD.type_of nd,
      data = VCD.cdata_of nd, userData = "" }

  fun describe_edge (en, ((bd,ed), (n1, n2))) =
    { name = E.dest en, eType = ECD.type_of ed,
      sourceName = V.dest n1, targetName = V.dest n2,
      directed = (bd = G.Directed), data = ECD.cdata_of ed,
      userData = "" }

  fun describe_bangbox (bn, vs) =
    { name = B.dest bn, vertexNames = map V.dest (V.NSet.list_of vs) }

  fun describe_graph (n, graph) =
    let
      val vert_list = V.NTab.list_of (G.get_vertices graph)
      val edge_list = E.NTab.list_of (G.get_edges graph)
      val bbox_list = G.VtoBoxMap.codf_list_of (G.get_bboxes graph)
    in
      { name = n,
        vertices = map describe_vertex vert_list,
        edges = map describe_edge edge_list,
        bangBoxen = map describe_bangbox bbox_list }
    end

  fun describe_rewrite ((rn, rule), result) =
    let
      val lhs = State.Theory.Rule.get_lhs rule
      val rhs = State.Theory.Rule.get_rhs rule
    in
      { ruleName = RuleName.dest rn, result = describe_graph ("",result),
        lhs = describe_graph ("",lhs), rhs = describe_graph ("",rhs) }
    end

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

    | serviceRequest' TheoryNameRequest state =
        (StringResponse { data = State.Theory.theory_name }, state)

    | serviceRequest' (ListGraphsRequest) (state:State.T) =
        let val graphNames = map GraphName.dest (State.get_graph_names state) in
          (StringListResponse { data = graphNames }, state)
        end

    | serviceRequest' (LoadGraphRequest { details }) state =
        let fun load_graph suggestedName reader data =
                (let
                   val graph = (Input.Graph.input (reader data))
                   val name = if suggestedName = "" then GraphName.default_name else GraphName.mk suggestedName
                   val (nm,state') = State.add_graph name graph state
                 in
                   (StringResponse { data = GraphName.dest nm },state')
                 end
                 handle XML_Parse_Utils.bad_input_exp (s1,s2) =>
                        (ErrorResponse { errorCode = "BADDATA",
                           message = ("The file is not a valid graph for the active theory ("^s1^" at "^s2^")") }, state)
                      | Input.Graph.bad_input_exp (s1,s2) => 
                        (ErrorResponse { errorCode = "BADDATA",
                           message = ("The file is not a valid graph for the active theory ("^s1^" at "^s2^")") }, state)
                      | XMLReader.bad_input_exp msg =>
                        (ErrorResponse { errorCode = "BADDATA",
                           message = ("The file is not a valid graph for the active theory ("^msg^")") }, state))
        in
          case details of
            LoadEmptyGraphRequest { requestedName } =>
              let
                val tryName = if requestedName = ""
                              then GraphName.default_name
                              else GraphName.mk requestedName
                val (nm,state') = State.add_graph tryName G.empty state
              in
                (StringResponse { data = GraphName.dest nm },state')
              end
          | LoadGraphFromFileRequest { fileName } =>
             (load_graph (OS.Path.file fileName) XMLReader.read_from_file fileName
              handle IO.Io {name : string, function : string, cause as (OS.SysErr (msg:string,_)) : exn} => 
                     (ErrorResponse { errorCode = "FILEACCESS",
                                      message = ("The file could not be read ("^msg^")") }, state)
                   | IO.Io {name : string, function : string, cause : exn} => 
                     (ErrorResponse { errorCode = "FILEACCESS",
                                      message = ("The file could not be read (unknown error in "^function^" of "^name^")") }, state))
          | LoadGraphFromDataRequest { data, requestedName } =>
             load_graph requestedName XMLReader.read_from_string data
          | CopyGraphRequest { graphName } =>
             let
                val gname = GraphName.mk graphName
                val otherGraph = State.get_graph state gname
                val (nm,state') = State.add_graph gname otherGraph state
              in
                (StringResponse { data = GraphName.dest nm },state')
              end
          | CopySubgraphRequest { source, graphName, vertexNames } =>
             (OkResponse,
              State.copy_subgraph (GraphName.mk source) (GraphName.mk graphName) (map V.mk vertexNames) state)
        end

    | serviceRequest' (SaveGraphToFileRequest { graphName, fileName }) state =
       (let
          val graph = get_graph graphName state
          val () = ((XMLWriter.write_to_file fileName) o Output.Graph.output) graph
        in
          (OkResponse, state)
        end
        handle IO.Io {name : string, function : string, cause as (OS.SysErr (msg:string,_)) : exn} => 
               (ErrorResponse {
                  errorCode = "FILEACCESS",
                  message = ("The file could not be written to ("^msg^")")
                }, state)
             | IO.Io {name : string, function : string, cause : exn} => 
               (ErrorResponse {
                  errorCode = "FILEACCESS",
                  message = ("The file could not be written to (unknown error in "^function^" of "^name^")")
                }, state))

    | serviceRequest' (RenameGraphRequest { oldName, newName }) state =
        let val (nm,state') = State.rename_graph (GraphName.mk oldName) (GraphName.mk newName) state in
          (StringResponse { data = GraphName.dest nm },state')
        end

    | serviceRequest' (DiscardGraphRequest { graphName }) state =
        (OkResponse, State.kill_graph (GraphName.mk graphName) state)

    | serviceRequest' (ExportGraphRequest { graphName, format }) state =
        let val graph = get_graph graphName state in
          if format = NativeGraphFormat then
              let val graph_data = (XMLWriter.write_to_string o Output.Graph.output) graph in
                (DataResponse { data = graph_data}, state)
              end
          else if format = XmlGraphFormat then
              (XmlResponse { data = Output.Graph.output graph}, state)
          else if format = PrettyGraphFormat then
              (PrettyResponse { data = G.pretty graph}, state)
          else (ErrorResponse { errorCode = "NOTIMP", message = "Not implemented" }, state)
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

    | serviceRequest' (DescribeGraphRequest { graphName, desc }) state =
       (let
          val graph = get_graph graphName state
        in
          ((case desc
              of DescribeFullGraph =>
                    GraphDescriptionResponse {
                      graph = describe_graph (graphName, graph)
                    }
               | ListVertices =>
                    StringListResponse {
                      data = (map V.dest (V.NSet.list_of (G.get_vnames graph)))
                    }
               | ListEdges =>
                    StringListResponse {
                      data = (map E.dest (E.NSet.list_of (G.get_enames graph)))
                    }
               | ListBangBoxes =>
                    StringListResponse {
                      data = (map B.dest (G.get_bbox_list graph))
                    }
               | VertexData { vertexName, format } =>
                    let
                      val vdata = G.get_vertex_data graph (V.mk vertexName)
                    in
                      case format
                        of BasicFormat =>
                              let
                                val (_,data_string) = VCD.type_and_string_of vdata
                              in
                                StringResponse {
                                  data = data_string
                                }
                              end
                         | PrettyFormat =>
                              PrettyResponse {
                                data = V.pretty_data vdata
                              }
                         | XmlFormat =>
                              let
                                val (_,data_xml_list) = VCD.type_and_xml_of vdata
                              in
                                XmlResponse {
                                  data = (XML.Elem (("data",[]),data_xml_list))
                                }
                              end
                         | DescriptionFormat =>
                              ComponentDataResponse {
                                data = VCD.cdata_of vdata
                              }
                    end
               | VertexUserData { vertexName } =>
                    ErrorResponse { errorCode = "NOTIMP", message = "Not implemented" }
               | EdgeData { edgeName, format } =>
                    let
                      val edata = G.get_edge_data graph (E.mk edgeName)
                    in
                      case format
                        of BasicFormat =>
                              let
                                val (_,data_string) = ECD.type_and_string_of edata
                              in
                                StringResponse {
                                  data = data_string
                                }
                              end
                         | PrettyFormat =>
                              PrettyResponse {
                                data = E.pretty_data edata
                              }
                         | XmlFormat =>
                              let
                                val (_,data_xml_list) = ECD.type_and_xml_of edata
                              in
                                XmlResponse {
                                  data = (XML.Elem (("data",[]),data_xml_list))
                                }
                              end
                         | DescriptionFormat =>
                              ComponentDataResponse {
                                data = ECD.cdata_of edata
                              }
                    end
               | EdgeUserData { edgeName } =>
                    ErrorResponse { errorCode = "NOTIMP", message = "Not implemented" }
               | BangBoxVertices { bangBoxName } =>
                    StringListResponse {
                      data = (map V.dest (V.NSet.list_of (G.get_bbox graph (B.mk bangBoxName))))
                    }
               | BangBoxUserData { bangBoxName } =>
                    ErrorResponse { errorCode = "NOTIMP", message = "Not implemented" }
               ), state)
        end
        handle G.no_such_vname_exp (_,vname,_) =>
                raise State.no_such_vertex_exp (vname,GraphName.mk graphName)
             | G.no_such_ename_exp (_,ename,_) =>
                raise State.no_such_edge_exp (ename,GraphName.mk graphName)
        )

    | serviceRequest' (EditGraphRequest { graphName, action }) state =
       (case action of
          UndoAction =>
            (OkResponse,
             State.undo (GraphName.mk graphName) state)
        | RedoAction =>
            (OkResponse,
             State.redo (GraphName.mk graphName) state)
        | InsertGraphAction { sourceGraphName } =>
            (OkResponse,
             State.insert_graph (GraphName.mk sourceGraphName) (GraphName.mk graphName) state)
        | AddVertexAction { vertexType, resultFormat } =>
            let
              val (v as (vn,_),state') = State.add_vertex vertexType (GraphName.mk graphName) state
            in
              case resultFormat
                of BasicFormat => (StringResponse { data = V.dest vn }, state')
                 | XmlFormat => (XmlResponse { data = Output.Vertex.output v }, state')
                 | PrettyFormat => (PrettyResponse { data = pretty_vert v }, state')
                 | DescriptionFormat =>
                     (VertexDescriptionResponse {
                        vertex = describe_vertex v
                      }, state')
            end
        | RenameVertexAction { oldName, newName } =>
            (OkResponse,
             State.rename_vertex (V.mk oldName) (V.mk newName) (GraphName.mk graphName) state)
        | DeleteVerticesAction { vertexNames } =>
            (OkResponse,
             State.delete_vertices (map V.mk vertexNames) (GraphName.mk graphName) state)
        | SetVertexDataAction { vertexName, vertexData } =>
           (let
              val updater =
                case vertexData
                  of (StringCData s)      => VCD.update_from_string s
                   | (XmlCData xml)       => VCD.update_from_xml xml
                   | (DescriptionCData d) => VCD.update (K d)
            in
              (OkResponse,
               State.update_vertex_data updater (V.mk vertexName) (GraphName.mk graphName) state)
            end
            handle (ComponentData.bad_format_exp s) =>
                      (ErrorResponse { errorCode = "BADDATA", message = s }, state))
        | SetVertexUserDataAction { vertexName, userData } =>
            (* OkResponse *)
            (ErrorResponse { errorCode = "NOTIMP", message = "Not implemented" }, state)
        | AddEdgeAction { edgeType, directed, source, target, resultFormat } =>
            let
              val dir_or_undir = if directed then G.Directed else G.UnDirected
              val (e as (en,_),state') = State.add_edge edgeType dir_or_undir (V.mk source) (V.mk target) (GraphName.mk graphName) state
            in
              case resultFormat
                of BasicFormat => (StringResponse { data = E.dest en }, state')
                 | XmlFormat => (XmlResponse { data = Output.Edge.output e }, state')
                 | PrettyFormat => (PrettyResponse { data = pretty_edge e }, state')
                 | DescriptionFormat => 
                     (EdgeDescriptionResponse {
                        edge = describe_edge e
                      }, state')
            end
        | RenameEdgeAction { oldName, newName } =>
            (OkResponse,
             State.rename_edge (E.mk oldName) (E.mk newName) (GraphName.mk graphName) state)
        | DeleteEdgesAction { edgeNames } =>
            (OkResponse,
             State.delete_edges (map E.mk edgeNames) (GraphName.mk graphName) state)
        | SetEdgeDataAction { edgeName, edgeData } =>
           (let
              val updater =
                case edgeData
                  of (StringCData s)      => ECD.update_from_string s
                   | (XmlCData xml)       => ECD.update_from_xml xml
                   | (DescriptionCData d) => ECD.update (K d)
            in
              (OkResponse,
               State.update_edge_data updater (E.mk edgeName) (GraphName.mk graphName) state)
            end
            handle (ComponentData.bad_format_exp s) =>
                      (ErrorResponse { errorCode = "BADFORMAT", message = s }, state))
        | SetEdgeUserDataAction { edgeName, userData } =>
            (* OkResponse *)
            (ErrorResponse { errorCode = "NOTIMP", message = "Not implemented" }, state)
        | AddBangBoxAction { vertexNames, resultFormat } =>
            let
              val gname = (GraphName.mk graphName)
              val (nm,state') = State.add_bang gname state
              val state'' = if vertexNames = [] then state' else
                               State.bang_vertices nm (map V.mk vertexNames) gname state'
            in
              case resultFormat
                of BasicFormat => (StringResponse { data = B.dest nm }, state'')
                 | PrettyFormat => (ErrorResponse { errorCode = "NOTIMP", message = "Not implemented" }, state)
                 | XmlFormat  =>  let
                                    val graph = (State.get_graph state'' gname)
                                    val verts = (G.get_bbox graph nm)
                                    val bbox  = (nm,verts)
                                  in
                                   (XmlResponse { data = Output.BBox.output bbox }, state'')
                                  end
                 | DescriptionFormat => let
                                          val graph = (State.get_graph state'' gname)
                                          val verts = (G.get_bbox graph nm)
                                          val bbox  = (nm,verts)
                                        in
                                         (BangBoxDescriptionResponse {
                                            bangBox = describe_bangbox (nm,(G.get_bbox graph nm))
                                          }, state'')
                                        end
            end
        | RenameBangBoxAction { oldName, newName } =>
            (OkResponse,
             State.rename_bang (B.mk oldName) (B.mk newName) (GraphName.mk graphName) state)
        | DropBangBoxesAction { bangBoxNames } =>
            (OkResponse,
             State.drop_bang (map B.mk bangBoxNames) (GraphName.mk graphName) state)
        | KillBangBoxesAction { bangBoxNames } =>
            (OkResponse,
             State.kill_bang (map B.mk bangBoxNames) (GraphName.mk graphName) state)
        | DuplicateBangBoxAction { bangBoxName } =>
            let val (nm,state') = State.duplicate_bang (B.mk bangBoxName) (GraphName.mk graphName) state in
              (StringResponse { data = B.dest nm }, state')
            end
        | MergeBangBoxesAction { bangBoxNames } =>
            let val (nm,state') = State.merge_bang (map B.mk bangBoxNames) (GraphName.mk graphName) state in
              (StringResponse { data = B.dest nm }, state')
            end
        | BangVerticesAction { bangBoxName, vertexNames } =>
            (OkResponse,
             State.bang_vertices (B.mk bangBoxName) (map V.mk vertexNames) (GraphName.mk graphName) state)
        | UnbangVerticesAction { vertexNames } =>
            (OkResponse,
             State.unbang_vertices (map V.mk vertexNames) (GraphName.mk graphName) state)
        | SetBangBoxUserDataAction { bangBoxName, userData } =>
            (* OkResponse *)
            (ErrorResponse { errorCode = "NOTIMP", message = "Not implemented" }, state)
        )


    (* Rulesets *)
    | serviceRequest' (ImportRulesetFromFileRequest { fileName }) state =
       (load_ruleset XMLReader.read_from_file fileName state
        handle IO.Io {name : string, function : string, cause as (OS.SysErr (msg:string,_)) : exn} => 
               (ErrorResponse { errorCode = "FILEACCESS",
                                message = ("The file could not be read ("^msg^")") }, state)
             | IO.Io {name : string, function : string, cause : exn} => 
               (ErrorResponse { errorCode = "FILEACCESS",
                                message = ("The file could not be read (unknown error in "^function^" of "^name^")") }, state))

    | serviceRequest' (ImportRulesetFromDataRequest { data }) state =
        load_ruleset XMLReader.read_from_string data state

    | serviceRequest' (ExportRulesetToFileRequest { fileName }) state =
       (let
          val () = ((XMLWriter.write_to_file fileName) o Output.Ruleset.output o State.get_rset) state
        in
          (OkResponse, state)
        end
        handle IO.Io {name : string, function : string, cause as (OS.SysErr (msg:string,_)) : exn} => 
               (ErrorResponse { errorCode = "FILEACCESS",
                                message = ("The file could not be written to ("^msg^")") }, state)
             | IO.Io {name : string, function : string, cause : exn} => 
               (ErrorResponse { errorCode = "FILEACCESS",
                                message = ("The file could not be written to (unknown error in "^function^" of "^name^")") }, state))

    | serviceRequest' (ExportRulesetAsDataRequest) state =
        let val get_rset_data = XMLWriter.write_to_string o Output.Ruleset.output o State.get_rset in
          (DataResponse { data = get_rset_data state }, state)
        end


    (* Rules *)
    | serviceRequest' (ListRulesRequest) state =
        let val rules = (State.list_allrules  state) in
          (StringListResponse { data = map RuleName.dest rules }, state)
        end

    | serviceRequest' (ListActiveRulesRequest) state =
        let val rules = (State.list_active_rules  state) in
          (StringListResponse { data = map RuleName.dest rules }, state)
        end

    | serviceRequest' (ListRulesByTagRequest { tagName }) state =
        let val rules = (State.list_rules_with_tag (TagName.mk tagName) state) in
          (StringListResponse { data = map RuleName.dest rules }, state)
        end

    | serviceRequest' (OpenRuleLhsRequest { ruleName }) state =
        let val (nm,state') = (State.open_rule_lhs state (RuleName.mk ruleName)) in
          (StringResponse { data = GraphName.dest nm }, state')
        end

    | serviceRequest' (OpenRuleRhsRequest { ruleName }) state =
        let val (nm,state') = (State.open_rule_rhs state (RuleName.mk ruleName)) in
          (StringResponse { data = GraphName.dest nm }, state')
        end

    | serviceRequest' (SetRuleRequest { ruleName, lhsGraphName, rhsGraphName }) state =
        (OkResponse,
         State.set_rule (RuleName.mk ruleName) (GraphName.mk lhsGraphName) (GraphName.mk rhsGraphName) state)

    | serviceRequest' (DeleteRuleRequest { ruleName }) state =
        (OkResponse,
         State.delete_rule (RuleName.mk ruleName) state)

    | serviceRequest' (ActivateRuleRequest { ruleName }) state =
        (OkResponse,
         State.activate_rule (RuleName.mk ruleName) state)

    | serviceRequest' (DeactivateRuleRequest { ruleName }) state =
        (OkResponse,
         State.deactivate_rule (RuleName.mk ruleName) state)


    (* Tags *)
    | serviceRequest' (ListTagsRequest) state =
        let val tags = (State.list_tags  state) in
          (StringListResponse { data = map TagName.dest tags }, state)
        end

    | serviceRequest' (TagRuleRequest { ruleName, tagName }) state =
        (OkResponse,
         State.tag_rule (RuleName.mk ruleName) (TagName.mk tagName) state)

    | serviceRequest' (UntagRuleRequest { ruleName, tagName }) state =
        (OkResponse,
         State.untag_rule (RuleName.mk ruleName) (TagName.mk tagName) state)

    | serviceRequest' (ForgetTagRequest { tagName }) state =
        (OkResponse,
         State.delete_tag (TagName.mk tagName) state)

    | serviceRequest' (DeleteRulesByTagRequest { tagName }) state =
        (OkResponse,
         State.delete_tag_and_its_rules (TagName.mk tagName) state)

    | serviceRequest' (ActivateRulesByTagRequest { tagName }) state =
        (OkResponse,
         State.activate_rules_with_tag (TagName.mk tagName) state)

    | serviceRequest' (DeactivateRulesByTagRequest { tagName }) state =
        (OkResponse,
         State.deactivate_rules_with_tag (TagName.mk tagName) state)


    (* Rewriting *)
    | serviceRequest' (AttachRewritesRequest { graphName, vertexNames }) state =
        let
          val gname = GraphName.mk graphName
          val graph = get_graph graphName state
          val verts = if vertexNames <> [] then
                        map V.mk vertexNames
                      else
                        (V.NSet.list_of (G.get_vnames graph))
          val (count,state') = State.attach_rewrites gname verts state
        in
          (CountResponse { count = count }, state')
        end

    | serviceRequest' (AttachOneRewriteRequest { graphName, vertexNames }) state =
        let
          val gname = GraphName.mk graphName
          val graph = get_graph graphName state
          val verts = if vertexNames <> [] then
                        map V.mk vertexNames
                      else
                        (V.NSet.list_of (G.get_vnames graph))
          val (count,state') = State.attach_first_rewrite gname verts state
        in
          (CountResponse { count = count }, state')
        end

    | serviceRequest' (ListAttachedRewritesRequest { graphName, format }) state =
        let
          val rewrites = State.get_attached_rewrites  (GraphName.mk graphName) state
        in
          case format
            of BasicFormat =>
                 (StringListResponse {
                    data = map (fn ((rn,_),_) => RuleName.dest rn) rewrites
                  }, state)
             | XmlFormat =>
                 (XmlResponse {
                    data = Output.RewriteList.output rewrites
                  }, state)
             | PrettyFormat => (* ??? *)
                 (ErrorResponse {
                    errorCode = "NOTIMP",
                    message = "Not implemented"
                  }, state)
             | DescriptionFormat =>
                 (RewriteListResponse {
                    rewrites = map describe_rewrite rewrites
                  }, state)
        end

    | serviceRequest' (ApplyAttachedRewriteRequest { graphName, index: int }) state =
        (OkResponse,
         State.apply_rewrite (GraphName.mk graphName) index state)

    | serviceRequest' (UnknownRequest { code }) state =
        (UnknownResponse { code = code },state)
    ;

    fun serviceRequest req =
     (let
        val (resp,newstate) = serviceRequest' req (!cur_state)
        val () = cur_state := newstate
      in
        resp
      end
      handle State.no_such_graph_exp gr =>
                ErrorResponse {
                  errorCode = "NOSUCHGRAPH",
                  message = ("No such graph \""^(GraphName.dest gr)^"\"")
                }
           | State.no_such_vertex_exp (vertexName,graphName) =>
                ErrorResponse {
                  errorCode = "NOSUCHVERTEX",
                  message = ("Graph \""^
                             (GraphName.dest graphName)^
                             "\" has no vertex named \""^
                             (V.dest vertexName)^"\"")
                }
           | State.no_such_vertex_type_exp vt =>
                ErrorResponse {
                  errorCode = "BADVERTEXTYPE",
                  message = ("Unknown vertex type \""^vt^"\"")
                }
           | State.no_such_edge_exp (edgeName,graphName) =>
                ErrorResponse {
                  errorCode = "NOSUCHEDGE",
                  message = ("Graph \""^
                             (GraphName.dest graphName)^
                             "\" has no edge named \""^
                             (E.dest edgeName)^"\"")
                }
           | State.no_such_bbox_exp (bboxName,graphName) =>
                ErrorResponse {
                  errorCode = "NOSUCHBBOX",
                  message = ("Graph \""^
                             (GraphName.dest graphName)^
                             "\" has no !-box named \""^
                             (B.dest bboxName)^"\"")
                }
           | State.no_such_rule_exp ruleName =>
                ErrorResponse {
                  errorCode = "NOSUCHRULE",
                  message = ("No such rule \""^
                             (RuleName.dest ruleName)^"\"")
                }
           | State.no_such_tag_exp tagName =>
                ErrorResponse {
                  errorCode = "NOSUCHTAG",
                  message = ("No such tag \""^
                             (TagName.dest tagName)^"\"")
                }
           | State.vertex_name_exists_exp (vertexName,graphName) =>
                ErrorResponse {
                  errorCode = "ALREADYEXISTS",
                  message = ("Graph \""^
                             (GraphName.dest graphName)^
                             "\" already has a vertex named \""^
                             (V.dest vertexName)^"\"")
                }
           | State.edge_name_exists_exp (edgeName,graphName) =>
                ErrorResponse {
                  errorCode = "ALREADYEXISTS",
                  message = ("Graph \""^
                             (GraphName.dest graphName)^
                             "\" already has an edge named \""^
                             (E.dest edgeName)^"\"")
                }
           | State.bbox_name_exists_exp (bboxName,graphName) =>
                ErrorResponse {
                  errorCode = "ALREADYEXISTS",
                  message = ("Graph \""^
                             (GraphName.dest graphName)^
                             "\" already has a !-box named \""^
                             (B.dest bboxName)^"\"")
                }
           | State.bad_rule_exp msg =>
                ErrorResponse {
                  errorCode = "BADRULE",
                  message = msg
                }
           | State.bad_state_exp msg =>
                ErrorResponse {
                  errorCode = "BADSTATE",
                  message = msg
                }
           | State.out_of_bounds_exp n =>
                ErrorResponse {
                  errorCode = "OUTOFRANGE",
                  message = ("Argument "^(Int.toString n)^" is out of range")
                }
           );

  val _ = ControllerRegistry.register_eval_func State.Theory.theory_name serviceRequest;
end;

structure DummyController : CONTROLLER =
struct
  fun serviceRequest _ =
        Commands.ErrorResponse {
          errorCode = "NOTHEORY",
          message = "No theory has been selected"
        };
  val theoryName = "dummy_theory";
end;

(* vi:ts=2:sw=2:sts=2:et
*)
