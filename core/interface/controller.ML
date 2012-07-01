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
                   | ImportRulesetFromFileRequest of { fileName: string, replace: bool }
                     (* OkResponse | ErrorResponse *)
                   | ImportRulesetFromDataRequest of { data: string, replace: bool }
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
                   | RenameRuleRequest of { oldName: string, newName: string }
                     (* OkResponse | ErrorResponse *)
                   | RuleUserDataRequest of { ruleName: string, dataName: string}
                     (* DataResponse | ErrorResponse *)
                   | SetRuleUserDataRequest of { ruleName: string, dataName: string,
                                                userData: string}
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

                   (*** Metrics related commands ***)
                   | ListMetricsNamesRequest
                   (* StringListResponse *)
                   | ListMetricsDescsRequest
                   (* StringListResponse *)
                   | GetCurrentMetricNameRequest
                   (* StringResponse *)
                   | SetMetricRequest of { metricName: string }
                   (* OkResponse *)
                   | ComputeMetricRequest of { graphName: string }
                   (* MetricResponse *)
                   | OrdMetricRequest of 
                        { metric1 : string list, metric2: string list}
                   (* Count Response: 0 1 2 for le, eq, gr *)
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
                               | GraphUserData of { dataName: string }
                                 (* UserDataResponse | ErrorResponse *)
                               | ListVertices
                                 (* StringListResponse | ErrorResponse *)
                               | ListEdges
                                 (* StringListResponse | ErrorResponse *)
                               | ListBangBoxes
                                 (* StringListResponse | ErrorResponse *)
                               | VertexData of { vertexName: string, format: resultFormat }
                                 (* ComponentDataResponse | ErrorResponse *)
                               | VertexUserData of { vertexName: string, dataName: string}
                                 (* UserDataResponse | ErrorResponse *)
                               | EdgeData of { edgeName: string, format: resultFormat }
                                 (* ComponentDataResponse | ErrorResponse *)
                               | EdgeUserData of { edgeName: string, dataName: string }
                                 (* UserDataResponse | ErrorResponse *)
                               | BangBoxVertices of { bangBoxName: string }
                                 (* StringListResponse | ErrorResponse *)
                               | BangBoxUserData of { bangBoxName: string,
                                                      dataName: string }
                                 (* UserDataResponse | ErrorResponse *)
                               | PrintUserData
                                 (* PrettyResponse | ErrorResponse *)

       and graphEditAction = UndoAction
                             (* OkResponse | ErrorResponse *)
                           | RedoAction
                             (* OkResponse | ErrorResponse *)
                           | UndoRewriteAction
                              (* OkResponse | ErrorResponse *)
                           | RedoRewriteAction
                              (* OkResponse | ErrorResponse *)
                           | StartUndoGroupAction
                             (* OkResponse | ErrorResponse *)
                           | EndUndoGroupAction
                              (* OkResponse | ErrorResponse *)
                           | InsertGraphAction of { sourceGraphName: string }
                             (* OkResponse | ErrorResponse *)
                           | SetGraphUserDataAction of { dataName: string, userData: string }
                              (* OkResponse | ErrorResponse  *)
                           | DeleteGraphUserDataAction of { dataName: string}
                              (* OkResponse | ErrorResponse *)
                           | AddVertexAction of { vertexType: string, resultFormat: resultFormat }
                             (* VertexDescriptionAction | ErrorResponse *)
                           | RenameVertexAction of { oldName: string, newName: string }
                             (* StringListResponse | ErrorResponse *)
                           | DeleteVerticesAction of { vertexNames: string list }
                             (* OkResponse | ErrorResponse *)
                           | SetVertexDataAction of { vertexName: string, vertexData: componentData }
                             (* OkResponse | ErrorResponse *)
                           | SetVertexUserDataAction of { vertexName: string,
                           dataName: string, userData: string }
                             (* OkResponse | ErrorResponse *)
                           | DeleteVertexUserDataAction of { vertexName: string,
                           dataName: string}
                             (* OkResponse | ErrorResponse *)
                           | AddEdgeAction of { edgeType: string, directed: bool, source: string, target: string, resultFormat: resultFormat }
                             (* EdgeDescriptionAction | ErrorResponse *)
                           | RenameEdgeAction of { oldName: string, newName: string }
                             (* StringResponse | ErrorResponse *)
                           | DeleteEdgesAction of { edgeNames: string list }
                             (* OkResponse | ErrorResponse *)
                           | SetEdgeDataAction of { edgeName: string, edgeData: componentData }
                             (* OkResponse | ErrorResponse *)
                           | SetEdgeUserDataAction of { edgeName: string,
                           dataName: string, userData: string }
                             (* OkResponse | ErrorResponse *)
                           | DeleteEdgeUserDataAction of {edgeName: string,
                              dataName : string }
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
                           | SetBangBoxUserDataAction of { bangBoxName: string,
                              dataName : string, userData: string }
                             (* OkResponse | ErrorResponse *)
                           | DeleteBangBoxUserDataAction of {bangBoxName: string,
                              dataName : string }
                             (* OkResponse | ErrorResponse *)

       and response = OkResponse
                    | ErrorResponse of { errorCode: string, message: string }

                    | StringResponse of { data: string }
                    | StringListResponse of { data: string list }

                    | MetricResponse of { data: int list}

                    | DataResponse of { data: string }
                    | PrettyResponse of { data: Pretty.T }
                    | XmlResponse of { data: XML.tree }
                    | CountResponse of { count: int }

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
  val get_theories: unit -> string list;
end;

structure ControllerRegistry : CONTROLLER_REGISTRY
= struct
  val eval_funcs = Unsynchronized.ref Symtab.empty;

  fun register_eval_func theory_name func =
        eval_funcs := Symtab.update_new (theory_name,func) (!eval_funcs)
        handle Symtab.DUP _ => 
          raise ERROR ("Cannot register theory: '" ^ theory_name 
            ^ "'; this theory is already registered.");

  fun get_eval_func name = Symtab.lookup (!eval_funcs) name

  fun get_theories () = Symtab.keys (!eval_funcs)
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
  structure Rule = State.Theory.Rule;
  structure G = State.Theory.Graph;
  structure OVData = State.Theory.OVData;
  structure EData = State.Theory.EData;
  structure VCD = State.Theory.VertexComponentData;
  structure ECD = State.Theory.EdgeComponentData;
  structure Input = State.Theory.IO_Xml.Input;
  structure Output = State.Theory.IO_Xml.Output;
  structure UIDataOutput = OutputUIDataXML(structure UIData = State.UIData);
  structure RulesetUIDataOutput = OutputRulesetUIDataXML(structure UIData = State.UIData);
  structure GraphAndUIOutput = OutputWrapXML(
              val tagname = "graph_and_user_data"
              val attrs = []
              structure Output1 = OutputStructAndUserDataXMLList);
  structure RulesetAndUIOutput = OutputWrapXML(
              val tagname = "ruleset_and_user_data"
              val attrs = []
              structure Output1 = OutputStructAndUserDataXMLList);
  structure UIDataInput = InputUIDataXML(structure UIData = State.UIData);
  structure InputRulesetUIData = InputRulesetUIDataXML(structure UIData =
        State.UIData);

  val theoryName = State.Theory.theory_name;

  fun get_graph (name:string) (state:State.T) : G.T = State.get_graph state (GraphName.mk name)
  fun get_graph_ud (name:string) (state:State.T) : State.UIData.T = State.get_graph_ud
    state (GraphName.mk name)

  fun update_ruleset rset true state =
       State.update_rset (Ruleset.merge rset) state
    | update_ruleset rset false state =
       State.set_rset rset state

  fun update_ruleset_ud rset_ud true state =
      State.update_rset_uidata (R.NTab.merge_joint (K true) rset_ud) state 
    | update_ruleset_ud rset_ud _ state =
      State.set_rset_uidata rset_ud state

  fun load_ruleset reader source replace state =
     ((OkResponse,
       state |> update_ruleset (Input.Ruleset.input (reader source)) replace 
             |> update_ruleset_ud (InputRulesetUIData.input (reader source)) replace
       )
      handle XML_Parse_Utils.bad_input_exp (s1,s2) =>
             (ErrorResponse { errorCode = "BADDATA",
                message = ("The file is not a valid ruleset for the active theory ("^s1^" at "^s2^")") }, state)
           | Input.Ruleset.bad_input_exp (s1,s2) =>
             (ErrorResponse { errorCode = "BADDATA",
                message = ("The file is not a valid ruleset for the active theory ("^s1^" at "^s2^")") }, state)
           | XMLReader.bad_input_exp msg =>
             (ErrorResponse { errorCode = "BADDATA",
                message = ("The file is not a valid ruleset for the active theory ("^msg^")") }, state)
          | _ =>
             (ErrorResponse { errorCode = "BADDATA",
                message = ("The file is not a valid ruleset for the active theory.") }, state))

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
      val bbox_list = VBMap.codf_list_of (G.get_bboxes graph)
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
      { ruleName = R.dest rn, result = describe_graph ("",result),
        lhs = describe_graph ("",lhs), rhs = describe_graph ("",rhs) }
    end

  fun pretty_vert (n, (nd,(ins,outs))) =
      Pretty.block [V.pretty_name n,
                    Pretty.str "; ",
                    Pretty.list "i{" "}" (map E.pretty_name (E.NSet.list_of ins)),
                    Pretty.list "; o{" "}" (map E.pretty_name (E.NSet.list_of outs)),
                    Pretty.str "; ",
                    OVData.pretty_data nd
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
                    EData.pretty_data e
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
                   val ud = UIDataInput.input(reader data)
                   val name = if suggestedName = "" then GraphName.default_name else GraphName.mk suggestedName
                   val (nm,state') = State.add_graph name graph state
                   val state_ =  State.set_graph_ud ud nm state'
                 in
                   (StringResponse { data = GraphName.dest nm },state_)
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
          (* The user data is attached to the state, so we cannot
           * put it in io_interface. Therefore we build our structures
           * are the very last moment. There should be a better way to do that
           * as it prevents us form storing uidata on rules...*)
          val graph = get_graph graphName state
          val graphXMLTree = Output.Graph.output graph
          val ud = get_graph_ud graphName state
          val userDataXMLTree = UIDataOutput.output ud
          val () = XMLWriter.write_to_file fileName
                      (GraphAndUIOutput.output (graphXMLTree, userDataXMLTree))
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
               | GraphUserData { dataName }=>
                    let
                      val tab = State.get_graph_user_data
                        (GraphName.mk graphName) state
                      val data = Symtab.lookup tab dataName
                    in
                      (case data of SOME a => 
                          DataResponse { data = a }
                       | NONE => 
                          raise State.no_such_graph_user_data_exp (GraphName.mk
                          graphName, dataName))
                    end
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
                                data = OVData.pretty_data vdata
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
               | VertexUserData { vertexName, dataName } =>
                let
                  val tab = (case State.get_vertex_user_data (GraphName.mk
                              graphName) (V.mk vertexName) state of
                              SOME u => u
                              | NONE => raise 
                                State.no_such_vertex_user_data_exp (V.mk
                                  vertexName,dataName))
                  val data = (case Symtab.lookup tab dataName of
                       SOME a => a
                       | NONE => 
                          raise State.no_such_vertex_user_data_exp (V.mk
                            vertexName, dataName))
                in
                      DataResponse { data = data }
                end
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
                                data = EData.pretty_data edata
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
               | EdgeUserData { edgeName , dataName} =>
                let
                  val tab = (case State.get_edge_user_data (GraphName.mk
                              graphName) (E.mk edgeName) state of
                              SOME u => u
                              | NONE => raise 
                                State.no_such_edge_user_data_exp (E.mk
                                  edgeName,dataName))
                  val data = (case Symtab.lookup tab dataName of
                       SOME a => a
                       | NONE => 
                          raise State.no_such_edge_user_data_exp (E.mk
                            edgeName, dataName))
                in
                      DataResponse { data = data }
                end
               | BangBoxVertices { bangBoxName } =>
                    StringListResponse {
                      data = (map V.dest (V.NSet.list_of (G.get_bbox graph (B.mk bangBoxName))))
                    }
               | BangBoxUserData { bangBoxName, dataName } =>
                let
                  val tab = (case State.get_bangbox_user_data (GraphName.mk
                              graphName) (B.mk bangBoxName) state of
                              SOME u => u
                              | NONE => raise 
                                State.no_such_bangbox_user_data_exp (B.mk
                                  bangBoxName,dataName))
                  val data = (case Symtab.lookup tab dataName of
                       SOME a => a
                       | NONE => 
                          raise State.no_such_bangbox_user_data_exp (B.mk
                            bangBoxName, dataName))
                in
                      DataResponse { data = data }
                end

               | PrintUserData => 
                let
                  val ud = State.get_graph_ud state (GraphName.mk graphName)
                in
                  PrettyResponse {data = State.UIData.pretty_user_data ud}
                end

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
        | UndoRewriteAction =>
            (OkResponse,
             State.undo_rewrite (GraphName.mk graphName) state)
        | RedoRewriteAction =>
            (OkResponse,
             State.redo_rewrite (GraphName.mk graphName) state)
        | StartUndoGroupAction =>
            (OkResponse,
             State.start_undo_group (GraphName.mk graphName) state)
        | EndUndoGroupAction =>
            (OkResponse,
             State.end_undo_group (GraphName.mk graphName) state)
        | InsertGraphAction { sourceGraphName } =>
            (OkResponse,
             State.insert_graph (GraphName.mk sourceGraphName) (GraphName.mk graphName) state)
        | SetGraphUserDataAction {dataName, userData} =>
            (OkResponse,
              State.update_graph_user_data dataName userData
              (GraphName.mk graphName) state)
        | DeleteGraphUserDataAction {dataName} =>
            (OkResponse,
              State.delete_graph_user_data dataName (GraphName.mk graphName)
              state)
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
          let
            val (vlist, state') = State.rename_vertex (V.mk oldName) (V.mk newName) 
                                (GraphName.mk graphName) state
          in
            (StringListResponse {data = map V.dest vlist}, state')
          end
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
        | SetVertexUserDataAction { vertexName, dataName, userData } =>
            (OkResponse,
              State.update_vertex_user_data dataName userData (V.mk vertexName) (GraphName.mk graphName) state)
        | DeleteVertexUserDataAction { vertexName, dataName } =>
            (OkResponse,
              State.delete_vertex_user_data dataName (V.mk vertexName) 
                (GraphName.mk graphName) state)
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
        | SetEdgeUserDataAction { edgeName, dataName, userData } =>
            (OkResponse,
              State.update_edge_user_data dataName userData (E.mk edgeName) (GraphName.mk graphName) state)
        | DeleteEdgeUserDataAction { edgeName, dataName } =>
            (OkResponse,
              State.delete_edge_user_data dataName (E.mk edgeName) 
                (GraphName.mk graphName) state)
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
        | SetBangBoxUserDataAction { bangBoxName, dataName, userData } =>
            (OkResponse,
              State.update_bangbox_user_data dataName userData (B.mk bangBoxName) (GraphName.mk graphName) state)
        | DeleteBangBoxUserDataAction { bangBoxName, dataName } =>
            (OkResponse,
              State.delete_bangbox_user_data dataName (B.mk bangBoxName) 
                (GraphName.mk graphName) state)
        )


    (* Rulesets *)
    | serviceRequest' (ImportRulesetFromFileRequest { fileName, replace }) state =
       (load_ruleset XMLReader.read_from_file fileName replace state
        handle IO.Io {name : string, function : string, cause as (OS.SysErr (msg:string,_)) : exn} => 
               (ErrorResponse { errorCode = "FILEACCESS",
                                message = ("The file could not be read ("^msg^")") }, state)
             | IO.Io {name : string, function : string, cause : exn} => 
               (ErrorResponse { errorCode = "FILEACCESS",
                                message = ("The file could not be read (unknown error in "^function^" of "^name^")") }, state))

    | serviceRequest' (ImportRulesetFromDataRequest { data, replace }) state =
        load_ruleset XMLReader.read_from_string data replace state

    | serviceRequest' (ExportRulesetToFileRequest { fileName }) state =
       (let
          val ruleset_tree = (Output.Ruleset.output o State.get_rset) state
          val uidata_tree = (RulesetUIDataOutput.output o State.get_rset_uidata) state
          val () = (XMLWriter.write_to_file fileName)
                      (RulesetAndUIOutput.output (ruleset_tree, uidata_tree))
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
          (StringListResponse { data = map R.dest rules }, state)
        end

    | serviceRequest' (ListActiveRulesRequest) state =
        let val rules = (State.list_active_rules  state) in
          (StringListResponse { data = map R.dest rules }, state)
        end

    | serviceRequest' (ListRulesByTagRequest { tagName }) state =
        let val rules = (State.list_rules_with_tag (TagName.mk tagName) state) in
          (StringListResponse { data = map R.dest rules }, state)
        end

    | serviceRequest' (OpenRuleLhsRequest { ruleName }) state =
        let val (nm,state') = (State.open_rule_lhs state (R.mk ruleName)) in
          (StringResponse { data = GraphName.dest nm }, state')
        end

    | serviceRequest' (OpenRuleRhsRequest { ruleName }) state =
        let val (nm,state') = (State.open_rule_rhs state (R.mk ruleName)) in
          (StringResponse { data = GraphName.dest nm }, state')
        end

    | serviceRequest' (SetRuleRequest { ruleName, lhsGraphName, rhsGraphName }) state =
        (OkResponse,
         State.set_rule (R.mk ruleName) (GraphName.mk lhsGraphName) (GraphName.mk rhsGraphName) state)

    | serviceRequest' (DeleteRuleRequest { ruleName }) state =
        (OkResponse,
         State.delete_rule (R.mk ruleName) state)

    | serviceRequest' (ActivateRuleRequest { ruleName }) state =
        (OkResponse,
         State.activate_rule (R.mk ruleName) state)

    | serviceRequest' (DeactivateRuleRequest { ruleName }) state =
        (OkResponse,
         State.deactivate_rule (R.mk ruleName) state)

    | serviceRequest' (RenameRuleRequest { oldName, newName }) state =
        (OkResponse,
         State.rename_rule (R.mk oldName) (R.mk newName) state)
    | serviceRequest' (RuleUserDataRequest { ruleName, dataName }) state =
      let
        val tab = fst (State.get_rule_ud state (R.mk ruleName))
        val data = (case Symtab.lookup tab dataName of
          SOME a => a
          | NONE => 
              raise State.no_such_rule_user_data_exp (R.mk ruleName, dataName))
      in
        (DataResponse { data = data }, state)
      end
    | serviceRequest' (SetRuleUserDataRequest {ruleName, dataName, userData}) state =
      let
        val ud = State.get_rule_ud state (R.mk ruleName)
        val tab = Symtab.update (dataName, userData) (fst ud) 
        val state' = State.set_rule_ud (tab, (snd ud)) (R.mk ruleName) state
      in
            (OkResponse, state')
      end
    (* Tags *)
    | serviceRequest' (ListTagsRequest) state =
        let val tags = (State.list_tags  state) in
          (StringListResponse { data = map TagName.dest tags }, state)
        end

    | serviceRequest' (TagRuleRequest { ruleName, tagName }) state =
        (OkResponse,
         State.tag_rule (R.mk ruleName) (TagName.mk tagName) state)

    | serviceRequest' (UntagRuleRequest { ruleName, tagName }) state =
        (OkResponse,
         State.untag_rule (R.mk ruleName) (TagName.mk tagName) state)

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
                    data = map (fn ((rn,_),_) => R.dest rn) rewrites
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
    | serviceRequest' ListMetricsNamesRequest state =
      (StringListResponse { data = State.get_names_metrics state }, state)
    | serviceRequest' ListMetricsDescsRequest state =
      (StringListResponse { data = State.get_descs_metrics state}, state)
    | serviceRequest' GetCurrentMetricNameRequest state =
      (StringResponse { data = State.get_current_name_metric state}, state)
    | serviceRequest' (SetMetricRequest { metricName}) state =
      (OkResponse, State.set_metric metricName state)
    | serviceRequest' (ComputeMetricRequest { graphName }) state =
      (MetricResponse 
        { data = State.compute_metric (GraphName.mk graphName) state }, state)
    | serviceRequest' (OrdMetricRequest { metric1, metric2}) state = 
        (CountResponse {count = State.ord_metric metric1 metric2 state}, state) 
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
           | State.no_such_graph_user_data_exp (graphName, dataName) =>
                ErrorResponse {
                  errorCode = "NOSUCHGRAPHUSERDATA",
                  message = ("Graph \""^
                             (GraphName.dest graphName)^
                             "\" has no user data named \""^
                             (dataName)^"\"")
                }
           | State.no_such_vertex_user_data_exp (vertexName, dataName) =>
                ErrorResponse {
                  errorCode = "NOSUCHGRAPHUSERDATA",
                  message = ("Vertex \""^
                             (V.dest vertexName)^
                             "\" has no user data named \""^
                             (dataName)^"\"")
                }
           | State.no_such_edge_user_data_exp (edgeName, dataName) =>
                ErrorResponse {
                  errorCode = "NOSUCHGRAPHUSERDATA",
                  message = ("Edge \""^
                             (E.dest edgeName)^
                             "\" has no user data named \""^
                             (dataName)^"\"")
                }
           | State.no_such_bangbox_user_data_exp (bbName, dataName) =>
                ErrorResponse {
                  errorCode = "NOSUCHGRAPHUSERDATA",
                  message = ("!-Box \""^
                             (B.dest bbName)^
                             "\" has no user data named \""^
                             (dataName)^"\"")
                }
           | State.no_such_rule_user_data_exp (rName, dataName) =>
                ErrorResponse {
                  errorCode = "NOSUCHRULEUSERDATA",
                  message = ("Rule \""^
                             (R.dest rName)^
                             "\" has no user data named \""^
                             (dataName)^"\"")
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
           | State.no_such_edge_type_exp vt =>
                ErrorResponse {
                  errorCode = "BADEDGETYPE",
                  message = ("Unknown edge type \""^vt^"\"")
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
                             (R.dest ruleName)^"\"")
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
           | State.rule_name_exists_exp ruleName =>
                ErrorResponse {
                  errorCode = "ALREADYEXISTS",
                  message = ("There is already a rule named \""^
                             (R.dest ruleName)^"\"")
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
           | State.no_such_metric_exp name =>
                ErrorResponse {
                  errorCode = "NOSUCHMETRIC",
                  message = ("Metric "^name^" does not exist")
                }
           | State.unsupported_metric_exp name =>
                ErrorResponse {
                  errorCode = "UNSUPPORTEDMETRIC",
                  message = (name^" cannot be used with this graphical theory")
                }
           | State.not_an_int_exp s =>
                ErrorResponse {
                  errorCode = "NOTANINT",
                  message = (s^" could not be parsed as an integer")
                }
 
           );

  val _ = ControllerRegistry.register_eval_func State.Theory.theory_name serviceRequest;
end;

structure DummyController : CONTROLLER =
struct
  fun serviceRequest (Commands.DummyRequest { response }) = response
    | serviceRequest _ =
        Commands.ErrorResponse {
          errorCode = "NOTHEORY",
          message = "No theory has been selected"
        };
  val theoryName = "dummy_theory";
end;

(* vi:ts=2:sw=2:sts=2:et
*)
