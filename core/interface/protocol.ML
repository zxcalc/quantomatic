(* {{{ Protocol documentation
 *
 * This file implements a protocol for talking to other tools.  It uses the
 * escape character (\027) as a tag for structural elements.  It is inspired
 * by the Poly/ML IDE protocol.
 *
 * The protocol consists of a series of requests (sent from the tool to the
 * core) and their responses (sent from the core to the tool).  Each of these is
 * termed a "message" - a single cohesive and complete chunk of data.  There is
 * one message that is neither a request nor a response, but informational: the
 * version message.  There is potential scope for other informational messages
 * in later revisions of the protocol.
 *
 * ESC ::= Char.chr 27;
 * Code ::= [A-Z]+ ;
 * String ::= ([^ESC] | ESC ESC)* ;
 * StringList ::= [0-9]+ ESC ':' (String (ESC ',' String)* )? ; length, then list
 * RequestId ::= String ;
 * Integer ::= [0-9]+ ;
 * MsgStart ::= ESC '<' ;
 * MsgSep ::= ESC '|' ; separates msg header from body
 * MsgEnd ::= ESC '>' ;
 * DataChunk ::= ESC '[' Integer ESC '|' .{n} ESC ']' ;
 *   where n is the Integer (ie: the Integer describes how much data to read, in
 *   bytes)
 * MsgHeader ::= Code (ESC ':' RequestId)? ; RequestId is omitted for
 *                                           informational messages
 * MsgBody ::= ([^ESC] | ESC ESC | DataChunk)* ;
 *
 * All messages are of the form
 *   Message ::= MsgStart MsgHeader MsgSep MsgBody MsgEnd
 *
 * The protocol starts with the core sending the version message, to indicate
 * that it is ready to start communication:
 *
 * VersionMessage ::= ESC '<' 'V' ESC '|' "1.0" ESC '>' ;
 *
 * The tool can then send a Request, to which the core will respond with a
 * Response.  The structure and effect of the Request depends on the code.  The
 * Response it generates will have the same RequestId - the tool has full
 * control over RequestIds.
 *
 * For requests and responses, we will only give the code and the body.  The
 * message is then of the form
 *   MsgStart Code ESC ':' RequestId MsgSep Body MsgEnd ;
 *
 * note: ESC can also be written as #"\027", or #"\u0021b", we avoid the
 * latter as some modification to ML don't like the \u syntax. 
 *
 ******************** RESPONSES **********************
 *
 * The possible responses the core can generate are:
 *
 * Name: ErrorResponse
 * Code: "Q"
 * Args:
 *   ErrorIdent ::= [A-Z]+ ;
 *   ErrorMessage ::= String ;
 *   ErrorResponseBody ::= ErrorIdent ESC ';' ErrorMessage ;
 * Desc: The request could not be fulfilled.
 *
 * Name: OkResponse
 * Code: "O"
 * Args:
 *   OkResponseBody ::= ;
 * Desc: The requested action completed successfully.
 *
 * Name: ConsoleResponse
 * Code: "C"
 * Args:
 *   ConsoleResponseBody ::= DataChunk ;
 * Desc: The response to a console command.
 *
 * Name: ConsoleHelpResponse
 * Code: "H"
 * Args:
 *   CommandArgs ::= String ;
 *   CommandHelp ::= String ;
 *   HelpResponseBody ::= CommandArgs ESC ';' CommandHelp ;
 * Desc: The argument list and help text for a command
 *
 * Name: DataResponse
 * Code: "R"
 * Args:
 *   DataResponseBody ::= DataChunk ;
 * Desc: Raw data of some description; not intended to be parsed.
 *
 * Name: PrettyResponse
 * Code: "P"
 * Args:
 *   PrettyResponseBody ::= DataChunk ;
 * Desc: "Pretty-print" data, for displaying to humans; not
 *       intended to be parsed.
 *
 * Name: XmlResponse
 * Code: "X"
 * Args:
 *   XmlResponseBody ::= DataChunk ;
 * Desc: XML-formatted data.
 *
 * Name: CountResponse
 * Code: "I"
 * Args:
 *   CountResponseBody ::= Integer ;
 * Desc: A number, usually the quantity of something.
 *
 * Name: StringResponse
 * Code: "N"
 * Args:
 *   NameResponseBody ::= String ;
 * Desc: A string, usually a name.
 *
 * Name: StringListResponse
 * Code: "M"
 * Args:
 *   NameListRespBody ::= StringList ;
 * Desc: A list of strings, usually names.
 *
 * Name: UserDataResponse
 * Code: "U"
 * Args:
 *   UserDataRespBody ::= DataChunk ;
 * Desc: Data that was previously supplied by the tool.
 *
 * Name: StructuredDataResponse
 * Code: "S"
 * Args:
 *   Element ::= ESC 'A' ElementContent ESC 'a'
 *             | ...
 *             | ESC 'Y' ElementContent ESC 'y'
 *             | ESC 'ZA' ElementContent ESC 'za'
 *             | ...
 *             | ESC 'ZY' ElementContent ESC 'zy'
 *             | ESC 'ZZA' ElementContent ESC 'zza'
 *             | ...
 *   ElementContent ::= Element | String | DataChunk
 *   StructuredDataResponseBody ::= Element
 * Desc: A sort of binary-xml style data structure.  Each element
 *       starts with a series of capital letters (zero or more 'Z's,
 *       followed by one other letter) and ends with the lowercase
 *       version of the same sequence.  In between, it can contain
 *       other elements, string data or data chunks.
 *
 * Name: UnknownRequestResponse
 * Code: "Z"
 * Args:
 *   UnknownRequestResponseBody ::= Code ;
 * Desc: The request code was not recognised.
 *
 *
 *
 ***************** REQUESTS *******************
 *
 * In this section, error responses are the codes that
 * may be returned in an error response.
 *
 * A request looks like
 *   XmlDataChunk ::= ESC 'X' DataChunk ;
 *   StringDataChunk ::= ESC 'N' DataChunk ;
 *   StructuredDataChunk ::= ESC 'S' DataChunk ;
 *   TaggedDataChunk ::= XmlDataChunk | StringDataChunk | StructedDataChunk ;
 *   ObjectResultFormat ::= "name" | "xml" | "pretty" | "native" ;
 *   GraphExportFormat ::= "native" | "xml" | "pretty" | "hilb" | "mathematica" | "matlab" | "tikz"
 *
 *   Argument ::= String | StringList | DataChunk | TaggedDataChunk |
 *                Integer | ObjectResultFormat | GraphExportFormat ;
 *
 *   RequestBody ::= ( Argument ( ESC ';' Argument )* )?
 *
 *
 * The possible requests are:
 *
 * Name: ConsoleCommandRequest
 * Code: "CC"
 * Args:
 *   Command: DataChunk
 * Desc: Execute user-console command
 * Responses: ConsoleResponse
 * Error responses:
 *
 * Name: ConsoleCommandListRequest
 * Code: "CL" ;
 * Args:
 * Desc: Get the list of possible user-console commands
 * Responses: StringListResponse
 * Error responses:
 *
 * Name: ConsoleHelpRequest
 * Code: "CH"
 * Args:
 *   CommandName: String
 * Desc: Get help for a user-console command.
 * Responses: ConsoleHelpResponse
 * Error responses: BADCMD
 *
 * Name: ChangeTheoryRequest
 * Code: "TS"
 * Args:
 *   TheoryName: String
 * Desc: Switch to a different theory.
 * Responses: OkResponse
 * ErrorResponses: BADTHEORY
 *
 * Name: CurrentTheoryRequest
 * Code: "TG"
 * Args:
 * Desc: Get the name of the current theory
 * Responses: StringResponse
 * ErrorResponses:
 *
 *
 * All the following requests operate on the current theory
 *
 * Name: ListGraphsRequest
 * Code: "GL"
 * Args:
 * Desc: List the loaded graphs.
 * Responses: StringListResponse
 *
 * Name: LoadEmptyGraphRequest
 * Code: "GOE"
 * Args:
 *   GraphName: String
 * Desc: Load an empty graph, optionally naming it as requested,
 *       or a variant if the name is taken.  If no name is given
 *       (an empty string), a default name will be used.
 * Responses: StringResponse
 *
 * Name: LoadGraphFromFileRequest
 * Code: "GOF"
 * Args:
 *   FileName: String
 * Desc: Load a graph from a file.  It will be given a name based on
 *       the file name.
 * Responses: StringResponse
 * ErrorResponses: FILEACCESS | BADDATA
 *
 * Name: LoadGraphFromDataRequest
 * Code: "GOD"
 * Args:
 *   GraphName: String
 *   GraphData: DataChunk
 * Desc: Load a graph from the given data, giving it (a variation on)
 *       the given name.  If the name is an empty string, a default
 *       one will be used.
 * Responses: StringResponse
 * ErrorResponses: BADDATA
 *
 * Name: CopyGraphRequest
 * Code: "GOG"
 * Args:
 *   ExistingGraphName: String
 * Desc: Create a copy of an existing graph.  The new graph's
 *       name will be based on the old one.
 * Responses: StringResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: CopySubgraphRequest
 * Code: "GOS"
 * Args:
 *   SourceGraphName: String
 *   TargetGraphName: String
 *   VertexNames: StringList
 * Desc: Create a copy of part of an existing graph (defined by
 *       VertexNames), overwriting TargetGraphName if it exists.
 *       This is mainly intended for clipboard support in GUI tools.
 * Responses: OkResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: SaveGraphToFileRequest
 * Code: "GS"
 * Args:
 *   GraphName: String
 *   FileName: String
 * Desc: Save the named graph to a file..
 * Responses: OkResponse
 * ErrorResponses: NOSUCHGRAPH | FILEACCESS
 *
 * Name: RenameGraphRequest
 * Code: "GR"
 * Args:
 *   OldGraphName: String
 *   NewGraphName: String
 * Desc: Rename a graph; if the requested new name already exists,
 *       a similar one will be picked.
 * Responses: StringResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: DiscardGraphRequest
 * Code: "GD"
 * Args:
 *   GraphName: String
 * Desc: Discard a graph, forgetting everything about it.
 * Responses: OkResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: ExportGraphRequest
 * Code: "GE"
 * Args:
 *   GraphName: String
 *   ExportFormat: GraphExportFormat
 * Desc: Get a graph in a certain format.
 *       "native" is the format that would be written to disk by "GS",
 *       and is suitable for loading with "GOF" or "GOD".
 *       "hilb", "mathematica" and "matlab" are vector representations,
 *       the latter two suitable for input into those
 *       applications.
 *       "tikz" is a LaTeX-suitable format.
 * Responses: DataResponse | XmlResponse | PrettyResponse
 * ErrorResponses: NOSUCHGRAPH | BADFORMAT
 *
 * Name: DescribeGraphRequest
 * Code: GVA
 * Args:
 *   GraphName: String
 * Desc: Describe the graph in a structured manner
 * Response: StructuredDataResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: GraphUserDataRequest
 * Code: "GVGU"
 * Args:
 *   GraphName: String
 *   DataName: String
 * Desc: Get the user data attached to a graph under the name
 *       dataName
 * Response: UserDataResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX
 *
 * Name: ListVerticesRequest
 * Code: "GVVL"
 * Args:
 *   GraphName: String
 * Desc: List the vertices present in a graph
 * Response: StringListResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: ListEdgesRequest
 * Code: "GVEL"
 * Args:
 *   GraphName: String
 * Desc: List the edges present in a graph
 * Response: StringListResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: ListBangBoxesRequest
 * Code: "GVBL"
 * Args:
 *   GraphName: String
 * Desc: List the !-boxes present in a graph
 * Response: StringListResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: VertexDataRequest
 * Code: "GVVD"
 * Args:
 *   GraphName: String
 *   VertexName: String
 *   ComponentDataFormat: "pretty" | "xml"
 * Desc: Get the component data attached to a vertex
 * Response: XmlResponse | PrettyResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX | BADFORMAT
 *
 * Name: VertexUserDataRequest
 * Code: "GVVU"
 * Args:
 *   GraphName: String
 *   VertexName: String
 *   DataName: String
 * Desc: Get the user data attached to a vertex under the name
 *       DataName
 * Response: DataResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX
 *
 * Name: EdgeDataRequest
 * Code: "GVED"
 * Args:
 *   GraphName: String
 *   EdgeName: String
 *   ComponentDataFormat: "pretty" | "xml"
 * Desc: Get the component data attached to an edge
 * Response: XmlResponse | PrettyResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHEDGE | BADFORMAT
 *
 * Name: EdgeUserDataRequest
 * Code: "GVEU"
 * Args:
 *   GraphName: String
 *   EdgeName: String
 * Desc: Get the user data attached to an edge
 * Response: DataResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHEDGE
 *
 * Name: BangBoxVerticesRequest
 * Code: "GVBV"
 * Args:
 *   GraphName: String
 *   BangBoxName: String
 * Desc: Get the vertices contained in a !-box
 * Response: StringListResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX | BADFORMAT
 *
 * Name: BangBoxUserDataRequest
 * Code: "GVBU"
 * Args:
 *   GraphName: String
 *   BangBoxName: String
 * Desc: Get the user data attached to a !-box
 * Response: DataResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX
 *
 * Name: UndoRequest
 * Code: "GMU"
 * Args:
 *   GraphName: String
 * Desc: Undo the last modification to a graph
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | BADSTATE
 *
 * Name: RedoRequest
 * Code: "GMR"
 * Args:
 *   GraphName: String
 * Desc: Redo the most recently undone modification to a graph
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | BADSTATE
 *
 * Name: UndoRewriteRequest
 * Code: "GMUR"
 * Args:
 *   GraphName: String
 * Desc: Undo the last modification to a graph
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | BADSTATE
 *
 * Name: RedoRewriteRequest
 * Code: "GMRR"
 * Args:
 *   GraphName: String
 * Desc: Redo the most recently undone modification to a graph
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | BADSTATE
 * 
 * Name: StartUndoGroupRequest
 * Code: "GMSU"
 * Args:
 *   GraphName: String
 * Desc: Starts an undo group: changes are merge on the undo stack
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | BADSTATE
 *
 * Name: EndUndoGroupRequest
 * Code: "GMFU"
 * Args:
 *   GraphName: String
 * Desc: Ends the undo group: create an undo point whenver the state is changed
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | BADSTATE
*
 * Name: InsertGraphRequest
 * Code: "GMI"
 * Args:
 *   TargetGraphName: String
 *   SourceGraphName: String
 * Desc: Insert a copy of one graph into another
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: SetGraphUserDataRequest
 * Code: "GMGU"
 * Args:
 *   GraphName: String
 *   dataName: String
 *   UserData: DataChunk
 * Desc: Set user data on a graph.  This data will not be parsed by the core
 *       in any way.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH
 * 
 * Name: DeleteGraphUserDataRequest
 * Code: "GMDGU"
 * Args:
 *  GraphName: String
 *  dataName: String
 * Desc: Delete user data on a graph.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: AddVertexRequest
 * Code: "GMVA"
 * Args:
 *   GraphName: String
 *   VertexType: String
 *   ResultFormat: ObjectResultFormat
 * Desc: Add a vertex to a graph
 * Response: StringResponse | XmlResponse | PrettyResponse | StructuredDataResponse
 * ErrorResponses: NOSUCHGRAPH | BADVERTEXTYPE | BADFORMAT
 *
 * Name: RenameVertexRequest
 * Code: "GMVR"
 * Args:
 *   GraphName: String
 *   OldVertexName: String
 *   NewVertexName: String
 * Desc: Rename a vertex
 * Response: StringListResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX | ALREADYEXISTS
 *
 * Name: DeleteVerticesRequest
 * Code: "GMVD"
 * Args:
 *   GraphName: String
 *   VertexNames: StringList
 * Desc: Delete vertices from a graph
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX
 *
 * Name: SetVertexDataRequest
 * Code: "GMVS"
 * Args:
 *   GraphName: String
 *   VertexName: String
 *   ComponentData: TaggedDataChunk
 * Desc: Set the data on a vertex.  The data can be given in XML or
 *       string form.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX | BADFORMAT | BADDATA
 *
 * Name: SetVertexUserDataRequest
 * Code: "GMVU"
 * Args:
 *   GraphName: String
 *   VertexName: String
 *   DataName: String
 *   UserData: DataChunk
 * Desc: Set user data on a vertex.  This data will not be parsed by the core
 *       in any way.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX
 *
 * Name: DeleteVertexUserDataRequest
 * Code: "GMDVU"
 * Args:
 *  GraphName: String
 *  VertexName: String
 *  DataName: String
 * Desc: Remove user data DataName on vertex VertexName
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX
 *
 * Name: AddEdgeRequest
 * Code: "GMEA"
 * Args:
 *   GraphName: String
 *   EdgeType: String
 *   Directedness: 'd' | 'u'
 *   SourceVertex: String
 *   TargetVertex: String
 *   ResultFormat: ObjectResultFormat
 * Desc: Add an edge to a graph.
 * Response: StringResponse | XmlResponse | PrettyResponse | StructuredDataResponse
 * ErrorResponses: NOSUCHGRAPH | BADEDGETYPE | BADFORMAT
 *
 * Name: RenameEdgeRequest
 * Code: "GMER"
 * Args:
 *   GraphName: String
 *   OldEdgeName: String
 *   NewEdgeName: String
 * Desc: Rename an edge
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHEDGE | ALREADYEXISTS
 *
 * Name: DeleteEdgesRequest
 * Code: "GMED"
 * Args:
 *   GraphName: String
 *   EdgeNames: StringList
 * Desc: Delete edges from a graph
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHEDGE
 *
 * Name: SetEdgeDataRequest
 * Code: "GMES"
 * Args:
 *   GraphName: String
 *   EdgeName: String
 *   ComponentData: TaggedDataChunk
 * Desc: Set the data on an edge.  The data can be given in XML or
 *       string form.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHEDGE | BADFORMAT | BADDATA
 *
 * Name: SetEdgeUserDataRequest
 * Code: "GMEU"
 * Args:
 *   GraphName: String
 *   EdgeName: String
 *   UserData: DataChunk
 * Desc: Set user data on an edge.  This data will not be parsed by the core
 *       in any way.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHEDGE
 * 
 * Name: DeleteEdgeUserDataRequest
 * Code: "GMDEU"
 * Args:
 *  GraphName: String
 *  EdgeName: String
 *  DataName: String
 * Desc: Remove user data DataName on vertex EdgeName
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHEDGE
 *
 * Name: AddBangBoxRequest
 * Code: "GMBA"
 * Args:
 *   GraphName: String
 *   VertexNames: StringList
 *   ResultFormat: ObjectResultFormat
 * Desc: Add a !-box to a graph.  A list of vertices to include in
 *       the !-box may be provided
 * Response: StringResponse | XmlResponse | PrettyResponse | StructuredDataResponse
 * ErrorResponses: NOSUCHGRAPH | BADFORMAT
 *
 * Name: RenameBangBoxRequest
 * Code: "GMBR"
 * Args:
 *   GraphName: String
 *   OldBangBoxName: String
 *   NewBangBoxName: String
 * Desc: Rename a !-box.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX | ALREADYEXISTS
 *
 * Name: DropBangBoxesRequest
 * Code: "GMBD"
 * Args:
 *   GraphName: String
 *   BangBoxNames: StringList
 * Desc: Drop the given !-boxes.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX
 *
 * Name: KillBangBoxesRequest
 * Code: "GMBK"
 * Args:
 *   GraphName: String
 *   BangBoxNames: StringList
 * Desc: Kill the given !-boxes.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX
 *
 * Name: DuplicateBangBoxRequest
 * Code: "GMBC"
 * Args:
 *   GraphName: String
 *   BangBoxName: String
 * Desc: Duplicate the given !-boxe.
 * Response: StringResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX
 *
 * Name: MergeBangBoxRequest
 * Code: "GMBM"
 * Args:
 *   GraphName: String
 *   BangBoxNames: StringList
 * Desc: Merge the given !-boxes.
 * Response: StringResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX
 *
 * Name: BangVerticesRequest
 * Code: "GMBB"
 * Args:
 *   GraphName: String
 *   BangBoxName: String
 *   VertexNames: StringList
 * Desc: Add vertices to a !-box.  They will be removed from any
 *       other !-boxes they are contained in.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX | NOSUCHVERTEX
 *
 * Name: UnbangVerticesRequest
 * Code: "GMBL"
 * Args:
 *   GraphName: String
 *   VertexNames: StringList
 * Desc: Remove some vertices from their !-boxes.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX
 *
 * Name: SetBangBoxUserDataRequest
 * Code: "GMBU"
 * Args:
 *   GraphName: String
 *   BangBoxName: String
 *   UserData: DataChunk
 * Desc: Set user data on a !-box.  This data will not be parsed by the core
 *       in any way.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX
 *
 * Name: DeleteBangBoxUserDataRequest
 * Code: "GMDBU"
 * Args:
 *  GraphName: String
 *  BangBoxName: String
 *  DataName: String
 * Desc: Remove user data DataName on vertex BangBoxName
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBANGBOX
 *
 * Name: ImportRulesetFromFileRequest
 * Code: "RSO"
 * Args:
 *   FileName: String
 * Desc: Import a ruleset from a file.  It will be merged with the
 *       loaded ruleset.
 * Response: OkResponse
 * ErrorResponses: FILEACCESS
 *
 * Name: ImportRulesetFromDataRequest
 * Code: "RSI"
 * Args:
 *   Data: DataChunk
 * Desc: Import a ruleset from data.  It will be merged with the
 *       loaded ruleset.
 * Response: OkResponse
 * ErrorResponses: BADDATA
 *
 * Name: ExportRulesetToFileRequest
 * Code: "RSS"
 * Args:
 *   FileName: String
 * Desc: Export the loaded ruleset to a file.
 * Response: OkResponse
 * ErrorResponses: FILEACCESS
 *
 * Name: ExportRulesetAsDataRequest
 * Code: "RSE"
 * Args:
 * Desc: Export the loaded ruleset as data.
 * Response: DataResponse
 * ErrorResponses: BADDATA
 *
 * Name: ListRulesRequest
 * Code: "RRL"
 * Args:
 * Desc: List all loaded rules in the current theory.
 * Response: StringListResponse
 * ErrorResponses:
 *
 * Name: ListActiveRulesRequest
 * Code: "RRA"
 * Args:
 * Desc: List all active rules in the current theory.
 * Response: StringListResponse
 * ErrorResponses:
 *
 * Name: OpenRuleLhsRequest
 * Code: "RRP"
 * Args:
 *   RuleName: String
 * Desc: Open the lhs of a rule as a graph.
 * Response: StringResponse
 * ErrorResponses: NOSUCHRULE
 *
 * Name: OpenRuleRhsRequest
 * Code: "RRQ"
 * Args:
 *   RuleName: String
 * Desc: Open the rhs of a rule as a graph.
 * Response: StringResponse
 * ErrorResponses: NOSUCHRULE
 *
 * Name: SetRuleRequest
 * Code: "RRU"
 * Args:
 *   RuleName: String
 *   LhsGraphName: String
 *   RhsGraphName: String
 * Desc: Create or update a rule.
 * Response: OkResponse
 * ErrorResponses: NOSUCHRULE | NOSUCHGRAPH | BADRULE
 *
 * Name: DeleteRuleRequest
 * Code: "RRD"
 * Args:
 *   RuleName: String
 * Desc: Delete a rule.
 * Response: OkResponse
 * ErrorResponses: NOSUCHRULE
 *
 * Name: ActivateRuleRequest
 * Code: "RRY"
 * Args:
 *   RuleName: String
 * Desc: Activate a rule.
 * Response: OkResponse
 * ErrorResponses: NOSUCHRULE
 *
 * Name: DeactivateRuleRequest
 * Code: "RRN"
 * Args:
 *   RuleName: String
 * Desc: Deactivate a rule.
 * Response: OkResponse
 * ErrorResponses: NOSUCHRULE
 *
 * Name: ListTagsRequest
 * Code: "RTL"
 * Args:
 * Desc: List all tags in the current theory.  If a tag exists if and only
 *       if there is at least one rule tagged with it.
 * Response: StringListResponse
 * ErrorResponses:
 *
 * Name: ListRulesByTagRequest
 * Code: "RTR"
 * Args:
 *   TagName: String
 * Desc: List all rules with the given tag in the current theory.
 *       If a tag does not exist, an empty list will be returned.
 * Response: StringListResponse
 * ErrorResponses:
 *
 * Name: TagRuleRequest
 * Code: "RTT"
 * Args:
 *   RuleName: String
 *   TagName: String
 * Desc: Tag a rule.
 * Response: OkResponse
 * ErrorResponses: NOSUCHRULE
 *
 * Name: UntagRuleRequest
 * Code: "RTU"
 * Args:
 *   RuleName: String
 *   TagName: String
 * Desc: Untag a rule.
 * Response: OkResponse
 * ErrorResponses: NOSUCHRULE | NOSUCHTAG
 *
 * Name: ForgetTagRequest
 * Code: "RTF"
 * Args:
 *   TagName: String
 * Desc: Remove a tag from all rules with the given tag.
 * Response: OkResponse
 * ErrorResponses: NOSUCHRULE | NOSUCHTAG
 *
 * Name: DeleteRulesByTagRequest
 * Code: "RTD"
 * Args:
 *   TagName: String
 * Desc: Delete all rules with the given tag.
 * Response: OkResponse
 * ErrorResponses: NOSUCHTAG
 *
 * Name: ActivateRulesByTagRequest
 * Code: "RTY"
 * Args:
 *   TagName: String
 * Desc: Activate all rules with the given tag.
 * Response: OkResponse
 * ErrorResponses: NOSUCHTAG
 *
 * Name: DeactivateRulesByTagRequest
 * Code: "RTN"
 * Args:
 *   TagName: String
 * Desc: Deactivate all rules with the given tag.
 * Response: OkResponse
 * ErrorResponses: NOSUCHTAG
 *
 * Name: AttachRewritesRequest
 * Code: "WA"
 * Args:
 *   GraphName: String
 *   VertexNames: StringList
 * Desc: Finds all possible rewrites for a subgraph using the
 *       currently active rules.  If no vertex names are given,
 *       the whole graph is used.  The number of rewrites found
 *       is returned.
 * Response: CountResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX
 *
 * Name: AttachOneRewriteRequest
 * Code: "WO"
 * Args:
 *   GraphName: String
 *   VertexNames: StringList
 * Desc: Finds an arbitrary rewrite (if one exists) for a subgraph
 *       using the currently active rules.  If no vertex names are given,
 *       the whole graph is used.  The number of rewrites found
 *       is returned.
 * Response: CountResponse ???
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX ???
 *
 * Name: ListAttachedRewritesRequest
 * Code: "WL"
 * Args:
 *   GraphName: String
 *   ResultFormat: ObjectResultFormat
 * Desc: Lists the attached rewrites for a graph.  If the requested
 *       result format is "name", only the rule applied will be returned
 *       for each attached rewrite.  Note that the same rule may well
 *       appear more than once in the list.
 * Response: StructuredDataResponse | XmlResponse | StringListResponse
 * ErrorResponses: NOSUCHGRAPH | BADSTATE
 *
 * Name: ApplyAttachedRewriteRequest
 * Code: "WW"
 * Args:
 *   GraphName: String
 *   Index ::= Integer ;
 * Desc: Applies one of the attached rewrites to a graph.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | BADSTATE | OUTOFRANGE
 *
 * }}} *)

structure ProtocolInterface : CONTROL_INTERFACE =
struct

  val ESC = Char.chr 27;

  fun protocolError error =
  let
      open OS.Process
  in
      (TextIO.output (TextIO.stdErr, "Protocol error: " ^ error ^ "\n"); TextIO.flushOut TextIO.stdErr) handle _ => ();
      exit failure;
      raise Fail "bad" (* Never called but sets return type as 'a *)
  end

  fun run_in_textstreams (instream,outstream) = 
  let
    open Commands
    open TextIO

    (* {{{ Output helpers *)
    fun print s = TextIO.output(outstream, s)
    fun printString s = print (String.translate (fn #"\027" => "\027\027" | c => String.str c) s)
    fun printInt i = print (Int.toString i)
    fun printLength s = printInt (String.size s)
    fun printEsc ch = print (String.concat["\027", String.str ch])
    fun printClose () = (printEsc #">"; TextIO.flushOut outstream)
    fun printOpen ch requestId = print (String.concat["\027<", String.str ch, "\027:", requestId, "\027|"])
    fun printOpenInfo ch = print (String.concat["\027<", String.str ch, "\027|"])
    fun printDelim () = print "\027;"
    fun printDataChunk s = (printEsc #"["; printLength s; printEsc #"|"; print s; printEsc #"]")
    fun printList [] = (print "0"; printEsc #":")
      | printList [s] = (print "1"; printEsc #":"; printString s)
      | printList (s::ss) = (print (Int.toString (1 + (length ss))); printEsc #":"; printString s; List.app (fn e => (printEsc #","; printString e)) ss)

    fun printWrappedString ch str = (printEsc ch; printString str; printEsc (Char.toLower ch))
    fun printWrappedData ch data = (printEsc ch; printDataChunk data; printEsc (Char.toLower ch))
    fun printComponentData ch ComponentData.Unit =
          (printEsc ch; printEsc (Char.toLower ch))
      | printComponentData ch (ComponentData.String str) = (
          printEsc ch;
          printWrappedString #"S" str;
          printEsc (Char.toLower ch))
      | printComponentData ch (ComponentData.LinratAngleExpr (expr as (c,varTab))) = (
          let
            val as_string = Pretty.string_of (LinratAngleExpr.pretty expr)
            fun printWrappedFrac ch (n,d) = (
                  printEsc ch;
                  printWrappedString #"N" (Int.toString n);
                  printWrappedString #"D" (Int.toString d);
                  printEsc (Char.toLower ch))
            fun printVar ch (var, coeff) _ = (
                  printEsc ch;
                  printWrappedString #"N" (X.string_of_name var);
                  printWrappedFrac #"C" coeff;
                  printEsc (Char.toLower ch))
          in
            printEsc ch;
            printEsc #"A";
              printWrappedString #"S" as_string;
              if LinratAngleExpr.Coeff.is_zero c then () else
                printWrappedFrac #"K" c;
              X.NTab.fold (printVar #"V") varTab ();
            printEsc #"a";
            printEsc (Char.toLower ch)
          end)
    fun printVertexDesc { name, vType, data, userData } = (
          printEsc #"V";
          printWrappedString #"N" name;
          printWrappedString #"T" vType;
          printComponentData #"D" data;
          printWrappedData #"U" userData;
          printEsc #"v");
    fun printEdgeDesc' { name, eType, sourceName, targetName, directed, data, userData } = (
          printWrappedString #"N" name;
          printWrappedString #"T" eType;
          printEsc #"A"; printString sourceName; printEsc #"a";
          printEsc #"B"; printString targetName; printEsc #"b";
          printComponentData #"D" data;
          printWrappedData #"U" userData)
    fun printEdgeDesc (desc as { name, eType, sourceName, targetName, directed as true, data, userData }) = (
          printEsc #"E";
          printEdgeDesc' desc;
          printEsc #"e");
    fun printEdgeDesc (desc as { name, eType, sourceName, targetName, directed as false, data, userData }) = (
          printEsc #"F";
          printEdgeDesc' desc;
          printEsc #"f");
    fun printBangBoxDesc { name, vertexNames } = (
          printEsc #"B";
          printWrappedString #"N" name;
          map (printWrappedString #"V") vertexNames;
          printEsc #"b");
    fun printGraphDesc { name, vertices, edges, bangBoxen } = (
          printEsc #"G";
          printWrappedString #"N" name;
          map (printVertexDesc) vertices;
          map (printEdgeDesc) edges;
          map (printBangBoxDesc) bangBoxen;
          printEsc #"g");
    fun printRewriteDesc { ruleName, result, lhs, rhs } = (
          printEsc #"W";
          printEsc #"R";
            printWrappedString #"N" ruleName;
            printEsc #"L"; printGraphDesc lhs; printEsc #"l";
            printEsc #"R"; printGraphDesc rhs; printEsc #"r";
          printEsc #"r";
          printGraphDesc result;
          printEsc #"w");
    fun printRewriteListDesc rws = (
          printEsc #"L";
          map printRewriteDesc rws;
          printEsc #"l");
    (* }}} *)

    (* {{{ outputResponse *)
    fun outputResponse (requestId, OkResponse) =
          (printOpen #"O" requestId; printClose ())
      | outputResponse (requestId, ErrorResponse { errorCode, message }) =
         (printOpen #"Q" requestId;
          print errorCode;
          printDelim ();
          printString message;
          printClose ())
      | outputResponse (requestId, DataResponse { data }) =
         (printOpen #"R" requestId;
          printDataChunk data;
          printClose ())
      | outputResponse (requestId, PrettyResponse { data }) =
          let val s = Pretty.string_of data in
            printOpen #"P" requestId;
            printDataChunk s;
            printClose ()
          end
      | outputResponse (requestId, XmlResponse { data }) =
          let val xml = XML.string_of data in
            printOpen #"X" requestId;
            printDataChunk xml;
            printClose ()
          end
      | outputResponse (requestId, CountResponse { count }) =
         (printOpen #"I" requestId;
          printInt count;
          printClose ())
      | outputResponse (requestId, StringResponse { data }) =
         (printOpen #"N" requestId;
          printString data;
          printClose ())
      | outputResponse (requestId, StringListResponse { data }) =
         (printOpen #"M" requestId;
          printList data;
          printClose ())
      | outputResponse (requestId, GraphDescriptionResponse { graph }) =
         (printOpen #"S" requestId;
          printGraphDesc graph;
          printClose ())
      | outputResponse (requestId, VertexDescriptionResponse { vertex }) =
         (printOpen #"S" requestId;
          printVertexDesc vertex;
          printClose ())
      | outputResponse (requestId, EdgeDescriptionResponse { edge }) =
         (printOpen #"S" requestId;
          printEdgeDesc edge;
          printClose ())
      | outputResponse (requestId, BangBoxDescriptionResponse { bangBox }) =
         (printOpen #"S" requestId;
          printBangBoxDesc bangBox;
          printClose ())
      | outputResponse (requestId, ComponentDataResponse { data }) =
         (printOpen #"S" requestId;
          printComponentData #"D" data;
          printClose ())
      | outputResponse (requestId, RewriteListResponse { rewrites }) =
         (printOpen #"S" requestId;
          printRewriteListDesc rewrites;
          printClose ())
      | outputResponse (requestId, UserDataResponse { data }) =
         (printOpen #"U" requestId;
          printDataChunk data;
          printClose ())
      | outputResponse (requestId, UnknownResponse { code }) =
         (printOpen #"Z" requestId;
          print code;
          printClose ())

    fun outputOkResponse requestId = (outputResponse (requestId, OkResponse))
    fun outputNameResponse requestId name =
          (outputResponse (requestId, StringResponse { data = name }))
    fun outputErrorResponse requestId code message =
          (outputResponse (requestId, ErrorResponse { errorCode = code, message = message }))
    fun outputStringListResponse requestId items =
          (outputResponse (requestId, StringListResponse { data = items }))
    (* }}} *)

    (* {{{ Input helpers *)
    (* Returns the string as far as the next ESC and the terminator. *)
    local
      fun readToEscape' (soFar: string) (terminator: char) : string =
        case input1 instream
          of SOME #"\027" =>
              (
                case input1 instream of
                  NONE => protocolError "End of file"
                | SOME #"\027" => (* Escaped ESC. *)
                                    readToEscape' (soFar ^ str #"\027") terminator
                | SOME ch => if ch = terminator
                             then soFar
                             else protocolError(str ch ^ " not " ^ str terminator)
              )
           | SOME ch => readToEscape' (soFar ^ str ch) terminator
           | NONE => protocolError "End of file"
    in
      val readToEscape = readToEscape' ""
    end

    fun readChar () =
      case input1 instream
        of NONE => protocolError "End of file"
         | SOME ch => ch

    fun eatChar (ch: char) =
      case input1 instream
        of NONE => protocolError "End of file"
         | SOME c => if c = ch then () else
                     protocolError("Expected " ^ (str ch) ^ ", got " ^ (str c))

    fun eatEscapedChar (ch: char) = (eatChar #"\027"; eatChar ch)
    fun readEscapedChar () = (eatChar #"\027"; readChar ())

    (* Parse an integer.  Returns zero if it isn't a valid int. *)
    fun readInt termCh : int =
      case Int.fromString (readToEscape termCh) of
          NONE => 0
        | SOME i => i

    local
      fun readList' _ 0 (_, terminator) = (eatEscapedChar terminator; [])
        | readList' _ 1 (_, terminator) = [readToEscape terminator]
        | readList' soFar count (splitter, terminator) =
           (case input1 instream
              of SOME #"\027" =>
                  (
                    case input1 instream
                      of NONE           => protocolError "End of file"
                       | SOME #"\027" => readList' (soFar ^ "\027") count (splitter, terminator)
                       | SOME ch        =>
                            if ch = splitter then
                              soFar::(readList' "" (count-1) (splitter, terminator))
                            else if ch = terminator then
                              protocolError("Wrong number of elements in list")
                            else
                              protocolError(str ch ^ " not " ^ str terminator)
                  )
               | SOME ch => readList' (soFar ^ str ch) count (splitter, terminator)
               | NONE    => protocolError "End of file"
               )
    in
      fun readList (splitter:char,terminator:char) : string list =
        readList' "" (readInt #":") (splitter,terminator)
    end

    fun readHeader () : (string * string) =
      let
        val () =
          case input1 instream of
            NONE => OS.Process.exit OS.Process.success (* Close down. *)
          | SOME #"\027" => () (* Escape- start of packet. *)
          | SOME ch => protocolError((str ch) ^ " not ESCAPE at start of packet")
        val () = eatChar #"<"
        val code = readToEscape #":"
        val requestId = readToEscape #"|"
      in
        (code, requestId)
      end

    local
      fun finishDataChunk () =
        let
          val dataLength = readInt #"|"
          val data = TextIO.inputN (instream, dataLength)
          val () = eatEscapedChar #"]"
        in
          data
        end;
    in
      fun readDataChunk () = (eatEscapedChar #"["; finishDataChunk ())

      fun skipToBodyEnd () =
        case input1 instream
          of SOME #"\027" =>
              (
                case input1 instream of
                  NONE => protocolError "End of file"
                | SOME #">" => () (* done *)
                | SOME #"[" => let val _ = finishDataChunk () in () end
                | SOME _ => skipToBodyEnd ()
              )
           | SOME _ => skipToBodyEnd ()
           | NONE => protocolError "End of file"

      fun readToBodyEnd () = readToEscape #">"
    end

    fun readComponentData () =
      let
        val mode = readEscapedChar ()
      in
        case mode
          of #"X" =>
            let val data = readDataChunk () in
              (SOME (XmlCData [XMLReader.read_from_string data]))
            end
           | #"N" => SOME (StringCData (readDataChunk ()))
           | _    => NONE
      end

    fun readToEnd () = readToEscape #">"
    fun readToDelim () = readToEscape #";"
    fun readListToDelim () = readList (#",",#";")
    fun readListToEnd () = readList (#",",#">")

    fun assertAtDelim () = eatEscapedChar #";";
    fun assertAtEnd () = eatEscapedChar #">";
    (* }}} *)

    (* {{{ readRequestBody *)
    fun readRequestBody commandCode =
      case commandCode
        of "TG"   => (assertAtEnd (); TheoryNameRequest)
         | "GL"   => (assertAtEnd (); ListGraphsRequest)
         | "GOE"  =>  LoadGraphRequest {
                        details = LoadEmptyGraphRequest {
                          requestedName = readToEnd ()
                        }
                      }
         | "GOF"  =>  LoadGraphRequest {
                        details = LoadGraphFromFileRequest {
                          fileName = readToEnd()
                        }
                      }
         | "GOD"  =>  let
                        val requestedName = readToDelim ()
                        val data = readDataChunk ()
                        val () = assertAtEnd ()
                      in
                        LoadGraphRequest {
                          details = LoadGraphFromDataRequest {
                            data = data,
                            requestedName = requestedName
                          }
                        }
                      end
         | "GOG"  =>  LoadGraphRequest {
                        details = CopyGraphRequest {
                          graphName = readToEnd ()
                        }
                      }

         | "GOS"  => let
                       val sourceGraphName = readToDelim ()
                       val graphName = readToDelim ()
                       val vertexNames = readListToEnd ()
                     in
                       LoadGraphRequest {
                         details = CopySubgraphRequest {
                                     source = sourceGraphName,
                                     graphName = graphName,
                                     vertexNames = vertexNames
                                   }
                       }
                     end

         | "GS"   =>  let
                        val graphName = readToDelim ()
                        val fileName  = readToEnd ()
                      in
                        SaveGraphToFileRequest {
                          graphName = graphName,
                          fileName = fileName
                        }
                      end

         | "GR"   =>  let
                        val oldName = readToDelim ()
                        val newName = readToEnd ()
                      in
                        RenameGraphRequest {
                          oldName = oldName,
                          newName = newName
                        }
                      end

         | "GD"   =>  DiscardGraphRequest {
                        graphName = readToEnd ()
                      }

         | "GE"   =>  let
                        val graphName  = readToDelim ()
                        val formatName = readToEnd ()
                        val format     = case formatName
                                           of "native"      => SOME NativeGraphFormat
                                            | "xml"         => SOME XmlGraphFormat
                                            | "pretty"      => SOME PrettyGraphFormat
                                            | "hilb"        => SOME HilbertSpaceGraphFormat
                                            | "mathematica" => SOME MathematicaGraphFormat
                                            | "matlab"      => SOME MatlabGraphFormat
                                            | "tikz"        => SOME TikzGraphFormat
                                            | _             => NONE
                      in
                        case format
                          of (SOME f) =>
                                ExportGraphRequest {
                                  graphName = graphName,
                                  format = f
                                }
                           | NONE =>
                                DummyRequest { response =
                                  ErrorResponse {
                                    errorCode = "BADFORMAT",
                                    message = "Unknown format requested"
                                  }
                                }
                      end

         | "GVA"  =>  DescribeGraphRequest {
                        graphName = readToEnd (),
                        desc = DescribeFullGraph
                      }

         | "GVGU" => let
                        val graphName = readToDelim()
                        val dataName = readToEnd ()
                     in
                       DescribeGraphRequest {
                         graphName = graphName,
                         desc = GraphUserData { dataName = dataName }
                       }
                     end
         | "GVVL" =>  DescribeGraphRequest {
                        graphName = readToEnd (),
                        desc = ListVertices
                      }

         | "GVEL" =>  DescribeGraphRequest {
                        graphName = readToEnd (),
                        desc = ListEdges
                      }

         | "GVBL" =>  DescribeGraphRequest {
                        graphName = readToEnd (),
                        desc = ListBangBoxes
                      }

         | "GVVD" =>  let
                        val graphName  = readToDelim ()
                        val vertexName  = readToDelim ()
                        val format = case readToEnd ()
                                       of "xml"    => SOME XmlFormat
                                        (*| "native" => SOME DescriptionFormat*)
                                        | "pretty" => SOME PrettyFormat
                                        | _        => NONE
                      in
                        case format
                          of (SOME f) =>
                              DescribeGraphRequest {
                                graphName = graphName,
                                desc = VertexData {
                                  vertexName = vertexName,
                                  format = f
                                }
                              }
                           | NONE =>
                              DummyRequest { response =
                                ErrorResponse {
                                  errorCode = "BADFORMAT",
                                  message = "Unknown format requested"
                                }
                              }
                      end

         | "GVVU" =>  let
                        val graphName  = readToDelim ()
                        val vertexName = readToDelim ()
                        val dataName = readToEnd ()
                      in
                        DescribeGraphRequest {
                          graphName = graphName,
                          desc = VertexUserData { vertexName = vertexName,
                                                  dataName = dataName }
                        }
                      end

         | "GVED" =>  let
                        val graphName = readToDelim ()
                        val edgeName  = readToDelim ()
                        val format = case readToEnd ()
                                       of "xml"    => SOME XmlFormat
                                        (*| "native" => SOME DescriptionFormat*)
                                        | "pretty" => SOME PrettyFormat
                                        | _        => NONE
                      in
                        case format
                          of (SOME f) =>
                              DescribeGraphRequest {
                                graphName = graphName,
                                desc = EdgeData {
                                  edgeName = edgeName,
                                  format = f
                                }
                              }
                           | NONE =>
                              DummyRequest { response =
                                ErrorResponse {
                                  errorCode = "BADFORMAT",
                                  message = "Unknown format requested"
                                }
                              }
                      end

         | "GVEU" =>  let
                        val graphName  = readToDelim ()
                        val edgeName = readToDelim ()
                        val dataName = readToEnd ()
                      in
                        DescribeGraphRequest {
                          graphName = graphName,
                          desc = EdgeUserData { edgeName = edgeName,
                                                  dataName = dataName }
                        }
                      end

         | "GVBV" =>  let
                        val graphName   = readToEscape #";"
                        val bangBoxName = readToEnd ()
                      in
                        DescribeGraphRequest {
                          graphName = graphName,
                          desc = BangBoxVertices { bangBoxName = bangBoxName }
                        }
                      end

         | "GVBU" =>  let
                        val graphName  = readToDelim ()
                        val bangBoxName = readToDelim ()
                        val dataName = readToEnd ()
                      in
                        DescribeGraphRequest {
                          graphName = graphName,
                          desc = BangBoxUserData { bangBoxName = bangBoxName,
                                                  dataName = dataName }
                        }
                      end

         | "GMU"  =>  EditGraphRequest {
                        graphName = readToEnd (),
                        action = UndoAction
                      }

         | "GMR"  =>  EditGraphRequest {
                        graphName = readToEnd (),
                        action = RedoAction
                      }
         | "GMUR"  =>  EditGraphRequest {
                        graphName = readToEnd (),
                        action = UndoRewriteAction
                      }

         | "GMRR"  =>  EditGraphRequest {
                        graphName = readToEnd (),
                        action = RedoRewriteAction
                      }

         | "GMSU"  =>  EditGraphRequest {
                        graphName = readToEnd (),
                        action = StartUndoGroupAction
                      }

         | "GMFU"  =>  EditGraphRequest {
                        graphName = readToEnd (),
                        action = EndUndoGroupAction
                      }

         | "GMI"  =>  let
                        val graphName = readToDelim ()
                        val sourceGraphName = readToEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  InsertGraphAction {
                                      sourceGraphName = sourceGraphName
                                    }
                        }
                      end

         | "GMGU" =>  let
                        val graphName = readToDelim ()
                        val dataName =  readToDelim ()
                        val userData = readDataChunk ()
                        val () = assertAtEnd ()
                       in
                         EditGraphRequest {
                           graphName = graphName,
                           action = SetGraphUserDataAction {
                                      dataName = dataName,
                                      userData = userData
                                    }        
                         }
                       end

         | "GMDGU" =>  let
                        val graphName = readToDelim ()
                        val dataName =  readToEnd ()
                       in
                         EditGraphRequest {
                           graphName = graphName,
                           action = DeleteGraphUserDataAction {
                                      dataName = dataName
                                    }        
                         }
                       end

         | "GMVA" =>  let
                        val graphName = readToDelim ()
                        val vertexType = readToDelim ()
                        val format = case readToEnd ()
                                       of "xml"    => SOME XmlFormat
                                        | "native" => SOME DescriptionFormat
                                        | "pretty" => SOME PrettyFormat
                                        | "name"   => SOME BasicFormat
                                        | _        => NONE
                      in
                        case format
                          of (SOME f) =>
                              EditGraphRequest {
                                graphName = graphName,
                                action =  AddVertexAction {
                                            vertexType = vertexType,
                                            resultFormat = f
                                          }
                              }
                           | NONE =>
                              DummyRequest { response =
                                ErrorResponse {
                                  errorCode = "BADFORMAT",
                                  message = "Unknown format requested"
                                }
                              }
                      end

         | "GMVR" =>  let
                        val graphName = readToDelim ()
                        val oldName = readToDelim ()
                        val newName = readToEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  RenameVertexAction {
                                      oldName = oldName,
                                      newName = newName
                                    }
                        }
                      end

         | "GMVD" =>  let
                        val graphName = readToDelim ()
                        val vertexNames = readListToEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action = DeleteVerticesAction {
                            vertexNames = vertexNames
                          }
                        }
                      end

         | "GMVS" => (let
                        val graphName = readToDelim ()
                        val vertexName = readToDelim ()
                        val data = readComponentData ()
                        val () = assertAtEnd ()
                      in
                        case data
                          of (SOME d) =>
                              EditGraphRequest {
                                graphName = graphName,
                                action =  SetVertexDataAction {
                                            vertexName = vertexName,
                                            vertexData = d
                                          }
                              }
                           | NONE =>
                              DummyRequest { response =
                                ErrorResponse {
                                  errorCode = "BADFORMAT",
                                  message = "Unknown vertex data format"
                                }
                              }
                      end
                      handle XMLReader.bad_input_exp msg =>
                              DummyRequest { response =
                                ErrorResponse {
                                  errorCode = "BADDATA",
                                  message = ("Malformed XML ("^msg^")")
                                }
                              })

         | "GMVU" =>  let
                        val graphName = readToDelim ()
                        val vertexName = readToDelim ()
                        val dataName = readToDelim ()
                        val data = readDataChunk ()
                        val () = assertAtEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  SetVertexUserDataAction {
                                      vertexName = vertexName,
                                      dataName = dataName,
                                      userData = data
                                    }
                        }
                      end

         | "GMDVU" =>  let
                        val graphName = readToDelim ()
                        val vertexName = readToDelim ()
                        val dataName = readToEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  DeleteVertexUserDataAction {
                                      vertexName = vertexName,
                                      dataName = dataName
                                    }
                        }
                      end

         | "GMEA" =>  let
                        val graphName = readToDelim ()
                        val edgeType = readToDelim ()
                        val d_or_u = readToDelim ()
                        val directed = if d_or_u = "d" then true else
                                       if d_or_u = "u" then false else
                                       protocolError "Expected \"d\" or \"u\""
                        val source = readToDelim ()
                        val target = readToDelim ()
                        val format = case readToEnd ()
                                       of "xml"    => SOME XmlFormat
                                        | "native" => SOME DescriptionFormat
                                        | "pretty" => SOME PrettyFormat
                                        | "name"   => SOME BasicFormat
                                        | _        => NONE
                      in
                        case format
                          of (SOME f) =>
                              EditGraphRequest {
                                graphName = graphName,
                                action =  AddEdgeAction {
                                            source = source,
                                            target = target,
                                            directed = directed,
                                            edgeType = edgeType,
                                            resultFormat = f
                                          }
                              }
                           | NONE =>
                              DummyRequest { response =
                                ErrorResponse {
                                  errorCode = "BADFORMAT",
                                  message = "Unknown format requested"
                                }
                              }
                      end

         | "GMER" =>  let
                        val graphName = readToDelim ()
                        val oldName = readToDelim ()
                        val newName = readToEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  RenameEdgeAction {
                                      oldName = oldName,
                                      newName = newName
                                    }
                        }
                      end

         | "GMED" =>  let
                        val graphName = readToDelim ()
                        val edgeNames = readListToEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  DeleteEdgesAction {
                                      edgeNames = edgeNames
                                    }
                        }
                      end

         | "GMES" => (let
                        val graphName = readToDelim ()
                        val edgeName = readToDelim ()
                        val data = readComponentData ()
                        val () = assertAtEnd ()
                      in
                        case data
                          of (SOME d) =>
                              EditGraphRequest {
                                graphName = graphName,
                                action =  SetEdgeDataAction {
                                            edgeName = edgeName,
                                            edgeData = d
                                          }
                              }
                           | NONE =>
                              DummyRequest { response =
                                ErrorResponse {
                                  errorCode = "BADFORMAT",
                                  message = "Unknown vertex data format"
                                }
                              }
                      end
                      handle XMLReader.bad_input_exp msg =>
                              DummyRequest { response =
                                ErrorResponse {
                                  errorCode = "BADDATA",
                                  message = ("Malformed XML ("^msg^")")
                                }
                              })

         | "GMEU" =>  let
                        val graphName = readToDelim ()
                        val edgeName = readToDelim ()
                        val dataName = readToDelim ()
                        val data = readDataChunk ()
                        val () = assertAtEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  SetEdgeUserDataAction {
                                      edgeName = edgeName,
                                      dataName = dataName,
                                      userData = data
                                    }
                        }
                      end

         | "GMDEU" =>  let
                        val graphName = readToDelim ()
                        val edgeName = readToDelim ()
                        val dataName = readToEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  DeleteEdgeUserDataAction {
                                      edgeName = edgeName,
                                      dataName = dataName
                                    }
                        }
                      end

         | "GMBA" =>  let
                        val graphName = readToDelim ()
                        val vertexNames = readListToDelim ()
                        val format = case readToEnd ()
                                       of "xml"    => SOME XmlFormat
                                        | "native" => SOME DescriptionFormat
                                        | "pretty" => SOME PrettyFormat
                                        | "name"   => SOME BasicFormat
                                        | _        => NONE
                      in
                        case format
                          of (SOME f) =>
                              EditGraphRequest {
                                graphName = graphName,
                                action =  AddBangBoxAction {
                                            vertexNames = vertexNames,
                                            resultFormat = f
                                          }
                              }
                           | NONE =>
                              DummyRequest { response =
                                ErrorResponse {
                                  errorCode = "BADFORMAT",
                                  message = "Unknown format requested"
                                }
                              }
                      end

         | "GMBR" =>  let
                        val graphName = readToDelim ()
                        val oldName = readToDelim ()
                        val newName = readToEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  RenameBangBoxAction {
                                      oldName = oldName,
                                      newName = newName
                                    }
                        }
                      end

         | "GMBD" =>  let
                        val graphName = readToDelim ()
                        val bangBoxNames = readListToEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  DropBangBoxesAction {
                                      bangBoxNames = bangBoxNames
                                    }
                        }
                      end

         | "GMBK" =>  let
                        val graphName = readToDelim ()
                        val bangBoxNames = readListToEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  KillBangBoxesAction {
                                      bangBoxNames = bangBoxNames
                                    }
                        }
                      end

         | "GMBC" =>  let
                        val graphName = readToDelim ()
                        val bangBoxName = readToEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  DuplicateBangBoxAction {
                                      bangBoxName = bangBoxName
                                    }
                        }
                      end

         | "GMBM" =>  let
                        val graphName = readToDelim ()
                        val bangBoxNames = readListToEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  MergeBangBoxesAction {
                                      bangBoxNames = bangBoxNames
                                    }
                        }
                      end

         | "GMBB" =>  let
                        val graphName = readToDelim ()
                        val bangBoxName = readToDelim ()
                        val vertexNames = readListToEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  BangVerticesAction {
                                      bangBoxName = bangBoxName,
                                      vertexNames = vertexNames
                                    }
                        }
                      end

         | "GMBL" =>  let
                        val graphName = readToDelim ()
                        val vertexNames = readListToEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  UnbangVerticesAction {
                                      vertexNames = vertexNames
                                    }
                        }
                      end

         | "GMBU" =>  let
                        val graphName = readToDelim ()
                        val bangBoxName = readToDelim ()
                        val dataName = readToDelim ()
                        val data = readDataChunk ()
                        val () = assertAtEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  SetBangBoxUserDataAction {
                                      bangBoxName = bangBoxName,
                                      dataName = dataName,
                                      userData = data
                                    }
                        }
                      end

         | "GMDBU" =>  let
                        val graphName = readToDelim ()
                        val bangBoxName = readToDelim ()
                        val dataName = readToEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  DeleteBangBoxUserDataAction {
                                      bangBoxName = bangBoxName,
                                      dataName = dataName
                                    }
                        }
                      end

         | "RSO" =>  ImportRulesetFromFileRequest {
                                fileName = readToEnd (),
                                replace = false
                              }

         | "RSP" =>  ImportRulesetFromFileRequest {
                                fileName = readToEnd (),
                                replace = true
                              }

         | "RSI"  =>  let
                        val data = readDataChunk ()
                        val () = assertAtEnd ()
                      in
                        ImportRulesetFromDataRequest {
                          data = data,
                          replace = false
                        }
                      end

         | "RSJ"  =>  let
                        val data = readDataChunk ()
                        val () = assertAtEnd ()
                      in
                        ImportRulesetFromDataRequest {
                          data = data,
                          replace = true
                        }
                      end

         | "RSS"  =>  ExportRulesetToFileRequest {
                        fileName = readToEnd()
                      }

         | "RSE"  => (assertAtEnd (); ExportRulesetAsDataRequest)

         | "RRL"  => (assertAtEnd (); ListRulesRequest)

         | "RRA"  => (assertAtEnd (); ListActiveRulesRequest)

         | "RRP"  =>  OpenRuleLhsRequest {
                        ruleName = readToEnd()
                      }

         | "RRQ"  =>  OpenRuleRhsRequest {
                        ruleName = readToEnd()
                      }

         | "RRU"  =>  let
                        val ruleName = readToDelim ()
                        val lhsName = readToDelim ()
                        val rhsName = readToEnd ()
                      in
                        SetRuleRequest {
                          ruleName = ruleName,
                          lhsGraphName = lhsName,
                          rhsGraphName = rhsName
                        }
                      end

         | "RRD"  =>  DeleteRuleRequest {
                        ruleName = readToEnd()
                      }

         | "RRY"  =>  ActivateRuleRequest {
                        ruleName = readToEnd()
                      }

         | "RRN"  =>  DeactivateRuleRequest {
                        ruleName = readToEnd()
                      }

         | "RRR"  =>  RenameRuleRequest {
                        oldName = readToDelim(),
                        newName = readToEnd()
                      }
         | "RRR"  =>  let
                        val oldName = readToDelim ()
                        val newName = readToEnd ()
                      in
                        RenameRuleRequest {
                          oldName = oldName,
                          newName = oldName
                        }
                      end

         | "RTL"  => (assertAtEnd (); ListTagsRequest)

         | "RTR"  =>  ListRulesByTagRequest {
                        tagName = readToEnd()
                      }

         | "RTT"  =>  let
                        val ruleName = readToDelim ()
                        val tagName = readToEnd ()
                      in
                        TagRuleRequest {
                          ruleName = ruleName,
                          tagName = tagName
                        }
                      end

         | "RTU"  =>  let
                        val ruleName = readToDelim ()
                        val tagName = readToEnd ()
                      in
                        UntagRuleRequest {
                          ruleName = ruleName,
                          tagName = tagName
                        }
                      end

         | "RTF"  =>  ForgetTagRequest {
                        tagName = readToEnd ()
                      }

         | "RTD"  =>  DeleteRulesByTagRequest {
                        tagName = readToEnd ()
                      }

         | "RTY"  =>  ActivateRulesByTagRequest {
                        tagName = readToEnd ()
                      }

         | "RTN"  =>  DeactivateRulesByTagRequest {
                        tagName = readToEnd ()
                      }

         | "WA"   =>  let
                        val graphName = readToDelim ()
                        val vertexNames = readListToEnd ()
                      in
                        AttachRewritesRequest {
                          graphName = graphName,
                          vertexNames = vertexNames
                        }
                      end

         | "WO"   =>  let
                        val graphName = readToDelim ()
                        val vertexNames = readListToEnd ()
                      in
                        AttachOneRewriteRequest {
                          graphName = graphName,
                          vertexNames = vertexNames
                        }
                      end

         | "WL" =>    let
                        val graphName = readToDelim ()
                        val format = case readToEnd ()
                                       of "xml"    => SOME XmlFormat
                                        | "native" => SOME DescriptionFormat
                                        | "pretty" => SOME PrettyFormat
                                        | "name"   => SOME BasicFormat
                                        | _        => NONE
                      in
                        case format
                          of (SOME f) =>
                              ListAttachedRewritesRequest {
                                graphName = graphName,
                                format = f
                              }
                           | NONE =>
                              DummyRequest { response =
                                ErrorResponse {
                                  errorCode = "BADFORMAT",
                                  message = "Unknown format requested"
                                }
                              }
                      end

         | "WW"   =>  let
                        val graphName = readToDelim ()
                        val index = readInt #">"
                      in
                        ApplyAttachedRewriteRequest {
                          graphName = graphName,
                          index = index
                        }
                      end

         | code   =>  (* Something else.  Reply with empty response. *)
                     (skipToBodyEnd ();
                      UnknownRequest {
                        code = code
                      })
      (* }}} *)

    fun run_in_textstreams' eval =
    let
      val (commandCode,requestId) = readHeader ()
      fun next_loop () = run_in_textstreams' eval
      fun change_loop new_eval = run_in_textstreams' new_eval

    in
      case commandCode
        of "CC" =>  let (* Console command. *)
                      val command = readDataChunk ()
                      val _ = assertAtEnd ()
                      fun outputCmdResult data =
                             (printOpen #"C" requestId;
                              printDataChunk data;
                              printClose ())
                    in
                      (case Console.eval_command_string_for_gui eval command
                        of InterfaceResult.NORMAL output =>
                            (outputCmdResult output; next_loop ())
                         | InterfaceResult.CHANGE_THEORY th =>
                            (case ControllerRegistry.get_eval_func th
                               of NONE =>
                                   (outputCmdResult ("!!! Unknown theory \""^th^"\"");
                                    next_loop ())
                                | (SOME func) =>
                                   (outputCmdResult "ok";
                                    change_loop func))
                         | InterfaceResult.SKIP => (outputCmdResult ""; next_loop ())
                         | InterfaceResult.QUIT => ())
                    end

         | "CL" => (assertAtEnd();
                    printOpen #"M" requestId;
                    printList Console.command_name_list;
                    printClose ();
                    next_loop ())

         | "CH" =>  let
                      val commandName = readToEnd ()
                    in
                      printOpen #"H" requestId;
                      print (the (Console.get_command_args commandName));
                      printDelim ();
                      print (the (Console.get_command_help commandName));
                      printClose ();
                      next_loop ()
                    end

         | "TS" => (let
                      val theoryName = readToEnd ()
                    in
                      case (ControllerRegistry.get_eval_func theoryName)
                        of (SOME func) =>
                             (outputOkResponse requestId;
                              change_loop func)
                         | NONE =>
                             (outputErrorResponse requestId "BADTHEORY" "Unknown theory";
                              next_loop())
                    end)

         | "TL" => (assertAtEnd();
                    outputStringListResponse requestId (ControllerRegistry.get_theories());
                    next_loop())

         | code => (outputResponse (Commands.serviceRequestMessage eval
                                      (requestId, (readRequestBody code)));
                    next_loop())

    end
  in
    printOpenInfo #"V";
    print "1.0";
    printClose ();
    run_in_textstreams' DummyController.serviceRequest
  end;

  fun flushIn () = case TextIO.canInput (TextIO.stdIn, 1)
                        of SOME n => (TextIO.input1 TextIO.stdIn;())
                         | NONE => ();

  fun run_in_stdio () = (flushIn(); run_in_textstreams (TextIO.stdIn,TextIO.stdOut));
end;

(* vi:ts=2:sw=2:sts=2:et:foldmethod=marker
*)
