(* {{{ Protocol documentation
 *
 * This file implements a protocol for talking to other tools.  It uses the
 * escape character (\u001b) as a tag for structural elements.  It is inspired
 * by the Poly/ML IDE protocol.
 *
 * The protocol consists of a series of requests (sent from the tool to the
 * core) and their responses (sent from the core to the tool).  Each of these is
 * termed a "message" - a single cohesive and complete chunk of data.  There is
 * one message that is neither a request nor a response, but informational: the
 * version message.  There is potential scope for other informational messages
 * in later revisions of the protocol.
 *
 * ESC ::= '\u001b' ;
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
 *
 ******************** RESPONSES **********************
 *
 * The possible responses the core can generate are:
 *
 * Name: ErrorResponse
 * Code: "E"
 * Body:
 *   ErrorIdent ::= [A-Z]+ ;
 *   ErrorMessage ::= String ;
 *   ErrorResponseBody ::= ErrorIdent ESC ';' ErrorMessage ;
 * Desc: The request could not be fulfilled.
 *
 * Name: OkResponse
 * Code: "O"
 * Body:
 *   OkResponseBody ::= ;
 * Desc: The requested action completed successfully.
 *
 * Name: ConsoleResponse
 * Code: "C"
 * Body:
 *   ConsoleResponseBody ::= DataChunk ;
 * Desc: The response to a console command.
 *
 * Name: ConsoleHelpResponse
 * Code: "H"
 * Body:
 *   CommandArgs ::= String ;
 *   CommandHelp ::= String ;
 *   HelpResponseBody ::= CommandArgs ESC ';' CommandHelp ;
 * Desc: The argument list and help text for a command
 *
 * Name: DataResponse
 * Code: "R"
 * Body:
 *   DataResponseBody ::= DataChunk ;
 * Desc: Raw data of some description; not intended to be parsed.
 *
 * Name: PrettyResponse
 * Code: "P"
 * Body:
 *   PrettyResponseBody ::= DataChunk ;
 * Desc: "Pretty-print" data, for displaying to humans; not
 *       intended to be parsed.
 *
 * Name: XmlResponse
 * Code: "X"
 * Body:
 *   XmlResponseBody ::= DataChunk ;
 * Desc: XML-formatted data.
 *
 * Name: CountResponse
 * Code: "I"
 * Body:
 *   CountResponseBody ::= Integer ;
 * Desc: A number, usually the quantity of something.
 *
 * Name: NameResponse
 * Code: "N"
 * Body:
 *   NameResponseBody ::= String ;
 * Desc: A string, usually a name.
 *
 * Name: NameListResponse
 * Code: "M"
 * Body:
 *   NameListRespBody ::= StringList ;
 * Desc: A list of strings, usually names.
 *
 * Name: UserDataResponse
 * Code: "U"
 * Body:
 *   UserDataRespBody ::= DataChunk ;
 * Desc: Data that was previously supplied by the tool.
 *
 * Name: RewriteListResponse
 * Code: "W"
 * Body:
 *   PrettyChunk ::= ESC 'P' DataChunk ;
 *   XmlChunk ::= ESC 'X' DataChunk ;
 *   GraphChunk ::= (PrettyChunk | XmlChunk) ;
 *   RuleName ::= String ;
 *   NewGraph ::= GraphChunk ;
 *   RuleLhsGraph ::= GraphChunk ;
 *   RuleRhsGraph ::= GraphChunk ;
 *   Rewrite ::= RuleName ESC ',' NewGraph ESC ',' RuleLhsGraph ESC ',' RuleRhsGraph ;
 *   RewriteListResponseBody ::= (Rewrite (ESC ';' Rewrite)* )? ;
 * Desc: A list of rewrites.
 *
 * Name: UnknownRequestResponse
 * Code: "Z"
 * Body:
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
 * The possible requests are:
 *
 * Name: ConsoleCommandRequest
 * Code: "CC"
 * Body:
 *   ConsoleCommandRequestBody ::= DataChunk ;
 * Desc: Execute user-console command
 * Responses: ConsoleResponse
 * Error responses:
 *
 * Name: ConsoleCommandListRequest
 * Code: "CL" ;
 * Body
 *   ConsoleCommandListRequestBody ::= ;
 * Desc: Get the list of possible user-console commands
 * Responses: NameListResponse
 * Error responses:
 *
 * Name: ConsoleHelpRequest
 * Code: "CH"
 * Body:
 *   CommandName ::= String ;
 *   ConsoleHelpRequestBody ::= CommandName ;
 * Desc: Get help for a user-console command.
 * Responses: ConsoleHelpResponse
 * Error responses: BADCMD
 *
 * Name: ChangeTheoryRequest
 * Code: "TS"
 * Body:
 *   TheoryName ::= String ;
 *   ChangeTheoryRequestBody ::= TheoryName
 * Desc: Switch to a different theory.
 * Responses: OkResponse
 * ErrorResponses: BADTHEORY
 *
 * Name: CurrentTheoryRequest
 * Code: "TG"
 * Body:
 *   CurrentTheoryRequestBody ::= ;
 * Desc: Get the name of the current theory
 * Responses: NameResponse
 * ErrorResponses:
 *
 *
 * All the following requests operate on the current theory
 *
 * Name: ListGraphsRequest
 * Code: "GL"
 * Body:
 *   ListGraphsRequestBody ::= ;
 * Desc: List the loaded graphs.
 * Responses: NameListResponse
 *
 * Name: LoadEmptyGraphRequest
 * Code: "GOE"
 * Body:
 *   GraphName ::= String ;
 *   LoadEmptyGraphRequestBody ::= (GraphName)? ;
 * Desc: Load an empty graph, optionally naming it as requested,
 *       or a variant if the name is taken.
 * Responses: NameResponse
 *
 * Name: LoadGraphFromFileRequest
 * Code: "GOF"
 * Body:
 *   FileName ::= String ;
 *   LoadGraphFromFileRequestBody ::= FileName ;
 * Desc: Load a graph from a file.
 * Responses: NameResponse
 * ErrorResponses: FILEACCESS | BADDATA
 *
 * Name: LoadGraphFromDataRequest
 * Code: "GOD"
 * Body:
 *   LoadGraphFromDataRequestBody ::= DataChunk ;
 * Desc: Load a graph from the given data.
 * Responses: NameResponse
 * ErrorResponses: BADDATA
 *
 * Name: CopyGraphRequest
 * Code: "GOG"
 * Body:
 *   CopyGraphRequestBody ::= GraphName ;
 * Desc: Create a copy of an existing graph; the argument is the
 *       existing graph.  The new graph's name will be based on
 *       the old one.
 * Responses: NameResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: CopySubgraphRequest
 * Code: "GOS"
 * Body:
 *   SourceGraphName ::= GraphName ;
 *   TargetGraphName ::= GraphName ;
 *   VertexNames ::= StringList ;
 *   CopySubgraphRequestBody ::= SourceGraphName ESC ';'
 *                               TargetGraphName ESC ';'
 *                               VertexNames ;
 * Desc: Create a copy of part of an existing graph (defined by
 *       VertexNames), overwriting TargetGraphName if it exists.
 *       This is mainly intended for clipboard support in GUI tools.
 * Responses: OkResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: SaveGraphToFileRequest
 * Code: "GS"
 * Body:
 *   SaveGraphToFileRequestBody ::= GraphName ESC ';' FileName ;
 * Desc: Save the named graph to a file..
 * Responses: OkResponse
 * ErrorResponses: NOSUCHGRAPH | FILEACCESS
 *
 * Name: RenameGraphRequest
 * Code: "GR"
 * Body:
 *   OldGraphName ::= GraphName ;
 *   NewGraphName ::= GraphName ;
 *   RenameGraphRequestBody ::= OldGraphName ESC ';' NewGraphName ;
 * Desc: Rename a graph; if the requested new name already exists,
 *       a similar one will be picked.
 * Responses: NameResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: DiscardGraphRequest
 * Code: "GD"
 * Body:
 *   DiscardGraphRequestBody ::= GraphName ;
 * Desc: Discard a graph, forgetting everything about it.
 * Responses: OkResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: ExportGraphRequest
 * Code: "GE"
 * Body:
 *   ExportFormat ::= "native" | "xml" | "pretty" | "hilb" | "mathematica" | "matlab" | "tikz" ;
 *   ExportGraphRequestBody ::= GraphName ESC ';' ExportFormat ;
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
 * ("GVA": DescribeGraphRequest; not implemented)
 *
 * Name: ListVerticesRequest
 * Code: "GVV"
 * Body:
 *   ListVerticesRequestBody ::= GraphName ;
 * Desc: List the vertices present in a graph
 * Response: NameListResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: ListEdgesRequest
 * Code: "GVE"
 * Body:
 *   ListEdgesRequestBody ::= GraphName ;
 * Desc: List the edges present in a graph
 * Response: NameListResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: ListBangBoxesRequest
 * Code: "GVB"
 * Body:
 *   ListBangBoxesRequestBody ::= GraphName ;
 * Desc: List the !-boxes present in a graph
 * Response: NameListResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: VertexDataRequest
 * Code: "GVW"
 * Body:
 *   ComponentDataFormat ::= "pretty" | "xml"
 *   VertexName ::= String ;
 *   VertexDataRequestbody ::= GraphName ESC ';' VertexName ESC ';' ComponentDataFormat ;
 * Desc: Get the component data attached to a vertex
 * Response: XmlResponse | PrettyResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX | BADFORMAT
 *
 * Name: VertexUserDataRequest
 * Code: "GVX"
 * Body:
 *   VertexUserDataRequestbody ::= GraphName ESC ';' VertexName ;
 * Desc: Get the user data attached to a vertex
 * Response: DataResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX
 *
 * Name: EdgeDataRequest
 * Code: "GVF"
 * Body:
 *   ComponentDataFormat ::= "pretty" | "xml"
 *   EdgeName ::= String ;
 *   EdgeDataRequestbody ::= GraphName ESC ';' EdgeName ESC ';' ComponentDataFormat ;
 * Desc: Get the component data attached to an edge
 * Response: XmlResponse | PrettyResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHEDGE | BADFORMAT
 *
 * Name: EdgeUserDataRequest
 * Code: "GVG"
 * Body:
 *   EdgeUserDataRequestbody ::= GraphName ESC ';' EdgeName ;
 * Desc: Get the user data attached to an edge
 * Response: DataResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHEDGE
 *
 * Name: BangBoxVerticesRequest
 * Code: "GVC"
 * Body:
 *   BangBoxName ::= String ;
 *   BangBoxDataRequestbody ::= GraphName ESC ';' BangBoxName ;
 * Desc: Get the vertices contained in a !-box
 * Response: NameListResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX | BADFORMAT
 *
 * Name: BangBoxUserDataRequest
 * Code: "GVD"
 * Body:
 *   BangBoxUserDataRequestbody ::= GraphName ESC ';' BangBoxName ;
 * Desc: Get the user data attached to a !-box
 * Response: DataResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX
 *
 * Name: UndoRequest
 * Code: "GMU"
 * Body:
 *   UndoRequestBody ::= GraphName ;
 * Desc: Undo the last modification to a graph
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | BADSTATE
 *
 * Name: RedoRequest
 * Code: "GMR"
 * Body:
 *   RedoRequestBody ::= GraphName ;
 * Desc: Redo the most recently undone modification to a graph
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | BADSTATE
 *
 * Name: InsertGraphRequest
 * Code: "GMI"
 * Body:
 *   InsertGraphRequestBody ::= TargetGraphName ESC ';' SourceGraphName
 * Desc: Insert a copy of one graph into another
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH
 *
 * Name: AddVertexRequest
 * Code: "GMVA"
 * Body:
 *   VertexType ::= String
 *   ResultFormat ::= "name" | "xml" | "pretty"
 *   AddVertexRequestBody ::= GraphName ESC ';'
 *                            VertexType ESC ';'
 *                            ResultFormat ;
 * Desc: Add a vertex to a graph
 * Response: NameResponse | XmlResponse | PrettyResponse
 * ErrorResponses: NOSUCHGRAPH | BADVERTEXTYPE | BADFORMAT
 *
 * Name: RenameVertexRequest
 * Code: "GMVR"
 * Body:
 *   OldVertexName ::= VertexName
 *   NewVertexName ::= VertexName
 *   RenameVertexRequestBody ::= GraphName ESC ';'
 *                               OldVertexName ESC ';'
 *                               NewVertexName ;
 * Desc: Rename a vertex
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX | ALREADYEXISTS
 *
 * Name: DeleteVerticesRequest
 * Code: "GMVD"
 * Body:
 *   DeleteVerticesRequestBody ::= GraphName ESC ';'
 *                                 VertexNames ;
 * Desc: Delete vertices from a graph
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX
 *
 * Name: SetVertexDataRequest
 * Code: "GMVS"
 * Body:
 *   XmlDataChunk ::= ESC 'X' DataChunk
 *   StringDataChunk ::= ESC 'S' DataChunk
 *   ComponentData ::= XmlDataChunk | StringDataChunk
 *   SetVertexDataRequestBody ::= GraphName ESC ';'
 *                                VertexName ESC ';'
 *                                ComponentData ;
 * Desc: Set the data on a vertex.  The data can be given in XML or
 *       string form.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX | BADFORMAT | BADDATA
 *
 * Name: SetVertexUserDataRequest
 * Code: "GMVU"
 * Body:
 *   SetVertexDataRequestBody ::= GraphName ESC ';'
 *                                VertexName ESC ';'
 *                                DataChunk ;
 * Desc: Set user data on a vertex.  This data will not be parsed by the core
 *       in any way.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX
 *
 * Name: AddEdgeRequest
 * Code: "GMEA"
 * Body:
 *   EdgeType ::= String
 *   Directedness ::= 'd' | 'u'
 *   SourceVertex ::= VertexName
 *   TargetVertex ::= VertexName
 *   AddEdgeRequestBody ::= GraphName ESC ';'
 *                          EdgeType ESC ';'
 *                          Directedness ESC ';'
 *                          SourceVertex ESC ';'
 *                          TargetVertex ESC ';'
 *                          ResultFormat ;
 * Desc: Add an edge to a graph.
 * Response: NameResponse | XmlResponse | PrettyResponse
 * ErrorResponses: NOSUCHGRAPH | BADFORMAT
 *
 * Name: RenameEdgeRequest
 * Code: "GMER"
 * Body:
 *   OldEdgeName ::= EdgeName
 *   NewEdgeName ::= EdgeName
 *   RenameEdgeRequestBody ::= GraphName ESC ';'
 *                             OldEdgeName ESC ';'
 *                             NewEdgeName ;
 * Desc: Rename an edge
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHEDGE | ALREADYEXISTS
 *
 * Name: DeleteEdgesRequest
 * Code: "GMED"
 * Body:
 *   EdgeNames ::= StringList ;
 *   DeleteEdgesRequestBody ::= GraphName ESC ';'
 *                              EdgeNames ;
 * Desc: Delete edges from a graph
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHEDGE
 *
 * Name: SetEdgeDataRequest
 * Code: "GMES"
 * Body:
 *   XmlDataChunk ::= ESC 'X' DataChunk
 *   StringDataChunk ::= ESC 'S' DataChunk
 *   ComponentData ::= XmlDataChunk | StringDataChunk
 *   SetEdgeDataRequestBody ::= GraphName ESC ';'
 *                              EdgeName ESC ';'
 *                              ComponentData ;
 * Desc: Set the data on an edge.  The data can be given in XML or
 *       string form.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHEDGE | BADFORMAT | BADDATA
 *
 * Name: SetEdgeUserDataRequest
 * Code: "GMEU"
 * Body:
 *   SetEdgeDataRequestBody ::= GraphName ESC ';'
 *                              EdgeName ESC ';'
 *                              DataChunk ;
 * Desc: Set user data on an edge.  This data will not be parsed by the core
 *       in any way.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHEDGE
 *
 * Name: AddBangBoxRequest
 * Code: "GMBA"
 * Body:
 *   AddBangBoxRequestBody ::= GraphName ESC ';'
 *                             VertexNames ESC ';'
 *                             ResultFormat ;
 * Desc: Add a !-box to a graph.  A list of vertices to include in
 *       the !-box may be provided
 * Response: NameResponse | XmlResponse | PrettyResponse
 * ErrorResponses: NOSUCHGRAPH | BADFORMAT
 *
 * Name: RenameBangBoxRequest
 * Code: "GMBR"
 * Body:
 *   OldBangBoxName ::= String ;
 *   NewBangBoxName ::= String ;
 *   RenameBangBoxRequestBody ::= GraphName ESC ';'
 *                                OldBangBoxName ESC ';'
 *                                NewBangBoxName ;
 * Desc: Rename a !-box.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX | ALREADYEXISTS
 *
 * Name: DropBangBoxesRequest
 * Code: "GMBD"
 * Body:
 *   BangBoxNames ::= StringList ;
 *   DropBangBoxesRequestBody ::= GraphName ESC ';'
 *                                BangBoxNames ;
 * Desc: Drop the given !-boxes.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX
 *
 * Name: KillBangBoxesRequest
 * Code: "GMBK"
 * Body:
 *   KillBangBoxesRequestBody ::= GraphName ESC ';'
 *                                BangBoxNames ;
 * Desc: Kill the given !-boxes.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX
 *
 * Name: DuplicateBangBoxRequest
 * Code: "GMBC"
 * Body:
 *   DuplicateBangBoxRequestBody ::= GraphName ESC ';'
 *                                   BangBoxName ;
 * Desc: Duplicate the given !-boxe.
 * Response: NameResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX
 *
 * Name: MergeBangBoxRequest
 * Code: "GMBM"
 * Body:
 *   MergeBangBoxRequestBody ::= GraphName ESC ';'
 *                               BangBoxNames ;
 * Desc: Merge the given !-boxes.
 * Response: NameResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX
 *
 * Name: BangVerticesRequest
 * Code: "GMBB"
 * Body:
 *   BangVerticesRequestBody ::= GraphName ESC ';'
 *                               BangBoxName ESC ';'
 *                               VertexNames ;
 * Desc: Add vertices to a !-box.  They will be removed from any
 *       other !-boxes they are contained in.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX | NOSUCHVERTEX
 *
 * Name: UnbangVerticesRequest
 * Code: "GMBL"
 * Body:
 *   UnbangVerticesRequestBody ::= GraphName ESC ';'
 *                                 VertexNames ;
 * Desc: Remove some vertices from their !-boxes.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX
 *
 * Name: SetBangBoxUserDataRequest
 * Code: "GMBU"
 * Body:
 *   SetBangBoxDataRequestBody ::= GraphName ESC ';'
 *                                 BangBoxName ESC ';'
 *                                 DataChunk ;
 * Desc: Set user data on a !-box.  This data will not be parsed by the core
 *       in any way.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHBBOX
 *
 * Name: ImportRulesetFromFileRequest
 * Code: "RSO"
 * Body:
 *   ImportRulesetFromFileRequestBody ::= FileName ;
 * Desc: Import a ruleset from a file.  It will be merged with the
 *       loaded ruleset.
 * Response: OkResponse
 * ErrorResponses: FILEACCESS
 *
 * Name: ImportRulesetFromDataRequest
 * Code: "RSI"
 * Body:
 *   ImportRulesetFromDataRequestBody ::= DataChunk ;
 * Desc: Import a ruleset from data.  It will be merged with the
 *       loaded ruleset.
 * Response: OkResponse
 * ErrorResponses: BADDATA
 *
 * Name: ExportRulesetToFileRequest
 * Code: "RSS"
 * Body:
 *   ExportRulesetToFileRequestBody ::= FileName ;
 * Desc: Export the loaded ruleset to a file.
 * Response: OkResponse
 * ErrorResponses: FILEACCESS
 *
 * Name: ExportRulesetAsDataRequest
 * Code: "RSE"
 * Body:
 *   ExportRulesetAsDataRequestBody ::= ;
 * Desc: Export the loaded ruleset as data.
 * Response: DataResponse
 * ErrorResponses: BADDATA
 *
 * Name: ListRulesRequest
 * Code: "RRL"
 * Body:
 *   ListRulesRequestBody ::= ;
 * Desc: List all loaded rules in the current theory.
 * Response: NameListResponse
 * ErrorResponses:
 *
 * Name: ListActiveRulesRequest
 * Code: "RRA"
 * Body:
 *   ListActiveRulesRequestBody ::= ;
 * Desc: List all active rules in the current theory.
 * Response: NameListResponse
 * ErrorResponses:
 *
 * Name: OpenRuleLhsRequest
 * Code: "RRP"
 * Body:
 *   RuleName ::= String ;
 *   OpenRuleLhsRequestBody ::= RuleName ;
 * Desc: Open the lhs of a rule as a graph.
 * Response: NameResponse
 * ErrorResponses: NOSUCHRULE
 *
 * Name: OpenRuleRhsRequest
 * Code: "RRQ"
 * Body:
 *   OpenRuleRhsRequestBody ::= RuleName ;
 * Desc: Open the rhs of a rule as a graph.
 * Response: NameResponse
 * ErrorResponses: NOSUCHRULE
 *
 * Name: SetRuleRequest
 * Code: "RRU"
 * Body:
 *   LhsGraphName ::= GraphName ;
 *   RhsGraphName ::= GraphName ;
 *   SetRuleRequestBody ::= RuleName ESC ';'
 *                          LhsGraphName ESC ';'
 *                          RhsGraphName ;
 * Desc: Create or update a rule.
 * Response: OkResponse
 * ErrorResponses: NOSUCHRULE | NOSUCHGRAPH | BADRULE
 *
 * Name: DeleteRuleRequest
 * Code: "RRD"
 * Body:
 *   DeleteRuleRequestBody ::= RuleName ;
 * Desc: Delete a rule.
 * Response: OkResponse
 * ErrorResponses: NOSUCHRULE
 *
 * Name: ActivateRuleRequest
 * Code: "RRY"
 * Body:
 *   ActivateRuleRequestBody ::= RuleName ;
 * Desc: Activate a rule.
 * Response: OkResponse
 * ErrorResponses: NOSUCHRULE
 *
 * Name: DeactivateRuleRequest
 * Code: "RRN"
 * Body:
 *   DeactivateRuleRequestBody ::= RuleName ;
 * Desc: Deactivate a rule.
 * Response: OkResponse
 * ErrorResponses: NOSUCHRULE
 *
 * Name: ListTagsRequest
 * Code: "RTL"
 * Body:
 *   ListTagsRequestBody ::= ;
 * Desc: List all tags in the current theory.  If a tag exists if and only
 *       if there is at least one rule tagged with it.
 * Response: NameListResponse
 * ErrorResponses:
 *
 * Name: ListRulesByTagRequest
 * Code: "RTR"
 * Body:
 *   TagName ::= String ;
 *   ListRulesByTagRequestBody ::= TagName ;
 * Desc: List all rules with the given tag in the current theory.
 *       If a tag does not exist, an empty list will be returned.
 * Response: NameListResponse
 * ErrorResponses:
 *
 * Name: TagRuleRequest
 * Code: "RTT"
 * Body:
 *   TagRuleRequestBody ::= RuleName ESC ';'
 *                          TagName ;
 * Desc: Tag a rule.
 * Response: OkResponse
 * ErrorResponses: NOSUCHRULE
 *
 * Name: UntagRuleRequest
 * Code: "RTU"
 * Body:
 *   UntagRuleRequestBody ::= RuleName ESC ';'
 *                            TagName ;
 * Desc: Untag a rule.
 * Response: OkResponse
 * ErrorResponses: NOSUCHRULE | NOSUCHTAG
 *
 * Name: ForgetTagRequest
 * Code: "RTF"
 * Body:
 *   ForgetTagRequestBody ::= TagName ;
 * Desc: Remove a tag from all rules with the given tag.
 * Response: OkResponse
 * ErrorResponses: NOSUCHRULE | NOSUCHTAG
 *
 * Name: DeleteRulesByTagRequest
 * Code: "RTD"
 * Body:
 *   DeleteRulesByTagRequestBody ::= TagName ;
 * Desc: Delete all rules with the given tag.
 * Response: OkResponse
 * ErrorResponses: NOSUCHTAG
 *
 * Name: ActivateRulesByTagRequest
 * Code: "RTY"
 * Body:
 *   ActivateRulesByTagRequestBody ::= TagName ;
 * Desc: Activate all rules with the given tag.
 * Response: OkResponse
 * ErrorResponses: NOSUCHTAG
 *
 * Name: DeactivateRulesByTagRequest
 * Code: "RTN"
 * Body:
 *   DeactivateRulesByTagRequestBody ::= TagName ;
 * Desc: Deactivate all rules with the given tag.
 * Response: OkResponse
 * ErrorResponses: NOSUCHTAG
 *
 * Name: AttachRewritesRequest
 * Code: "WA"
 * Body:
 *   AttachRewritesRequestBody ::= GraphName ESC ';'
 *                                 VertexNames ;
 * Desc: Finds all possible rewrites for a subgraph using the
 *       currently active rules.  If no vertex names are given,
 *       the whole graph is used.  The number of rewrites found
 *       is returned.
 * Response: CountResponse
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX
 *
 * Name: AttachOneRewriteRequest
 * Code: "WO"
 * Body:
 *   AttachOneRewriteRequestBody ::= GraphName ESC ';'
 *                                   VertexNames ;
 * Desc: Finds an arbitrary rewrite (if one exists) for a subgraph
 *       using the currently active rules.  If no vertex names are given,
 *       the whole graph is used.  The number of rewrites found
 *       is returned.
 * Response: CountResponse ???
 * ErrorResponses: NOSUCHGRAPH | NOSUCHVERTEX ???
 *
 * Name: ListAttachedRewritesRequest
 * Code: "WL"
 * Body:
 *   ListAttachedRewritesRequestBody ::= GraphName ESC ';' ResultFormat ;
 * Desc: Lists the attached rewrites for a graph.  If the requested
 *       result format is "name", only the rule applied will be returned
 *       for each attached rewrite.  Note that the same rule may well
 *       appear more than once in the list.
 * Response: RewriteListResponse | NameListResponse
 * ErrorResponses: NOSUCHGRAPH | BADSTATE
 *
 * Name: ApplyAttachedRewriteRequest
 * Code: "WW"
 * Body:
 *   Index ::= Integer ;
 *   ApplyAttachedRewriteRequestBody ::= GraphName ESC ';'
 *                                       Index ;
 * Desc: Applies one of the attached rewrites to a graph.
 * Response: OkResponse
 * ErrorResponses: NOSUCHGRAPH | BADSTATE | OUTOFRANGE
 *
 * }}} *)

structure ProtocolInterface : CONTROL_INTERFACE =
struct

  structure RG_Controller = Controller(ControllerState);

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
    fun printString s = print (String.translate (fn #"\u001b" => "\u001b\u001b" | c => String.str c) s)
    fun printInt i = print (Int.toString i)
    fun printLength s = printInt (String.size s)
    fun printEsc ch = print (String.concat["\u001b", String.str ch])
    fun printClose () = (printEsc #">"; TextIO.flushOut outstream)
    fun printOpen ch requestId = print (String.concat["\u001b<", String.str ch, "\u001b:", requestId, "\u001b|"])
    fun printOpenInfo ch = print (String.concat["\u001b<", String.str ch, "\u001b|"])
    fun printDelim () = print "\u001b;"
    fun printDataChunk s = (printEsc #"["; printLength s; printEsc #"|"; print s; printEsc #"]")
    fun printList [] = (print "0"; printEsc #":")
      | printList [s] = (print "1"; printEsc #":"; printString s)
      | printList (s::ss) = (print (Int.toString (1 + (length ss))); printEsc #":"; printString s; List.app (fn e => (printEsc #","; printString e)) ss)
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
      | outputResponse (requestId, NameResponse { name }) =
         (printOpen #"N" requestId;
          printString name;
          printClose ())
      | outputResponse (requestId, NameListResponse { names }) =
         (printOpen #"M" requestId;
          printList names;
          printClose ())
    (* FIXME descs *)
      | outputResponse (requestId, UserDataResponse { data }) =
         (printOpen #"U" requestId;
          printDataChunk data;
          printClose ())
      | outputResponse (requestId, RewriteListResponse { rewrites }) =
          let
            fun outputGraphData (PrettyGData p) =
                  let val s = Pretty.string_of p in
                    printEsc #"P";
                    printDataChunk s
                  end
              | outputGraphData (XmlGData t) = 
                  let val xml = XML.string_of t in
                    printEsc #"X";
                    printDataChunk xml
                  end
              (* FIXME graph descriptions *)
            fun outputRewrite ({ruleName, result, lhs, rhs}) =
                 (printString ruleName;
                  printEsc #",";
                  outputGraphData result;
                  printEsc #",";
                  outputGraphData lhs;
                  printEsc #",";
                  outputGraphData rhs)
            fun outputRewrites [] = ()
              | outputRewrites (w::[]) = (outputRewrite w)
              | outputRewrites (w::ws) = (outputRewrite w; printDelim (); outputRewrites ws)
          in
           (printOpen #"W" requestId;
            outputRewrites rewrites;
            printClose ())
          end
      | outputResponse (requestId, UnknownResponse { code }) =
         (printOpen #"Z" requestId;
          print code;
          printClose ())

    fun outputOkResponse requestId = (outputResponse (requestId, OkResponse))
    fun outputNameResponse requestId name =
          (outputResponse (requestId, NameResponse { name = name }))
    fun outputErrorResponse requestId code message =
          (outputResponse (requestId, ErrorResponse { errorCode = code, message = message }))
    (* }}} *)

    (* {{{ Input helpers *)
    (* Returns the string as far as the next ESC and the terminator. *)
    local
      fun readToEscape' (soFar: string) (terminator: char) : string =
        case input1 instream
          of SOME #"\u001b" =>
              (
                case input1 instream of
                  NONE => protocolError "End of file"
                | SOME #"\u001b" => (* Escaped ESC. *)
                                    readToEscape' (soFar ^ str #"\u001b") terminator
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

    fun eatEscapedChar (ch: char) = (eatChar #"\u001b"; eatChar ch)
    fun readEscapedChar () = (eatChar #"\u001b"; readChar ())

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
              of SOME #"\u001b" =>
                  (
                    case input1 instream
                      of NONE           => protocolError "End of file"
                       | SOME #"\u001b" => readList' (soFar ^ "\u001b") count (splitter, terminator)
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
          | SOME #"\u001b" => () (* Escape- start of packet. *)
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
          of SOME #"\u001b" =>
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
           | #"S" => SOME (StringCData (readDataChunk ()))
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
        of "GL"   => (assertAtEnd (); ListGraphsRequest)
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
                        val data = readDataChunk ()
                        val () = assertAtEnd ()
                      in
                        LoadGraphRequest {
                          details = LoadGraphFromDataRequest {
                            data = data
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

         | "GVV"  =>  DescribeGraphRequest {
                        graphName = readToEnd (),
                        desc = ListVertices
                      }

         | "GVE"  =>  DescribeGraphRequest {
                        graphName = readToEnd (),
                        desc = ListEdges
                      }

         | "GVB"  =>  DescribeGraphRequest {
                        graphName = readToEnd (),
                        desc = ListBangBoxes
                      }

         | "GVW"  =>  let
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

         | "GVX"  =>  let
                        val graphName  = readToDelim ()
                        val vertexName = readToEnd ()
                      in
                        DescribeGraphRequest {
                          graphName = graphName,
                          desc = VertexUserData { vertexName = vertexName }
                        }
                      end

         | "GVF" =>  let
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

         | "GVG"  =>  let
                        val graphName = readToEscape #";"
                        val edgeName  = readToEnd ()
                      in
                        DescribeGraphRequest {
                          graphName = graphName,
                          desc = EdgeUserData { edgeName = edgeName }
                        }
                      end

         | "GVC"  =>  let
                        val graphName   = readToEscape #";"
                        val bangBoxName = readToEnd ()
                      in
                        DescribeGraphRequest {
                          graphName = graphName,
                          desc = BangBoxVertices { bangBoxName = bangBoxName }
                        }
                      end

         | "GVD"  =>  let
                        val graphName   = readToEscape #";"
                        val bangBoxName = readToEnd ()
                      in
                        DescribeGraphRequest {
                          graphName = graphName,
                          desc = BangBoxUserData { bangBoxName = bangBoxName }
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

         | "GMVA" =>  let
                        val graphName = readToDelim ()
                        val vertexType = readToDelim ()
                        val format = case readToEnd ()
                                       of "xml"    => SOME XmlFormat
                                        | "native" => SOME DescriptionFormat
                                        | "pretty" => SOME PrettyFormat
                                        | "name"   => SOME NameFormat
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
                        val data = readDataChunk ()
                        val () = assertAtEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  SetVertexUserDataAction {
                                      vertexName = vertexName,
                                      userData = data
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
                                        | "name"   => SOME NameFormat
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
                        val data = readDataChunk ()
                        val () = assertAtEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  SetEdgeUserDataAction {
                                      edgeName = edgeName,
                                      userData = data
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
                                        | "name"   => SOME NameFormat
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
                        val vertexNames = readListToDelim ()
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
                        val vertexNames = readListToDelim ()
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
                        val data = readDataChunk ()
                        val () = assertAtEnd ()
                      in
                        EditGraphRequest {
                          graphName = graphName,
                          action =  SetBangBoxUserDataAction {
                                      bangBoxName = bangBoxName,
                                      userData = data
                                    }
                        }
                      end

         | "RSO" =>  ImportRulesetFromFileRequest {
                                fileName = readToEnd ()
                              }

         | "RSI"  =>  let
                        val data = readDataChunk ()
                        val () = assertAtEnd ()
                      in
                        ImportRulesetFromDataRequest {
                          data = data
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
                                        | "name"   => SOME NameFormat
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
                      (case Console.eval_command_string_no_quit eval command
                        of InterfaceResult.NORMAL output =>
                            (outputCmdResult output; next_loop ())
                         | InterfaceResult.CHANGE_THEORY "red_green" =>
                            (change_loop RG_Controller.serviceRequest)
                         | InterfaceResult.CHANGE_THEORY th =>
                            (outputCmdResult ("!!! Unknown theory \""^th^"\"\n"); next_loop ())
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
                      case theoryName
                        of "red_green" => (outputOkResponse requestId; change_loop RG_Controller.serviceRequest)
                         | _ => (outputErrorResponse requestId "BADTHEORY" "Unknown theory"; change_loop RG_Controller.serviceRequest)
                    end;
                    next_loop ())

         | "TG" => (assertAtEnd(); outputNameResponse requestId "red_green"; next_loop())

         | code => (outputResponse (Commands.serviceRequestMessage eval
                                      (requestId, (readRequestBody code)));
                    next_loop())

    end
  in
    printOpenInfo #"V";
    print "1.0";
    printClose ();
    run_in_textstreams' RG_Controller.serviceRequest
  end;

  fun flushIn () = case TextIO.canInput (TextIO.stdIn, 1)
                        of SOME n => (TextIO.input1 TextIO.stdIn;())
                         | NONE => ();

  fun run_in_stdio () = (flushIn(); run_in_textstreams (TextIO.stdIn,TextIO.stdOut));
end;

(* vi:ts=2:sw=2:sts=2:et
*)
