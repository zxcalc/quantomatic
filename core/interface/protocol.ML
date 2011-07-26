signature PROTOCOL =
sig
  type state;
end;

structure Protocol(Controller : CONTROLLER) : PROTOCOL =
struct
  type state = Commands.state;

  fun protocolError error =
  let
      open OS.Process
  in
      TextIO.StreamIO.output (stdErr, "Protocol error: " ^ error ^ "\n") handle _ => ();
      exit failure;
      raise Fail "bad" (* Never called but sets return type as 'a *)
  end

  fun print s = TextIO.StreamIO.output(stdOut, s)
  fun printInt i = print (Int.toString i)
  fun printLength s = printInt (length s)
  fun printEsc ch = print (String.concat["\u001b", String.str ch])
  fun printOpen ch requestId = print (String.concat["\u001b", String.str ch, ":", requestId, ":"])
  fun printDelim ch = print "\u001b;"
  fun printList [] = ()
    | printList [s] = print s
    | printList s::ss = print s; List.app (fn e => (printEsc #","; print e)) ss

  let
    open Commands
    open TextIO
  in
    fun outputResponse (OkResponse of { requestId }) =
          printOpen #"O" requestId; printEsc #"o"
      | outputResponse (ErrorResponse of { requestId, errorCode, message }) =
          printOpen #"X" requestId;
          print errorCode;
          printDelim;
          print message;
          printEsc #"x"
      | outputResponse (DataResponse of { requestId, data }) =
          printOpen #"R" requestId;
          printLength data;
          printDelim;
          print data;
          printEsc #"r"
      | outputResponse (PrettyResponse of { requestId, data }) =
          let val s = Pretty.string_of data in
            printOpen #"R" requestId; (* Pretty is just data *)
            printLength s;
            printDelim;
            print s;
            printEsc #"r"
          end
      | outputResponse (XmlResponse of { requestId, data }) =
          let val xml = XML.string_of data in
            printOpen #"R" requestId; (* XML is just data *)
            printLength xml;
            printDelim;
            print xml;
            printEsc #"r"
          end
      | outputResponse (NameResponse of { requestId, name }) =
          printOpen #"N" requestId;
          print name;
          printEsc #"n"
      | outputResponse (NameListResponse of { requestId, names }) =
          printOpen #"M" requestId;
          printList names;
          printEsc #"m"
    (* FIXME descs *)
      | outputResponse (UserDataResponse of { requestId, data }) =
          printOpen #"U" requestId;
          printLength data;
          printDelim;
          print data;
          printEsc #"u"
    (* FIXME rewrite list *)
      | unknownResponse (UnknownResponse of { requestId, code }) =
          printOpen #"Z" requestId;
          print code;
          printEsc #"z"
  end;

  fun doRequest (state:state) : state =
  let
    open Commands
    open TextIO

    fun serviceRequest r = outputResponse (Commands.serviceRequest state r);

    (* Returns the string as far as the next ESC and the terminator. *)
    fun readToEscape' (soFar: string) (terminator: char) : string =
    case input1 stdIn of
      SOME #"\u001b" =>
      (
        case input1 stdIn of
          NONE => protocolError "End of file"
        | SOME ch =>
            if ch = terminator
            then soFar
            else if ch = #"\u001b" (* Escaped ESC. *)
            then readToEscape' (soFar ^ str #"\u001b") terminator
            else protocolError(str ch ^ " not " ^ str terminator)
      )
    | SOME ch => readToEscape' (soFar ^ str ch) terminator
    | NONE => protocolError "End of file"
    val readToEscape = readToEscape' ""

    (* Parse an integer.  Returns zero if it isn't a valid int. *)
    fun getInt termCh : int =
      case Int.fromString (readToEscape termCh) of
          NONE => 0
        | SOME i => i

    fun readList' (soFar:string) (splitter : char, terminator : char) : string =
    case input1 stdIn of
      SOME #"\u001b" =>
      (
        case input1 stdIn of
          NONE => protocolError "End of file"
        | SOME ch =>
            if ch = terminator
            then [soFar]
            if ch = splitter
            then soFar::(readList "" (splitter, terminator))
            else if ch = #"\u001b" (* Escaped ESC. *)
            then readList' (soFar ^ str #"\u001b") (splitter, terminator)
            else protocolError(str ch ^ " not " ^ str terminator)
      )
    | SOME ch => readList (soFar ^ str ch) (splitter, terminator)
    | NONE => protocolError "End of file"
    val readList = readList' ""

    fun readData () =
          let val dataLength = getInt #";" in
            TextIO.inputN (TextIO.stdIn, dataLength)
          end

    fun readComponentData "xml" = (XmlCData (XML.parse (readData ())))
      | readComponentData "string" = (StringCData (readData ()))

    val () =
      case input1 stdIn of
        NONE => OS.Process.exit OS.Process.success (* Close down. *)
      | SOME #"\u001b" => () (* Escape- start of packet. *)
      | SOME ch => protocolError(str ch ^ " not ESCAPE at start of packet")
    val startCh =
      case input1 stdIn of
        NONE => protocolError "End of file"
      | SOME ch => ch

    val commandCode = startCh ^ (readToEscape #":")
    val requestId = readToEscape #":"

    fun readToEnd () = readToEscape (Char.toLower startCh)
    fun readToDelim () = readToEscape #";"
    fun readListToDelim () = readList (#",",#";")
    fun readListToEnd () = readList (#",",(Char.toLower startCh))

    fun assertAtDelim () =
      let val str = readToEscape #";" in
        if str = "" then () else protocolError "End of packet expected"
      end
    fun assertAtEnd () =
      let val str = readToEscape (Char.toLower startCh) in
        if str = "" then () else protocolError "End of packet expected"
      end

    fun readDataToEnd format =
          let
            val data = readData ()
            val () = assertAtEnd ()
          in
            data
          end

    fun readDataToDelim format =
          let
            val data = readData ()
            val () = assertAtDelim ()
          in
            data
          end

    fun readComponentDataToEnd format =
          let
            val data = readComponentData format
            val () = assertAtEnd ()
          in
            data
          end

    fun readComponentDataToDelim format =
          let
            val data = readComponentData format
            val () = assertAtDelim ()
          in
            data
          end

    fun readRequest "GL" => assertAtEnd; ListGraphsRequest { requestId = requestId }
      | readRequest "GOE" => assertAtEnd;
                             LoadGraphRequest {
                               requestId = requestId,
                               details   = LoadEmptyGraphRequest
                             }
      | readRequest "GOF" => LoadGraphRequest {
                               requestId = requestId,
                               details = LoadGraphFromFileRequest {
                                           fileName = readToEnd()
                                         }
                             }
      | readRequest "GOD" => let
                               val data = readData ()
                               val () = assertAtEnd ()
                             in
                               LoadGraphRequest {
                                 requestId = requestId,
                                 details = LoadGraphFromDataRequest {
                                             data = data
                                           }
                               }
                             end
      | readRequest "GOG" => LoadGraphRequest {
                               requestId = requestId,
                               details = CopyGraphRequest { graphName = readToEnd () }
                             }

      | readRequest "GOS" => let
                               val graphName = readToEscape #";"
                               val vertexNames = readListToEnd ()
                             in
                               LoadGraphRequest {
                                 requestId = requestId,
                                 details = CopySubgraphRequest {
                                             graphName = graphName,
                                             vertexNames = vertexNames
                                           }
                               }
                             end

      | readRequest "GS" => let
                              val graphName = readToEscape #";"
                              val fileName  = readToEnd ()
                            in
                              SaveGraphToFileRequest {
                                requestId = requestId,
                                graphName = graphName,
                                fileName = fileName }
                            end

      | readRequest "GR" => let
                              val oldName   = readToEscape #";"
                              val newName   = readToEnd ()
                            in
                              RenameGraphRequest {
                                requestId = requestId,
                                oldName = oldName,
                                newName = newName
                              }
                            end

      | readRequest "GD" => DiscardGraphRequest {
                              requestId = requestId,
                              graphName = readToEnd ()
                            }

      | readRequest "GE" => let
                              val graphName  = readToEscape #";"
                              val formatName = readToEnd ()
                              val format     = case formatName of
                                                 "native"      => NativeGraphFormat
                                               | "hilb"        => HilbertSpaceGraphFormat
                                               | "mathematica" => MathematicaGraphFormat
                                               | "matlab"      => MatlabGraphFormat
                                               | "tikz"        => TikzGraphFormat
                                               | _             => UnknownGraphFormat
                            in
                              ExportGraphRequest {
                                requestId = requestId,
                                graphName = graphName,
                                format = format
                              }
                            end

      | readRequest "GVA" => DescribeGraphRequest {
                               requestId = requestId,
                               graphName = readToEnd (),
                               desc = DescribeFullGraph
                             }

      | readRequest "GVV" => DescribeGraphRequest {
                               requestId = requestId,
                               graphName = readToEnd (),
                               desc = ListVertices
                             }

      | readRequest "GVE" => DescribeGraphRequest {
                               requestId = requestId,
                               graphName = readToEnd (),
                               desc = ListEdges
                             }

      | readRequest "GVB" => DescribeGraphRequest {
                               requestId = requestId,
                               graphName = readToEnd (),
                               desc = ListBangBoxes
                             }

      | readRequest "GVW" => let
                               val graphName  = readToEscape #";"
                               val vertexName  = readToEnd ()
                             in
                               DescribeGraphRequest {
                                 requestId = requestId,
                                 graphName = graphName,
                                 desc = VertexData { vertexName = vertexName }
                               }
                             end

      | readRequest "GVX" => let
                               val graphName  = readToEscape #";"
                               val vertexName  = readToEnd ()
                             in
                               DescribeGraphRequest {
                                 requestId = requestId,
                                 graphName = graphName,
                                 desc = VertexUserData { vertexName = vertexName }
                               }
                             end

      | readRequest "GVF" =>  let
                                val graphName  = readToEscape #";"
                                val edgeName  = readToEnd ()
                              in
                                DescribeGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  desc = EdgeData { edgeName = edgeName }
                                }
                              end

      | readRequest "GVG" =>  let
                                val graphName  = readToEscape #";"
                                val edgeName  = readToEnd ()
                              in
                                DescribeGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  desc = EdgeUserData { edgeName = edgeName }
                                }
                              end

      | readRequest "GVC" =>  let
                                val graphName  = readToEscape #";"
                                val bangBoxName  = readToEnd ()
                              in
                                DescribeGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  desc = BangBoxVertices { bangBoxName = bangBoxName }
                                }
                              end

      | readRequest "GVD" =>  let
                                val graphName  = readToEscape #";"
                                val bangBoxName  = readToEnd ()
                              in
                                DescribeGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  desc = BangBoxUserData { bangBoxName = bangBoxName }
                                }
                              end

      | readRequest "GMU" =>  EditGraphRequest {
                                requestId = requestId,
                                graphName = readToEnd (),
                                action = UndoAction
                              }

      | readRequest "GMR" =>  EditGraphRequest {
                                requestId = requestId,
                                graphName = readToEnd (),
                                action = RedoAction
                              }

      | readRequest "GMI" =>  let
                                val graphName = readToDelim ()
                                val sourceGraphName = readToEnd ()
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  InsertGraphAction {
                                              sourceGraphName = sourceGraphName
                                            }
                                }
                              end

      | readRequest "GMVA" => let
                                val graphName = readToDelim ()
                                val vertexType = readToDelim ()
                                val format = case readToEnd ()
                                               of "xml"    => XmlFormat
                                                | "native" => DescriptionFormat
                                                | "pretty" => PrettyFormat
                                                | "name"   => NameFormat
                                                | _        => protocolError
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  AddVertexAction {
                                              vertexType = vertexType
                                              resultFormat = format
                                            }
                                }
                              end

      | readRequest "GMVR" => let
                                val graphName = readToDelim ()
                                val oldName = readToDelim ()
                                val newName = readToEnd ()
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  RenameVertexAction {
                                              oldName = oldName
                                              newName = newName
                                            }
                                }
                              end

      | readRequest "GMVD" => let
                                val graphName = readToDelim ()
                                val vertexNames = readListToEnd ()
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  DeleteVerticesAction {
                                              vertexNames = vertexNames
                                            }
                                }
                              end

      | readRequest "GMVS" => let
                                val graphName = readToDelim ()
                                val vertexName = readToDelim ()
                                val format = readToDelim ()
                                val data = readComponentDataToEnd format
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  SetVertexDataAction {
                                              vertexName = vertexName
                                              vertexData = data
                                            }
                                }
                              end

      | readRequest "GMVU" => let
                                val graphName = readToDelim ()
                                val vertexName = readToDelim ()
                                val data = readDataToEnd ()
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  SetVertexUserDataAction {
                                              vertexName = vertexName
                                              userData = data
                                            }
                                }
                              end

      | readRequest "GMEA" => let
                                val graphName = readToDelim ()
                                val edgeType = readToDelim ()
                                val format = case readToEnd ()
                                               of "xml"    => XmlFormat
                                                | "native" => DescriptionFormat
                                                | "pretty" => PrettyFormat
                                                | "name"   => NameFormat
                                                | _        => protocolError
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  AddEdgeAction {
                                              edgeType = edgeType
                                              resultFormat = format
                                            }
                                }
                              end

      | readRequest "GMER" => let
                                val graphName = readToDelim ()
                                val oldName = readToDelim ()
                                val newName = readToEnd ()
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  RenameEdgeAction {
                                              oldName = oldName
                                              newName = newName
                                            }
                                }
                              end

      | readRequest "GMED" => let
                                val graphName = readToDelim ()
                                val edgeNames = readListToEnd ()
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  DeleteEdgesAction {
                                              edgeNames = edgeNames
                                            }
                                }
                              end

      | readRequest "GMES" => let
                                val graphName = readToDelim ()
                                val edgeName = readToDelim ()
                                val format = readToDelim ()
                                val data = readComponentDataToEnd format
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  SetEdgeDataAction {
                                              edgeName = edgeName
                                              edgeData = data
                                            }
                                }
                              end

      | readRequest "GMEU" => let
                                val graphName = readToDelim ()
                                val edgeName = readToDelim ()
                                val data = readDataToEnd ()
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  SetEdgeUserDataAction {
                                              edgeName = edgeName
                                              userData = data
                                            }
                                }
                              end

      | readRequest "GMBA" => let
                                val graphName = readToDelim ()
                                val vertexNames = readListToDelim ()
                                val format = case readToEnd ()
                                               of "xml"    => XmlFormat
                                                | "native" => DescriptionFormat
                                                | "pretty" => PrettyFormat
                                                | "name"   => NameFormat
                                                | _        => protocolError
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  AddBangBoxAction {
                                              vertexNames = vertexNames
                                              resultFormat = format
                                            }
                                }
                              end

      | readRequest "GMBR" => let
                                val graphName = readToDelim ()
                                val oldName = readToDelim ()
                                val newName = readToEnd ()
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  RenameBangBoxAction {
                                              oldName = oldName
                                              newName = newName
                                            }
                                }
                              end

      | readRequest "GMBD" => let
                                val graphName = readToDelim ()
                                val bangBoxNames = readListToEnd ()
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  DropBangBoxesAction {
                                              bangBoxNames = bangBoxNames
                                            }
                                }
                              end

      | readRequest "GMBK" => let
                                val graphName = readToDelim ()
                                val bangBoxNames = readListToEnd ()
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  KillBangBoxesAction {
                                              bangBoxNames = bangBoxNames
                                            }
                                }
                              end

      | readRequest "GMBC" => let
                                val graphName = readToDelim ()
                                val bangBoxName = readToEnd ()
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  DuplicateBangBoxAction {
                                              bangBoxName = bangBoxName
                                            }
                                }
                              end

      | readRequest "GMBM" => let
                                val graphName = readToDelim ()
                                val bangBoxNames = readListToEnd ()
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  MergeBangBoxesAction {
                                              bangBoxNames = bangBoxNames
                                            }
                                }
                              end

      | readRequest "GMBB" => let
                                val graphName = readToDelim ()
                                val bangBoxName = readToDelim ()
                                val vertexNames = readListToDelim ()
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  BangVerticesAction {
                                              bangBoxName = bangBoxName
                                              vertexNames = vertexNames
                                            }
                                }
                              end

      | readRequest "GMBL" => let
                                val graphName = readToDelim ()
                                val vertexNames = readListToDelim ()
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  UnbangVerticesAction {
                                              vertexNames = vertexNames
                                            }
                                }
                              end

      | readRequest "GMEU" => let
                                val graphName = readToDelim ()
                                val bangBoxName = readToDelim ()
                                val data = readDataToEnd ()
                              in
                                EditGraphRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  action =  SetBangBoxUserDataAction {
                                              bangBoxName = bangBoxName
                                              userData = data
                                            }
                                }
                              end

      | readRequest "RSO" =>  ImportRulesetFromFileRequest {
                                requestId = requestId,
                                fileName = readToEnd ()
                              }

      | readRequest "RSI" =>  ImportRulesetFromDataRequest {
                                requestId = requestId,
                                data = readDataToEnd ()
                              }

      | readRequest "RSS" =>  ExportRulesetToFileRequest {
                                requestId = requestId,
                                fileName = readToEnd()
                              }

      | readRequest "RSE" =>  assertAtEnd ();
                              ExportRulesetAsDataRequest {
                                requestId = requestId
                              }

      | readRequest "RRL" =>  assertAtEnd ();
                              ListRulesRequest {
                                requestId = requestId
                              }

      | readRequest "RRA" =>  assertAtEnd ();
                              ListActiveRulesRequest {
                                requestId = requestId
                              }

      | readRequest "RRT" =>  ListRulesByTagRequest {
                                requestId = requestId,
                                tagName = readToEnd()
                              }

      | readRequest "RRP" =>  OpenRuleLhsRequest {
                                requestId = requestId,
                                ruleName = readToEnd()
                              }

      | readRequest "RRQ" =>  OpenRuleRhsRequest {
                                requestId = requestId,
                                ruleName = readToEnd()
                              }

      | readRequest "RRU" => let
                                val ruleName = readToDelim ()
                                val lhsName = readToDelim ()
                                val rhsName = readToEnd ()
                              in
                                SetRuleRequest {
                                  requestId = requestId,
                                  ruleName = ruleName,
                                  lhsGraphName = lhsName,
                                  rhsGraphName = rhsName
                                }
                              end

      | readRequest "RRD" =>  DeleteRuleRequest {
                                requestId = requestId,
                                ruleName = readToEnd()
                              }

      | readRequest "RRY" =>  ActivateRuleRequest {
                                requestId = requestId,
                                ruleName = readToEnd()
                              }

      | readRequest "RRN" =>  DeactivateRuleRequest {
                                requestId = requestId,
                                ruleName = readToEnd()
                              }

      | readRequest "RTL" =>  assertAtEnd ();
                              ListTagsRequest {
                                requestId = requestId
                              }

      | readRequest "RTT" => let
                                val ruleName = readToDelim ()
                                val tagName = readToEnd ()
                              in
                                TagRuleRequest {
                                  requestId = requestId,
                                  ruleName = ruleName,
                                  tagName = tagName
                                }
                              end

      | readRequest "RTU" => let
                                val ruleName = readToDelim ()
                                val tagName = readToEnd ()
                              in
                                UntagRuleRequest {
                                  requestId = requestId,
                                  ruleName = ruleName,
                                  tagName = tagName
                                }
                              end

      | readRequest "RTF" =>  ForgetTagRequest {
                                requestId = requestId,
                                tagName = readToEnd ()
                              }

      | readRequest "RTD" =>  DeleteRulesByTagRequest {
                                requestId = requestId,
                                tagName = readToEnd ()
                              }

      | readRequest "RTY" =>  ActivateRulesByTagRequest {
                                requestId = requestId,
                                tagName = readToEnd ()
                              }

      | readRequest "RTN" =>  DeactivateRulesByTagRequest {
                                requestId = requestId,
                                tagName = readToEnd ()
                              }

      | readRequest "WA" => let
                              val graphName = readToDelim ()
                              val vertexNames = readListToEnd ()
                            in
                              AttachRewritesRequest {
                                requestId = requestId,
                                graphName = graphName,
                                vertexNames = vertexNames
                              }
                            end

      | readRequest "WO" => let
                              val graphName = readToDelim ()
                              val vertexNames = readListToEnd ()
                            in
                              AttachOneRewriteRequest {
                                requestId = requestId,
                                graphName = graphName,
                                vertexNames = vertexNames
                              }
                            end

      | readRequest "WL" => ListAttachedRewritesRequest {
                              requestId = requestId,
                              graphName = readToEnd ()
                            }

      | readRequest "WW" => let
                              val graphName = readToDelim ()
                              val index = getInt #"w"
                            in
                              ApplyAttachedRewriteRequest {
                                requestId = requestId,
                                graphName = graphName,
                                index = index
                              }
                            end

      | readRequest code => (* Something else.  Reply with empty response. *)
              let
                (* Unlike the other cases we don't know what may follow ESCAPE. *)
                val terminator = Char.toLower startCh
                fun skipToTerminator () =
                case input1 stdIn of
                  SOME #"\u001b" =>
                  (
                    case input1 stdIn of
                      NONE => protocolError "End of file"
                    | SOME ch =>
                        if ch = terminator
                        then () (* Found the end. *)
                        else (* Some internal escape code. *) skipToTerminator()
                  )
                | SOME _ => skipToTerminator ()
                | NONE => protocolError "End of file"
              in
                skipToTerminator ();
                UnknownRequest { code = code }
              end

  in
    case commandCode of
      "C" =>
      let (* Console command. *)
        val requestId = readToEscape #";"
        val commandLength = getInt #";"
        val command = TextIO.inputN (TextIO.stdIn, commandLength)
        val _ = readToEscape #"c" (* should be empty - check? *)
        val (response,state') = Console.eval_command_string_no_quit command state
      in
        printOpen #"C";
        print requestId;
        printDelim;
        printLength response;
        printDelim;
        print response;
        printEsc #"c";
        state'
      end

    | "L" =>
      let
        val requestId = readToEscape #"l"
      in
        printOpen #"L";
        print requestId;
        printDelim;
        printList Console.command_name_list;
        printEsc #"l";
        state'
      end

    | "H" =>
      let
        val requestId = readToEscape #";"
        val commandName = readToEscape #"h"
      in
        printOpen #"H";
        print requestId;
        printDelim;
        print (the (Console.get_command_args commandName));
        printDelim;
        print (the (Console.get_command_help commandName));
        printEsc #"h";
        state'
      end

    | code => serviceRequest (readRequest code)

  end

end;

(* vi:ts=2:sw=2:sts=2:et
*)
