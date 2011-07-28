
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
    fun printInt i = print (Int.toString i)
    fun printLength s = printInt (String.size s)
    fun printEsc ch = print (String.concat["\u001b", String.str ch])
    fun printClose ch = (printEsc ch; TextIO.flushOut outstream)
    fun printOpen ch requestId = print (String.concat["\u001b", String.str ch, ":", requestId, ":"])
    fun printDelim ch = print "\u001b;"
    fun printList [] = ()
      | printList [s] = print s
      | printList (s::ss) = (print s; List.app (fn e => (printEsc #","; print e)) ss)
    (* }}} *)

    (* {{{ outputResponse *)
    fun outputResponse (OkResponse { requestId }) =
          (printOpen #"O" requestId; printEsc #"o")
      | outputResponse (ErrorResponse { requestId, errorCode, message }) =
         (printOpen #"X" requestId;
          print errorCode;
          printDelim;
          print message;
          printClose #"x")
      | outputResponse (DataResponse { requestId, data }) =
         (printOpen #"R" requestId;
          printLength data;
          printDelim;
          print data;
          printClose #"r")
      | outputResponse (PrettyResponse { requestId, data }) =
          let val s = Pretty.string_of data in
            printOpen #"R" requestId; (* Pretty is just data *)
            printLength s;
            printDelim;
            print s;
            printClose #"r"
          end
      | outputResponse (XmlResponse { requestId, data }) =
          let val xml = XML.string_of data in
            printOpen #"R" requestId; (* XML is just data *)
            printLength xml;
            printDelim;
            print xml;
            printClose #"r"
          end
      | outputResponse (NameResponse { requestId, name }) =
         (printOpen #"N" requestId;
          print name;
          printClose #"n")
      | outputResponse (NameListResponse { requestId, names }) =
         (printOpen #"M" requestId;
          printList names;
          printClose #"m")
    (* FIXME descs *)
      | outputResponse (UserDataResponse { requestId, data }) =
         (printOpen #"U" requestId;
          printLength data;
          printDelim;
          print data;
          printClose #"u")
    (* FIXME rewrite list *)
      | outputResponse (UnknownResponse { requestId, code }) =
         (printOpen #"Z" requestId;
          print code;
          printClose #"z")

    fun outputOkResponse requestId = (outputResponse (OkResponse { requestId = requestId }))
    fun outputErrorResponse requestId code message =
          (outputResponse (ErrorResponse { requestId = requestId, errorCode = code, message = message }))
    (* }}} *)

    (* {{{ Input helpers *)
    (* Returns the string as far as the next ESC and the terminator. *)
    fun readToEscape' (soFar: string) (terminator: char) : string =
    case input1 instream of
      SOME #"\u001b" =>
      (
        case input1 instream of
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

    fun readList' (soFar:string) (splitter : char, terminator : char) : string list =
    case input1 instream of
      SOME #"\u001b" =>
      (
        case input1 instream of
          NONE => protocolError "End of file"
        | SOME ch =>
            if ch = terminator
            then [soFar]
            else if ch = splitter
            then soFar::(readList' "" (splitter, terminator))
            else if ch = #"\u001b" (* Escaped ESC. *)
            then readList' (soFar ^ str #"\u001b") (splitter, terminator)
            else protocolError(str ch ^ " not " ^ str terminator)
      )
    | SOME ch => readList' (soFar ^ str ch) (splitter, terminator)
    | NONE => protocolError "End of file"
    val readList = readList' ""

    fun readData () =
          let val dataLength = getInt #";" in
            TextIO.inputN (instream, dataLength)
          end

    fun readComponentData "xml" = SOME (XmlCData [XML.parse (readData ())])
      | readComponentData "string" = SOME (StringCData (readData ()))
      | readComponentData _ = NONE
    (* }}} *)

    fun run_in_textstreams' eval =
    let
      val () =
        case input1 instream of
          NONE => OS.Process.exit OS.Process.success (* Close down. *)
        | SOME #"\u001b" => () (* Escape- start of packet. *)
        | SOME ch => protocolError((str ch) ^ " not ESCAPE at start of packet")
      val startCh =
        case input1 instream of
          NONE => protocolError "End of file"
        | SOME ch => ch

      val commandCode = (str startCh) ^ (readToEscape #":")
      val requestId = readToEscape #":"

      (* {{{ More input helpers *)
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
      (* }}} *)

      (* {{{ readRequest *)
      fun readRequest "GL" = (assertAtEnd; ListGraphsRequest { requestId = requestId })
        | readRequest "GOE" = (assertAtEnd;
                               LoadGraphRequest {
                                 requestId = requestId,
                                 details   = LoadEmptyGraphRequest
                               })
        | readRequest "GOF" =  LoadGraphRequest {
                                 requestId = requestId,
                                 details = LoadGraphFromFileRequest {
                                             fileName = readToEnd()
                                           }
                               }
        | readRequest "GOD" =  let
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
        | readRequest "GOG" =  LoadGraphRequest {
                                 requestId = requestId,
                                 details = CopyGraphRequest { graphName = readToEnd () }
                               }

        | readRequest "GOS" =  let
                                 val sourceGraphName = readToEscape #";"
                                 val graphName = readToEscape #";"
                                 val vertexNames = readListToEnd ()
                               in
                                 LoadGraphRequest {
                                   requestId = requestId,
                                   details = CopySubgraphRequest {
                                               source = sourceGraphName,
                                               graphName = graphName,
                                               vertexNames = vertexNames
                                             }
                                 }
                               end

        | readRequest "GS" =  let
                                val graphName = readToEscape #";"
                                val fileName  = readToEnd ()
                              in
                                SaveGraphToFileRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  fileName = fileName }
                              end

        | readRequest "GR" =  let
                                val oldName   = readToEscape #";"
                                val newName   = readToEnd ()
                              in
                                RenameGraphRequest {
                                  requestId = requestId,
                                  oldName = oldName,
                                  newName = newName
                                }
                              end

        | readRequest "GD" =  DiscardGraphRequest {
                                requestId = requestId,
                                graphName = readToEnd ()
                              }

        | readRequest "GE" =  let
                                val graphName  = readToEscape #";"
                                val formatName = readToEnd ()
                                val format     = case formatName
                                                   of "native"      => SOME NativeGraphFormat
                                                    | "hilb"        => SOME HilbertSpaceGraphFormat
                                                    | "mathematica" => SOME MathematicaGraphFormat
                                                    | "matlab"      => SOME MatlabGraphFormat
                                                    | "tikz"        => SOME TikzGraphFormat
                                                    | _             => NONE
                              in
                                case format
                                  of (SOME f) =>
                                        ExportGraphRequest {
                                          requestId = requestId,
                                          graphName = graphName,
                                          format = f
                                        }
                                   | NONE =>
                                        DummyRequest { response =
                                          ErrorResponse {
                                            requestId = requestId,
                                            errorCode = "BADFORMAT",
                                            message = "Unknown format requested"
                                          }
                                        }
                              end

        | readRequest "GVA" =  DescribeGraphRequest {
                                 requestId = requestId,
                                 graphName = readToEnd (),
                                 desc = DescribeFullGraph
                               }

        | readRequest "GVV" =  DescribeGraphRequest {
                                 requestId = requestId,
                                 graphName = readToEnd (),
                                 desc = ListVertices
                               }

        | readRequest "GVE" =  DescribeGraphRequest {
                                 requestId = requestId,
                                 graphName = readToEnd (),
                                 desc = ListEdges
                               }

        | readRequest "GVB" =  DescribeGraphRequest {
                                 requestId = requestId,
                                 graphName = readToEnd (),
                                 desc = ListBangBoxes
                               }

        | readRequest "GVW" =  let
                                 val graphName  = readToEscape #";"
                                 val vertexName  = readToEnd ()
                               in
                                 DescribeGraphRequest {
                                   requestId = requestId,
                                   graphName = graphName,
                                   desc = VertexData { vertexName = vertexName }
                                 }
                               end

        | readRequest "GVX" =  let
                                 val graphName  = readToEscape #";"
                                 val vertexName  = readToEnd ()
                               in
                                 DescribeGraphRequest {
                                   requestId = requestId,
                                   graphName = graphName,
                                   desc = VertexUserData { vertexName = vertexName }
                                 }
                               end

        | readRequest "GVF" =  let
                                  val graphName  = readToEscape #";"
                                  val edgeName  = readToEnd ()
                                in
                                  DescribeGraphRequest {
                                    requestId = requestId,
                                    graphName = graphName,
                                    desc = EdgeData { edgeName = edgeName }
                                  }
                                end

        | readRequest "GVG" =  let
                                  val graphName  = readToEscape #";"
                                  val edgeName  = readToEnd ()
                                in
                                  DescribeGraphRequest {
                                    requestId = requestId,
                                    graphName = graphName,
                                    desc = EdgeUserData { edgeName = edgeName }
                                  }
                                end

        | readRequest "GVC" =  let
                                  val graphName  = readToEscape #";"
                                  val bangBoxName  = readToEnd ()
                                in
                                  DescribeGraphRequest {
                                    requestId = requestId,
                                    graphName = graphName,
                                    desc = BangBoxVertices { bangBoxName = bangBoxName }
                                  }
                                end

        | readRequest "GVD" =  let
                                  val graphName  = readToEscape #";"
                                  val bangBoxName  = readToEnd ()
                                in
                                  DescribeGraphRequest {
                                    requestId = requestId,
                                    graphName = graphName,
                                    desc = BangBoxUserData { bangBoxName = bangBoxName }
                                  }
                                end

        | readRequest "GMU" =  EditGraphRequest {
                                  requestId = requestId,
                                  graphName = readToEnd (),
                                  action = UndoAction
                                }

        | readRequest "GMR" =  EditGraphRequest {
                                  requestId = requestId,
                                  graphName = readToEnd (),
                                  action = RedoAction
                                }

        | readRequest "GMI" =  let
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

        | readRequest "GMVA" = let
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
                                          requestId = requestId,
                                          graphName = graphName,
                                          action =  AddVertexAction {
                                                      vertexType = vertexType,
                                                      resultFormat = f
                                                    }
                                        }
                                     | NONE =>
                                        DummyRequest { response =
                                          ErrorResponse {
                                            requestId = requestId,
                                            errorCode = "BADFORMAT",
                                            message = "Unknown format requested"
                                          }
                                        }
                                end

        | readRequest "GMVR" = let
                                  val graphName = readToDelim ()
                                  val oldName = readToDelim ()
                                  val newName = readToEnd ()
                                in
                                  EditGraphRequest {
                                    requestId = requestId,
                                    graphName = graphName,
                                    action =  RenameVertexAction {
                                                oldName = oldName,
                                                newName = newName
                                              }
                                  }
                                end

        | readRequest "GMVD" = let
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

        | readRequest "GMVS" = let
                                  val graphName = readToDelim ()
                                  val vertexName = readToDelim ()
                                  val format = readToDelim ()
                                  val data = readComponentDataToEnd format
                                in
                                  case data
                                    of (SOME d) =>
                                        EditGraphRequest {
                                          requestId = requestId,
                                          graphName = graphName,
                                          action =  SetVertexDataAction {
                                                      vertexName = vertexName,
                                                      vertexData = d
                                                    }
                                        }
                                     | NONE =>
                                        DummyRequest { response =
                                          ErrorResponse {
                                            requestId = requestId,
                                            errorCode = "BADFORMAT",
                                            message = "Unknown vertex data format"
                                          }
                                        }
                                end

        | readRequest "GMVU" = let
                                  val graphName = readToDelim ()
                                  val vertexName = readToDelim ()
                                  val data = readDataToEnd ()
                                in
                                  EditGraphRequest {
                                    requestId = requestId,
                                    graphName = graphName,
                                    action =  SetVertexUserDataAction {
                                                vertexName = vertexName,
                                                userData = data
                                              }
                                  }
                                end

        | readRequest "GMEA" = let
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
                                          requestId = requestId,
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
                                            requestId = requestId,
                                            errorCode = "BADFORMAT",
                                            message = "Unknown format requested"
                                          }
                                        }
                                end

        | readRequest "GMER" = let
                                  val graphName = readToDelim ()
                                  val oldName = readToDelim ()
                                  val newName = readToEnd ()
                                in
                                  EditGraphRequest {
                                    requestId = requestId,
                                    graphName = graphName,
                                    action =  RenameEdgeAction {
                                                oldName = oldName,
                                                newName = newName
                                              }
                                  }
                                end

        | readRequest "GMED" = let
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

        | readRequest "GMES" = let
                                  val graphName = readToDelim ()
                                  val edgeName = readToDelim ()
                                  val format = readToDelim ()
                                  val data = readComponentDataToEnd format
                                in
                                  case data
                                    of (SOME d) =>
                                        EditGraphRequest {
                                          requestId = requestId,
                                          graphName = graphName,
                                          action =  SetEdgeDataAction {
                                                      edgeName = edgeName,
                                                      edgeData = d
                                                    }
                                        }
                                     | NONE =>
                                        DummyRequest { response =
                                          ErrorResponse {
                                            requestId = requestId,
                                            errorCode = "BADFORMAT",
                                            message = "Unknown vertex data format"
                                          }
                                        }
                                end

        | readRequest "GMEU" = let
                                  val graphName = readToDelim ()
                                  val edgeName = readToDelim ()
                                  val data = readDataToEnd ()
                                in
                                  EditGraphRequest {
                                    requestId = requestId,
                                    graphName = graphName,
                                    action =  SetEdgeUserDataAction {
                                                edgeName = edgeName,
                                                userData = data
                                              }
                                  }
                                end

        | readRequest "GMBA" = let
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
                                          requestId = requestId,
                                          graphName = graphName,
                                          action =  AddBangBoxAction {
                                                      vertexNames = vertexNames,
                                                      resultFormat = f
                                                    }
                                        }
                                     | NONE =>
                                        DummyRequest { response =
                                          ErrorResponse {
                                            requestId = requestId,
                                            errorCode = "BADFORMAT",
                                            message = "Unknown format requested"
                                          }
                                        }
                                end

        | readRequest "GMBR" = let
                                  val graphName = readToDelim ()
                                  val oldName = readToDelim ()
                                  val newName = readToEnd ()
                                in
                                  EditGraphRequest {
                                    requestId = requestId,
                                    graphName = graphName,
                                    action =  RenameBangBoxAction {
                                                oldName = oldName,
                                                newName = newName
                                              }
                                  }
                                end

        | readRequest "GMBD" = let
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

        | readRequest "GMBK" = let
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

        | readRequest "GMBC" = let
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

        | readRequest "GMBM" = let
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

        | readRequest "GMBB" = let
                                  val graphName = readToDelim ()
                                  val bangBoxName = readToDelim ()
                                  val vertexNames = readListToDelim ()
                                in
                                  EditGraphRequest {
                                    requestId = requestId,
                                    graphName = graphName,
                                    action =  BangVerticesAction {
                                                bangBoxName = bangBoxName,
                                                vertexNames = vertexNames
                                              }
                                  }
                                end

        | readRequest "GMBL" = let
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

        | readRequest "GMEU" = let
                                  val graphName = readToDelim ()
                                  val bangBoxName = readToDelim ()
                                  val data = readDataToEnd ()
                                in
                                  EditGraphRequest {
                                    requestId = requestId,
                                    graphName = graphName,
                                    action =  SetBangBoxUserDataAction {
                                                bangBoxName = bangBoxName,
                                                userData = data
                                              }
                                  }
                                end

        | readRequest "RSO" =  ImportRulesetFromFileRequest {
                                  requestId = requestId,
                                  fileName = readToEnd ()
                                }

        | readRequest "RSI" =  ImportRulesetFromDataRequest {
                                  requestId = requestId,
                                  data = readDataToEnd ()
                                }

        | readRequest "RSS" =  ExportRulesetToFileRequest {
                                  requestId = requestId,
                                  fileName = readToEnd()
                                }

        | readRequest "RSE" =  (assertAtEnd ();
                                ExportRulesetAsDataRequest {
                                  requestId = requestId
                                })

        | readRequest "RRL" =  (assertAtEnd ();
                                ListRulesRequest {
                                  requestId = requestId
                                })

        | readRequest "RRA" =  (assertAtEnd ();
                                ListActiveRulesRequest {
                                  requestId = requestId
                                })

        | readRequest "RRT" = ListRulesByTagRequest {
                                requestId = requestId,
                                tagName = readToEnd()
                              }

        | readRequest "RRP" = OpenRuleLhsRequest {
                                requestId = requestId,
                                ruleName = readToEnd()
                              }

        | readRequest "RRQ" = OpenRuleRhsRequest {
                                requestId = requestId,
                                ruleName = readToEnd()
                              }

        | readRequest "RRU" =  let
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

        | readRequest "RRD" =  DeleteRuleRequest {
                                  requestId = requestId,
                                  ruleName = readToEnd()
                                }

        | readRequest "RRY" =  ActivateRuleRequest {
                                  requestId = requestId,
                                  ruleName = readToEnd()
                                }

        | readRequest "RRN" =  DeactivateRuleRequest {
                                  requestId = requestId,
                                  ruleName = readToEnd()
                                }

        | readRequest "RTL" =  (assertAtEnd ();
                                ListTagsRequest {
                                  requestId = requestId
                                })

        | readRequest "RTT" = let
                                val ruleName = readToDelim ()
                                val tagName = readToEnd ()
                              in
                                TagRuleRequest {
                                  requestId = requestId,
                                  ruleName = ruleName,
                                  tagName = tagName
                                }
                              end

        | readRequest "RTU" = let
                                val ruleName = readToDelim ()
                                val tagName = readToEnd ()
                              in
                                UntagRuleRequest {
                                  requestId = requestId,
                                  ruleName = ruleName,
                                  tagName = tagName
                                }
                              end

        | readRequest "RTF" = ForgetTagRequest {
                                requestId = requestId,
                                tagName = readToEnd ()
                              }

        | readRequest "RTD" = DeleteRulesByTagRequest {
                                requestId = requestId,
                                tagName = readToEnd ()
                              }

        | readRequest "RTY" = ActivateRulesByTagRequest {
                                requestId = requestId,
                                tagName = readToEnd ()
                              }

        | readRequest "RTN" = DeactivateRulesByTagRequest {
                                requestId = requestId,
                                tagName = readToEnd ()
                              }

        | readRequest "WA" =  let
                                val graphName = readToDelim ()
                                val vertexNames = readListToEnd ()
                              in
                                AttachRewritesRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  vertexNames = vertexNames
                                }
                              end

        | readRequest "WO" =  let
                                val graphName = readToDelim ()
                                val vertexNames = readListToEnd ()
                              in
                                AttachOneRewriteRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  vertexNames = vertexNames
                                }
                              end

        | readRequest "WL" =  ListAttachedRewritesRequest {
                                requestId = requestId,
                                graphName = readToEnd ()
                              }

        | readRequest "WW" =  let
                                val graphName = readToDelim ()
                                val index = getInt #"w"
                              in
                                ApplyAttachedRewriteRequest {
                                  requestId = requestId,
                                  graphName = graphName,
                                  index = index
                                }
                              end

        | readRequest code = (* Something else.  Reply with empty response. *)
                let
                  (* Unlike the other cases we don't know what may follow ESCAPE. *)
                  val terminator = Char.toLower startCh
                  fun skipToTerminator () =
                  case input1 instream of
                    SOME #"\u001b" =>
                    (
                      case input1 instream of
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
                  UnknownRequest { requestId = requestId, code = code }
                end
        (* }}} *)

      fun next_loop () = run_in_textstreams' eval
      fun change_loop new_eval = run_in_textstreams' new_eval

    in
      case commandCode of
        "C" =>
        let (* Console command. *)
          val requestId = readToEscape #";"
          val commandLength = getInt #";"
          val command = TextIO.inputN (instream, commandLength)
          val _ = readToEscape #"c" (* should be empty - check? *)
          fun outputCmdResult data =
                 (printOpen #"C";
                  print requestId;
                  printDelim;
                  printLength data;
                  printDelim;
                  print data;
                  printClose #"c")
        in
          (case Console.eval_command_string_no_quit eval command
            of InterfaceResult.NORMAL output =>
                (outputCmdResult output; next_loop ())
             | InterfaceResult.CHANGE_THEORY "red-green" =>
                (change_loop RG_Controller.serviceRequest)
             | InterfaceResult.CHANGE_THEORY th =>
                (outputCmdResult ("!!! Unknown theory \""^th^"\"\n"); next_loop ())
             | InterfaceResult.SKIP => (outputCmdResult ""; next_loop ())
             | InterfaceResult.QUIT => ())
        end

      | "L" =>
        let
          val requestId = readToEscape #"l"
        in
          printOpen #"L";
          print requestId;
          printDelim;
          printList Console.command_name_list;
          printClose #"l"
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
          printClose #"h"
        end

      | "T" =>
        let
          val requestId = readToEscape #";"
          val theoryName = readToEscape #"t"
        in
          case theoryName
            of "red-green" => (outputOkResponse requestId; change_loop RG_Controller.serviceRequest)
             | _ => (outputErrorResponse requestId "BADTHEORY" "Unknown theory"; change_loop RG_Controller.serviceRequest)
        end

      | code => (outputResponse (eval (readRequest code)))

    end
  in
    printOpen #"V";
    print "1.0.0";
    printClose #"v";
    run_in_textstreams' RG_Controller.serviceRequest
  end;
  
  fun flushIn () = case TextIO.canInput (TextIO.stdIn, 1)
                        of SOME n => (TextIO.input1 TextIO.stdIn;())
                         | NONE => ();
  
  fun run_in_stdio () = (flushIn(); run_in_textstreams (TextIO.stdIn,TextIO.stdOut));
end;

(* vi:ts=2:sw=2:sts=2:et
*)
