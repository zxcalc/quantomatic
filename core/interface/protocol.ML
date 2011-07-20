structure Protocol =
struct

  fun protocolError error =
  let
      open OS.Process
  in
      TextIO.print ("Protocol error: " ^ error) handle _ => ();
      exit failure;
      raise Fail "bad" (* Never called but sets return type as 'a *)
  end

  fun readRequest (): Commands.request =
  let
    open Commands
    open TextIO

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

    val () =
      case input1 stdIn of
        NONE => OS.Process.exit OS.Process.success (* Close down. *)
      | SOME #"\u001b" => () (* Escape- start of packet. *)
      | SOME ch => protocolError(str ch ^ " not ESCAPE at start of packet")
    val startCh = (* Request code *)
      case input1 stdIn of
        NONE => protocolError "End of file"
      | SOME ch => ch
    val commandCode = startCh ^ (readToEscape #":")
  in
    case commandCode of
      "C" =>
      let (* Console command. *)
        val requestId = readToEscape #";"
        val command = readToEscape #"c"
      in
        ConsoleRequest { requestId = requestId, command = command }
      end

    | "L" =>
      let
        val requestId = readToEscape #"l"
      in
        ConsoleCommandListRequest { requestId = requestId }
      end

    | "H" =>
      let
        val requestId = readToEscape #";"
        val commandName = readToEscape #"h"
      in
        ConsoleCommandHelpRequest { requestId = requestId, commandName = commandName }
      end

    | "GL" =>
      let
        val requestId = readToEscape #"g"
      in
        ListGraphsRequest { requestId = requestId }
      end

    | "GOE" =>
      let
        val requestId = readToEscape #"g"
      in
        LoadGraphRequest { requestId = requestId, details = LoadEmptyGraphRequest }
      end

    | "GOF" =>
      let
        val requestId = readToEscape #";"
        val fileName = readToEscape #"g"
      in
        LoadGraphRequest { requestId = requestId, details = LoadGraphFromFileRequest { fileName = fileName } }
      end

    | "GOD" =>
      let
        val requestId = readToEscape #";"
        val dataLength = getInt #";"
        val data = TextIO.inputN (TextIO.stdIn, dataLength)
        val _ = readToEscape #"g" (* should be empty - check? *)
      in
        LoadGraphRequest { requestId = requestId, details = LoadGraphFromDataRequest { data = data } }
      end

    | "GOG" =>
      let
        val requestId = readToEscape #";"
        val graphName = readToEscape #"g"
      in
        LoadGraphRequest { requestId = requestId, details = CopyGraphRequest { graphName = graphName } }
      end

    | "GOS" =>
      let
        val requestId = readToEscape #";"
        val graphName = readToEscape #";"
        val vertexNames = readList (#",",#"g")
      in
        LoadGraphRequest { requestId = requestId, details = CopySubgraphRequest { graphName = graphName, vertexNames = vertexNames } }
      end

    | "GS" =>
      let
        val requestId = readToEscape #";"
        val graphName = readToEscape #";"
        val fileName  = readToEscape #"g"
      in
        SaveGraphToFileRequest { requestId = requestId, graphName = graphName, fileName = fileName }
      end

    | "GR" =>
      let
        val requestId = readToEscape #";"
        val oldName   = readToEscape #";"
        val newName   = readToEscape #"g"
      in
        RenameGraphRequest { requestId = requestId, oldName = oldName, newName = newName }
      end

    | "GD" =>
      let
        val requestId = readToEscape #";"
        val graphName   = readToEscape #"g"
      in
        RenameGraphRequest { requestId = requestId, graphName = graphName }
      end

    | "GE" =>
      let
        val requestId  = readToEscape #";"
        val graphName  = readToEscape #";"
        val formatName = readToEscape #"g"
        val format     = case formatName of
                           "native"      => NativeGraphFormat
                         | "hilb"        => HilbertSpaceGraphFormat
                         | "mathematica" => MathematicaGraphFormat
                         | "matlab"      => MatlabGraphFormat
                         | "tikz"        => TikzGraphFormat
                         | _             => UnknownGraphFormat
      in
        ExportGraphRequest { requestId = requestId, graphName = graphName, format = format }
      end

    | "GVA" =>
      let
        val requestId  = readToEscape #";"
        val graphName  = readToEscape #"g"
      in
        DescribeGraphRequest { requestId = requestId, graphName = graphName, desc = DescribeFullGraph }
      end

    | "GVV" =>
      let
        val requestId  = readToEscape #";"
        val graphName  = readToEscape #"g"
      in
        DescribeGraphRequest { requestId = requestId, graphName = graphName, desc = ListVertices }
      end

    | "GVE" =>
      let
        val requestId  = readToEscape #";"
        val graphName  = readToEscape #"g"
      in
        DescribeGraphRequest { requestId = requestId, graphName = graphName, desc = ListEdges }
      end

    | "GVB" =>
      let
        val requestId  = readToEscape #";"
        val graphName  = readToEscape #"g"
      in
        DescribeGraphRequest { requestId = requestId, graphName = graphName, desc = ListBangBoxes }
      end

    | "GVW" =>
      let
        val requestId  = readToEscape #";"
        val graphName  = readToEscape #";"
        val vertexName  = readToEscape #"g"
      in
        DescribeGraphRequest { requestId = requestId, graphName = graphName, desc = VertexData { vertexName = vertexName } }
      end

    | "GVX" =>
      let
        val requestId  = readToEscape #";"
        val graphName  = readToEscape #";"
        val vertexName  = readToEscape #"g"
      in
        DescribeGraphRequest { requestId = requestId, graphName = graphName, desc = VertexUserData { vertexName = vertexName } }
      end

    | "GVF" =>
      let
        val requestId  = readToEscape #";"
        val graphName  = readToEscape #";"
        val edgeName  = readToEscape #"g"
      in
        DescribeGraphRequest { requestId = requestId, graphName = graphName, desc = EdgeData { edgeName = edgeName } }
      end

    | "GVG" =>
      let
        val requestId  = readToEscape #";"
        val graphName  = readToEscape #";"
        val edgeName  = readToEscape #"g"
      in
        DescribeGraphRequest { requestId = requestId, graphName = graphName, desc = EdgeUserData { edgeName = edgeName } }
      end

    | "GVC" =>
      let
        val requestId  = readToEscape #";"
        val graphName  = readToEscape #";"
        val bangBoxName  = readToEscape #"g"
      in
        DescribeGraphRequest { requestId = requestId, graphName = graphName, desc = BangBoxVertices { bangBoxName = bangBoxName } }
      end

    | "GVD" =>
      let
        val requestId  = readToEscape #";"
        val graphName  = readToEscape #";"
        val bangBoxName  = readToEscape #"g"
      in
        DescribeGraphRequest { requestId = requestId, graphName = graphName, desc = BangBoxUserData { bangBoxName = bangBoxName } }
      end

    | "GMU" =>
      let
        val requestId  = readToEscape #";"
        val graphName  = readToEscape #"g"
      in
        EditGraphRequest { requestId = requestId, graphName = graphName, action = UndoAction }
      end

    | "GMR" =>
      let
        val requestId  = readToEscape #";"
        val graphName  = readToEscape #"g"
      in
        EditGraphRequest { requestId = requestId, graphName = graphName, action = RedoAction }
      end

    | code => (* Something else.  Reply with empty response. *)
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
  end

end;

(* vi:ts=2:sw=2:sts=2:et
*)
