(*
 * Infrastructure for doing tests on quanto-core
 *
 * Note: this will only work on Unix.
 *)

signature CORE_TESTER =
sig
  type session;
  type message;
  exception test_exp of string;

  val createMessage: (string*string) -> string -> message;
  val delim: string;
  val dataChunk: string -> string;
  val escapeStr: string -> string;
  val stringList: string list -> string;

  val writeMessage: session -> message -> unit;

  val readErrorResponse: session -> string -> (string*string);
  val demandErrorResponse: session -> string -> string -> unit;
  val readOkResponse: session -> string -> unit;
  val demandOkResponse: session -> string -> unit;
  val readConsoleResponse: session -> string -> string;
  val demandConsoleResponse: session -> string -> string -> unit;
  val readConsoleHelpResponse: session -> string -> (string*string);
  val demandConsoleHelpResponse: session -> string -> (string*string) -> unit;
  val readDataResponse: session -> string -> string;
  val demandDataResponse: session -> string -> string -> unit;
  val readPrettyResponse: session -> string -> string;
  val demandPrettyResponse: session -> string -> string -> unit;
  val readXmlResponse: session -> string -> string;
  val demandXmlResponse: session -> string -> string -> unit;
  val readCountResponse: session -> string -> int;
  val demandCountResponse: session -> string -> int -> unit;
  val readNameResponse: session -> string -> string;
  val demandNameResponse: session -> string -> string -> unit;
  val readNameListResponse: session -> string -> string list;
  val demandNameListResponse: session -> string -> string list -> unit;
  val readUserDataResponse: session -> string -> string;
  val demandUserDataResponse: session -> string -> string -> unit;
  val readUnknownRequestResponse: session -> string -> string;
  val demandUnknownRequestResponse: session -> string -> string -> unit;

  val startSession: string -> session;
  val endSession: session -> unit;
  val protocolVersion: session -> string;

  val registerTest: string -> (session -> unit) -> unit;
  val runTests: unit -> unit;
end

structure CoreTester =
struct
  type session = { proc: (TextIO.instream, TextIO.outstream) Unix.proc,
                   instream: TextIO.instream,
                   outstream: TextIO.outstream,
                   version: string
                 }
  type message = { code: string, requestId: string, body: string };
  exception test_exp of string;

  open TextIO;

  fun createMessage (code,requestId) body =
        { code = code, requestId = requestId, body = body };

  fun constructMessage { code, requestId, body } =
        ("\u001b<"^code^"\u001b:"^requestId^"\u001b|"^body^"\u001b>")

  val delim = "u\001b;";
  fun dataChunk data = "\u001b["^(Int.toString (String.size data))^"\u001b|"^data^"\u001b]";
  val escapeStr = String.translate (fn #"\u001b" => "\u001b\u001b" | c => String.str c);
  fun stringList ss = String.concatWith "\u001b," (map escapeStr ss)

  fun writeMessage ({ outstream, ... }:session) message =
        (TextIO.output (outstream,constructMessage message); TextIO.flushOut outstream)
  fun eatChar ch instream =
        case (TextIO.input1 instream)
          of NONE => raise test_exp ("Unexpected EOF, expecting "^(String.str ch))
           | SOME c => if ch = c then () else
                raise test_exp ("Unexpected char "^(String.str c)^", expecting "^(String.str ch));
  val eatEsc = eatChar #"\u001b";

  local
    fun readToEscape' (soFar: string) (terminator: char) (instream:instream) : string =
          case input1 instream
            of SOME #"\u001b" =>
                (
                  case input1 instream of
                    NONE => raise test_exp "End of file"
                  | SOME #"\u001b" => (* Escaped ESC. *)
                                      readToEscape' (soFar ^ str #"\u001b") terminator instream
                  | SOME ch => if ch = terminator
                               then soFar
                               else raise test_exp (str ch ^ " not " ^ str terminator)
                )
             | SOME ch => readToEscape' (soFar ^ str ch) terminator instream
             | NONE => raise test_exp "End of file"
  in
    val readToEscape = readToEscape' ""
  end
  fun readHeader (expCode, expRequestId) instream =
        let
          val () = eatEsc instream
          val () = eatChar #"<" instream
          val code = readToEscape #":" instream
          val requestId = readToEscape #"|" instream
        in
          if code <> expCode then
            raise test_exp ("Expected a "^expCode^" response, got a "^code^" response")
          else if requestId <> expRequestId then
            raise test_exp ("Expected request id was "^expRequestId^", got "^requestId)
          else
            ()
        end

  fun readInt termCh instream : int =
        case Int.fromString (readToEscape termCh instream) of
            NONE => 0
          | SOME i => i
  fun readDataChunk instream =
        let
          val () = (eatEsc instream; eatChar #"[" instream)
          val dataLength = readInt #"|" instream
          val data = TextIO.inputN (instream, dataLength)
          val () = (eatEsc instream; eatChar #"]" instream)
        in
          data
        end;
  val printableEscapes = String.map (fn #"\u001b" => #"\u00a4" | c => c)
  fun assertAtBodyEnd instream =
    let
      val body = readToEscape #">" instream
    in
      if "" <> body then
        raise test_exp ("Expected end of body, got "^(printableEscapes body))
      else ()
    end;
  fun eatEscapedChar c instream = (eatEsc instream; eatChar c instream)

  local
    fun readList' _ 0 (_, terminator) instream = (eatEscapedChar terminator instream; [])
      | readList' _ 1 (_, terminator) instream = [readToEscape terminator instream]
      | readList' soFar count (splitter, terminator) instream =
         (case input1 instream
            of SOME #"\u001b" =>
                (
                  case input1 instream
                    of NONE           => raise test_exp "End of file"
                     | SOME #"\u001b" => readList' (soFar ^ "\u001b") count (splitter, terminator) instream
                     | SOME ch        =>
                          if ch = splitter then
                            soFar::(readList' "" (count-1) (splitter, terminator) instream)
                          else if ch = terminator then
                            raise test_exp ("Wrong number of elements in list")
                          else
                            raise test_exp (str ch ^ " not " ^ str terminator)
                )
             | SOME ch => readList' (soFar ^ str ch) count (splitter, terminator) instream
             | NONE    => raise test_exp "End of file"
             )
  in
    fun readList (splitter:char,terminator:char) (instream:instream) : string list =
      readList' "" (readInt #":" instream) (splitter,terminator) instream
  end

  fun readDataChunkResponse code ({ instream, ... }:session) requestId =
    let
      val () = readHeader (code,requestId) instream
      val data = readDataChunk instream
      val _ = assertAtBodyEnd instream
    in
      data
    end;
  fun demandDataChunkResponse code session requestId expResp =
    let val resp = (readDataChunkResponse code session requestId) in
      if expResp <> resp then
        raise test_exp ("Expected response "^(printableEscapes expResp)^", got "^(printableEscapes resp))
      else ()
    end
 
  fun readErrorResponse ({ instream, ... }:session) requestId =
    let
      val () = readHeader ("Q",requestId) instream
      val errorCode = readToEscape #";" instream
      val errorMsg = readToEscape #">" instream
    in
      (errorCode, errorMsg)
    end;
  fun demandErrorResponse session requestId expErrorCode =
    let val (errorCode,_) = readErrorResponse session requestId in
      if expErrorCode <> errorCode then
        raise test_exp ("Expected error code "^expErrorCode^", got "^errorCode)
      else ()
    end;

  fun readOkResponse ({ instream, ... }:session) requestId =
    let
      val () = readHeader ("O",requestId) instream
      val body = readToEscape #">" instream
    in
      if "" <> body then
        raise test_exp ("Expected empty body, got "^(printableEscapes body))
      else ()
    end;
  val demandOkResponse = readOkResponse;

  val readConsoleResponse = readDataChunkResponse "O";
  val demandConsoleResponse = demandDataChunkResponse "O";

  fun readConsoleHelpResponse ({ instream, ... }:session) requestId =
    let
      val () = readHeader ("H",requestId) instream
      val args = readToEscape #";" instream
      val help = readToEscape #">" instream
    in
      (args, help)
    end;
  fun demandConsoleHelpResponse session requestId (expArgs,expHelp) =
    let val (args,help) = (readConsoleHelpResponse session requestId) in
      if expArgs <> args then
        raise test_exp ("Expected args list "^(printableEscapes expArgs)^", got "^(printableEscapes args))
      else if expHelp <> help then
        raise test_exp ("Expected help "^(printableEscapes expHelp)^", got "^(printableEscapes help))
      else ()
    end

  val readDataResponse = readDataChunkResponse "R";
  val demandDataResponse = demandDataChunkResponse "R";

  val readPrettyResponse = readDataChunkResponse "P";
  val demandPrettyResponse = demandDataChunkResponse "P";

  val readXmlResponse = readDataChunkResponse "X";
  val demandXmlResponse = demandDataChunkResponse "X";

  fun readCountResponse ({ instream, ... }:session) requestId =
    let val () = readHeader ("I",requestId) instream in
      readInt #">" instream
    end;
  fun demandCountResponse session requestId expVal =
    let val i = readCountResponse session requestId in
      if expVal <> i then
        raise test_exp ("Expected count was "^(Int.toString expVal)^", got "^(Int.toString i))
      else ()
    end;

  fun readNameResponse ({ instream, ... }:session) requestId =
    let val () = readHeader ("N",requestId) instream in
      readToEscape #">" instream
    end;
  fun demandNameResponse session requestId expName =
    let val name = readNameResponse session requestId in
      if expName <> name then
        raise test_exp ("Expected response was "^(printableEscapes expName)^", got "^(printableEscapes name))
      else ()
    end;

  fun readNameListResponse ({ instream, ... }:session) requestId =
    let val () = readHeader ("N",requestId) instream in
      readList (#",",#">") instream
    end
  fun demandNameListResponse session requestId expNames =
    let val names = readNameListResponse session requestId in
      if expNames <> names then
        raise test_exp ("Expected response was "^(printableEscapes (String.concatWith "\u001b," expNames))^", got "^(printableEscapes (String.concatWith "\u001b," names)))
      else ()
    end;

  val readUserDataResponse = readDataChunkResponse "U";
  val demandUserDataResponse = demandDataChunkResponse "U";

  fun readUnknownRequestResponse ({ instream, ... }:session) requestId =
    let val () = readHeader ("Z",requestId) instream in
      readToEscape #">" instream
    end;
  fun demandUnknownRequestResponse session requestId expCode =
    let val code = readUnknownRequestResponse session requestId in
      if expCode <> code then
        raise test_exp ("Expected code was "^(printableEscapes expCode)^", got "^(printableEscapes code))
      else ()
    end;

  fun startSession path =
        let
          val proc = Unix.execute (path,["--protocol"])
          val (instream,outstream) = Unix.streamsOf proc
          val () = (eatEsc instream; eatChar #"<" instream)
          val () = (eatChar #"V" instream)
          val () = (eatEsc instream; eatChar #"|" instream)
          val version = readToEscape #">" instream
        in
          { proc = proc, instream = instream, outstream = outstream, version = version }
        end;
  fun endSession ({ proc, ... }:session) =
        let
          val status = Unix.reap proc
        in
          if (OS.Process.isSuccess status) then
            ()
          else
            raise test_exp "Core process exited with failure code"
        end
  fun protocolVersion ({ version, ... }:session) = version;

  val tests: ((string*(session -> unit)) list ref) = ref [];

  fun registerTest name testFun =
    (tests := (name,testFun)::(!tests));

  fun runTests' [] = ()
    | runTests' ((name,testFun)::ts) =
      ((let
          val () = TextIO.print (name^"...")
          val session = startSession "../../bin/quanto-core"
          val () = testFun session
          val () = endSession session
        in TextIO.print "passed\n" end
        handle test_exp msg =>
          (TextIO.print "failed:\n  "; TextIO.print msg; TextIO.print "\n"));
      runTests' ts)
        
  fun runTests () = runTests' (rev (!tests))
end

(* vi:et:sw=2:sts=2
*)
