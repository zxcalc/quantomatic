(* just in case cleanup before building the heap *)
PolyML.fullGC();

(* load needed libs  *)
use "json.sig";
use "json.sml";

(* reopen the print function *)
val print = TextIO.print;

(* very basic profiling *)
(*use "profiling.sml";*)

structure PolyChrome
= struct

    exception DOMExn of string

    (* IMPROVE: make var names reflect socket function *)

    (* code to be evaluated goes through here; e.g. initial embedded code compilation, 
       event handling, as well as console input   *)
    val socket1 = Unsynchronized.ref (NONE : Socket.active INetSock.stream_sock option)

    (* responses for JS wrappers go through here *)
    val socket2 = Unsynchronized.ref (NONE : Socket.active INetSock.stream_sock option)

    (* constants for socket communication *)
    val PREFIX_SIZE = 9; (* bytes *)
    val CHUNK_SIZE = 65536; (* bytes *)
    
    val requestCounter = Unsynchronized.ref 0
    
    val code_location = Unsynchronized.ref ""
    val code_offset = Unsynchronized.ref 1
    
    exception Error of string

    fun the (reference) = Option.valOf (!reference)

    fun make_socket (port) =
        let
            val client = INetSock.TCP.socket()
            val me = Option.valOf (NetHostDB.getByName "localhost")
            val localhost = NetHostDB.addr me
            val _ = Socket.connect(client,INetSock.toAddr(localhost, port))
            val _ = INetSock.TCP.setNODELAY(client,true)
        in
            client
        end
    
    fun recv_loop (0,_) = ""
        | recv_loop (length, socket) =
          let
              val len = if (length<CHUNK_SIZE)
                      then length
                      else CHUNK_SIZE
              val vectorReceived = Socket.recvVec(socket, len)
              val nbytes = Word8Vector.length vectorReceived
              val chunk = Byte.bytesToString(vectorReceived)
          in
              chunk ^ recv_loop(length-nbytes, socket)
          end
        
    fun recv_ (socket) =
        let
            val prefix = Byte.bytesToString(
                    Socket.recvVec(socket, PREFIX_SIZE))
            val length = Option.valOf (Int.fromString prefix)
            val data = recv_loop(length, socket)
            val t = Option.valOf (Int.fromString (String.substring (data, 0, 1)))
            val m = String.substring (data, 1, (String.size data)-1)
            (*val _ = Profiling.profile ("D;recv response;"^(Int.toString (!requestCounter))^";"^(Int.toString length))*)
        in
            if t=0 then m else raise DOMExn (m)
        end

    fun recv_code () = recv_ (the socket1)
    fun recv_response () = recv_ (the socket2)

    fun expand (str, 9) = str
              | expand (str, x) = expand (str^" ", x+1);
    
    fun send_loop (pos, length, data) =
        let
            val len = if ((pos+CHUNK_SIZE) > length)
                    then length-pos
                    else CHUNK_SIZE
            val chunk = substring (data, pos, len)
            val outv = Word8VectorSlice.full
                    (Byte.stringToBytes chunk)
            val nbytes = Socket.sendVec(the socket1, outv)
        in
            if pos+nbytes>=length
            then ()
            else send_loop(pos+nbytes, length, data)
        end
    
    fun send_request (data) =
        let
            val _ = (requestCounter := (!requestCounter + 1))
            val prefix = Int.toString (size data)
            (*val _ = Profiling.profile ("A;sending req;"^(Int.toString (!requestCounter))^";"^prefix)*)
            val prefix = expand (prefix, size prefix)
            val prefixed_data = prefix^data
            val length = size prefixed_data
        in
            send_loop(0, length, prefixed_data)
        end

    fun close_sock s =
      (Socket.shutdown(s,Socket.NO_RECVS_OR_SENDS);
       Socket.close s)

    fun evaluate code_location code_offset code =
      let
        (* uses input and output buffers for compilation and output message *)
        val in_buffer = Unsynchronized.ref (String.explode code)
        val out_buffer = Unsynchronized.ref ([]: string list);
        val current_line = Unsynchronized.ref 1;

        (* helper function *)
        fun drop_newline s =
          if String.isSuffix "\n" s then String.substring (s, 0, size s - 1)
          else s;

        fun output () = (String.concat (rev (! out_buffer)));

        (* take a charcter out of the input txt string *)
        fun get () =
          (case ! in_buffer of
              [] => NONE
            | c :: cs =>
              (in_buffer := cs; if c = #"\n" then current_line := ! current_line + 1 else (); SOME c));

        (* add to output buffer *)
        fun put s = (out_buffer := s :: ! out_buffer);

        (* handling error messages *)
        fun put_message { message = msg1, hard, location : PolyML.location,
                          context } =
          let val line_width = 76; in
            (put (if hard then "Error: " else "Warning: ");
              PolyML.prettyPrint (put, line_width) msg1;
              (case context of NONE => ()
              | SOME msg2 => PolyML.prettyPrint (put, line_width) msg2);
              put ("At line " ^ (Int.toString ((#startLine location) - code_offset)) ^ " in "
              ^ code_location ^ "\n"))
          end;

            val compile_params =
              [(* keep track of line numbers *)
                PolyML.Compiler.CPLineNo (fn () => ! current_line),
                (* the following catches any output durin
                compilation/evaluation and store it in the put stream. *)
                PolyML.Compiler.CPOutStream put,
                (* the following handles error messages specially
                to say where they come from in the error message into
                the put stream. *)
                PolyML.Compiler.CPErrorMessageProc put_message
              ]

            val (worked,_) =
              (true, while not (List.null (! in_buffer)) do
                     PolyML.compiler (get, compile_params) ())
            handle
                   (*SysErr => (* sockets can throw this *)*)
                (*(false, (raise SysErr; ()))*)
                 Exn.Interrupt => (* the PolyML process is being killed *)
                (false, (raise Exn.Interrupt; ()))
                 | exn => (* something went wrong... *)
                (false, (put ("Exception - " ^ General.exnMessage exn ^ " raised"); ()
                (* Can do other stuff here: e.g. raise exn *) ));

              (* finally, print out any messages in the output buffer *)
            val output_str = output();
            val json_obj = if output_str = ""
                           then JSON.empty
                           else
                           JSON.empty
                           |> JSON.add ("type", JSON.Int 0)
                           |> JSON.add ("r", JSON.Int 0)
                           |> JSON.add ("output", (JSON.String output_str))
            val json_obj = if worked
                then json_obj
                else JSON.update ("type", JSON.Int 1) json_obj
        in
            if (output_str="") then () else send_request (JSON.encode json_obj)
        end
        
    (*****************************************************************************)
    (*                  "use": compile from a file.                              *)
    (*****************************************************************************)

    fun use (originalName: string): unit =
    let
        (* use "f" first tries to open "f" but if that fails it tries "f.ML", "f.sml" etc. *)
        (* We use the functional layer and a reference here rather than TextIO.input1 because
           that requires locking round every read to make it thread-safe.  We know there's
           only one thread accessing the stream so we don't need it here. *)
        fun trySuffixes [] =
            (* Not found - attempt to open the original and pass back the
               exception. *)
            (TextIO.openIn originalName, originalName)
         |  trySuffixes (s::l) =
            (TextIO.openIn (originalName ^ s), originalName ^ s)
                handle IO.Io _ => trySuffixes l
        (* First in list is the name with no suffix. *)
        val (inStream, fileName) = trySuffixes("" :: ! PolyML.suffixes)
        val contents = TextIO.inputAll inStream
    in
        evaluate fileName 0 contents;
        (* Normal termination: close the stream. *)
        TextIO.closeIn (inStream)
    end (* use *)

    fun loop () =
        let
          val code = recv_code();
          val _ = evaluate "" 0 code
          val code = recv_code();
          val _ = evaluate (!code_location) (!code_offset) code
        in
            loop()
        end

    fun main (socket1port, socket2port, sandboxPath) =
        let
            val _ = (socket1 := (SOME (make_socket socket1port)))
            val _ = (socket2 := (SOME (make_socket socket2port)))
            
            val _ = OS.FileSys.chDir(sandboxPath)
            
            (* disable access to this structure *)
            (*val _ = map PolyML.Compiler.forgetStructure ["PolyChrome"]*)
            
            val _ = loop() handle
                        Exn.Interrupt => ()
                      (*| SysErr => ()*)
                        
            val _ = print "PolyML process stopped.\n"
            
            (* clean up *)
            val _ = close_sock (the socket1)
            val _ = close_sock (the socket2)
        in
            OS.Process.exit OS.Process.success
        end

end;

(* Browser / DOM goodies *)
use "console.sml";
use "jsffi.sig";
use "jsffi.sml";
use "dom.sig";
use "dom.sml";
use "canvas.sig";
use "canvas.sml";