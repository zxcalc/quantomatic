(*  Title:      Pure/General/socket_io.ML
    Author:     Timothy Bourke, NICTA
    Author:     Makarius

Stream IO over TCP sockets.  Following example 10.2 in "The Standard
ML Basis Library" by Emden R. Gansner and John H. Reppy.

Note: BinIO requires Poly/ML 5.5.x to work reliably.
*)

signature SOCKET_IO =
sig
  val make_streams: Socket.active INetSock.stream_sock -> BinIO.instream * BinIO.outstream
  val open_streams: string -> BinIO.instream * BinIO.outstream
end;

structure Socket_IO: SOCKET_IO =
struct

fun make_streams socket =
  let
    val (host, port) = INetSock.fromAddr (Socket.Ctl.getSockName socket);
    val name = NetHostDB.toString host ^ ":" ^ string_of_int port;

    val rd =
      BinPrimIO.RD {
        name = name,
        chunkSize = Socket.Ctl.getRCVBUF socket,
        readVec = SOME (fn n => Socket.recvVec (socket, n)),
        readArr = SOME (fn buffer => Socket.recvArr (socket, buffer)),
        readVecNB = NONE,
        readArrNB = NONE,
        block = NONE,
        canInput = NONE,
        avail = fn () => NONE,
        getPos = NONE,
        setPos = NONE,
        endPos = NONE,
        verifyPos = NONE,
        close = fn () => Socket.close socket,
        ioDesc = NONE
      };

    val wr =
      BinPrimIO.WR {
        name = name,
        chunkSize = Socket.Ctl.getSNDBUF socket,
        writeVec = SOME (fn buffer => Socket.sendVec (socket, buffer)),
        writeArr = SOME (fn buffer => Socket.sendArr (socket, buffer)),
        writeVecNB = NONE,
        writeArrNB = NONE,
        block = NONE,
        canOutput = NONE,
        getPos = NONE,
        setPos = NONE,
        endPos = NONE,
        verifyPos = NONE,
        close = fn () => Socket.close socket,
        ioDesc = NONE
      };

    val in_stream =
      BinIO.mkInstream
        (BinIO.StreamIO.mkInstream (rd, Word8Vector.fromList []));

    val out_stream =
      BinIO.mkOutstream
        (BinIO.StreamIO.mkOutstream (wr, IO.BLOCK_BUF));

  in (in_stream, out_stream) end;


fun open_streams socket_name =
  let
    fun err () = error ("Bad socket name: " ^ quote socket_name);
    val (host, port) =
      (case space_explode ":" socket_name of
        [h, p] =>
         (case NetHostDB.getByName h of SOME host => host | NONE => err (),
          case Int.fromString p of SOME port => port | NONE => err ())
      | _ => err ());
    val socket: Socket.active INetSock.stream_sock = INetSock.TCP.socket ();
    val _ = Socket.connect (socket, INetSock.toAddr (NetHostDB.addr host, port));
  in make_streams socket end;

end;

