signature TEXT_SOCKET =
sig
  type T
  val local_connect : int -> T (* connects to port *)
  val write : T -> string -> unit (* write to stream *)
  val read : T -> string (* read from stream *)
  val close : T -> unit (* close stream *)
  (* donesn't work currently*)
  val safe_local_connect : int -> T (* connects to port *)
  val safe_close : T -> unit (* close stream *)

  val get_instream : T -> TextIO.instream
  val get_outstream : T -> TextIO.outstream
  val get_io_stream : T -> TextIO.instream * TextIO.outstream
  val get_socket : T -> Socket.active INetSock.stream_sock
end
 
structure TextSocket : TEXT_SOCKET =
struct
  
  (* debugging, can be turned off or on *)
  fun server_print str = ()(*writeln str*);

  datatype T = SockData of { 
   sock : Socket.active INetSock.stream_sock,
   passive : Socket.passive INetSock.stream_sock,
   sin : TextIO.instream,
   sout : TextIO.outstream
 }

  fun get_instream (SockData rep) = #sin rep;
  fun get_outstream (SockData rep) = #sout rep;
  fun get_io_stream (SockData rep) = (#sin rep, #sout rep);
  fun get_socket (SockData rep) = #sock rep;
  
  (* export elements of array to a list *)
  fun arrayToList0 l idx char_arr =
    if idx < CharArray.length char_arr
    then arrayToList0 ((CharArray.sub(char_arr, idx)) :: l) (idx + 1) char_arr
    else (rev l);
  val arrayToList = arrayToList0 [] 0;
  
  (* in WR: send vector *)
  fun sendVec0 sock char_slice = 
    let
      (* char slice to byte slice *)
      val (str, i, sz) = CharVectorSlice.base char_slice;
      val byte_slice = Word8VectorSlice.slice (Byte.stringToBytes str, i ,SOME sz)
    in
      Socket.sendVec(sock, byte_slice)
    end
  
  (* in WR: send array *)
  fun sendArr0 sock char_slice = 
    let
      (* char slice to byte slice *)
      val (char_array, i, sz) = CharArraySlice.base char_slice;
      val word_array = 
        arrayToList char_array 
        |> map Char.ord 
        |> map Word8.fromInt
        |> Word8Array.fromList;
      val byte_slice = Word8ArraySlice.slice ( word_array, i ,SOME sz)
    in
      Socket.sendArr(sock, byte_slice)
    end
  
  (* with given sock, init TEXT_IO stream for input and output *)
  fun make_streams sock =
    let
      val bufsize = (*Unsynchronized.ref*) 4096;
      val buffering = IO.BLOCK_BUF;
      val socket_name = "psgraph_gui_socket";
      val reader = TextPrimIO.RD {
        name      = "psgraph_gui_socket",
        chunkSize = bufsize,
        readVec   =  SOME (fn vec => Byte.bytesToString(Socket.recvVec(sock, vec))),
        readArr   =  NONE, (* would this be a problem ??*)
        readVecNB = NONE,
        readArrNB = NONE,                          
        block     = NONE,
        canInput  = NONE,
        avail     = fn() => NONE,
        getPos    = NONE,
        setPos    = NONE,
        endPos    = NONE,
        verifyPos = NONE,
        close     = (fn () => Socket.close sock),
        ioDesc    = NONE };
  
      val writer = TextPrimIO.WR {
        name       = socket_name,
        chunkSize  = bufsize,
        writeVec   = SOME(sendVec0 sock) (* don't think it's used anywhere*),                    
        writeArr   = SOME (sendArr0 sock),
        writeVecNB = NONE,
        writeArrNB = NONE,
        block      = NONE,
        canOutput  = NONE,
        getPos     = NONE,
        setPos     = NONE,
        endPos     = NONE,
        verifyPos  = NONE,
        close      = fn () => Socket.close sock,
        ioDesc     = NONE };
      val input = TextIO.mkInstream(TextIO.StreamIO.mkInstream(reader, ""));
      val output = TextIO.mkOutstream(TextIO.StreamIO.mkOutstream(writer, buffering));
    in
      (input, output)
    end

 (*fun init_server_socket opt_host port =
  let
    val sock = INetSock.TCP.socket ();
    val addr =
      (case opt_host of
         NONE => INetSock.any port
       | SOME host =>
           NetHostDB.getByName host
           |> the
           |> NetHostDB.addr
           |> rpair port
           |> INetSock.toAddr
           handle Option => raise Fail ("Cannot resolve hostname: " ^ host));
    val _ = Socket.bind (sock, addr);
    val _ = Socket.listen (sock, 5);
  in sock end;*)

(* setup a passive socket server *)
fun init_server_socket port =
  let
    val sock = INetSock.TCP.socket ();
    val _ = Socket.Ctl.setREUSEADDR (sock, true);
    val addr = INetSock.any port;
    val _ = Socket.bind (sock, addr); 
    val _ = Socket.listen (sock, 5);
  in sock end;

 fun local_connect port =
  let  
   val passive_sock = init_server_socket port;
   val (sock, _)= Socket.accept passive_sock;
   val (sin, sout) = make_streams sock;
  in
   SockData { sock = sock, passive = passive_sock, sin = sin, sout = sout }
  end;

 fun write (SockData data) msg = TextIO.output (#sout data,msg);
 fun read (SockData data) = TextIO.input (#sin data);

 fun close (SockData data) =
      (TextIO.closeOut (#sout data) handle e => warning ("can't close output stream");
      TextIO.closeIn (#sin data) handle e => warning ("can't close input stream");
      Socket.close (#sock data) handle e => warning ("can't close active socket");
      Socket.close (#passive data) handle _ =>  warning ("can't close passive socket"));

 val last_sock :
   { sock : Socket.active INetSock.stream_sock option,
     passive : Socket.passive INetSock.stream_sock option,
     sin : TextIO.instream option,
     sout : TextIO.outstream option
   } Synchronized.var
   = Synchronized.var "last sock" {sock = NONE, passive = NONE, sin= NONE, sout = NONE};

 val socket_tracing = K;

  fun clear_last_sock _ =
    let
      val _ = socket_tracing "clear last sock"
      val {sock, passive, sin, sout} = Synchronized.value last_sock
    in 
      ((case sout of (SOME s) => TextIO.closeOut s | NONE => ());
      (case sin of (SOME s) => TextIO.closeIn s | NONE => ());
      (case sock of (SOME s) => Socket.close s | NONE => ());
      (case passive of (SOME s) =>  Socket.close s | NONE => ());
      Synchronized.change last_sock 
        (fn _ =>  {sock = NONE, passive = NONE, sin = NONE, sout = NONE}))
    end
  handle _ =>  
    (Synchronized.change last_sock 
      (fn _ =>  {sock = NONE, passive = NONE, sin = NONE, sout = NONE}))

  exception exp_text_socket of string
  fun safe_local_connect port = 
    let  
      val _ = socket_tracing "start safe connect"
      val passive_sock = init_server_socket port;
      val _ = 
        Synchronized.change 
          last_sock 
          (fn {sock, passive, sin, sout} =>  {sock = sock, passive = SOME passive_sock, sin = sin, sout = sout});
      (* a time out is working here *)
      val sock0 = 
        let 
          val timer = Timer.startRealTimer ()
          fun accept_until_timeout t = 
            if  (Timer.checkRealTimer timer|> Time.toSeconds) > t (* time out *)
            then raise exp_text_socket "time out for socket: no incoming connection"
            else 
              (case Socket.acceptNB passive_sock 
                of SOME ret => fst ret
                | _ => accept_until_timeout t)             
        in
          accept_until_timeout 5 (* timeout for 5 s *)
        end

      val _ = 
        Synchronized.change 
          last_sock 
          (fn {sock, passive, sin, sout} =>  {sock = SOME sock0, passive = passive, sin = sin, sout = sout})
      val (sin0, sout0) = make_streams sock0;
      val _ = 
        Synchronized.change 
          last_sock 
          (fn {sock, passive, sin, sout} =>  {sock = sock, passive = passive, sin = SOME sin0, sout = SOME sout0})
    in
      SockData { sock = sock0, passive = passive_sock, sin = sin0, sout = sout0}
    end
  handle exp_text_socket msg => (clear_last_sock () ; raise exp_text_socket msg) (* timeout , stop trying *)
        | _                  => (clear_last_sock () ; safe_local_connect port) (* address is in used, try again *)

  fun safe_close data = 
    let val _ = socket_tracing"cleaning by it self" in
    (close data;
     Synchronized.change 
      last_sock 
      (fn _ =>  {sock = NONE, passive = NONE, sin = NONE, sout = NONE})) end


end (* struct *)