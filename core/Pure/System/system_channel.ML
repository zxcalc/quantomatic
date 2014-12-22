(*  Title:      Pure/System/system_channel.ML
    Author:     Makarius

Portable system channel for inter-process communication, based on
named pipes or sockets.
*)

signature SYSTEM_CHANNEL =
sig
  type T
  val input_line: T -> string option
  val inputN: T -> int -> string
  val output: T -> string -> unit
  val flush: T -> unit
  val fifo_rendezvous: string -> string -> T
  val socket_rendezvous: string -> T
end;

structure System_Channel: SYSTEM_CHANNEL =
struct

datatype T = System_Channel of
 {input_line: unit -> string option,
  inputN: int -> string,
  output: string -> unit,
  flush: unit -> unit};

fun input_line (System_Channel {input_line = f, ...}) = f ();
fun inputN (System_Channel {inputN = f, ...}) n = f n;
fun output (System_Channel {output = f, ...}) s = f s;
fun flush (System_Channel {flush = f, ...}) = f ();


(* named pipes *)

fun fifo_rendezvous fifo1 fifo2 =
  let
    val in_stream = TextIO.openIn fifo1;
    val out_stream = TextIO.openOut fifo2;
    val _ = TextIO.StreamIO.setBufferMode (TextIO.getOutstream out_stream, IO.BLOCK_BUF);
  in
    System_Channel
     {input_line = fn () => TextIO.inputLine in_stream,
      inputN = fn n => TextIO.inputN (in_stream, n),
      output = fn s => TextIO.output (out_stream, s),
      flush = fn () => TextIO.flushOut out_stream}
  end;


(* sockets *)

fun read_line in_stream =
  let
    fun result cs = String.implode (rev (#"\n" :: cs));
    fun read cs =
      (case BinIO.input1 in_stream of
        NONE => if null cs then NONE else SOME (result cs)
      | SOME b =>
          (case Byte.byteToChar b of
            #"\n" => SOME (result cs)
          | c => read (c :: cs)));
  in read [] end;

fun socket_rendezvous name =
  let
    val (in_stream, out_stream) = Socket_IO.open_streams name;
    val _ = BinIO.StreamIO.setBufferMode (BinIO.getOutstream out_stream, IO.BLOCK_BUF);
  in
    System_Channel
     {input_line = fn () => read_line in_stream,
      inputN = fn n =>
        if n = 0 then ""  (*workaround for polyml-5.5.1 or earlier*)
        else Byte.bytesToString (BinIO.inputN (in_stream, n)),
      output = fn s => BinIO.output (out_stream, Byte.stringToBytes s),
      flush = fn () => BinIO.flushOut out_stream}
  end;

end;

