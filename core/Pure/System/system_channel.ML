(*  Title:      Pure/System/system_channel.ML
    Author:     Makarius

Socket-based system channel for inter-process communication.
*)

signature SYSTEM_CHANNEL =
sig
  type T
  val input_line: T -> string option
  val inputN: T -> int -> string
  val output: T -> string -> unit
  val flush: T -> unit
  val rendezvous: string -> T
end;

structure System_Channel: SYSTEM_CHANNEL =
struct

datatype T = System_Channel of BinIO.instream * BinIO.outstream;

fun input_line (System_Channel (in_stream, _)) =
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

fun inputN (System_Channel (in_stream, _)) n =
  if n = 0 then ""  (*workaround for polyml-5.5.1 or earlier*)
  else Byte.bytesToString (BinIO.inputN (in_stream, n));

fun output (System_Channel (_, out_stream)) s =
  BinIO.output (out_stream, Byte.stringToBytes s);

fun flush (System_Channel (_, out_stream)) =
  BinIO.flushOut out_stream;

fun rendezvous name =
  let
    val (in_stream, out_stream) = Socket_IO.open_streams name;
    val _ = BinIO.StreamIO.setBufferMode (BinIO.getOutstream out_stream, IO.BLOCK_BUF);
  in System_Channel (in_stream, out_stream) end;

end;

