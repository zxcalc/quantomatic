(*  Title:      Pure/General/buffer.ML
    ID:         $Id: buffer.ML,v 1.13 2008/04/03 19:23:36 wenzelm Exp $
    Author:     Markus Wenzel, TU Muenchen

Efficient text buffers.
*)

signature BUFFER =
sig
  type T
  val empty: T
  val add: string -> T -> T
  val add_substring: substring -> T -> T
  val markup: Markup.T -> (T -> T) -> T -> T
  val content: T -> string
  val output: T -> TextIO.outstream -> unit
  val write: Path.T -> T -> unit
end;

structure Buffer: BUFFER =
struct

(* datatype *)

datatype T =
    EmptyBuffer
  | String of string * T
  | Substring of substring * T;

val empty = EmptyBuffer;


(* add content *)

fun add s buf = if s = "" then buf else String (s, buf);
fun add_substring s buf = if Substring.isEmpty s then buf else Substring (s, buf);

fun markup m body =
  let val (bg, en) = Markup.output m
  in add bg #> body #> add en end;


(* cumulative content *)

fun rev_content EmptyBuffer acc = acc
  | rev_content (String (s, buf)) acc = rev_content buf (s :: acc)
  | rev_content (Substring (s, buf)) acc = rev_content buf (Substring.string s :: acc);

fun content buf = implode (rev_content buf []);


(* file output *)

fun rev_buffer EmptyBuffer acc = acc
  | rev_buffer (String (s, buf)) acc = rev_buffer buf (String (s, acc))
  | rev_buffer (Substring (s, buf)) acc = rev_buffer buf (Substring (s, acc));

fun output buffer stream =
  let
    fun rev_output EmptyBuffer = ()
      | rev_output (String (s, buf)) = (TextIO.output (stream, s); rev_output buf)
      | rev_output (Substring (s, buf)) = (TextIO.outputSubstr (stream, s); rev_output buf);
  in rev_output (rev_buffer buffer empty) end;

fun write path buf = File.open_output (output buf) path;

end;
