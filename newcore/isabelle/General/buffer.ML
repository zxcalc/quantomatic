(*  Title:      Pure/General/buffer.ML
    Author:     Markus Wenzel, TU Muenchen

Efficient text buffers.
*)

signature BUFFER =
sig
  type T
  val empty: T
  val add: string -> T -> T
  val markup: Markup.T -> (T -> T) -> T -> T
  val content: T -> string
  val output: T -> TextIO.outstream -> unit
end;

structure Buffer: BUFFER =
struct

datatype T = Buffer of string list;

val empty = Buffer [];

fun add "" buf = buf
  | add x (Buffer xs) = Buffer (x :: xs);

fun markup m body =
  let val (bg, en) = Markup.output m
  in add bg #> body #> add en end;

fun content (Buffer xs) = implode (rev xs);
fun output (Buffer xs) stream = List.app (fn s => TextIO.output (stream, s)) (rev xs);

end;
