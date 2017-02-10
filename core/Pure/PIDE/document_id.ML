(*  Title:      Pure/PIDE/document_id.ML
    Author:     Makarius

Unique identifiers for document structure.

NB: ML ticks forwards > 0, JVM ticks backwards < 0.
*)

signature DOCUMENT_ID =
sig
  type generic = int
  type version = generic
  type command = generic
  type exec = generic
  type execution = generic
  val none: generic
  val make: unit -> generic
  val parse: string -> generic
  val print: generic -> string
end;

structure Document_ID: DOCUMENT_ID =
struct

type generic = int;
type version = generic;
type command = generic;
type exec = generic;
type execution = generic;

val none = 0;
val make = Counter.make ();

val parse = Value.parse_int;
val print = Value.print_int;

end;

