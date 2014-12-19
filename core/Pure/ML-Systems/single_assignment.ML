(*  Title:      Pure/ML-Systems/single_assignment.ML
    Author:     Makarius

References with single assignment.  Unsynchronized!
*)

signature SINGLE_ASSIGNMENT =
sig
  type 'a saref
  exception Locked
  val saref: unit -> 'a saref
  val savalue: 'a saref -> 'a option
  val saset: 'a saref * 'a -> unit
end;

structure SingleAssignment: SINGLE_ASSIGNMENT =
struct

exception Locked;

abstype 'a saref = SARef of 'a option ref
with

fun saref () = SARef (ref NONE);

fun savalue (SARef r) = ! r;

fun saset (SARef (r as ref NONE), x) = r := SOME x
  | saset _ = raise Locked;

end;

end;
