(*  Title:      Pure/ML-Systems/time_limit.ML
    ID:         $Id: time_limit.ML,v 1.1 2007/09/24 11:54:57 wenzelm Exp $
    Author:     Makarius

Dummy implementation of NJ's TimeLimit structure.
*)

signature TIME_LIMIT =
sig
  exception TimeOut
  val timeLimit : Time.time -> ('a -> 'b) -> 'a -> 'b
end;

structure TimeLimit: TIME_LIMIT =
struct

exception TimeOut;
fun timeLimit _ f x = f x;

end;

