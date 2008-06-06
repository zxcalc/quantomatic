(*  Title:      Pure/ML-Systems/exn.ML
    ID:         $Id: exn.ML,v 1.1 2007/07/24 17:44:33 wenzelm Exp $
    Author:     Makarius

Runtime exceptions as values.
*)

structure Exn =
struct

datatype 'a result =
  Result of 'a |
  Exn of exn;

fun get_result (Result x) = SOME x
  | get_result _ = NONE;

fun get_exn (Exn exn) = SOME exn
  | get_exn _ = NONE;

fun capture f x = Result (f x) handle e => Exn e;

fun release (Result y) = y
  | release (Exn e) = raise e;

exception EXCEPTIONS of exn list * string;

end;
