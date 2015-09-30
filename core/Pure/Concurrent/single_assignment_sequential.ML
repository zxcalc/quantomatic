(*  Title:      Pure/Concurrent/single_assignment_sequential.ML
    Author:     Makarius

Single-assignment variables (sequential version).
*)

structure Single_Assignment: SINGLE_ASSIGNMENT =
struct

abstype 'a var = Var of 'a SingleAssignment.saref
with

fun var _ = Var (SingleAssignment.saref ());

fun peek (Var var) = SingleAssignment.savalue var;

fun await v =
  (case peek v of
    SOME x => x
  | NONE => Thread.unavailable ());

fun assign (v as Var var) x =
  (case peek v of
    SOME _ => raise Fail "Duplicate assignment to variable"
  | NONE => SingleAssignment.saset (var, x));

end;

end;

