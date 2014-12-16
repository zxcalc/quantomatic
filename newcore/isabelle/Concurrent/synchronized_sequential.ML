(*  Title:      Pure/Concurrent/synchronized_sequential.ML
    Author:     Makarius

Sequential version of state variables -- plain refs.
*)

structure Synchronized: SYNCHRONIZED =
struct

abstype 'a var = Var of 'a Unsynchronized.ref
with

fun var _ x = Var (Unsynchronized.ref x);
fun value (Var var) = ! var;

fun timed_access (Var var) _ f =
  (case f (! var) of
    SOME (y, x') => (var := x'; SOME y)
  | NONE => Thread.unavailable ());

fun guarded_access var f = the (timed_access var (K NONE) f);

fun change_result var f = guarded_access var (SOME o f);
fun change var f = change_result var (fn x => ((), f x));

end;

fun counter () =
  let
    val counter = var "counter" (0: int);
    fun next () =
      change_result counter
        (fn i =>
          let val j = i + (1: int)
          in (j, j) end);
  in next end;

end;
