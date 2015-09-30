(*  Title:      Pure/Concurrent/lazy_sequential.ML
    Author:     Florian Haftmann and Makarius, TU Muenchen

Lazy evaluation with memoing of results and regular exceptions
(sequential version).
*)

structure Lazy: LAZY =
struct

(* datatype *)

datatype 'a expr =
  Expr of unit -> 'a |
  Result of 'a Exn.result;

abstype 'a lazy = Lazy of 'a expr Unsynchronized.ref
with

fun lazy e = Lazy (Unsynchronized.ref (Expr e));
fun value a = Lazy (Unsynchronized.ref (Result (Exn.Res a)));

fun peek (Lazy r) =
  (case ! r of
    Expr _ => NONE
  | Result res => SOME res);

fun is_running _ = false;
fun is_finished x = is_some (peek x);
val finished_result = peek;


(* force result *)

fun force_result (Lazy r) =
  let
    val result =
      (case ! r of
        Expr e => Exn.capture e ()
      | Result res => res);
    val _ = if Exn.is_interrupt_exn result then () else r := Result result;
  in result end;

fun force r = Exn.release (force_result r);
fun map f x = lazy (fn () => f (force x));


(* future evaluation *)

fun future params x =
  if is_finished x then Future.value_result (force_result x)
  else (singleton o Future.forks) params (fn () => force x);

end;
end;

type 'a lazy = 'a Lazy.lazy;

