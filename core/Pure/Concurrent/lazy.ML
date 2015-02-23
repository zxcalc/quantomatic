(*  Title:      Pure/Concurrent/lazy.ML
    Author:     Makarius

Lazy evaluation with memoing of results and regular exceptions.
Parallel version based on (passive) futures, to avoid critical or
multiple evaluation (unless interrupted).
*)

signature LAZY =
sig
  type 'a lazy
  val peek: 'a lazy -> 'a Exn.result option
  val is_finished: 'a lazy -> bool
  val lazy: (unit -> 'a) -> 'a lazy
  val value: 'a -> 'a lazy
  val force_result: 'a lazy -> 'a Exn.result
  val force: 'a lazy -> 'a
  val map: ('a -> 'b) -> 'a lazy -> 'b lazy
  val future: Future.params -> 'a lazy -> 'a future
end;

structure Lazy: LAZY =
struct

(* datatype *)

datatype 'a expr =
  Expr of unit -> 'a |
  Result of 'a future;

abstype 'a lazy = Lazy of 'a expr Synchronized.var
with

fun peek (Lazy var) =
  (case Synchronized.value var of
    Expr _ => NONE
  | Result res => Future.peek res);

fun lazy e = Lazy (Synchronized.var "lazy" (Expr e));
fun value a = Lazy (Synchronized.var "lazy" (Result (Future.value a)));

fun is_finished x = is_some (peek x);


(* force result *)

fun force_result (Lazy var) =
  (case peek (Lazy var) of
    SOME res => res
  | NONE =>
      uninterruptible (fn restore_attributes => fn () =>
        let
          val (expr, x) =
            Synchronized.change_result var
              (fn Expr e =>
                    let val x = Future.promise I
                    in ((SOME e, x), Result x) end
                | Result x => ((NONE, x), Result x));
        in
          (case expr of
            SOME e =>
              let
                val res0 = Exn.capture (restore_attributes e) ();
                val _ = Exn.capture (fn () => Future.fulfill_result x res0) ();
                val res = Future.join_result x;
                (*semantic race: some other threads might see the same
                  interrupt, until there is a fresh start*)
                val _ =
                  if Exn.is_interrupt_exn res then
                    Synchronized.change var (fn _ => Expr e)
                  else ();
              in res end
          | NONE => Exn.capture (restore_attributes (fn () => Future.join x)) ())
        end) ());


end;

fun force r = Exn.release (force_result r);
fun map f x = lazy (fn () => f (force x));


(* future evaluation *)

fun future params x =
  if is_finished x then Future.value_result (force_result x)
  else (singleton o Future.forks) params (fn () => force x);

end;

type 'a lazy = 'a Lazy.lazy;

