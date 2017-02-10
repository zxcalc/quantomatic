(*  Title:      Pure/General/stack.ML
    Author:     Makarius

Non-empty stacks.
*)

signature STACK =
sig
  type 'a T
  val make: 'a -> 'a list -> 'a T
  val dest: 'a T -> 'a * 'a list
  val level: 'a T -> int
  val init: 'a -> 'a T
  val top: 'a T -> 'a
  val map_top: ('a -> 'a) -> 'a T -> 'a T
  val map_all: ('a -> 'a) -> 'a T -> 'a T
  val push: 'a T -> 'a T
  val pop: 'a T -> 'a T      (*exception List.Empty*)
end;

structure Stack: STACK =
struct

abstype 'a T = Stack of 'a * 'a list
with

fun make x xs = Stack (x, xs);
fun dest (Stack (x, xs)) = (x, xs);

fun level (Stack (_, xs)) = length xs;

fun init x = Stack (x, []);

fun top (Stack (x, _)) = x;

fun map_top f (Stack (x, xs)) = Stack (f x, xs);

fun map_all f (Stack (x, xs)) = Stack (f x, map f xs);

fun push (Stack (x, xs)) = Stack (x, x :: xs);

fun pop (Stack (_, x :: xs)) = Stack (x, xs)
  | pop (Stack (_, [])) = raise List.Empty;

end;

end;
