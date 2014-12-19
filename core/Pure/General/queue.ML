(*  Title:      Pure/General/queue.ML
    Author:     Makarius

Efficient queues.
*)

signature QUEUE =
sig
  type 'a T
  val empty: 'a T
  val is_empty: 'a T -> bool
  val content: 'a T -> 'a list
  val enqueue: 'a -> 'a T -> 'a T
  val dequeue: 'a T -> 'a * 'a T  (*exception List.Empty*)
end;

structure Queue: QUEUE =
struct

datatype 'a T = Queue of 'a list * 'a list;

val empty = Queue ([], []);

fun is_empty (Queue ([], [])) = true
  | is_empty _ = false;

fun content (Queue (xs, ys)) = ys @ rev xs;

fun enqueue x (Queue (xs, ys)) = Queue (x :: xs, ys);

fun dequeue (Queue (xs, y :: ys)) = (y, Queue (xs, ys))
  | dequeue (Queue (xs as _ :: _, [])) = let val y :: ys = rev xs in (y, Queue ([], ys)) end
  | dequeue (Queue ([], [])) = raise List.Empty;

end;
