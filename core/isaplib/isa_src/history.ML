(*  Title:      Pure/General/history.ML
    ID:         $Id: history.ML,v 1.10 2007/06/12 22:02:02 wenzelm Exp $
    Author:     Markus Wenzel, TU Muenchen

Histories of values, with undo and redo, and optional limit.
*)

signature HISTORY =
sig
  type 'a T
  val init: int option -> 'a -> 'a T
  val is_initial: 'a T -> bool
  val current: 'a T -> 'a
  val previous: 'a T -> 'a option
  val clear: int -> 'a T -> 'a T
  val undo: 'a T -> 'a T
  val redo: 'a T -> 'a T
  val apply': 'a -> ('a -> 'a) -> 'a T -> 'a T
  val apply: ('a -> 'a) -> 'a T -> 'a T
  val map_current: ('a -> 'a) -> 'a T -> 'a T
end;

structure History: HISTORY =
struct

datatype 'a T =
  History of 'a * (int option * int * 'a list * 'a list);

fun init lim x = History (x, (lim, 0, [], []));

fun is_initial (History (_, (_, len, _, _))) = len = 0;

fun current (History (x, _)) = x;

fun previous (History (_, (_, _, x :: _, _))) = SOME x
  | previous _ = NONE;

fun clear n (History (x, (lim, len, undo_list, redo_list))) =
  History (x, (lim, Int.max (0, len - n), Library.drop (n, undo_list), redo_list));

fun undo (History (_, (_, _, [], _))) = error "No further undo information"
  | undo (History (x, (lim, len, u :: undo_list, redo_list))) =
      History (u, (lim, len - 1, undo_list, x :: redo_list));

fun redo (History (_, (_, _, _, []))) = error "No further redo information"
  | redo (History (x, (lim, len, undo_list, r :: redo_list))) =
      History (r, (lim, len + 1, x :: undo_list, redo_list));

fun push NONE _ x xs = x :: xs
  | push (SOME 0) _ _ _ = []
  | push (SOME n) len x xs = if len < n then x :: xs else Library.take (n, x :: xs);

fun apply' x' f (History (x, (lim, len, undo_list, _))) =
  History (f x, (lim, len + 1, push lim len x' undo_list, []));

fun apply f hist = apply' (current hist) f hist;

fun map_current f (History (x, hist)) = History (f x, hist);

end;
