(*  Title:      Pure/Concurrent/par_list.ML
    Author:     Makarius

Parallel list combinators.

Notes:

  * These combinators only make sense if the operator (function or
    predicate) applied to the list of operands takes considerable
    time.  The overhead of scheduling is significantly higher than
    just traversing the list of operands sequentially.

  * The order of operator application is non-deterministic.  Watch out
    for operators that have side-effects or raise exceptions!
*)

signature PAR_LIST =
sig
  val managed_results: string -> ('a -> 'b) -> 'a list -> 'b Exn.result list
  val map_name: string -> ('a -> 'b) -> 'a list -> 'b list
  val map: ('a -> 'b) -> 'a list -> 'b list
  val get_some: ('a -> 'b option) -> 'a list -> 'b option
  val find_some: ('a -> bool) -> 'a list -> 'a option
  val exists: ('a -> bool) -> 'a list -> bool
  val forall: ('a -> bool) -> 'a list -> bool
end;

structure Par_List: PAR_LIST =
struct

fun managed_results name f xs =
  if null xs orelse null (tl xs) orelse
      not (Multithreading.enabled ()) orelse Multithreading.self_critical ()
  then map (Exn.capture f) xs
  else
    uninterruptible (fn restore_attributes => fn () =>
      let
        val group = Future.new_group (Future.worker_group ());
        val futures =
          Future.forks {name = name, group = SOME group, deps = [], pri = 0, interrupts = true}
            (map (fn x => fn () => f x) xs);
        val results =
          restore_attributes Future.join_results futures
            handle exn =>
              (if Exn.is_interrupt exn then Future.cancel_group group else (); reraise exn);
      in results end) ();

fun map_name name f xs = Par_Exn.release_first (managed_results name f xs);
fun map f = map_name "Par_List.map" f;

fun get_some f xs =
  let
    exception FOUND of 'b option;
    fun found (Exn.Exn (FOUND some)) = some
      | found _ = NONE;
    val results =
      managed_results "Par_List.get_some"
        (fn x => (case f x of NONE => () | some => raise FOUND some)) xs;
  in
    (case get_first found results of
      SOME y => SOME y
    | NONE => (Par_Exn.release_first results; NONE))
  end;

fun find_some P = get_some (fn x => if P x then SOME x else NONE);

fun exists P = is_some o get_some (fn x => if P x then SOME () else NONE);
fun forall P = not o exists (not o P);

end;
