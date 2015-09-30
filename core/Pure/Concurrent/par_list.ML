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
  val map_independent: ('a -> 'b) -> 'a list -> 'b list
  val map: ('a -> 'b) -> 'a list -> 'b list
  val get_some: ('a -> 'b option) -> 'a list -> 'b option
  val find_some: ('a -> bool) -> 'a list -> 'a option
  val exists: ('a -> bool) -> 'a list -> bool
  val forall: ('a -> bool) -> 'a list -> bool
end;

structure Par_List: PAR_LIST =
struct

fun managed_results name f xs =
  if null xs orelse null (tl xs) orelse not (Multithreading.enabled ())
  then map (Exn.capture f) xs
  else
    uninterruptible (fn restore_attributes => fn () =>
      let
        val (group, pri) =
          (case Future.worker_task () of
            SOME task =>
              (Future.new_group (SOME (Task_Queue.group_of_task task)), Task_Queue.pri_of_task task)
          | NONE => (Future.new_group NONE, 0));
        val futures =
          Future.forks {name = name, group = SOME group, deps = [], pri = pri, interrupts = true}
            (map (fn x => fn () => f x) xs);
        val results =
          restore_attributes Future.join_results futures
            handle exn =>
              (if Exn.is_interrupt exn then Future.cancel_group group else (); reraise exn);
      in results end) ();

fun map_name name f xs = Par_Exn.release_first (managed_results name f xs);
fun map f = map_name "Par_List.map" f;
fun map_independent f = map (Exn.interruptible_capture f) #> Par_Exn.release_all;

fun get_some f xs =
  let
    exception FOUND of 'b;
    val results =
      managed_results "Par_List.get_some"
        (fn x => (case f x of NONE => () | SOME y => raise FOUND y)) xs;
  in
    (case get_first (fn Exn.Exn (FOUND res) => SOME res | _ => NONE) results of
      NONE => (Par_Exn.release_first results; NONE)
    | some => some)
  end;

fun find_some P = get_some (fn x => if P x then SOME x else NONE);

fun exists P = is_some o find_some P;
fun forall P = not o exists (not o P);

end;
