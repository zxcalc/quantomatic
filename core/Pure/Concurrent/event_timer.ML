(*  Title:      Pure/Concurrent/event_timer.ML
    Author:     Makarius

Initiate event after given point in time.

Note: events are run as synchronized action within a dedicated thread
and should finish quickly without further ado.
*)

signature EVENT_TIMER =
sig
  eqtype request
  val request: Time.time -> (unit -> unit) -> request
  val cancel: request -> bool
  val shutdown: unit -> unit
  val future: Time.time -> unit future
end;

structure Event_Timer: EVENT_TIMER =
struct

(* type request *)

val request_counter = Counter.make ();
datatype request = Request of int;
fun new_request () = Request (request_counter ());


(* type requests *)

structure Requests = Table(type key = Time.time val ord = Time.compare);
type requests = (request * (unit -> unit)) list Requests.table;

fun add_request time entry (requests: requests) =
  Requests.cons_list (time, entry) requests;

fun del_request req (requests: requests) =
  let
    val old_request =
      requests |> Requests.get_first (fn (key, entries) =>
        entries |> get_first (fn entry => if fst entry = req then SOME (key, entry) else NONE));
  in
    (case old_request of
      NONE => (false, requests)
    | SOME old => (true, Requests.remove_list (eq_fst op =) old requests))
  end;

fun next_request_time (requests: requests) =
  Option.map fst (Requests.min requests);

fun next_request_event t0 (requests: requests) =
  (case Requests.min requests of
    NONE => NONE
  | SOME (time, entries) =>
      if Time.< (t0, time) then NONE
      else
        let
          val (rest, (_, event)) = split_last entries;
          val requests' =
            if null rest then Requests.delete time requests
            else Requests.update (time, rest) requests;
        in SOME (event, requests') end);


(* global state *)

type state = requests * Thread.thread option;
val init_state: state = (Requests.empty, NONE);

val state = Synchronized.var "Event_Timer.state" init_state;


(* manager thread *)

val manager_timeout = seconds 0.3;

fun manager_loop () =
  let
    val success =
      Synchronized.timed_access state
        (fn (requests, _) =>
          (case next_request_time requests of
            NONE => SOME (Time.+ (Time.now (), manager_timeout))
          | some => some))
        (fn (requests, manager) =>
          (case next_request_event (Time.now ()) requests of
            NONE => NONE
          | SOME (event, requests') => (Exn.capture event (); SOME ((), (requests', manager)))));
    val finished =
      is_none success andalso
        Synchronized.change_result state (fn (requests, manager) =>
          if Requests.is_empty requests then (true, init_state)
          else (false, (requests, manager)));
  in if finished then () else manager_loop () end;

fun manager_check manager =
  if is_some manager andalso Thread.isActive (the manager) then manager
  else SOME (Simple_Thread.fork false manager_loop);


(* main operations *)

fun request time event =
  Synchronized.change_result state (fn (requests, manager) =>
    let
      val req = new_request ();
      val requests' = add_request time (req, event) requests;
    in (req, (requests', manager_check manager)) end);

fun cancel req =
  Synchronized.change_result state (fn (requests, manager) =>
    let
      val (canceled, requests') = del_request req requests;
    in (canceled, (requests', manager)) end);

fun shutdown () =
  Synchronized.guarded_access state (fn (requests, manager) =>
    if not (Requests.is_empty requests)
    then raise Fail "Cannot shutdown event timer: pending requests"
    else if is_none manager then SOME ((), init_state)
    else NONE);


(* future *)

val future = uninterruptible (fn _ => fn time =>
  let
    val req: request Single_Assignment.var = Single_Assignment.var "request";
    fun abort () = ignore (cancel (Single_Assignment.await req));
    val promise: unit future = Future.promise abort;
    val _ = Single_Assignment.assign req (request time (Future.fulfill promise));
  in promise end);

end;

