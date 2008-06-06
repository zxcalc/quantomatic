(*  Title:      Pure/ML-Systems/multithreading.ML
    ID:         $Id: multithreading.ML,v 1.6 2008/02/16 15:44:00 wenzelm Exp $
    Author:     Makarius

Dummy implementation of multithreading interface.
*)

signature BASIC_MULTITHREADING =
sig
  val NAMED_CRITICAL: string -> (unit -> 'a) -> 'a
  val CRITICAL: (unit -> 'a) -> 'a
end;

signature MULTITHREADING =
sig
  include BASIC_MULTITHREADING
  val trace: int ref
  val tracing: int -> (unit -> string) -> unit
  val available: bool
  val max_threads: int ref
  val max_threads_value: unit -> int
  val self_critical: unit -> bool
  datatype 'a task =
    Task of {body: unit -> unit, cont: 'a -> 'a, fail: 'a -> 'a} | Wait | Terminate;
  val schedule: int -> ('a -> 'a task * 'a) -> 'a -> exn list
  val serial: unit -> int
  val get_data: 'a Universal.tag -> 'a option
  val put_data: 'a Universal.tag * 'a -> unit
end;

structure Multithreading: MULTITHREADING =
struct

(* options *)

val trace = ref (0: int);
fun tracing _ _ = ();

val available = false;
val max_threads = ref (1: int);
fun max_threads_value () = Int.max (! max_threads, 1);


(* critical section *)

fun self_critical () = false;
fun NAMED_CRITICAL _ e = e ();
fun CRITICAL e = e ();


(* scheduling *)

datatype 'a task =
  Task of {body: unit -> unit, cont: 'a -> 'a, fail: 'a -> 'a} | Wait | Terminate;

fun schedule _ _ _ =
  raise Fail "No multithreading support -- cannot schedule tasks";


(* serial numbers *)

local val count = ref (0: int)
in fun serial () = (count := ! count + 1; ! count) end;


(* thread data *)

local

val data = ref ([]: Universal.universal ref list);

fun find_data tag =
  let
    fun find (r :: rs) = if Universal.tagIs tag (! r) then SOME r else find rs
      | find [] = NONE;
  in find (! data) end;

in

fun get_data tag = Option.map (Universal.tagProject tag o !) (find_data tag);

fun put_data (tag, x) =
  (case find_data tag of
    SOME r => r := Universal.tagInject tag x
  | NONE => data := ref (Universal.tagInject tag x) :: ! data);

end;

end;

structure BasicMultithreading: BASIC_MULTITHREADING = Multithreading;
open BasicMultithreading;

