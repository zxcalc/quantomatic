(*  Title:      Pure/ML-Systems/thread_dummy.ML
    Author:     Makarius

Default (mostly dummy) implementation of thread structures
(cf. polyml/basis/Thread.sml).
*)

exception Thread of string;

local fun fail _ = raise Thread "No multithreading support on this ML platform" in

structure Mutex =
struct
  type mutex = unit;
  fun mutex _ = ();
  fun lock _ = fail ();
  fun unlock _ = fail ();
  fun trylock _ = fail ();
end;

structure ConditionVar =
struct
  type conditionVar = unit;
  fun conditionVar _ = ();
  fun wait _ = fail ();
  fun waitUntil _ = fail ();
  fun signal _ = fail ();
  fun broadcast _ = fail ();
end;

structure Thread =
struct
  type thread = unit;

  datatype threadAttribute = EnableBroadcastInterrupt of bool | InterruptState of interruptState
    and interruptState = InterruptDefer | InterruptSynch | InterruptAsynch | InterruptAsynchOnce;

  fun unavailable () = fail ();

  fun fork _ = fail ();
  fun exit _ = fail ();
  fun isActive _ = fail ();

  fun equal _ = fail ();
  fun self _ = fail ();

  fun interrupt _ = fail ();
  fun broadcastInterrupt _ = fail ();
  fun testInterrupt _ = fail ();

  fun kill _ = fail ();

  fun setAttributes _ = fail ();
  fun getAttributes _ = fail ();

  fun numProcessors _ = fail ();


(* thread data *)

local

val data = ref ([]: Universal.universal  ref list);

fun find_data tag =
  let
    fun find (r :: rs) = if Universal.tagIs tag (! r) then SOME r else find rs
      | find [] = NONE;
  in find (! data) end;

in

fun getLocal tag = Option.map (Universal.tagProject tag o !) (find_data tag);

fun setLocal (tag, x) =
  (case find_data tag of
    SOME r => r := Universal.tagInject tag x
  | NONE => data :=  ref (Universal.tagInject tag x) :: ! data);

end;
end;
end;
