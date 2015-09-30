(*  Title:      Pure/Concurrent/random.ML
    Author:     Lawrence C Paulson, Cambridge University Computer Laboratory

Pseudo random numbers.
*)

signature RANDOM =
sig
  val random: unit -> real
  exception RANDOM
  val random_range: int -> int -> int
end;

structure Random: RANDOM =
struct

fun rmod x y = x - y * Real.realFloor (x / y);

local
  val a = 16807.0;
  val m = 2147483647.0;
  val random_seed = Synchronized.var "random_seed" 1.0;
in

fun random () =
  Synchronized.change_result random_seed
    (fn r => let val r' = rmod (a * r) m in (r', r') end);

end;

exception RANDOM;

fun random_range l h =
  if h < l orelse l < 0 then raise RANDOM
  else l + Real.floor (rmod (random ()) (real (h - l + 1)));

end;

