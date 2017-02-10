(*  Title:      Pure/ML/ml_profiling.ML
    Author:     Makarius

ML profiling.
*)

signature ML_PROFILING =
sig
  val profile_time: ((int * string) list -> unit) -> ('a -> 'b) -> 'a -> 'b
  val profile_time_thread: ((int * string) list -> unit) -> ('a -> 'b) -> 'a -> 'b
  val profile_allocations: ((int * string) list -> unit) -> ('a -> 'b) -> 'a -> 'b
end;

structure ML_Profiling: ML_PROFILING =
struct

fun profile_time pr f x =
  PolyML.Profiling.profileStream pr PolyML.Profiling.ProfileTime f x;

fun profile_time_thread pr f x =
  PolyML.Profiling.profileStream pr PolyML.Profiling.ProfileTimeThisThread f x;

fun profile_allocations pr f x =
  PolyML.Profiling.profileStream pr PolyML.Profiling.ProfileAllocations f x;

end;
