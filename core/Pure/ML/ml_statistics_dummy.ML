(*  Title:      Pure/ML/ml_statistics_dummy.ML
    Author:     Makarius

ML runtime statistics -- dummy version.
*)

signature ML_STATISTICS =
sig
  val get: unit -> Properties.T
end;

structure ML_Statistics: ML_STATISTICS =
struct

fun get () = [];

end;

