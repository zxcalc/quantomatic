(*  Title:      Pure/ML/ml_print_depth0.ML
    Author:     Makarius

Print depth for ML toplevel pp -- global default (unsynchronized).
*)

signature ML_PRINT_DEPTH =
sig
  val set_print_depth: int -> unit
  val get_print_depth: unit -> int
end;

structure ML_Print_Depth: ML_PRINT_DEPTH =
struct

val depth = Unsynchronized.ref 0;

fun set_print_depth n = (depth := n; PolyML.print_depth n);
fun get_print_depth () = ! depth;

end;
