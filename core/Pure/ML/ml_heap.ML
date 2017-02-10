(*  Title:      Pure/ML/ml_heap.ML
    Author:     Makarius

ML heap operations.
*)

signature ML_HEAP =
sig
  val obj_size: 'a -> int
  val share_common_data: unit -> unit
  val save_child: string -> unit
end;

structure ML_Heap: ML_HEAP =
struct

val obj_size = PolyML.objSize;

fun share_common_data () = PolyML.shareCommonData PolyML.rootFunction;

fun save_child name =
  PolyML.SaveState.saveChild (name, List.length (PolyML.SaveState.showHierarchy ()));

end;
