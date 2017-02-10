(*  Title:      Pure/ML/ml_pervasive.ML
    Author:     Makarius

Initial pervasive ML environment.
*)

structure PolyML_Pretty =
struct
  datatype context = datatype PolyML.context;
  datatype pretty = datatype PolyML.pretty;
end;

val seconds = Time.fromReal;

val _ =
  List.app ML_Name_Space.forget_val
    ["isSome", "getOpt", "valOf", "foldl", "foldr", "print", "explode", "concat"];

val ord = SML90.ord;
val chr = SML90.chr;
val raw_explode = SML90.explode;
val implode = String.concat;

val pointer_eq = PolyML.pointerEq;

val error_depth = PolyML.error_depth;

open Thread;

datatype illegal = Interrupt;

structure Basic_Exn: BASIC_EXN = Exn;
open Basic_Exn;
