(*  Title:      Pure/ML-Systems/ml_system.ML
    Author:     Makarius

ML system and platform operations.
*)

signature ML_SYSTEM =
sig
  val name: string
  val is_polyml: bool
  val is_smlnj: bool
  val platform: string
  val platform_is_cygwin: bool
  val share_common_data: unit -> unit
  val save_state: string -> unit
end;

structure ML_System: ML_SYSTEM =
struct

val SOME name = OS.Process.getEnv "ML_SYSTEM";
val is_polyml = String.isPrefix "polyml" name;
val is_smlnj = String.isPrefix "smlnj" name;

val SOME platform = OS.Process.getEnv "ML_PLATFORM";
val platform_is_cygwin = String.isSuffix "cygwin" platform;

fun share_common_data () = ();
fun save_state _ = raise Fail "Cannot save state -- undefined operation";

end;

