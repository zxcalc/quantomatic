(*  Title:      Pure/ML/ml_system.ML
    Author:     Makarius

ML system and platform operations.
*)

signature ML_SYSTEM =
sig
  val name: string
  val platform: string
  val platform_is_windows: bool
  val platform_path: string -> string
  val standard_path: string -> string
end;

structure ML_System: ML_SYSTEM =
struct

val SOME name = OS.Process.getEnv "ML_SYSTEM";
val SOME platform = OS.Process.getEnv "ML_PLATFORM";
val platform_is_windows = String.isSuffix "windows" platform;

val platform_path =
  if platform_is_windows then
    fn path =>
      if String.isPrefix "/" path andalso not (String.isPrefix "//" path) then
        (case String.tokens (fn c => c = #"/") path of
          "cygdrive" :: drive :: arcs =>
            let
              val vol =
                (case Char.fromString drive of
                  NONE => drive ^ ":"
                | SOME d => String.str (Char.toUpper d) ^ ":");
            in String.concatWith "\\" (vol :: arcs) end
        | arcs =>
            (case OS.Process.getEnv "CYGWIN_ROOT" of
              SOME root => OS.Path.concat (root, String.concatWith "\\" arcs)
            | NONE => raise Fail "Unknown environment variable CYGWIN_ROOT"))
      else String.translate (fn #"/" => "\\" | c => String.str c) path
  else fn path => path;

val standard_path =
  if platform_is_windows then
    fn path =>
      let
        val {vol, arcs, isAbs} = OS.Path.fromString path;
        val path' = String.translate (fn #"\\" => "/" | c => String.str c) path;
      in
        if isAbs then
          (case String.explode vol of
            [d, #":"] =>
              String.concatWith "/" ("/cygdrive" :: String.str (Char.toLower d) :: arcs)
          | _ => path')
        else path'
      end
  else fn path => path;

end;
