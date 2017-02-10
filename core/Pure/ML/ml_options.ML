(*  Title:      Pure/ML/ml_options.ML
    Author:     Makarius

ML configuration options.
*)

signature ML_OPTIONS =
sig
  val source_trace_raw: Config.raw
  val source_trace: bool Config.T
  val exception_trace_raw: Config.raw
  val exception_trace: bool Config.T
  val exception_trace_enabled: Context.generic option -> bool
  val exception_debugger_raw: Config.raw
  val exception_debugger: bool Config.T
  val exception_debugger_enabled: Context.generic option -> bool
  val debugger_raw: Config.raw
  val debugger: bool Config.T
  val debugger_enabled: Context.generic option -> bool
end;

structure ML_Options: ML_OPTIONS =
struct

(* source trace *)

val source_trace_raw =
  Config.declare ("ML_source_trace", @{here}) (fn _ => Config.Bool false);
val source_trace = Config.bool source_trace_raw;


(* exception trace *)

val exception_trace_raw = Config.declare_option ("ML_exception_trace", @{here});
val exception_trace = Config.bool exception_trace_raw;

fun exception_trace_enabled NONE =
      (Options.default_bool (Config.name_of exception_trace_raw) handle ERROR _ => false)
  | exception_trace_enabled (SOME context) = Config.get_generic context exception_trace;


(* exception debugger *)

val exception_debugger_raw = Config.declare_option ("ML_exception_debugger", @{here});
val exception_debugger = Config.bool exception_debugger_raw;

fun exception_debugger_enabled NONE =
      (Options.default_bool (Config.name_of exception_debugger_raw) handle ERROR _ => false)
  | exception_debugger_enabled (SOME context) = Config.get_generic context exception_debugger;


(* debugger *)

val debugger_raw = Config.declare_option ("ML_debugger", @{here});
val debugger = Config.bool debugger_raw;

fun debugger_enabled NONE =
      (Options.default_bool (Config.name_of debugger_raw) handle ERROR _ => false)
  | debugger_enabled (SOME context) = Config.get_generic context debugger;

end;
