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
  val print_depth_raw: Config.raw
  val print_depth: int Config.T
  val get_print_depth: unit -> int
  val get_print_depth_default: int -> int
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


(* print depth *)

val print_depth_raw =
  Config.declare ("ML_print_depth", @{here}) (fn _ => Config.Int (get_default_print_depth ()));
val print_depth = Config.int print_depth_raw;

fun get_print_depth () =
  (case Context.thread_data () of
    NONE => get_default_print_depth ()
  | SOME context => Config.get_generic context print_depth);

fun get_print_depth_default default =
  (case Context.thread_data () of
    NONE => default
  | SOME context => Config.get_generic context print_depth);

end;
