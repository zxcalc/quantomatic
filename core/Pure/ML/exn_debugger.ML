(*  Title:      Pure/ML/exn_debugger.ML
    Author:     Makarius

Detailed exception trace via ML debugger.
*)

signature EXN_DEBUGGER =
sig
  val capture_exception_trace:
    (unit -> 'a) -> (string * ML_Compiler0.polyml_location) list * 'a Exn.result
end;

structure Exn_Debugger: EXN_DEBUGGER =
struct

(* thread data *)

val trace_var =
  Thread_Data.var () : ((string * ML_Compiler0.polyml_location) * exn) list Thread_Data.var;

fun start_trace () = Thread_Data.put trace_var (SOME []);

fun update_trace entry exn =
  (case Thread_Data.get trace_var of
    SOME trace => Thread_Data.put trace_var (SOME ((entry, exn) :: trace))
  | NONE => ());

fun stop_trace () =
  let
    val trace = the_default [] (Thread_Data.get trace_var);
    val _ = Thread_Data.put trace_var NONE;
  in trace end;

val _ = PolyML.DebuggerInterface.setOnExitException (SOME update_trace);


(* capture exception trace *)

local
  fun eq_exn exns =
    op = (apply2 General.exnName exns) andalso op = (apply2 Exn_Properties.position exns);
in

fun capture_exception_trace e =
  Thread_Attributes.uninterruptible (fn restore_attributes => fn () =>
    let
      val _ = start_trace ();
      val result = Exn.interruptible_capture (restore_attributes e) ();
      val trace = stop_trace ();
      val trace' =
        (case result of
          Exn.Res _ => []
        | Exn.Exn exn =>
            trace
            |> map_filter (fn (entry, e) => if eq_exn (exn, e) then SOME entry else NONE)
            |> rev);
    in (trace', result) end) ();

end;

end;
