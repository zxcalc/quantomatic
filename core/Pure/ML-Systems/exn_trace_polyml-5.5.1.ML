(*  Title:      Pure/ML-Systems/exn_trace_polyml-5.5.1.ML
    Author:     Makarius

Exception trace for Poly/ML 5.5.1 via ML output.
*)

fun print_exception_trace exn_message output e =
  PolyML.Exception.traceException
    (e, fn (trace, exn) =>
      let
        val title = "Exception trace - " ^ exn_message exn;
        val _ = output (String.concatWith "\n" (title :: trace));
      in reraise exn end);

PolyML.Compiler.reportExhaustiveHandlers := true;

