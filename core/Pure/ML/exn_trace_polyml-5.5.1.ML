(*  Title:      Pure/ML/exn_trace_polyml-5.5.1.ML
    Author:     Makarius

Exception trace for Poly/ML 5.5.1, using regular Isabelle output.
*)

fun print_exception_trace exn_message e =
  PolyML.Exception.traceException
    (e, fn (trace, exn) =>
      let
        val title = "Exception trace - " ^ exn_message exn;
        val _ = tracing (cat_lines (title :: trace));
      in reraise exn end);

PolyML.Compiler.reportExhaustiveHandlers := true;

