(*  Title:      Pure/ML-Systems/polyml-5.5.2.ML
    Author:     Makarius

Compatibility wrapper for Poly/ML 5.5.2.
*)

structure Thread =
struct
  open Thread;

  structure Thread =
  struct
    open Thread;

    fun numProcessors () =
      (case Thread.numPhysicalProcessors () of
        SOME n => n
      | NONE => Thread.numProcessors ());
  end;
end;

use "ML-Systems/polyml.ML";

