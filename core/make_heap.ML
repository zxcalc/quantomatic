(* quietly build the quanto heap *)
PolyML.SaveState.loadState "isaplib/heaps/all.polyml-heap";
val pdepth__ = ! PolyML.Compiler.printDepth;
PolyML.Compiler.printDepth := 0;
(* do and continue or die *)
val () = PolyML.exception_trace (fn () => (PolyML.use "ROOT.ML"; PolyML.fullGC ()))
    handle _ => OS.Process.exit OS.Process.failure;
PolyML.Compiler.printDepth := pdepth__;
forget_all(); PolyML.shareCommonData run; PolyML.fullGC ();
PolyML.SaveState.saveState "heaps/quanto.polyml-heap";
