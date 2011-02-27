local 
  val old_print_depth = ! PolyML.Compiler.printDepth;
in
val _ = PolyML.Compiler.printDepth := 0; (* quietly build the quanto heap *)
val _ = PolyML.exception_trace (fn () => PolyML.use "ROOT.ML")
    handle _ => OS.Process.exit OS.Process.failure;
val _ = PolyML.Compiler.printDepth := old_print_depth;
end;

PolyML.shareCommonData run; PolyML.fullGC ();
PolyML.SaveState.saveState "heaps/quanto.polyml-heap";
val _ = OS.Process.exit OS.Process.success;
