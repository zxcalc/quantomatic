val _ = PolyML.Compiler.printDepth := 0;
val _ = PolyML.exception_trace (fn () => PolyML.use "ROOT.ML")
        handle _ => OS.Process.exit OS.Process.failure;
PolyML.Project.ensure_at_latest_heap ();
PolyML.fullGC ();
PolyML.SaveState.saveState "heaps/quanto.heap";
val _ = OS.Process.exit OS.Process.success;
