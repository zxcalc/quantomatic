val _ = PolyML.exception_trace (fn () => PolyML.use "ROOT.ML")
        handle _ => OS.Process.exit OS.Process.failure;
PolyML.SaveState.saveState "heaps/quanto.heap";
val _ = OS.Process.exit OS.Process.success;
