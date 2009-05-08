local 
  val old_print_depth = ! PolyML.Compiler.printDepth;
in
val _ = PolyML.Compiler.printDepth := 0; (* quietly build the quanto heap *)
val _ = PolyML.exception_trace (fn () => PolyML.use "ROOT.ML")
    handle _ => OS.Process.exit OS.Process.failure;
val _ = PolyML.Compiler.printDepth := old_print_depth;
end;

forget_all(); PolyML.shareCommonData run; PolyML.fullGC ();
PolyML.export ("bin/quanto-core", (fn () => do_and_exit_or_die run));
OS.Process.exit OS.Process.success;
