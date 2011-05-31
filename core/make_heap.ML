local 
  val old_print_depth = ! PolyML.Compiler.printDepth;
in
val _ = PolyML.Compiler.printDepth := 0; (* quietly build the quanto heap *)
val _ = PolyML.exception_trace (fn () => PolyML.use "ROOT.ML")
    handle _ => OS.Process.exit OS.Process.failure;
val _ = PolyML.Compiler.printDepth := old_print_depth;
end;


PolyML.Project.Log.log 2 "finished updating polysaves";


PolyML.Project.ensure_at_latest_heap ();

PolyML.Project.Log.log 2 "finished ensure_at_latest_heap";

PolyML.shareCommonData run; PolyML.fullGC ();

PolyML.Project.Log.log 2 "finished garbage collection";

PolyML.SaveState.saveState "heaps/quanto.polyml-heap";

PolyML.Project.Log.log 2 "finished saving global heap";

val _ = OS.Process.exit OS.Process.success;
