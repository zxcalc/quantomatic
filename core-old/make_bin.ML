(* make the binary file from the heap. *)

PolyML.SaveState.loadState "heaps/quanto.polyml-heap"; 
forget_all(); 
PolyML.shareCommonData run; 
PolyML.fullGC ();
PolyML.export ("bin/quanto-core", (fn () => do_and_exit_or_die run));
val _ = OS.Process.exit OS.Process.success;
