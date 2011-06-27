(* include quanto heap *)
PolyML.SaveState.loadState "../core/heaps/quanto.polyml-heap";

(* build cosy *)
PolyML.Project.use_root "ROOT.ML";

(* include custom polychrome API *)
OS.FileSys.chDir "chromeui/polychrome";
PolyML.Project.basic_use "main.sml";
OS.FileSys.chDir "../..";

(* output the heap *)
PolyML.Project.Log.log 2 "finished updating polysaves";
PolyML.Project.ensure_at_latest_heap ();
PolyML.Project.Log.log 2 "finished ensure_at_latest_heap";
PolyML.shareCommonData run; PolyML.fullGC ();
PolyML.Project.Log.log 2 "finished garbage collection";
PolyML.SaveState.saveState "chromeui/polychrome.polyml-heap";
PolyML.Project.Log.log 2 "finished saving global heap";
val _ = OS.Process.exit OS.Process.success;
