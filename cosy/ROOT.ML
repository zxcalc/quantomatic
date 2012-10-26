val cosyRootDir = OS.FileSys.getDir();
(OS.FileSys.chDir (cosyRootDir ^ "/../core/"); 
use "ROOT.ML"; 
OS.FileSys.chDir cosyRootDir);

(* this may get clobbered by reloading the heap *)
val cosyRootDir = OS.FileSys.getDir();

print_depth 3;
PolyML.Project.depend_on_files [cosyRootDir^"/../core/ROOT.ML", cosyRootDir^"/ROOT.ML"];


PolyML.Project.make "default_rws.ML";
PolyML.Project.make "default_gens.ML";
PolyML.Project.make "metric.ML";

PolyML.Project.make "graph_equiv.ML";

PolyML.Project.make "eq_class_tab.ML";

PolyML.Project.make "semiring.ML";
PolyML.Project.make "tensor.ML";
PolyML.Project.make "tensor_equiv.ML";

PolyML.Project.make "fast_graph_enum.ML";

PolyML.Project.make "theories.ML";

(*PolyML.Project.make "enumerate.ML";
PolyML.Project.make "synthesize.ML";
PolyML.Project.make "ruleset_builder.ML";

PolyML.Project.make "theories.ML"; *)
