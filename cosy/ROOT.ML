(*val cosyRootDir = OS.FileSys.getDir();
(OS.FileSys.chDir (cosyRootDir ^ "/../core/"); 
use "ROOT.ML"; 
OS.FileSys.chDir cosyRootDir);

(* this may get clobbered by reloading the heap *)
val cosyRootDir = OS.FileSys.getDir();

print_depth 3;
PolyML.Project.depend_on_files [cosyRootDir^"/../core/ROOT.ML", cosyRootDir^"/ROOT.ML"];*)

val cosyRootDir = OS.FileSys.getDir();

use "default_gens.ML";
use "metric.ML";

use "graph_equiv.ML";
use "eq_class_tab.ML";
use "default_rws.ML";
use "fast_graph_enum.ML";
use "tensor_equiv.ML";

use "theories.ML";




(*PolyML.Project.make "enumerate.ML";
PolyML.Project.make "synthesize.ML";
PolyML.Project.make "ruleset_builder.ML";

PolyML.Project.make "theories.ML"; *)
