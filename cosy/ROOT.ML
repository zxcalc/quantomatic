val rootDir = OS.FileSys.getDir();
(OS.FileSys.chDir (rootDir ^ "/../core/"); 
use "ROOT.ML"; 
OS.FileSys.chDir rootDir);
print_depth 3;

PolyML.Project.depend_on_files [rootDir^"/../core/ROOT.ML", rootDir^"/ROOT.ML"];

PolyML.Project.make "default_rws.ML";
PolyML.Project.make "default_gens.ML";
PolyML.Project.make "metric.ML";

PolyML.Project.make "semiring.ML";
PolyML.Project.make "tensor.ML";
PolyML.Project.make "tensor_eval.ML";

PolyML.Project.make "enumerate.ML";
PolyML.Project.make "synthesize.ML";
PolyML.Project.make "ruleset_builder.ML";

PolyML.Project.make "theories.ML";
