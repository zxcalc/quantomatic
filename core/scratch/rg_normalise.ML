PolyML.SaveState.loadState "../heaps/quanto.heap";
PolyML.Compiler.printDepth:=10;

structure IO = JsonObjectAnnotatedGraphicalTheoryIO(
  structure Theory = RG_Theory
  structure GraphComponentDataIO = RG_ComponentDataIO)


val (rot, _) = IO.InputRuleJSON.input (Json.read_file "rot.qrule");
val rot1 = RG_Theory.Rule.expand_bbox (B.mk "bx0") rot

val (sample,_) = IO.InputGraphJSON.input (Json.read_file "sample.qgraph");
