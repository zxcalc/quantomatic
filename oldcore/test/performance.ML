PolyML.SaveState.loadState "../heaps/quanto.heap";
PolyML.Compiler.printDepth:=10;

val rsj = Json.read_file "../../examples/steane/steane.qrules";
val rs = RG_GraphicalTheoryIO.InputRulesetJSON.input rsj;
val gj = Json.read_file "../../examples/steane/enc_dec_sp_norm.qgraph";
val g1 = RG_GraphicalTheoryIO.InputGraphJSON.input gj;
val gj = Json.read_file "../../examples/steane/sp2legs.qgraph";
val g2 = RG_GraphicalTheoryIO.InputGraphJSON.input gj;

val gj = Json.read_file "../../examples/steane/splhs.qgraph";
val pat = RG_GraphicalTheoryIO.InputGraphJSON.input gj;



RG_Theory.MatchSearch.InnerLog.level_ref := 2;
RG_Theory.MatchSearch.Log.level_ref := 2;
