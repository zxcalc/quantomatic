
(* *)
structure Test_RG_IO_Interface = IO_Interface(
  structure Ruleset = Test_RG_Rewriting.Ruleset
  structure InputVertexData = InputRGVertexDataXML(Test_RGGraph.OVertex)
  structure OutputVertexData = OutputRGVertexDataXMLList(Test_RGGraph.OVertex)
  structure InputEdgeData = InputNoEdgeDataXML(Test_RGGraph.Edge)
  structure OutputEdgeData = OutputNoEdgeDataXMLList(Test_RGGraph.Edge)
);
