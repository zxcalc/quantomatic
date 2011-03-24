
(* *)
structure DB_RG_IO_Interface = IO_Interface(
  structure Ruleset = RG_Rewriting.Ruleset
  structure InputVertexData = InputRGVertexDataXML(RGGraph.OVertex)
  structure InputEdgeData = InputNoEdgeDataXML(RG_Rewriting.Rule.Graph.Edge)
  structure OutputVertexData = OutputRGVertexDataXMLList(RGGraph.OVertex)
  structure OutputEdgeData = OutputNoEdgeDataXMLList(RGGraph.Edge)
);
