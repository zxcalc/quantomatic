(* test IO construction *)
structure DB_Test_IO_Interface = IO_Interface(
  structure Ruleset = Test_Ruleset
  structure InputVertexData = InputRGVertexDataXML(Test_Graph.OVertex)
  structure OutputVertexData = OutputRGVertexDataXMLList(Test_Graph.OVertex)
  structure InputEdgeData = InputNoEdgeDataXML(Test_Graph.Edge)
  structure OutputEdgeData = OutputNoEdgeDataXMLList(Test_Graph.Edge)
);

structure Test_IO_Interface : IO_INTERFACE = DB_Test_IO_Interface;
