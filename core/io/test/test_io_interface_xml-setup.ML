(* Input/Output *)
(* structure DB_RG_IO_Interface_v1 = IO_Interface(
  structure Ruleset = RG_Theory.Ruleset

  structure InputVertexData = InputRGVertexDataXML_v1(RG_Theory.Vertex)
  structure OutputVertexData = OutputRGVertexDataXMLList_v1(RG_Theory.Vertex)

  structure InputEdgeData = InputNoEdgeDataXML_v1(RG_Theory.Edge)
  structure OutputEdgeData = OutputNoEdgeDataXMLList_v1(RG_Theory.Edge)
); *)

(* test IO construction *)
structure Test_IO_Interface_v1 = struct
  structure Input = struct
    structure InputVertexData = InputTestVertexDataXML_v1(Test_RG_Graph.OVData)
    structure InputEdgeData = InputNoEdgeDataXML_v1(Test_RG_Graph.EData)

    structure Graph = InputGraphXML_v1(
      structure Graph = Test_RG_Ruleset.Rule.Graph
      structure InputVertexData = InputVertexData
      structure InputEdgeData = InputEdgeData);

    structure Rule = InputRuleXML(
        structure Rule = Test_RG_Ruleset.Rule
        structure InputGraph = Graph);

    structure Ruleset = InputRulesetXML(
      structure Ruleset = Test_RG_Ruleset
      structure InputRule = Rule
      structure InputRuleName = InputRuleNameXML
      structure InputTagName = InputTagNameXML);
  end; (* input *)
end; (* DB_Test_IO_Interface_v1 *)

structure DB_Test_IO_Interface_v2 = IO_Interface_Xml(
  structure Ruleset = Test_RG_Ruleset
  structure VertexData = Test_OVertexComponentData;
  structure EdgeData = EmptyElementComponentData;
);
structure Test_IO_Interface_v2 : IO_INTERFACE_XML = DB_Test_IO_Interface_v2;

