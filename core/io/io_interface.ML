(* For loading and saving graphs as XML *)
(* rwd 9-jan-2009 *)

signature INPUT_INTERFACE_XML
= sig 
  structure Graph : INPUT_XML
  structure Rule : INPUT_XML
  structure Ruleset : INPUT_XML
end;

signature OUTPUT_INTERFACE_XML
= sig
  structure Vertex : OUTPUT_XML
  structure Edge : OUTPUT_XML
  structure BBox : OUTPUT_XML
  structure Graph : OUTPUT_XML
  structure Rule : OUTPUT_XML
  structure Ruleset : OUTPUT_XML
  structure Rewrite : OUTPUT_XML
    where type data = ((RuleName.name * Rule.data) * Graph.data)
  structure RewriteList : OUTPUT_XML where type data = Rewrite.data list
end;

signature IO_INTERFACE_XML =
sig 
  structure Input : INPUT_INTERFACE_XML
  structure Output : OUTPUT_INTERFACE_XML
  sharing type Input.Graph.data = Output.Graph.data
  sharing type Input.Rule.data = Output.Rule.data
  sharing type Input.Ruleset.data = Output.Ruleset.data
end;


(* Given Ruleset, IO for vertices and edges, construct all IO interface *)
functor IO_Interface_Xml(
  structure Ruleset : RULESET

  structure VertexData : ELEMENT_COMPONENT_DATA 
        and EdgeData : ELEMENT_COMPONENT_DATA 
  sharing type VertexData.data = Ruleset.Rule.Graph.Vertex.data
  sharing type EdgeData.data = Ruleset.Rule.Graph.Edge.data
) = struct

  structure Input = struct

    structure Graph = InputGraphXML_v2(
      structure Graph = Ruleset.Rule.Graph
      structure InputVertexData = InputTypedDataXML(VertexData)
      structure InputEdgeData = InputTypedDataXML(EdgeData));

    structure Rule = InputRuleXML(
        structure Rule = Ruleset.Rule
        structure InputGraph = Graph);

    structure Ruleset = InputRulesetXML(
      structure Ruleset = Ruleset
      structure InputRule = Rule
      structure InputRuleName = InputRuleNameXML
      structure InputTagName = InputTagNameXML);
  
  end; (* input *)


  structure Output = struct

    structure Graph = OutputGraphXML_v2(
      structure Graph = Ruleset.Rule.Graph
      structure OutputVertexData = OutputTypedDataXML(VertexData)
      structure OutputEdgeData = OutputTypedDataXML(EdgeData));

    structure Vertex = OutputVertexXML_v2(
      structure Graph = Ruleset.Rule.Graph
      structure OutputVertexData = OutputTypedDataXML(VertexData));

    structure Edge = OutputEdgeXML_v2(
      structure Graph = Ruleset.Rule.Graph
      structure OutputEdgeData = OutputTypedDataXML(EdgeData));

    structure BBox = OutputBBoxXML_v2(
      structure Graph = Ruleset.Rule.Graph);

    structure Rule = OutputRuleXML(
        structure Rule = Ruleset.Rule
        structure OutputGraph = Graph);

    structure Rewrite 
    : OUTPUT_XML where type data = (RuleName.name * Ruleset.Rule.T) * Ruleset.Rule.Graph.T
     = OutputRewriteXML(
        structure Rule = Ruleset.Rule
        and OutputGraph = Graph
       );

    structure RewriteList 
    : OUTPUT_XML where type data = Rewrite.data list
     = OutputWrapXML(
        val tagname = "rewrites"
        val attrs = []
        structure Output1 = OutputMap(Rewrite)
       );

    structure Ruleset = OutputRulesetXML(
      structure Ruleset = Ruleset
      structure OutputRule = OutputRulePartsXMLList(
        structure Rule = Ruleset.Rule
        structure OutputGraph = Graph
      )
    );
  
  end; (* input *)

end; (* IO_Interface *)


(* vim:et:sts=2:sw=2:ts=2
*)
