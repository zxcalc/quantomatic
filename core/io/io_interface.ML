(* For loading and saving graphs as XML *)
(* rwd 9-jan-2009 *)

signature INPUT_INTERFACE  
= sig 
  structure Ruleset : INPUT_XML
  structure Rule : INPUT_XML
  structure Graph : INPUT_XML
end;

signature OUTPUT_INTERFACE  
= sig
  structure Ruleset : OUTPUT_XML
  structure Rule : OUTPUT_XML
  structure Rewrite : OUTPUT_XML
  structure RewriteList : OUTPUT_XML where type data = Rewrite.data list
  structure Graph : OUTPUT_XML
end;

signature IO_INTERFACE =
sig 
  structure Input : INPUT_INTERFACE
  structure Output : OUTPUT_INTERFACE
    where type Rewrite.data = ((RuleName.name * Input.Rule.data) * Input.Graph.data)
  sharing type Input.Graph.data = Output.Graph.data
  sharing type Input.Rule.data = Output.Rule.data
  sharing type Input.Ruleset.data = Output.Ruleset.data
end;


(* Given Ruleset, IO for vertices and edges, construct all IO interface *)
functor IO_Interface(
  structure Ruleset : RULESET

  structure InputVertexData : INPUT_XML 
        and InputEdgeData : INPUT_XML 
  sharing type InputVertexData.data = Ruleset.Rule.Graph.Vertex.data
  sharing type InputEdgeData.data = Ruleset.Rule.Graph.Edge.data

  structure OutputVertexData : OUTPUT_XML_LIST
        and OutputEdgeData : OUTPUT_XML_LIST
  sharing type OutputVertexData.data = Ruleset.Rule.Graph.Vertex.data
  sharing type OutputEdgeData.data = Ruleset.Rule.Graph.Edge.data
) = struct

  structure Input = struct

    structure Graph = InputGraphXML_v2(
      structure Graph = Ruleset.Rule.Graph
      structure InputVertexData = InputVertexData
      structure InputEdgeData = InputEdgeData);

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
      structure OutputVertexData = OutputVertexData
      structure OutputEdgeData = OutputEdgeData);

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


(* 
  type graph = Graph.T;
  fun save_graph file g = (PrettyWriter.write_to_file file) (OutputGraph.output g)
  val load_graph = InputGraph.input o XMLReader.read_from_file;
*)

(*  -- just for debugging --
structure rgls = RGGraphLoaderSaver;
*)

(* vim:et:sts=2:sw=2:ts=2
*)
