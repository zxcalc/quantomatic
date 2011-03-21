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
  structure Ruleset : OUTPUT_PRETTY_XML
  structure Rule : OUTPUT_PRETTY_XML
  structure Rewrite : OUTPUT_PRETTY_XML
  structure RewriteList : OUTPUT_PRETTY_XML where type data = Rewrite.data list
  structure Graph : OUTPUT_PRETTY_XML
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

  structure OutputVertexData : OUTPUT_PRETTY_XML 
        and OutputEdgeData : OUTPUT_PRETTY_XML 
  sharing type OutputVertexData.data = Ruleset.Rule.Graph.Vertex.data
  sharing type OutputEdgeData.data = Ruleset.Rule.Graph.Edge.data
) = struct

  structure Input = struct

    structure Graph = InputGraphXML(
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

    structure Graph = OutputGraphXML(
      structure Graph = Ruleset.Rule.Graph
      structure OutputVertexData = OutputVertexData
      structure OutputEdgeData = OutputEdgeData);

    structure Rule = OutputRuleXML(
        structure Rule = Ruleset.Rule
        structure OutputGraph = Graph);

    structure Rewrite 
    : OUTPUT_PRETTY_XML where type data = (RuleName.name * Ruleset.Rule.T) * Ruleset.Rule.Graph.T
     = OutputRewriteXML(
        structure Rule = Ruleset.Rule
        and OutputGraph = Graph
       );

    structure RewriteList 
    : OUTPUT_PRETTY_XML where type data = Rewrite.data list
     = OutputListXML(
        val tagname = "rewrites"
        val item_tagname = "rewrite"
        structure Output1 = Rewrite
       );

    structure Ruleset = OutputRulesetXML(
      structure Ruleset = Ruleset
      structure OutputRule = OutputBareRuleXML(
        structure Rule = Ruleset.Rule
        structure OutputGraph = Graph
      )
      structure OutputRuleName = OutputRuleNameXML
      structure OutputTagName = OutputTagNameXML);
  
  end; (* input *)

end; (* IO_Interface *)

(* *)
structure DB_RG_IO_Interface = IO_Interface(
  structure Ruleset = RGRuleset
  structure InputVertexData = InputRGVertexDataXML(RGVertex)
  structure InputEdgeData = InputNoEdgeDataXML(RGRuleset.Rule.Graph.Edge)
  structure OutputVertexData = OutputRGVertexDataXML(RGGraph.RGVertex)
  structure OutputEdgeData = OutputNoEdgeDataXML(RGGraph.Edge)
);

structure RG_IO_Interface : IO_INTERFACE = DB_RG_IO_Interface; 

(* 
  type graph = Graph.T;
  fun save_graph file g = (PrettyWriter.write_to_file file) (OutputGraph.output g)
  val load_graph = InputGraph.input o XMLReader.read_from_file;
*)

(*  -- just for debugging --
structure rgls = RGGraphLoaderSaver;
*)
