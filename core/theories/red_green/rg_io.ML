
(* *)
structure DB_RG_IO_Interface = IO_Interface(
  structure Ruleset = RG_Rewriting.Ruleset
  structure InputVertexData = InputRGVertexDataXML(RGGraph.OVertex)
  structure InputEdgeData = InputNoEdgeDataXML(RG_Rewriting.Rule.Graph.Edge)
  structure OutputVertexData = OutputRGVertexDataXMLList(RGGraph.OVertex)
  structure OutputEdgeData = OutputNoEdgeDataXMLList(RGGraph.Edge)
);

structure RG_IO_Interface : IO_INTERFACE = DB_RG_IO_Interface; 

(* The RG theory, all in once place. *)
structure RG = 
struct 
  structure VertexData = RG_VertexData;
  structure Vertex = RGGraph.OVertex;
  structure Edge = RGGraph.Edge;
  structure Graph = RGGraph;
  structure IO = RG_IO_Interface;
  structure Rule = RG_Rewriting.Rule;  
  structure Ruleset = RG_Rewriting.Ruleset;  
  structure Match = RG_Rewriting.Match;  
  structure Subst = RG_Rewriting.GraphSubst;
  structure SimpleMatchSearch = RG_Rewriting.SimpleMatchSearch;
  structure RulesetRewrites = RG_Rewriting.RulesetRewrites;
end;
