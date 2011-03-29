
(* *)

(* The RG theory, all in once place. *)
structure DB_RG_Theory = GraphicalTheory(structure Param = RG_GraphParam);
structure RG_Theory : GRAPHICAL_THEORY = DB_RG_Theory;

(* Input/Output *)
structure DB_RG_IO_Interface = IO_Interface(
  structure Ruleset = RG_Theory.Ruleset

  structure InputVertexData = InputRGVertexDataXML(RG_Theory.Vertex)
  structure OutputVertexData = OutputRGVertexDataXMLList(RG_Theory.Vertex)

  structure InputEdgeData = InputNoEdgeDataXML(RG_Theory.Edge)
  structure OutputEdgeData = OutputNoEdgeDataXMLList(RG_Theory.Edge)
);

structure RG_IO_Interface : IO_INTERFACE = DB_RG_IO_Interface; 

(* 
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
*)
