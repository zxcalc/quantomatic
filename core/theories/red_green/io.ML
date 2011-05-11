
(* *)

(* The RG theory, all in once place. *)
structure DB_RG_Theory = GraphicalTheory(structure OParam = RG_OGraphParam);
structure RG_Theory : GRAPHICAL_THEORY = DB_RG_Theory;

(* Input/Output *)
(* structure DB_RG_IO_Interface_v1 = IO_Interface(
  structure Ruleset = RG_Theory.Ruleset

  structure InputVertexData = InputRGVertexDataXML_v1(RG_Theory.Vertex)
  structure OutputVertexData = OutputRGVertexDataXMLList_v1(RG_Theory.Vertex)

  structure InputEdgeData = InputNoEdgeDataXML_v1(RG_Theory.Edge)
  structure OutputEdgeData = OutputNoEdgeDataXMLList_v1(RG_Theory.Edge)
); *)


structure DB_RG_IO_Interface_v2 = IO_Interface(
  structure Ruleset = RG_Theory.Ruleset

  structure InputVertexData = InputTypedData(RG_OVertexData_ParamIO);
  structure OutputVertexData = OutputTypedData(RG_OVertexData_ParamIO);

  structure InputEdgeData = InputTypedData(NoEdgeData_ParamIO);
  structure OutputEdgeData = OutputTypedData(NoEdgeData_ParamIO);
);


structure RG_IO_Interface : IO_INTERFACE = DB_RG_IO_Interface_v2; 

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
