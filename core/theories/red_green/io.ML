
(* *)

(* The RG theory, all in once place. *)
structure DB_RG_Theory = GraphicalTheory(structure OParam = RG_OGraphParam);
structure RG_Theory : GRAPHICAL_THEORY = DB_RG_Theory;

structure DB_RG_IO_Interface_v2 = IO_Interface(
  structure Ruleset = RG_Theory.Ruleset

  structure InputVertexData = InputTypedData(RG_OVertexData_ParamIO);
  structure OutputVertexData = OutputTypedData(RG_OVertexData_ParamIO);

  structure InputEdgeData = InputTypedData(NoEdgeData_ParamIO);
  structure OutputEdgeData = OutputTypedData(NoEdgeData_ParamIO);
);


structure RG_IO_Interface : IO_INTERFACE = DB_RG_IO_Interface_v2; 

structure RG_DotStyle : DOT_STYLE =
struct
  structure Graph = RG_Theory.Graph
  fun style_for_ivertex_data (RG_VertexData.Znd _) = "[style=filled,fillcolor=green,fontcolor=black,shape=circle]"
    | style_for_ivertex_data (RG_VertexData.Xnd _) = "[style=filled,fillcolor=red,fontcolor=white,shape=circle]"
end

structure RG_OutputGraphDot = OutputGraphDot(structure DotStyle = RG_DotStyle)


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
