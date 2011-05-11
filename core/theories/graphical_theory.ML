signature GRAPHICAL_THEORY = 
sig
  (* vertices and edges of a graph *)
  structure Vertex : OVERTEX
  structure Edge : EDGE
  (* share the same generic graph data subst *)
  sharing type Vertex.subst = Edge.subst 

  (* Graph *)
  structure Graph : BANG_GRAPH;
  sharing Graph.OVertex.SharingWithIData = Vertex.SharingWithIData
  sharing Graph.Edge.SharingWithData = Edge.SharingWithData

  (* Rules = pairs of graph + cached computed matching data *)
  structure Rule : RULE
  sharing Rule.Graph.SharingBangGraph = Graph.SharingBangGraph

  (* sets of rules for a graph *)
  structure Ruleset : RULESET
  sharing Ruleset.Sharing.Rule = Rule.Sharing 

  (* A matching between two graphs *)
  structure Match : MATCH
  sharing Match.G.SharingBangGraph = Graph.SharingBangGraph;

  (* Substitutions for graphs, replacing one component with another *)
  structure GraphSubst : GRAPH_SUBST
  sharing type GraphSubst.Sharing.match = Match.T
  sharing type GraphSubst.Sharing.graph = Graph.T

  (* Matching algorithm *)
  structure SimpleMatchSearch : SIMPLE_MATCH_SEARCH
  sharing SimpleMatchSearch.Sharing.Match = Match.Sharing

  (* Sets of rules and matching for those sets *)
  structure RulesetRewrites : RULESET_REWRITES
  sharing RulesetRewrites.Sharing.Ruleset = Ruleset.Sharing 
  sharing RulesetRewrites.Sharing.Match = Match.Sharing;

  (* Input/Output *)
  (* structure IO : IO_INTERFACE
  sharing type IO.Input.Graph.data = Graph.T
  sharing type IO.Input.Rule.data = Rule.T
  sharing type IO.Input.Ruleset.data = Ruleset.T
  *)

end;



functor GraphicalTheory(
  structure OParam : OGRAPH_PARAM
(* 
  structure InputVertexData : INPUT_XML 
        and InputEdgeData : INPUT_XML 
  sharing type InputVertexData.data = Param.Vertex.data
  sharing type InputEdgeData.data = Param.Edge.data

  structure OutputVertexData : OUTPUT_XML_LIST
        and OutputEdgeData : OUTPUT_XML_LIST
  sharing type OutputVertexData.data = Param.Vertex.data
  sharing type OutputEdgeData.data = Param.Edge.data *)
) = 
struct 
  (* structure OVertex = OVertex(Param.Vertex);
  structure OParam = OGraphParam(
    structure Param = Param 
    structure OVertex = OVertex);
  *)

  local 
    structure BasicBangGraph = BasicBangGraphFun(
      structure Param = OParam
      type data = unit (* no extra inherrited data for graphs *)
      val empty_data = ());
    structure BBGraph = BangGraphTools(BasicBangGraph);
  in 
    structure DB_Graph = struct open BasicBangGraph; open BBGraph; end
  end;
  structure Graph = DB_Graph;
  structure Edge = Graph.Edge; 
  structure Vertex = Graph.OVertex; 

  structure DB_Rule = Rule(Graph);
  structure Rule : RULE = DB_Rule;

  structure DB_Ruleset = Ruleset(Rule);  
  structure Ruleset = DB_Ruleset : RULESET

  structure DB_Match = Match(Graph);
  structure Match : MATCH = DB_Match;

  structure DB_GraphSubst = GraphSubstFun(Match);
  structure GraphSubst : GRAPH_SUBST = DB_GraphSubst;

  structure DB_SimpleMatchSearch = SimpleMatchSearch(Match);
  structure SimpleMatchSearch : SIMPLE_MATCH_SEARCH = DB_SimpleMatchSearch;

  structure DB_RulesetRewrites = RulesetRewrites(
    structure Ruleset = Ruleset;
    structure MatchSearch = SimpleMatchSearch;
    structure GraphSubst = GraphSubst;
  );
  structure RulesetRewrites : RULESET_REWRITES = DB_RulesetRewrites;
  
  (* install pretty printers *)
  local
      fun prettyVertex _ _ x =
        PolyML.PrettyString("\"" ^ Vertex.string_of_name x ^ "\"");
      fun prettyEdge _ _ x =
        PolyML.PrettyString("\"" ^ Edge.string_of_name x ^ "\"");
      fun prettyBBox _ _ x =
        PolyML.PrettyString("\"" ^ Graph.BBox.string_of_name x ^ "\"");
  in
      val () = PolyML.addPrettyPrinter prettyVertex;
      val () = PolyML.addPrettyPrinter prettyEdge;
      val () = PolyML.addPrettyPrinter prettyBBox;
  end; 
  
end;


(* tested in next file... don't need it twice...
structure Test_RG_DB_Theory = GraphicalTheory(
  structure Param = RG_GraphParam
);
structure Test_RG_Theory : GRAPHICAL_THEORY = RG_DB_Theory;
*)
