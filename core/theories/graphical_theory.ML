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

  (* Component data info *)
  structure VertexComponentData : ELEMENT_COMPONENT_DATA
  sharing type VertexComponentData.data = Vertex.data;
  structure EdgeComponentData : ELEMENT_COMPONENT_DATA
  sharing type EdgeComponentData.data = Edge.data;

  (* Input/Output *)
  structure IO_Xml : IO_INTERFACE_XML
    where type Output.Vertex.data = (Graph.Vertex.name * (Graph.Vertex.data * (Graph.Edge.NSet.T * Graph.Edge.NSet.T)))
      and type Output.Edge.data = (Graph.Edge.name * ((Graph.dir_or_undir * Graph.Edge.data) * (Graph.Vertex.name * Graph.Vertex.name)))
      and type Output.BBox.data = (Graph.BBox.name * Graph.Vertex.NSet.T)
  sharing type IO_Xml.Input.Graph.data = Graph.T
  sharing type IO_Xml.Input.Rule.data = Rule.T
  sharing type IO_Xml.Input.Ruleset.data = Ruleset.T

end;



functor GraphicalTheory(
  structure OParam : OGRAPH_PARAM
  structure VComponentData : ELEMENT_COMPONENT_DATA
  sharing type VComponentData.data = OParam.OVertex.data
  structure EComponentData : ELEMENT_COMPONENT_DATA
  sharing type EComponentData.data = OParam.Edge.data
) = 
struct 
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

  structure VertexComponentData = VComponentData;
  structure EdgeComponentData = EComponentData;

  structure IO_Xml = IO_Interface_Xml(
    structure Ruleset = Ruleset
    structure VertexData = VertexComponentData;
    structure EdgeData = EdgeComponentData;
  );
  
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

