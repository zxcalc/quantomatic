signature GRAPHICAL_THEORY =
sig
  val theory_name : string;


  (* Graph *)
  structure Graph : BANG_GRAPH

  (* Rules = pairs of graph + cached computed matching data *)
  structure Rule : BANG_GRAPH_RULE
  sharing Rule.Graph.Sharing = Graph.Sharing

  (* sets of rules for a graph *)
  structure Ruleset : RULESET
  sharing Ruleset.Sharing.Rule = Rule.Sharing 

  (* A matching between two graphs *)
  structure Match : BANG_GRAPH_MATCH
  sharing Match.Graph.Sharing = Graph.Sharing;

  (* Matching algorithm *)
  structure MatchSearch : BG_MATCH_SEARCH
  sharing MatchSearch.Sharing.Match = Match.Sharing
  sharing MatchSearch.Sharing.Graph = Graph.Sharing

  (* Sets of rules and matching for those sets *)
  structure RulesetRewriter : RULESET_REWRITER
  sharing RulesetRewriter.Sharing.Ruleset = Ruleset.Sharing 
  sharing type RulesetRewriter.Sharing.match = Match.T;

  (*(* Component data info *)
  structure VertexComponentData : ELEMENT_COMPONENT_DATA
  sharing type VertexComponentData.data = OVData.data;
  structure EdgeComponentData : ELEMENT_COMPONENT_DATA
  sharing type EdgeComponentData.data = EData.data;

  (* Input/Output *)
  structure IO_Xml : IO_INTERFACE_XML
    where type Output.Vertex.data = (V.name * (Graph.VData.data * (E.NSet.T * E.NSet.T)))
      and type Output.Edge.data = (E.name * ((Graph.dir_or_undir * Graph.EData.data) * (V.name * V.name)))
      and type Output.BBox.data = (B.name * V.NSet.T)
  sharing type IO_Xml.Input.Graph.data = Graph.T
  sharing type IO_Xml.Input.Rule.data = Rule.T
  sharing type IO_Xml.Input.Ruleset.data = Ruleset.T*)

end;



functor GraphicalTheory(
  structure Data : GRAPH_DATA
) : GRAPHICAL_THEORY =
struct
  val theory_name = Pretty.string_of Data.pretty_theory_name;

  structure Graph = BangGraph(structure Data = Data)
  structure Rule = BangGraphRule(Graph)
  structure Ruleset = BangGraphRuleset(Rule)

  structure BBoxOperableGraph = ReplayableBBoxOperableGraph(structure Graph = Graph)
  structure MatchSearch = GreedyMatchSearch(BBoxOperableGraph)
  structure Match = MatchSearch.Match;

  structure RulesetRewriter = RulesetRewriter(
    structure Ruleset = Ruleset;
    structure Rewriter = BangGraphRewriter(
      structure Rule = Ruleset.BangGraphRule;
      structure Matcher = MatchSearch;
      structure BBoxOperableGraph = BBoxOperableGraph;
    )
  )

  (*structure VertexComponentData = VComponentData;
  structure EdgeComponentData = EComponentData;

  structure IO_Xml = IO_Interface_Xml(
    structure Ruleset = Ruleset
    structure VertexData = VertexComponentData;
    structure EdgeData = EdgeComponentData;
  );*)
end

