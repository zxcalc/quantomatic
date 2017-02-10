signature GRAPHICAL_THEORY =
sig
  val theory_name : string;


  (* Graph *)
  structure Graph : BANG_GRAPH

  (* Rules = pairs of graph + cached computed matching data *)
  structure Rule : BANG_GRAPH_RULE
  sharing Rule.Graph.Sharing = Graph.Sharing

  (* sets of rules for a graph *)
  structure Ruleset : BANG_GRAPH_RULESET
  sharing Ruleset.Sharing.Rule = Rule.Sharing 

  (* A matching between two graphs *)
  structure Match : BANG_GRAPH_MATCH
  sharing Match.Graph.Sharing = Graph.Sharing;

  (* Matching algorithm *)
  structure MatchSearch : BG_MATCH_SEARCH
  sharing MatchSearch.Sharing.Match = Match.Sharing
  sharing MatchSearch.Sharing.Graph = Graph.Sharing

  structure Rewriter : REWRITER
  sharing Rewriter.Sharing.Rule = Rule.Sharing
  sharing type Rewriter.Sharing.match = Match.T

  (* Sets of rules and matching for those sets *)
  structure RulesetRewriter : RULESET_REWRITER
  sharing RulesetRewriter.Sharing.Ruleset = Ruleset.Sharing 
  sharing type RulesetRewriter.Sharing.match = Match.T

  structure GraphJSON : GRAPH_JSON
  sharing Graph.Sharing = GraphJSON.Graph.Sharing

  structure RuleJSON : RULE_JSON
  sharing Rule.Sharing = RuleJSON.Rule.Sharing

  structure RulesetJSON : RULESET_JSON
  sharing Ruleset.Sharing = RulesetJSON.Ruleset.Sharing
end



functor GraphicalTheory(
  structure Data : GRAPH_DATA
  structure DataIO : GRAPH_COMPONENT_DATA_IO
  sharing type Data.nvdata = DataIO.nvdata
  sharing type Data.edata = DataIO.edata
) : GRAPHICAL_THEORY =
struct
  val theory_name = Pretty.string_of Data.pretty_theory_name;

  structure Graph = BangGraph(structure Data = Data)
  structure Rule = BangGraphRule(Graph)
  structure Ruleset = BangGraphRuleset(Rule)
  structure MatchSearch = GreedyMatchSearch(Graph)
  structure Match = MatchSearch.Match

  structure Rewriter = BangGraphRewriter(
      structure Rule = Ruleset.BangGraphRule
      structure Matcher = MatchSearch
    )

  structure RulesetRewriter = RulesetRewriter(
    structure Ruleset = Ruleset
    structure Rewriter = Rewriter
  )

  (* NEW json I/O structures *)
  structure GraphJSON = GraphJSON(
    structure Graph = Graph
    structure InputVertexData  = DataIO.IVDataInputJSON
    structure InputEdgeData    = DataIO.EDataInputJSON
    structure OutputVertexData = DataIO.IVDataOutputJSON
    structure OutputEdgeData   = DataIO.EDataOutputJSON
  )

  structure RuleJSON = RuleJSON(
    structure Rule = Rule
    structure GraphJSON = GraphJSON
  )

  structure RulesetJSON = RulesetJSON(
    structure Ruleset = Ruleset
    structure RuleJSON = RuleJSON
  )
end

