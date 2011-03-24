signature MonoidalSignature = 
sig

end;




functor MonoidalSignature(Param : ) = 
struct 
  structure RGVertex = RGVertex;
  structure BasicBangGraph = BasicBangGraphFun(
    structure OVertex = RGVertex  
    structure Param = RG_GraphParam
    type data = unit (* no extra inherrited data for graphs *)
    val empty_data = ());
  structure BBGraph = BangGraphTools(BasicBangGraph);
  open BBGraph;

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
end;

structure RG : MONOIDAL_SIGNATURE = 
struct 
  structure G = RGGraph; 
  structure IO = RG_IO_Interface;
  open RG_Rewriting; 
end;




functor MonoidalSignature(Param : GRAPH_PARAM) = 
struct 
  
  structure RGVertex = RGVertex;
  structure BasicBangGraph = BasicBangGraphFun(
    structure Param = Param
    type data = unit (* no extra inherrited data for graphs *)
    val empty_data = ());
  structure G = BangGraphTools(BasicBangGraph);

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
end;


(* *)
structure DB_RG_IO_Interface = IO_Interface(
  structure Ruleset = RGRuleset
  structure InputVertexData = InputRGVertexDataXML(RGVertex)
  structure InputEdgeData = InputNoEdgeDataXML(RGRuleset.Rule.Graph.Edge)
  structure OutputVertexData = OutputRGVertexDataXMLList(RGGraph.RGVertex)
  structure OutputEdgeData = OutputNoEdgeDataXMLList(RGGraph.Edge)
);

structure RG_IO_Interface : IO_INTERFACE = DB_RG_IO_Interface; 


