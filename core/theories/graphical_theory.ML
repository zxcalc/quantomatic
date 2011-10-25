signature GRAPHICAL_THEORY = 
sig
  val theory_name : string;

  (* vertices and edges of a graph *)
  structure OVData : OVERTEX_DATA
  structure EData : UNIFIABLE_DATA
  (* share the same generic graph data subst *)
  sharing type OVData.subst = EData.subst 

  (* Graph *)
  structure Graph : BANG_GRAPH;
  sharing Graph.OVData.SharingWithIData = OVData.SharingWithIData
  sharing Graph.EData.Sharing = EData.Sharing

  (* Rules = pairs of graph + cached computed matching data *)
  structure Rule : RULE
  sharing Rule.Graph.SharingOGraph = Graph.SharingOGraph

  (* sets of rules for a graph *)
  structure Ruleset : RULESET
  sharing Ruleset.Sharing.Rule = Rule.Sharing 

  (* A matching between two graphs *)
  structure Match : MATCH
  sharing Match.G.SharingOGraph = Graph.SharingOGraph;

  (* Substitutions for graphs, replacing one component with another *)
  structure GraphSubst : GRAPH_SUBST
  sharing type GraphSubst.Sharing.match = Match.T
  sharing type GraphSubst.Sharing.graph = Graph.T

  (* Matching algorithm *)
  structure RuleMatchSearch : RULE_MATCH_SEARCH
  sharing RuleMatchSearch.Sharing.Match = Match.Sharing

  (* Sets of rules and matching for those sets *)
  structure RulesetRewrites : RULESET_REWRITES
  sharing RulesetRewrites.Sharing.Ruleset = Ruleset.Sharing 
  sharing RulesetRewrites.Sharing.Match = Match.Sharing;

  (* Component data info *)
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
  sharing type IO_Xml.Input.Ruleset.data = Ruleset.T

end;



functor GraphicalTheory(
  structure OParam : OGRAPH_PARAM
  structure VComponentData : ELEMENT_COMPONENT_DATA
  sharing type VComponentData.data = OParam.OVData.data
  structure EComponentData : ELEMENT_COMPONENT_DATA
  sharing type EComponentData.data = OParam.EData.data
) = 
struct 
  val theory_name = Pretty.string_of OParam.pretty_theory_name;

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
  structure EData = Graph.EData; 
  structure OVData = Graph.OVData;
  
  structure DB_Rule = Rule(Graph);
  structure Rule : RULE = DB_Rule;

  structure DB_Ruleset = Ruleset(Rule);  
  structure Ruleset = DB_Ruleset : RULESET

  structure DB_Match = Match(Graph);
  structure Match : MATCH = DB_Match;

  structure DB_GraphSubst = GraphSubstFun(Match);
  structure GraphSubst : GRAPH_SUBST = DB_GraphSubst;

  (* removed DB_RuleMatchSearch, suggest removing other DB_'s *)
  structure BasicMatchSearch = SimpleMatchSearch(Match);
  structure RuleMatchSearch = SimpleRuleMatchSearch(
    structure MatchSearch = BasicMatchSearch;
    structure Rule = Rule;);

  structure DB_RulesetRewrites = RulesetRewrites(
    structure Ruleset = Ruleset;
    structure RuleMatchSearch = RuleMatchSearch;
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
        PolyML.PrettyString("\"" ^ V.string_of_name x ^ "\"");
      fun prettyEdge _ _ x =
        PolyML.PrettyString("\"" ^ E.string_of_name x ^ "\"");
      fun prettyBBox _ _ x =
        PolyML.PrettyString("\"" ^ B.string_of_name x ^ "\"");
  in
      val () = PolyML.addPrettyPrinter prettyVertex;
      val () = PolyML.addPrettyPrinter prettyEdge;
      val () = PolyML.addPrettyPrinter prettyBBox;
  end;
  
end;

