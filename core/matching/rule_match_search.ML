signature RULE_MATCH_SEARCH_SHARING
= sig
  structure Match : MATCH_SHARING 
end;


signature RULE_MATCH_SEARCH
= sig
  structure Match : MATCH 
  structure BangGraph : BANG_GRAPH
  structure Edge : EDGE
  structure Vertex : OVERTEX
  structure Rule : RULE
  
  structure Sharing : RULE_MATCH_SEARCH_SHARING 
  sharing Sharing.Match = Match.Sharing

  (* sharing Vertex = BangGraph.RGVertex *)
  (* not clear why I need this this way round - but get type strange error the other way *)
  sharing BangGraph.SharingBangGraph = Match.Sharing.G;
  sharing Rule.Graph.SharingBangGraph = Match.Sharing.G;
  sharing Vertex.SharingWithIData = BangGraph.OVertex.SharingWithIData;
  sharing Edge.EdgeSharingWithData = BangGraph.Edge.EdgeSharingWithData;
  
  val match : (BangGraph.Vertex.NSet.T * BangGraph.Edge.NSet.T * BangGraph.BBox.NSet.T)
              -> Rule.T -> BangGraph.T
              -> Match.T Seq.seq
  
end;