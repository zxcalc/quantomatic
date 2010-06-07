structure DB_StrBBGraph = BangGraphFun(
  structure BVertex = StrBVertex 
  and Edge = StrEdge
  type data = unit
  val empty_data = ()
);

structure StrBBGraph : BGRAPH = DB_StrBBGraph;
