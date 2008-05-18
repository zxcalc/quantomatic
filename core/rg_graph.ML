structure DB_RGGraph = BGraphFun(
  structure Vertex = RGVertex 
  and Edge = UnitEdge 
  and Boundary = UnitData);
structure RGGraph : BGRAPH = DB_RGGraph;
