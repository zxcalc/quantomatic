
(* define some concrete structures ----- *)

structure DB_StrBGraph = BGraphFun(
  structure BVertex = StrBVertex 
  and Edge = StrEdge);

structure StrBGraph : BGRAPH = DB_StrBGraph;
