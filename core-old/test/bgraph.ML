
(* define some concrete structures ----- *)

structure DB_StrBGraph = BGraphFun(
  structure BVertex = StrBVertex 
  and Edge = StrEdge
  type data = unit
  val empty_data = ()
);

structure StrBGraph : BGRAPH = DB_StrBGraph;
