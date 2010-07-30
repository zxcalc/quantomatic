

structure DB_StrUnitBGraph = BGraphFun(
  structure BVertex = StrBVertex 
  and Edge = UnitEdge
  type data = unit
  val empty_data = ()
);

structure StrUnitBGraph : BGRAPH where type Edge.data = unit 
  = DB_StrUnitBGraph;


structure StrUnitMatchCtxt = EqMatchUnitEdgeNoCtxtFun(StrUnitBGraph);


(* signature RG_MATCH
= sig

structure RG_NE : RG_NODES_AND_EDGES

include NE_MATCH sharing NE = RG_NE;

val vars_of_ctxt : T -> VName.N.T
end;
*)
