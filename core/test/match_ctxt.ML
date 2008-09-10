structure StrUnitMatchCtxt = 
EqMatchUnitEdgeNoCtxtFun(structure IVertex = StrVertex and Edge = UnitEdge);


(* signature RG_MATCH
= sig

structure RG_NE : RG_NODES_AND_EDGES

include NE_MATCH sharing NE = RG_NE;

val vars_of_ctxt : T -> VName.N.T
end;
*)
