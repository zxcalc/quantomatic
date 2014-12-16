

structure RP_Theory = GraphicalTheory(
  structure Data = RP_Data
  structure DataIO = RP_ComponentDataIO)

(* Use this for convenience if you don't need annotations *)
(*structure RG_GraphicalTheoryIO = GraphicalTheoryIO(
  structure Theory = RG_Theory
  structure GraphComponentDataIO = RG_ComponentDataIO
)*)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* terms used within the RG theory. *)
(*structure RGHilbTerm = TensorTermFun(

structure Graph = RG_Theory.Graph
fun pretty_vertex fmt graph vert = let
    val (v, (ie,oe)) = Graph.get_vertex graph vert
    val (ic,oc) = (E.NSet.cardinality ie,
		   E.NSet.cardinality oe)
    val nums = [Pretty.str (Int.toString ic),
		Pretty.str ", ",
		Pretty.str (Int.toString oc)]
in Pretty.block	
   (case v of Graph.OVData.NVert iv =>
	     (case iv
	       of RG_InternVData.Xnd e =>
		        [Pretty.str "xsp[", LinratAngleExpr.pretty_math fmt e, Pretty.str ", "] @
		        nums @ [Pretty.str "]"]
		      | RG_InternVData.Znd e =>
		        [Pretty.str "zsp[", LinratAngleExpr.pretty_math fmt e, Pretty.str ", "] @
		        nums @ [Pretty.str "]"]
          | RG_InternVData.Hnd => [Pretty.str "h"])
	  | _ => [Pretty.str "id2[1]"])
end

)
*)
