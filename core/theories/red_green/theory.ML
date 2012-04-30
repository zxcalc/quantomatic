
(* *)

(* The RG theory, all in once place. *)
structure DB_RG_Theory = GraphicalTheory(
structure OParam = RG_OGraphParam
structure VComponentData = RG_OVertexComponentData
structure EComponentData = EmptyElementComponentData
);
structure RG_Theory : GRAPHICAL_THEORY = DB_RG_Theory;

structure RG_Controller = Controller(RG_Theory);

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* terms used within the RG theory. *)
structure RGHilbTerm = TensorTermFun(

structure Graph = RG_Theory.Graph
fun pretty_vertex fmt graph vert = let
    val (v, (ie,oe)) = Graph.get_vertex graph vert
    val (ic,oc) = (E.NSet.cardinality ie,
		   E.NSet.cardinality oe)
    val nums = [Pretty.str (Int.toString ic),
		Pretty.str ", ",
		Pretty.str (Int.toString oc)]
in Pretty.block	
   (case v of Graph.OVData.IVert iv =>
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

