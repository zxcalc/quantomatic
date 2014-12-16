

structure Substr_Linrat_Theory = GraphicalTheory(
  structure Data = Substr_Linrat_Data
  structure DataIO = Substr_Linrat_ComponentDataIO)


(* Use this for convenience if you don't need annotations *)
structure Substr_Linrat_GraphicalTheoryIO = GraphicalTheoryIO(
  structure Theory = Substr_Linrat_Theory
  structure GraphComponentDataIO = Substr_Linrat_ComponentDataIO
)

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

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* terms used within the SUBSTR_LINRAT theory. *)
(*
structure SUBSTR_LINRAT_HilbTerm = TensorTermFun(

structure Graph = SUBSTR_LINRAT_Theory.Graph

fun pretty_vertex fmt graph vert = let
    val (v, (ie,oe)) = Graph.get_vertex graph vert
    val (ic,oc) = (E.NSet.cardinality ie,
		   E.NSet.cardinality oe)
    val nums = [Pretty.str (Int.toString ic),
		Pretty.str ", ",
		Pretty.str (Int.toString oc)]
in Pretty.block	
   (case v of Graph.OVData.NVert (SUBSTR_LINRAT_InternVData.S_E (s, e)) =>
    [Pretty.str s, Pretty.str "[", LinratAngleExpr.pretty_math fmt e, Pretty.str ","] @
      nums @ [Pretty.str "]"] 
	  | _ => [Pretty.str "id2[1]"])
end


)
*)

