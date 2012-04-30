
(* *)

(* The SUBSTR_LINRAT theory, all in once place. *)
structure DB_SUBSTR_LINRAT_Theory = GraphicalTheory(
structure OParam = SUBSTR_LINRAT_OGraphParam
structure VComponentData = SUBSTR_LINRAT_OVertexComponentData
structure EComponentData = EmptyElementComponentData
);
structure SUBSTR_LINRAT_Theory : GRAPHICAL_THEORY = DB_SUBSTR_LINRAT_Theory;

structure SUBSTR_LINRAT_Controller = Controller(SUBSTR_LINRAT_Theory);

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* terms used within the SUBSTR_LINRAT theory. *)
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
   (case v of Graph.OVData.IVert (SUBSTR_LINRAT_InternVData.S_E (s, e)) =>
    [Pretty.str s, Pretty.str "[", LinratAngleExpr.pretty_math fmt e, Pretty.str ","] @
      nums @ [Pretty.str "]"] 
	  | _ => [Pretty.str "id2[1]"])
end


)

