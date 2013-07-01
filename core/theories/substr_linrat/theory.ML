structure Substr_Linrat_Data =
struct
  val pretty_theory_name = Pretty.str "substr_linrat"
  type psubst = LinratAngleExpr.match
  type subst  = psubst

  type nvdata = string * LinratAngleExpr.T
  val default_nvdata = ("",LinratAngleExpr.zero)

  val nvdata_typestrings = ["Default"]
  fun default_nvdata_of_typestring "Default" = default_nvdata
    | default_nvdata_of_typestring s = raise unknown_typestring_exp s
  fun typestring_of_nvdata _ = "Default"

  fun nvdata_eq ((s1,a1),(s2,a2)) = s1 = s2 andalso LinratAngleExpr.eq a1 a2
      
  fun pretty_nvdata (s,a) = 
        Pretty.block [Pretty.str s, Pretty.str "(", LinratAngleExpr.pretty a, Pretty.str ")"]
  
  fun match_nvdata ((s1,a1),(s2,a2)) m =
        if String.isPrefix s1 s2
        then LinratAngleExpr.match a1 a2 m
        else NONE

  fun subst_in_nvdata u (s,a) = (s,LinratAngleExpr.subst u a)

  open EmptyEdgeData

  fun init_psubst_from_data _ _ = LinratAngleExpr.empty_match
  val solve_psubst = Seq.single
end

structure Substr_Linrat_Theory = GraphicalTheory(structure Data = Substr_Linrat_Data)


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

