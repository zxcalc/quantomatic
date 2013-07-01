structure RG_Data =
struct
  val pretty_theory_name = Pretty.str "red_green"
  type psubst = LinratAngleExpr.match
  type subst  = psubst

  datatype nvdata = Xnd of LinratAngleExpr.T (* Red: defined using H of Z *) 
                  | Znd of LinratAngleExpr.T (* Green *)
                  | Hnd (* Hadamard node *)
  val default_nvdata = Znd LinratAngleExpr.zero

  val nvdata_typestrings = ["X","Z","hadamard"]
  fun default_nvdata_of_typestring s =
    case s of "X" => Xnd LinratAngleExpr.zero
            | "Z" => Znd LinratAngleExpr.zero
            | "hadamard" => Hnd
            | _ => raise unknown_typestring_exp s
  fun typestring_of_nvdata d =
    case d of Xnd _ => "X"
            | Znd _ => "Z"
            | Hnd => "hadamard"

  fun nvdata_eq (Hnd, Hnd) = true
    | nvdata_eq (Znd a, Znd b) = LinratAngleExpr.eq a b
    | nvdata_eq (Xnd a, Xnd b) = LinratAngleExpr.eq a b
    | nvdata_eq _ = false
      
  fun pretty_nvdata (Xnd a) = 
      Pretty.block [Pretty.str "X(", LinratAngleExpr.pretty a, Pretty.str ")"]  
    | pretty_nvdata (Znd a) = 
      Pretty.block [Pretty.str "Z(", LinratAngleExpr.pretty a, Pretty.str ")"]
    | pretty_nvdata Hnd = Pretty.str "H"
  
  fun match_nvdata (Hnd, Hnd) m = SOME m
    | match_nvdata (Znd a1,Znd a2) m = LinratAngleExpr.match a1 a2 m
    | match_nvdata (Xnd a1,Xnd a2) m = LinratAngleExpr.match a1 a2 m
    | match_nvdata _ _ = NONE

  fun subst_in_nvdata _ Hnd = Hnd
    | subst_in_nvdata u (Xnd a) = Xnd (LinratAngleExpr.subst u a)
    | subst_in_nvdata u (Znd a) = Znd (LinratAngleExpr.subst u a)

  open EmptyEdgeData

  fun init_psubst_from_data _ _ = LinratAngleExpr.empty_match
  val solve_psubst = Seq.single
end

structure RG_Theory = GraphicalTheory(structure Data = RG_Data)


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
