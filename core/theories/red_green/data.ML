structure RG_Data =
struct
  val pretty_theory_name = Pretty.str "red_green"
  type psubst = LinratAngleMatcher.psubst
  type subst  = LinratAngleMatcher.subst

  datatype nvdata = Xnd of LinratAngleExpr.T (* Red: defined using H of Z *) 
                  | Znd of LinratAngleExpr.T (* Green *)
                  | Hnd (* Hadamard node *)
                  | Var of string
  val default_nvdata = Znd LinratAngleExpr.zero

  fun default_nvdata_of_typestring s =
    case s of "X" => Xnd LinratAngleExpr.zero
            | "Z" => Znd LinratAngleExpr.zero
            | "hadamard" => Hnd
            | "var" => Var ""
            | _ => raise unknown_typestring_exp s

  fun nvdata_eq (Hnd, Hnd) = true
    | nvdata_eq (Znd a, Znd b) = LinratAngleExpr.eq a b
    | nvdata_eq (Xnd a, Xnd b) = LinratAngleExpr.eq a b
    | nvdata_eq (Var s, Var t) = (s = t)
    | nvdata_eq _ = false
      
  fun pretty_nvdata (Xnd a) = 
      Pretty.block [Pretty.str "X(", LinratAngleExpr.pretty a, Pretty.str ")"]  
    | pretty_nvdata (Znd a) = 
      Pretty.block [Pretty.str "Z(", LinratAngleExpr.pretty a, Pretty.str ")"]
    | pretty_nvdata Hnd = Pretty.str "H"
    | pretty_nvdata (Var s) = Pretty.block [Pretty.str "Var(", Pretty.str s, Pretty.str ")"]
  
  fun match_nvdata (Hnd, Hnd) m = SOME m
    | match_nvdata (Znd a1,Znd a2) m = LinratAngleMatcher.match (a1,a2) m
    | match_nvdata (Xnd a1,Xnd a2) m = LinratAngleMatcher.match (a1,a2) m
    | match_nvdata (Var s, Var t) m = if s = t then SOME m else NONE
    | match_nvdata _ _ = NONE

  fun subst_in_nvdata sub Hnd = (sub, Hnd)
    | subst_in_nvdata sub (Var s) = (sub, Var s)
    | subst_in_nvdata u (Xnd a) =
        let val (sub',a') = LinratAngleMatcher.subst_in_expr u a
        in (sub', Xnd a') end
    | subst_in_nvdata u (Znd a) =
        let val (sub',a') = LinratAngleMatcher.subst_in_expr u a
        in (sub', Znd a') end

  fun xnames_in_nvdata (Znd a) = LinratAngleExpr.free_vars a
    | xnames_in_nvdata (Xnd a) = LinratAngleExpr.free_vars a
    | xnames_in_nvdata _ = X.NSet.empty

  fun rename_in_nvdata _ d = d

  open EmptyEdgeData

  local
    fun pull_names (nvtab,_) = X.NSet.empty
      |> VTab.fold (
           fn (_,Znd a) => X.NSet.union_merge (LinratAngleExpr.free_vars a)
            | (_,Xnd a) => X.NSet.union_merge (LinratAngleExpr.free_vars a)
            | _ => I
         ) nvtab
  in
    fun init_psubst_from_data p_data t_data =
      LinratAngleMatcher.init_psubst_from_names (pull_names p_data, pull_names t_data)
  end
  
  fun solve_psubst ps = Seq.single (LinratAngleMatcher.solve_psubst ps)
end