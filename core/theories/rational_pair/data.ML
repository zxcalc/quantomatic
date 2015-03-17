structure RP_Data =
struct
  val pretty_theory_name = Pretty.str "rational_pair"
  type psubst = LinratMatcher.psubst
  type subst  = LinratMatcher.subst

  datatype nvdata = Bnd of LinratExpr.T (* Black node *) 
                  | Wnd of LinratExpr.T (* White node *)
  val default_nvdata = Bnd LinratExpr.zero

  fun default_nvdata_of_typestring s =
    case s of "B" => Bnd LinratExpr.zero
            | "W" => Wnd LinratExpr.zero
            | _ => raise unknown_typestring_exp s

  fun nvdata_eq (Bnd a, Bnd b) = LinratExpr.eq a b
    | nvdata_eq (Wnd a, Wnd b) = LinratExpr.eq a b
    | nvdata_eq _ = false
      
  fun pretty_nvdata (Bnd a) = 
      Pretty.block [Pretty.str "B(", LinratExpr.pretty a, Pretty.str ")"]  
    | pretty_nvdata (Wnd a) = 
      Pretty.block [Pretty.str "W(", LinratExpr.pretty a, Pretty.str ")"]
  
  fun match_nvdata (Bnd a1,Bnd a2) m = LinratMatcher.match (a1,a2) m
    | match_nvdata (Wnd a1,Wnd a2) m = LinratMatcher.match (a1,a2) m
    | match_nvdata _ _ = NONE

  fun subst_in_nvdata u (Bnd a) =
        let val (sub',a') = LinratMatcher.subst_in_expr u a
        in (sub', Bnd a') end
    | subst_in_nvdata u (Wnd a) =
        let val (sub',a') = LinratMatcher.subst_in_expr u a
        in (sub', Wnd a') end

  open EmptyEdgeData
  open NoXNames

  local
    fun pull_names (nvtab,_) = X.NSet.empty
      |> VTab.fold (
           fn (_,Bnd a) => X.NSet.union_merge (LinratExpr.free_vars a)
            | (_,Wnd a) => X.NSet.union_merge (LinratExpr.free_vars a)
         ) nvtab
  in
    fun init_psubst_from_data p_data t_data =
      LinratMatcher.init_psubst_from_names (pull_names p_data, pull_names t_data)
  end
  
  fun solve_psubst ps = Seq.single (LinratMatcher.solve_psubst ps)
end