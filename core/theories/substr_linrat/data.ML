structure Substr_Linrat_Data =
struct
  val pretty_theory_name = Pretty.str "substr_linrat"
  type psubst = LinratAngleMatcher.psubst
  type subst  = LinratAngleMatcher.subst

  type nvdata = string * LinratAngleExpr.T
  val default_nvdata = ("",LinratAngleExpr.zero)

  fun default_nvdata_of_typestring "Default" = default_nvdata
    | default_nvdata_of_typestring s = raise unknown_typestring_exp s

  fun nvdata_eq ((s1,a1),(s2,a2)) = s1 = s2 andalso LinratAngleExpr.eq a1 a2
      
  fun pretty_nvdata (s,a) = 
        Pretty.block [Pretty.str s, Pretty.str "(", LinratAngleExpr.pretty a, Pretty.str ")"]
  
  fun match_nvdata ((s1,a1),(s2,a2)) m =
        if String.isPrefix s1 s2
        then LinratAngleMatcher.match (a1,a2) m
        else NONE

  fun subst_in_nvdata u (s,a) =
    let val (sub',a') = LinratAngleMatcher.subst_in_expr u a
    in (sub', (s,a')) end

  open EmptyEdgeData
  open NoXNames

  local
    fun pull_names (nvtab,_) = X.NSet.empty
      |> VTab.fold (
           fn (_,(_,a)) => X.NSet.union_merge (LinratAngleExpr.free_vars a)
         ) nvtab
  in
    fun init_psubst_from_data p_data t_data =
      LinratAngleMatcher.init_psubst_from_names (pull_names p_data, pull_names t_data)
  end

  fun solve_psubst ps = Seq.single (LinratAngleMatcher.solve_psubst ps)
end
