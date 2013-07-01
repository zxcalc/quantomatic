signature LINRAT_MATCHER =
sig
  (* a matrix in Gauss normal form, along with pattern names and target names. The columns
   * of the matrix are labelled by (list_of p_names) @ (list_of t_names) @ [CONST]. Note
   * this code doesn't assume NSet.list_of returns names in a particular order, only that it
   * will always return names in the same order. *)
  type psubst = RationalMatrix.T * X.NSet.T * X.NSet.T

  (* a substitution table, along with a set of names to avoid when introducing new free variables
   * (e.g. for under-specified systems of equations) *)
  type subst = LinratAngleExpr.T XTab.T * X.NSet.T

  (* compute the matrix row corresponding to these expressions and add it with gauss_update.
   * If gauss_update retuns NONE, this signals a match failure, so return NONE.  *)
  val match : psubst -> LinratAngleExpr.T * LinratAngleExpr.T -> psubst option

  (* take a rational matrix in GNF with column labels to the associated
   * substitution map. For under-specified systems, free variables
   * will be sent to new variables fresh w.r.t. the target. *)
  val solve_psubst : psubst -> subst

  (* apply the substitution. If an uncognised variable is encountered, replace it with a
   * new fresh variable. (hence the updated subst needs to be returned) *)
  val subst_in_expr : subst -> LinratAngleExpr.T -> subst * LinratAngleExpr.T
end

structure LinratMatcher : LINRAT_MATCHER =
struct
  type psubst = RationalMatrix.T * X.NSet.T * X.NSet.T
  type subst = LinratAngleExpr.T XTab.T * X.NSet.T

  fun match (m,p_names,t_names) (p_expr, t_expr) = let
    val const = Rational.subtr (LinratAngleExpr.coeff_for_var p_expr NONE)
                               (LinratAngleExpr.coeff_for_var t_expr NONE)
    val row =
      map (fn x => (
            LinratAngleExpr.coeff_for_var p_expr (SOME x)
          )) (X.NSet.list_of p_names) @
      map (fn x => (
            Rational.negate (LinratAngleExpr.coeff_for_var t_expr (SOME x))
          )) (X.NSet.list_of t_names) @
      [const]
  in case RationalMatrix.gauss_update (X.NSet.cardinality p_names) row m
       of SOME m' => SOME (m',p_names,t_names)
        | NONE    => NONE
  end

  fun solve_psubst (m,p_names,t_names) = let
    val cols = (X.NSet.list_of p_names) @ (X.NSet.list_of t_names)
    val free = X.NSet.cardinality p_names
    fun add_entry i (j,y) (x_opt,expr,tab,avoids) =
      case x_opt
        of NONE => if Rational.is_one (RationalMatrix.get m (i,j))
                   then (SOME y, LinratAngleExpr.zero, tab, avoids)
                   else (NONE, LinratAngleExpr.zero, tab, avoids)
         | SOME x =>
            let
              val (tab',avoids',e) =
                if j < free then
                  case XTab.get_opt tab y
                    of SOME e => (tab,avoids,e)
                     | NONE   => 
                         let
                           val (fresh,avoids') = avoids |> X.NSet.add_new y
                           val fresh_e = LinratAngleExpr.mk_var fresh
                         in (tab |> XTab.add (y, fresh_e), avoids', fresh_e)
                         end
                else (tab, avoids, LinratAngleExpr.mk_var y)
            in
              (
                SOME x, 
                LinratAngleExpr.subtr_expr expr
                  (LinratAngleExpr.scale_expr (RationalMatrix.get m (i,j)) e),
                tab',
                avoids'
              )
            end
    fun add_row i (tab,avoids) =
      let
        (* extract the leading column name, along with the rest of the expression *)
        val (x_opt,expr,tab',avoids') =
          fold_index (add_entry i) cols (NONE, LinratAngleExpr.zero, tab, avoids)
        (* treat the rightmost column as the constant value *)
        val expr = LinratAngleExpr.subtr_expr expr
          (LinratAngleExpr.mk_const (RationalMatrix.get m (i, RationalMatrix.num_cols m - 1)))
      in
        case x_opt
          of SOME x =>
                if XTab.dom_contains tab x
                then raise RationalMatrix.not_in_rref_exp m
                else (tab' |> XTab.add (x,expr), avoids')
           | NONE => raise RationalMatrix.not_in_rref_exp m
      end

  in
      fold add_row
         ((RationalMatrix.num_rows m - 1) downto 0)
         (XTab.empty, t_names)
  end

  fun subst_in_expr (tab, avoids) expr = ((tab,avoids), expr)
end
