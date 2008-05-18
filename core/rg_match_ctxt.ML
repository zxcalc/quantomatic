
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  matching and substitution for Basic Quantum Expressions *)
structure RGMatchCtxt 
: GDATA_EXPR where type varexpr = AngleExpr.T
= struct
  type varexpr = AngleExpr.T;
  type T = RG_NE.node;
	val ord = RG_NE.node_ord;
  val eq = RG_NE.node_eq;

  fun get_vars RG_NE.Hnd = VName.NTab.N.empty
		| get_vars (RG_NE.Znd a) = AngleExpr.get_vars a
		| get_vars (RG_NE.Xnd a) = AngleExpr.get_vars a;

  fun subs t (e as RG_NE.Hnd) = e
		| subs t (RG_NE.Znd a) = RG_NE.Znd (AngleExpr.subs t a)
		| subs t (RG_NE.Xnd a) = RG_NE.Znd (AngleExpr.subs t a);

  fun match (e1 as RG_NE.Hnd) (e2 as RG_NE.Hnd) t = SOME t
		| match (RG_NE.Znd a1) (RG_NE.Znd a2) t = 
			AngleExpr.match a1 a2 t
		| match (RG_NE.Xnd a1) (RG_NE.Xnd a2) t = 
			AngleExpr.match a1 a2 t
		| match _ _ _ = NONE;

  val pretty_varexp = AngleExpr.pretty;
  val print_varexp = AngleExpr.print;
  val pretty = RG_NE.pretty_node;
  val print = RG_NE.print_node;
end;
