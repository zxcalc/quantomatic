structure VName = StrName :> NAME;

signature EXPR =
sig 

  type T;  (* type of exprressions *)
  type var = VName.T; (* type of variables allowed in expressions *)
  type table = T VName.NTab.T; (* mapping var -> expr *)

  exception BadExpression of string;

  val equal : expr -> expr -> bool;
  val reduce : expr -> expr;
  val get_vars : expr -> VName.Col.T;
  val pretty_print : expr -> Pretty.T;

  val subs : table -> expr -> expr; (* The table gives the value of variables,
				     returns new expression *)

  val match : expr -> expr -> table -> table; (* arg 1 = pattern,
					       arg 2 = "ground" term
					       arg 3 = matches done so far
					       return final matches *)
end
