structure VName = StrName :> NAME;

signature EXPR =
sig 

  type T;  (* type of expressions *)
  type var = VName.T; (* type of variables allowed in expressions *)
  type table = T VName.NTab.T; (* mapping var -> T *)

  exception BadExpression of string;

  val equal : T -> T -> bool;
  val reduce : T -> T;
  val get_vars : T -> VName.Col.T;
  val pretty_print : T -> Pretty.T;

  val subs : table -> T -> T; (* The table gives the value of variables,
				     returns new expression *)

  val match : T -> T -> table -> table; (* arg 1 = pattern,
					       arg 2 = "ground" term
					       arg 3 = matches done so far
					       return final matches *)
end
