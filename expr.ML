structure VName = StrName :> NAME;

signature EXPR =
sig 

  type T;  (* type of expressions *)
  type var = VName.T; (* type of variables allowed in expressions *)
  type table = T VName.NTab.T; (* mapping var -> T *)

  exception BadExpression of string;

  val ord : T * T -> General.order;
  val equal : T -> T -> bool;
  val reduce : T -> T;
  val get_vars : T -> VName.Col.T;
  val pretty : T -> Pretty.T;

  val subs : table -> T -> T; (* The table gives the value of variables,
				     returns new expression *)

  val match : T -> T -> table -> table; (* arg 1 = pattern,
					       arg 2 = "ground" term
					       arg 3 = matches done so far
					       return final matches *)
end

(* dummy Expr stub for typechecking to continue *)
structure Expr : EXPR =
struct 
  type T = string;
  type var = VName.T; (* type of variables allowed in expressions *)
  type table = T VName.NTab.T; (* mapping var -> T *)
  exception BadExpression of string;
	val ord = String.compare;
  fun equal x y = x = y;
  fun reduce e = e;
  fun get_vars e = VName.Col.empty;
  fun pretty e = Pretty.str "unimplemented";
  fun subs t e = e;
  fun match pate e tab = tab;
end
