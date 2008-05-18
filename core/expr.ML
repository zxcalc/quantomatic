(* Variable names in expressions *)
structure VName :> STR_NAME = StrName;
install_pp (make_pp ["VName", "T"] (Pretty.pprint o VName.pretty));

(* Basic stuff for an expression which might have variables *)
signature EXPR =
sig 
  (* NOTE: 
     VName.T = type of variables allowed in expressions 
     T VName.NTab.T = mapping of vars to expressions *)
  exception BadExpression of string;
  type T; (* expressions *)
  val ord : T * T -> General.order;
  val eq : T -> T -> bool;
  val get_vars : T -> VName.NTab.N.T;
  val subs : T VName.NTab.T -> T -> T;
  val match : T -> T -> T VName.NTab.T -> T VName.NTab.T option;
  val pretty : T -> Pretty.T;
	val print : T -> unit;
end
