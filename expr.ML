(* Variable names in expressions *)
structure VName = StrName :> STR_NAME;

(* Basic stuff for an expression which might have variables *)
signature BASIC_EXPR =
sig 
  (* NOTE: 
     VName.T = type of variables allowed in expressions 
     T VName.NTab.T = mapping of vars to expressions *)
  type T; (* expressions *)
  type varexpr; (* type of things that instantiate vars - maybe subtype of T *)
  val ord : T * T -> General.order;
  val eq : T -> T -> bool;
  val get_vars : T -> VName.Col.T;
  val subs : varexpr VName.NTab.T -> T -> T;
  val match : T -> T -> varexpr VName.NTab.T -> varexpr VName.NTab.T option;

  val pretty : T -> Pretty.T;
	val print : T -> unit;
end

(* extra stuff for expressions... ? *)
signature EXPR =
sig 
	include BASIC_EXPR;
  exception BadExpression of string;
end;

(* dummy Expr stub strings: there are no vars, matching = eq *)
structure StrExpr : BASIC_EXPR =
struct 
  type varexpr = unit;
  type T = StrNE.node;
	val ord = StrNE.node_ord;
  fun eq x y = x = y;
  fun get_vars e = VName.Col.empty;
  fun subs t e = e;
  fun match pate e tab = if pate = e then SOME tab else NONE;
  fun pretty e = Pretty.str "unimplemented";
  val print = Pretty.writeln o pretty;
end
