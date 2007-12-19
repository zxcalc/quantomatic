(* Variable names in expressions *)
structure VName = StrName :> STR_NAME;
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

(* Graph Data Expressions - the part of the graph data that has basic
   expresion is the varexp *)
signature GDATA_EXPR =
sig 
  type T; (* expressions *)
  type varexpr; (* type of things that instantiate vars - maybe subtype of T *)
  val ord : T * T -> General.order;
  val eq : T -> T -> bool;
  val get_vars : T -> VName.NTab.N.T;
  val subs : varexpr VName.NTab.T -> T -> T;
  val match : T -> T -> varexpr VName.NTab.T -> varexpr VName.NTab.T option;

  val pretty_varexp : varexpr -> Pretty.T;
	val print_varexp : varexpr -> unit;
  val pretty : T -> Pretty.T;
	val print : T -> unit;
end

(* dummy Expr stub strings: there are no vars, matching = eq *)
structure StrExpr : GDATA_EXPR =
struct 
  type varexpr = unit;
  type T = StrNE.node;
	val ord = StrNE.node_ord;
  fun eq x y = x = y;
  fun get_vars e = VName.NTab.N.empty;
  fun subs t e = e;
  fun match pate e tab = if pate = e then SOME tab else NONE;
  fun pretty_varexp e = Pretty.str "unimplemented";
  val print_varexp = Pretty.writeln o pretty_varexp;
  fun pretty e = Pretty.str "unimplemented";
  val print = Pretty.writeln o pretty;
end
