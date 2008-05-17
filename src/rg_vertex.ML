
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Angle expressions stub *)
structure AngleExpr : EXPR 
= struct
  exception BadExpression of string;

  type T = string;
  type varexpr = T;
	val ord = String.compare;
  fun eq x y = x = y;
  fun get_vars e = VName.NTab.N.empty;
  fun subs t e = e;
  fun match pate e tab = if pate = e then SOME tab else NONE;
  fun pretty e = Pretty.str e;
  val print = Pretty.writeln o pretty;
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Vertices and Edges for quantum graphs *)
signature RG_VERTEX
= sig
  type angle = AngleExpr.T;
  val angle_ord : angle * angle -> General.order
  datatype qnd = Xnd of angle (* Red: defined using H of Z *) 
               | Znd of angle (* Green *)
               | Hnd; (* Hadamard vertex *)
  include NAME_AND_DATA sharing type data = qnd;
  val Edge : edge;
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
structure RG_NE
: RG_VERTEX
= struct

structure NName = NName;
structure EName = EName;

type angle = AngleExpr.T;

val angle_ord = AngleExpr.ord;

datatype qnd = Xnd of angle (* Red: defined using H of Z *) 
             | Znd of angle (* Green *)
             | Hnd; (* Hadamard node *)
type node = qnd;
datatype edge = Edge;

val default_nname = NName.mk "n_a";
val default_ename = EName.mk "e_a";

fun node_ord (Hnd, Hnd) = EQUAL
  | node_ord (Hnd, _) = LESS
  | node_ord (_, Hnd) = GREATER
  | node_ord (Znd a, Znd b) = angle_ord (a,b)
  | node_ord (Znd _, _) = LESS
  | node_ord (_, Znd _) = GREATER
  | node_ord (Xnd a, Xnd b) = angle_ord (a,b);
fun node_eq n1 n2 = case node_ord (n1, n2) of EQUAL => true | _ => false;

fun edge_ord (Edge,Edge) = EQUAL;
fun edge_eq e1 e2 = case edge_ord (e1, e2) of EQUAL => true | _ => false;

fun pretty_angle a = Pretty.str a;
val print_angle = Pretty.writeln o pretty_angle;
    
fun pretty_node (Xnd a) = 
    Pretty.block [Pretty.str "X(", pretty_angle a, Pretty.str ")"]  
  | pretty_node (Znd a) = 
    Pretty.block [Pretty.str "Z(", pretty_angle a, Pretty.str ")"]
  | pretty_node Hnd = Pretty.str "H";
val print_node = Pretty.writeln o pretty_node;

fun pretty_edge Edge = Pretty.str "<edge>"
val print_edge = Pretty.writeln o pretty_edge;

end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
structure NE = RG_NE;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  matching and substitution for Basic Quantum Expressions *)
structure BasicQuExpr 
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
