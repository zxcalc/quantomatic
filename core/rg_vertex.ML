
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
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
structure RGVertex
: RG_VERTEX
= struct

  type name = NName.T
  structure NSet = NName.NSet;
  structure NTab = NName.NTab;
  val default_name = "v_a";
  val pretty_name = NName.pretty;
  val print_name = NName.print;
  val name_eq = uncurry NName.eq;
  val name_ord = NName.ord;

  type angle = AngleExpr.T;
  val angle_ord = AngleExpr.ord;
  datatype qnd = Xnd of angle (* Red: defined using H of Z *) 
               | Znd of angle (* Green *)
               | Hnd; (* Hadamard node *)
  type data = qnd;

  fun data_ord (Hnd, Hnd) = EQUAL
    | data_ord (Hnd, _) = LESS
    | data_ord (_, Hnd) = GREATER
    | data_ord (Znd a, Znd b) = angle_ord (a,b)
    | data_ord (Znd _, _) = LESS
    | data_ord (_, Znd _) = GREATER
    | data_ord (Xnd a, Xnd b) = angle_ord (a,b);
  fun data_eq (n1, n2) = case node_ord (n1, n2) of EQUAL => true | _ => false;

  fun pretty_angle a = Pretty.str a;
  val print_angle = Pretty.writeln o pretty_angle;
      
  fun pretty_data (Xnd a) = 
      Pretty.block [Pretty.str "X(", pretty_angle a, Pretty.str ")"]  
    | pretty_data (Znd a) = 
      Pretty.block [Pretty.str "Z(", pretty_angle a, Pretty.str ")"]
    | pretty_data Hnd = Pretty.str "H";
  val print_data = Pretty.writeln o pretty_data;

end;
