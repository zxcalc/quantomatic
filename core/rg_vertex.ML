(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Vertices and Edges for quantum graphs *)
signature RG_VERTEX
= sig
  datatype qnd = Xnd of AngleExpr.T (* Red: defined using H of Z *) 
               | Znd of AngleExpr.T (* Green *)
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
  val default_name = "Va";
  val pretty_name = NName.pretty;
  val print_name = NName.print
  val name_eq = uncurry NName.eq;
  val name_ord = NName.ord;

  datatype qnd = Xnd of AngleExpr.T (* Red: defined using H of Z *) 
               | Znd of AngleExpr.T (* Green *)
               | Hnd; (* Hadamard node *)
  type data = qnd;

  fun data_ord (Hnd, Hnd) = EQUAL
    | data_ord (Hnd, _) = LESS
    | data_ord (_, Hnd) = GREATER
    | data_ord (Znd a, Znd b) = AngleExpr.ord (a,b)
    | data_ord (Znd _, _) = LESS
    | data_ord (_, Znd _) = GREATER
    | data_ord (Xnd a, Xnd b) = AngleExpr.ord (a,b);
  fun data_eq (n1, n2) = case data_ord (n1, n2) of EQUAL => true | _ => false;

  fun pretty_angle a = Pretty.str a;      
  fun pretty_data (Xnd a) = 
      Pretty.block [Pretty.str "X(", AngleExpr.pretty a, Pretty.str ")"]  
    | pretty_data (Znd a) = 
      Pretty.block [Pretty.str "Z(", AngleExpr.pretty a, Pretty.str ")"]
    | pretty_data Hnd = Pretty.str "H";
  val print_data = Pretty.writeln o pretty_data;

end;
