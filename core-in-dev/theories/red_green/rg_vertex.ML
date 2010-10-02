(* angle expressions *)
structure AngleExpr = LinratExpr

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Vertices and Edges for quantum graphs *)
signature RG_VERTEX
= sig
  datatype qnd = Xnd of AngleExpr.T (* Red: defined using H of Z *) 
               | Znd of AngleExpr.T (* Green *)
               | Hnd; (* Hadamard vertex *)
  include OVERTEX
  sharing type IData.data = qnd;
  val angle_of_data : data -> AngleExpr.T option;
  val map_to_angle : (AngleExpr.T -> AngleExpr.T) -> data -> data; 
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
structure RGVertex
: RG_VERTEX
= struct

  datatype qnd = Xnd of AngleExpr.T (* Red: defined using H of Z *) 
               | Znd of AngleExpr.T (* Green *)
               | Hnd; (* Hadamard node *)

  structure VName = AngleExpr.VName;

  structure V = OVertexFun(
    structure IData = 
    struct
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
      
      type unif = AngleExpr.T VName.NTab.T
      val empty_unif = VName.NTab.empty;
      fun compose_unif (u1,u2) = VName.NTab.map_all (AngleExpr.subs u2) u1
      fun unify_data ((e1 as Hnd), (e2 as Hnd)) m = SOME m
        | unify_data ((Znd a1),(Znd a2)) m = 
          AngleExpr.match a1 a2 m
        | unify_data ((Xnd a1),(Xnd a2)) m = 
          AngleExpr.match a1 a2 m
        | unify_data _ _ = NONE;
      fun subst_in_data u Hnd = Hnd
        | subst_in_data u (Xnd a) = Xnd (AngleExpr.subs u a)
        | subst_in_data u (Znd a) = Znd (AngleExpr.subs u a);
        
      fun pretty_unif u = Pretty.chunks (map (fn (n,e)
                                              => Pretty.block [Pretty.str n,
                                                               Pretty.str " = ",
                                                               AngleExpr.pretty e]
                                             )
                                             (VName.NTab.list_of u));
      val print_unif = Pretty.writeln o pretty_unif;
  
    end
    structure VertexNames = SStrName
    ); (* internal OVertex structure *)

  open V;
  
  (* *)
  fun map_to_angle f (IVert (Xnd angle)) = (IVert (Xnd (f angle)))
    | map_to_angle f (IVert (Znd angle)) = (IVert (Xnd (f angle)))
    | map_to_angle f x = x;

  fun angle_of_data (IVert (Xnd angle)) = SOME angle
    | angle_of_data (IVert (Znd angle)) = SOME angle
    | angle_of_data _ = NONE;

  val default_name = mk "a";

end;
