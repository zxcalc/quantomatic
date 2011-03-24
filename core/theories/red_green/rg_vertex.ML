(* angle expressions *)
structure AngleExpr = LinratExpr;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  internal vertex data for red-green vertex (i.e. before Graph Vertex 
    rather than OGraph Vertex.) *)
signature RG_UNIFIABLE_DATA =    
sig
  datatype qnd = Xnd of AngleExpr.T (* Red: defined using H of Z *) 
               | Znd of AngleExpr.T (* Green *)
               | Hnd; (* Hadamard node *)
  include UNIFIABLE_DATA
  sharing type data = qnd;
  val angle_of_data : data -> AngleExpr.T option;
  val map_to_angle : (AngleExpr.T -> AngleExpr.T) -> data -> data; 
end;

(* reg-green vertex data *)
structure RG_VertexData : RG_UNIFIABLE_DATA
= struct
  exception unimplemented_exp of string;

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
  
  type subst = AngleExpr.match;
  val empty_subst = AngleExpr.empty_match;
  fun compose_subst (u1,u2) = AngleExpr.compose_matches u1 u2;
  fun match_data ((e1 as Hnd), (e2 as Hnd)) m = SOME m
    | match_data ((Znd a1),(Znd a2)) m = 
      AngleExpr.match a1 a2 m
    | match_data ((Xnd a1),(Xnd a2)) m = 
      AngleExpr.match a1 a2 m
    | match_data _ _ = NONE;
  fun unify_data (x,y) m = 
      raise unimplemented_exp "RGVertex.unify_data: has not yet been written.";
  fun subst_in_data u Hnd = Hnd
    | subst_in_data u (Xnd a) = Xnd (AngleExpr.subst u a)
    | subst_in_data u (Znd a) = Znd (AngleExpr.subst u a);
    
  val pretty_subst = AngleExpr.pretty_match;
  val print_subst = Pretty.writeln o pretty_subst;

  fun map_to_angle f (Xnd angle) = (Xnd (f angle))
    | map_to_angle f (Znd angle) = (Xnd (f angle))
    | map_to_angle f x = x;

  fun angle_of_data (Xnd angle) = SOME angle
    | angle_of_data (Znd angle) = SOME angle
    | angle_of_data _ = NONE;
end;



(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
structure RG_InternVertex
: VERTEX
= struct
  
  open RG_VertexData;
  open SStrName;

  val default_name = mk "a";
  val default_data = RG_VertexData.Xnd AngleExpr.zero;

  structure Map = NameMapFun(structure Dom = SStrName and Cod = SStrName);
  structure InjEndo = NameInjEndoFun(SStrName);

  structure SharingWithData = struct 
    open Sharing; 
    type data = RG_VertexData.data;
    type subst = RG_VertexData.subst;
    structure Map = Map.Sharing;
    structure InjEndo = InjEndo.Sharing;
  end;
  (* Note signature constraint hides stuff for us *)
  structure SharingWithoutData = SharingWithData;
  
  val default_name = mk "a";

end;



  (* *)


  (* *)
(* 
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  OpenVertex for red-green theory; data is now internal 
    For functor application: OVertex(RG_IVERTEX), with some extras
*)
(* 

signature ANGLEDATA_VERTEX
= sig
  include VERTEX
  val angle_of_data : data -> AngleExpr.T option;
  val map_to_angle : (AngleExpr.T -> AngleExpr.T) -> data -> data; 
end;

signature ANGLEDATA_OVERTEX = 
sig
  include OVERTEX;
  val angle_of_data : data -> AngleExpr.T option;
  val map_to_angle : (AngleExpr.T -> AngleExpr.T) -> data -> data; 
end;

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
*)
  (*   structure V = OVertexFun(
 
    structure VertexNames = 
    );
  open V;
  *)
  (* 
  fun map_to_angle f (IVert (Xnd angle)) = (IVert (Xnd (f angle)))
    | map_to_angle f (IVert (Znd angle)) = (IVert (Xnd (f angle)))
    | map_to_angle f x = x;

  fun angle_of_data (IVert (Xnd angle)) = SOME angle
    | angle_of_data (IVert (Znd angle)) = SOME angle
    | angle_of_data _ = NONE;

  val default_name = mk "a";
  *)
