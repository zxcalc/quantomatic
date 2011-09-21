(* TODO: provide link to visualisation XML *)
(* val rg_gui_spec_xml = XMLReader.read_from_file "gui.xml"; *)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  internal vertex data for red-green vertex (i.e. before Graph Vertex 
    rather than OGraph Vertex.) *)
signature TEST_UNIFIABLE_DATA =    
sig
  datatype qnd = Xnd of LinratExpr.T (* Red: defined using H of Z *) 
               | Znd of LinratExpr.T (* Green *)
               | Hnd; (* Hadamard node *)
  include UNIFIABLE_DATA
  sharing type data = qnd;
  val angle_of_data : data -> LinratExpr.T option;
  val map_to_angle : (LinratExpr.T -> LinratExpr.T) -> data -> data; 
end;

(* reg-green vertex data *)
structure Test_VertexData : TEST_UNIFIABLE_DATA
= struct
  exception unimplemented_exp of string;

  datatype qnd = Xnd of LinratExpr.T (* Red: defined using H of Z *) 
               | Znd of LinratExpr.T (* Green *)
               | Hnd; (* Hadamard node *)
  type data = qnd; 

  fun data_ord (Hnd, Hnd) = EQUAL
    | data_ord (Hnd, _) = LESS
    | data_ord (_, Hnd) = GREATER
    | data_ord (Znd a, Znd b) = LinratExpr.ord (a,b)
    | data_ord (Znd _, _) = LESS
    | data_ord (_, Znd _) = GREATER
    | data_ord (Xnd a, Xnd b) = LinratExpr.ord (a,b);
  fun data_eq (n1, n2) = case data_ord (n1, n2) of EQUAL => true | _ => false;
      
  fun pretty_data (Xnd a) = 
      Pretty.block [Pretty.str "X(", LinratExpr.pretty a, Pretty.str ")"]  
    | pretty_data (Znd a) = 
      Pretty.block [Pretty.str "Z(", LinratExpr.pretty a, Pretty.str ")"]
    | pretty_data Hnd = Pretty.str "H";
  val print_data = Pretty.writeln o pretty_data;
  
  type subst = LinratExpr.match;
  val empty_subst = LinratExpr.empty_match;
  fun compose_subst (u1,u2) = LinratExpr.compose_matches u1 u2;
  fun match_data ((Hnd), (Hnd)) m = SOME m
    | match_data ((Znd a1),(Znd a2)) m = 
      LinratExpr.match a1 a2 m
    | match_data ((Xnd a1),(Xnd a2)) m = 
      LinratExpr.match a1 a2 m
    | match_data _ _ = NONE;
  fun unify_data (x,y) m = 
      raise unimplemented_exp "Test_Vertex.unify_data: has not yet been written.";
  fun subst_in_data _ Hnd = Hnd
    | subst_in_data u (Xnd a) = Xnd (LinratExpr.subst u a)
    | subst_in_data u (Znd a) = Znd (LinratExpr.subst u a);
    
  val pretty_subst = LinratExpr.pretty_match;
  val print_subst = Pretty.writeln o pretty_subst;

  fun map_to_angle f (Xnd angle) = (Xnd (f angle))
    | map_to_angle f (Znd angle) = (Xnd (f angle))
    | map_to_angle _ x = x;

  fun angle_of_data (Xnd angle) = SOME angle
    | angle_of_data (Znd angle) = SOME angle
    | angle_of_data _ = NONE;
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
structure Test_InternVertex
: VERTEX
= struct
  
  open Test_VertexData;
  open SStrName;

  val default_name = mk "a";
  val default_data = Test_VertexData.Xnd LinratExpr.zero;

  structure Map = NameMapFun(structure Dom = SStrName and Cod = SStrName);
  structure InjEndo = NameInjEndoFun(SStrName);

  structure SharingWithData = struct 
    open Sharing; 
    type data = Test_VertexData.data;
    type subst = Test_VertexData.subst;
    structure Map = Map.Sharing;
    structure InjEndo = InjEndo.Sharing;
  end;
  (* Note signature constraint hides stuff for us *)
  structure SharingWithoutData = SharingWithData;

end;

(*  *)
structure Test_VertexComponentData_Param
: ELEMENT_COMPONENT_DATA_PARAM where type data = Test_VertexData.data
= struct 
  type data = Test_VertexData.data;
  exception unknown_type_exp of string;
  exception unexpected_data_exp;

  fun type_of (Test_VertexData.Xnd _) = "X"
    | type_of (Test_VertexData.Znd _) = "Z"
    | type_of (Test_VertexData.Hnd)   = "hadamard";

  fun cdata_of (Test_VertexData.Xnd a) = (ComponentData.LinratExpr a)
    | cdata_of (Test_VertexData.Znd a) = (ComponentData.LinratExpr a)
    | cdata_of (Test_VertexData.Hnd)   = ComponentData.Unit;

  fun default_data "X" = (Test_VertexData.Xnd LinratExpr.zero)
    | default_data "Z" = (Test_VertexData.Znd LinratExpr.zero)
    | default_data "hadamard" = (Test_VertexData.Hnd)
    | default_data s = raise unknown_type_exp s

  fun unwrapUpd upd a =
    let
      fun unwrapLinrat (ComponentData.LinratExpr e) = e
        | unwrapLinrat _ = raise unexpected_data_exp
    in
      unwrapLinrat (upd (ComponentData.LinratExpr a))
    end;

  fun update upd (Test_VertexData.Xnd a) = (Test_VertexData.Xnd (unwrapUpd upd a))
    | update upd (Test_VertexData.Znd a) = (Test_VertexData.Znd (unwrapUpd upd a))
    | update _   (Test_VertexData.Hnd)   = Test_VertexData.Hnd;
end;

