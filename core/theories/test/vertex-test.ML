(* TODO: provide link to visualisation XML *)
(* val rg_gui_spec_xml = XMLReader.read_from_file "gui.xml"; *)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  internal vertex data for red-green vertex (i.e. before Graph Vertex 
    rather than OGraph VData.) *)
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
structure Test_InternVData : TEST_UNIFIABLE_DATA
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
      raise unimplemented_exp "Test_VData.unify_data: has not yet been written.";
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
    
  val default_data = Xnd LinratExpr.zero;
  
  structure Sharing = struct type data = data; type subst = subst; end
end;

(*  *)
structure Test_VertexComponentData_Param
: ELEMENT_COMPONENT_DATA_PARAM where type data = Test_InternVData.data
= struct 
  structure VData = Test_InternVData;
  type data = VData.data;
  exception unknown_type_exp of string;
  exception unexpected_data_exp;

  fun type_of (VData.Xnd _) = "X"
    | type_of (VData.Znd _) = "Z"
    | type_of (VData.Hnd)   = "hadamard";

  fun cdata_of (VData.Xnd a) = (ComponentData.LinratExpr a)
    | cdata_of (VData.Znd a) = (ComponentData.LinratExpr a)
    | cdata_of (VData.Hnd)   = ComponentData.Unit;

  fun default_data "X" = (VData.Xnd LinratExpr.zero)
    | default_data "Z" = (VData.Znd LinratExpr.zero)
    | default_data "hadamard" = (VData.Hnd)
    | default_data s = raise unknown_type_exp s

  fun unwrapUpd upd a =
    let
      fun unwrapLinrat (ComponentData.LinratExpr e) = e
        | unwrapLinrat _ = raise unexpected_data_exp
    in
      unwrapLinrat (upd (ComponentData.LinratExpr a))
    end;

  fun update upd (VData.Xnd a) = (VData.Xnd (unwrapUpd upd a))
    | update upd (VData.Znd a) = (VData.Znd (unwrapUpd upd a))
    | update _   (VData.Hnd)   = VData.Hnd;
end;

