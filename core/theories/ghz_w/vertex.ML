signature GHZW_UNIFIABLE_DATA =    
sig
  datatype node = GHZ | W;
  include UNIFIABLE_DATA
  sharing type data = node;
end;

structure GHZW_VertexData
: GHZW_UNIFIABLE_DATA
= struct
  datatype node = GHZ | W;
  type data = node;
  fun data_ord (W,GHZ) = GREATER
    | data_ord (GHZ,W) = LESS
    | data_ord (_,_) = EQUAL;
  fun data_eq (n1, n2) = case data_ord (n1, n2) of EQUAL => true | _ => false;
  fun pretty_data GHZ = Pretty.str "GHZ"
    | pretty_data W = Pretty.str "W";
  val print_data = Pretty.writeln o pretty_data;
    
  fun match_data (GHZ, GHZ) m = SOME m
    | match_data (W,W) m = SOME m
    | match_data _ m = NONE;
  fun unify_data _ _ = SOME ();
    
  type subst = unit;
  val empty_subst = ();
  fun compose_subst _ = ();
  fun subst_in_data _ d = d;  
  
  fun pretty_subst _ = Pretty.str "";
  val print_subst = Pretty.writeln o pretty_subst;
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
structure GHZW_Vertex
: VERTEX
= struct
  
  open GHZW_VertexData;
  open SStrName;

  val default_name = mk "a";
  val default_data = GHZW_VertexData.GHZ;

  structure Map = NameMapFun(structure Dom = SStrName and Cod = SStrName);
  structure InjEndo = NameInjEndoFun(SStrName);

  structure SharingWithData = struct 
    open Sharing; 
    type data = GHZW_VertexData.data;
    type subst = GHZW_VertexData.subst;
    structure Map = Map.Sharing;
    structure InjEndo = InjEndo.Sharing;
  end;
  (* Note signature constraint hides stuff for us *)
  structure SharingWithoutData = SharingWithData;
  
  val default_name = mk "a";

end;

(*  *)
structure GHZW_VertexComponentData_Param
: ELEMENT_COMPONENT_DATA_PARAM where type data = GHZW_VertexData.data
= struct 
  type data = GHZW_VertexData.data;
  exception unknown_type_exp of string;

  fun type_of GHZW_VertexData.GHZ = "GHZ"
    | type_of GHZW_VertexData.W = "W"

  fun cdata_of _ = ComponentData.Unit;

  fun default_data "GHZ" = GHZW_VertexData.GHZ
    | default_data "W" = GHZW_VertexData.W
    | default_data n = raise unknown_type_exp n;

  fun update _ = I;
end;
