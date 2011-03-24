(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  variable-verticies / boundary-verticies  *)

signature OVERTEX_SHARING
= sig
  include NAME_AND_UNIFIABLE_DATA_SHARING
  type idata
end;

signature OVERTEX
= sig
  
  (* internal data *)
  structure IData : UNIFIABLE_DATA 
  
  datatype external_data = 
           IVert of IData.data (* inner node with basic vertex data *)
         | EVert; (* boundary vertex with boundary data *)
  
  include VERTEX
  sharing type data = external_data
  
  val is_internal_data : data -> bool
  

  structure SharingWithIData : OVERTEX_SHARING
  sharing type subst = IData.subst; 
  sharing SharingWithData = SharingWithIData;
  sharing type IData.data = SharingWithIData.idata;

  val apply_to_idata : (IData.data -> IData.data) -> data -> data
  val idata_of : data -> IData.data option

end;


signature OVERTEX_DATA = 
sig
  (* internal data *)
  structure IData : UNIFIABLE_DATA
  datatype external_data = 
           IVert of IData.data (* inner node with basic vertex data *)
         | EVert; (* boundary vertex with boundary data *)
  include UNIFIABLE_DATA
  sharing type data = external_data
  sharing type subst = IData.subst
  val is_internal_data : data -> bool
  val apply_to_idata : (IData.data -> IData.data) -> data -> data
  val idata_of : data -> IData.data option
end;


(* *)
functor OVertexData(D : UNIFIABLE_DATA) 
= struct
  structure IData = D; 
  
  (* internal data *)
  datatype external_data = 
           IVert of IData.data (* vertex with data *)
         | EVert; (* a variable vertex *)
  
  fun is_internal_data (IVert _) = true
    | is_internal_data _ = false;
  
  fun apply_to_idata f (IVert x) = IVert (f x)
    | apply_to_idata f EVert = EVert;

  fun idata_of (IVert x) = SOME x
    | idata_of EVert = NONE;

  type data = external_data;
  
  fun data_ord (EVert, EVert) = EQUAL
    | data_ord (EVert, _) = LESS
    | data_ord (_, EVert) = GREATER
    | data_ord (IVert a, IVert b) = IData.data_ord (a,b);
  
  fun data_eq (n1, n2) = case data_ord (n1, n2) of EQUAL => true | _ => false;
  
  fun subst_in_data u EVert = EVert
    | subst_in_data u (IVert a) = IVert (IData.subst_in_data u a);
  fun pretty_data (EVert) = 
      Pretty.block [Pretty.str "EVert"]
    | pretty_data (IVert a) = 
      Pretty.block [Pretty.str "IVert(", IData.pretty_data a, Pretty.str ")"];
  val print_data = Pretty.writeln o pretty_data;

  val default_data = EVert; (* TODO: IData.default_data; make default data part
                              of Unifiable data structure *)

  type subst = IData.subst;
  val empty_subst = IData.empty_subst;
  val compose_subst = IData.compose_subst;
  fun unify_data ((IVert d1),(IVert d2)) u = IData.unify_data (d1,d2) u
    | unify_data (EVert,EVert) u = SOME u
    | unify_data _ _ = NONE;
  fun match_data ((IVert d1),(IVert d2)) u = IData.match_data (d1,d2) u
    | match_data (EVert,EVert) u = SOME u
    | match_data _ _ = NONE;
  val pretty_subst = IData.pretty_subst;
  val print_subst = IData.print_subst;
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Pattern Nodes construction *)
functor OVertex(V : VERTEX)
: OVERTEX
= struct
  (* signature restriction means we get just the unifiable data part of
     Vertex *)
  structure IData : UNIFIABLE_DATA = V; 
  
  (* make the new external data *)
  structure ExternalData = OVertexData(IData);  
  open ExternalData;

  (* open the Names Part of the Vertex structure *)
  structure VertexNames : SSTR_NAMES = V;
  open VertexNames; 

  (* also get the extra maps stuff in Vertex... TODO: push this into Names *)
  structure Map = V.Map;
  (* NameMapFun(structure Dom = VertexNames and Cod = VertexNames); *)
  structure InjEndo = V.InjEndo;
  (* NameInjEndoFun(VertexNames); *)
  
  (* create the extended sharing structures *)
  structure SharingWithData = struct 
    open Sharing; type data = data; type subst = subst;
    structure Map = Map.Sharing;
    structure InjEndo = InjEndo.Sharing;
  end;
  structure SharingWithIData = struct 
    open SharingWithData; type idata = IData.data; type subst = subst;
  end;
  structure SharingWithoutData = SharingWithData; 

end;


(* as above, but creates new Map and InjEndo's for the name structure 
   TODO: push Map and InjEndo into Names, then this becomes just an pair of
   opens... 
*)
functor OVertexOfDataAndNames(
  structure IData : UNIFIABLE_DATA
  structure VertexNames : SSTR_NAMES
)
: OVERTEX
= struct
  structure IData : UNIFIABLE_DATA = IData;
  
  (* make the new external data *)
  structure ExternalData = OVertexData(IData);  
  open ExternalData;

  (* open the Names Part of IVertex *)
  structure VertexNames : SSTR_NAMES = VertexNames;
  open VertexNames; 
  
  structure Map = NameMapFun(structure Dom = VertexNames and Cod = VertexNames);
  structure InjEndo = NameInjEndoFun(VertexNames);

  (* create the extended sharing structures *)
  structure SharingWithData = struct 
    open Sharing; type data = data; type subst = subst;
    structure Map = Map.Sharing;
    structure InjEndo = InjEndo.Sharing;
  end;
  structure SharingWithIData = struct 
    open SharingWithData; type idata = IData.data; type subst = subst;
  end;
  structure SharingWithoutData = SharingWithData; 
end;
