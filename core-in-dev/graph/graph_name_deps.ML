(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  The names that a graph depends on: Vertex names, Edge names, and any 
    names that might occur inside the data on vertices and edges.  *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

signature GRAPH_NAME_DEPS_SHARING
= sig 
  structure Vertex : NAME_AND_DATA_SHARING
  structure Edge : EDGE_AND_DATA_SHARING
  type T
  type inames_renaming
end;


(* Name dependencies for Graphs: how names in internal data and vertex 
   and edge names deepend on each other. *)
signature GRAPH_NAME_DEPS
= sig

  (* Names for vertices and edges *)
  structure Vertex : VERTEX (* defined in pregraph.ML *) 
  structure Edge : EDGE (* defined in pregraph.ML *) 

  (* internal name data dependencies e.g. w.r.t. vertex names *)
  type T

  (* rename internal names in T *)
  structure CRnmINames : COMPOUND_RENAMING 
    sharing type CRnmINames.obj = T
  (* rename internal names in Vertex Data *)
  structure VDataCRnmINames : COMPOUND_RENAMING
    sharing type VDataCRnmINames.obj = Vertex.data
    sharing type VDataCRnmINames.renaming = CRnmINames.renaming
  (* rename internal names in Edge Data *)
  structure EDataCRnmINames : COMPOUND_RENAMING
    sharing type EDataCRnmINames.obj = Edge.data
    sharing type EDataCRnmINames.renaming = CRnmINames.renaming
  
  (* syntax convenience for sharing types *)
  structure Sharing : GRAPH_NAME_DEPS_SHARING
    sharing Sharing.Vertex = Vertex.SharingWithData
    sharing Sharing.Edge = Edge.EdgeSharingWithData
    sharing type CRnmINames.renaming = Sharing.inames_renaming
    sharing type T = Sharing.T

  (* rename vertex and edge names in T *)
  structure CRnmVNames : COMPOUND_RENAMING 
  structure CRnmENames : COMPOUND_RENAMING 
    sharing type CRnmVNames.obj = T
    sharing type CRnmVNames.renaming = Vertex.Rnm.T
    sharing type CRnmENames.obj = T 
    sharing type CRnmENames.renaming = Edge.Rnm.T

  (* *)
  val empty : T

  val add_vertex : Vertex.data -> Vertex.name -> T -> T 
  val add_edge : Edge.data -> Edge.name -> T -> T

  val of_vertex : Vertex.data -> Vertex.name -> T
  val of_edge : Vertex.data -> Vertex.name -> T

  val delete_vertex : Vertex.name -> T -> T
  val delete_edge : Edge.name -> T -> T

  val rename_vname : Vertex.name -> Vertex.name -> T -> T (* raises *)
  val rename_ename : Edge.name -> Edge.name -> T -> T (* raises *)

  val replace_vertex : Vertex.data -> Vertex.name -> T -> T
  val replace_edge : Edge.data -> Edge.name -> T -> T

  

end;



