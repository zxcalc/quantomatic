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
  structure IRnm : COMPOUND_RENAMING 
  sharing type IRnm.obj = T

  (* syntax convenience for sharing types *)
  structure Sharing : GRAPH_NAME_DEPS_SHARING
  sharing Sharing.Vertex = Vertex.SharingWithData
  sharing Sharing.Edge = Edge.EdgeSharingWithData
  sharing type IRnm.renaming = Sharing.inames_renaming
  sharing type T = Sharing.T
  
  (* rename vertex and edge names in T *)
  structure VRnm : COMPOUND_RENAMING 
  structure ERnm : COMPOUND_RENAMING 
  sharing type VRnm.obj = T
  sharing type VRnm.renaming = Vertex.Rnm.T
  sharing type ERnm.obj = T 
  sharing type ERnm.renaming = Edge.Rnm.T

  (* *)
  val empty : T

  val of_vertex : Vertex.data -> Vertex.name -> T
  val of_edge : Vertex.data -> Vertex.name -> T

  val rename_in_vertex : IRnm.renaming -> Vertex.data -> Vertex.data
  val rename_in_edge : IRnm.renaming -> Edge.data -> Edge.data

  val add_vertex : Vertex.data -> Vertex.name -> T -> T 
  val add_edge : Edge.data -> Edge.name -> T -> T

  val replace_vertex : Vertex.data -> Vertex.name -> T -> T
  val replace_edge : Edge.data -> Edge.name -> T -> T

  val delete_vertex : Vertex.name -> T -> T
  val delete_edge : Edge.name -> T -> T
end;



