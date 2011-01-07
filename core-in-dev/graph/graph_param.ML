(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graph Parameterisation. 
    The names that a graph depends on: Vertex names, Edge names, and any 
    names that might occur inside the data on vertices and edges.  *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

signature GRAPH_PARAM_SHARING
= sig 
  structure Vertex : NAME_AND_DATA_SHARING
  structure Edge : EDGE_AND_DATA_SHARING
  type T
  type inames_renaming
  type unif 
end;


(* Name dependencies for Graphs: how names in internal data and vertex 
   and edge names deepend on each other. *)
signature GRAPH_PARAM
= sig

  (* Names for vertices and edges *)
  structure Vertex : VERTEX (* defined in pregraph.ML *) 
  structure Edge : EDGE (* defined in pregraph.ML *) 

  (* internal name data dependencies e.g. w.r.t. vertex names *)
  type T

  exception unif_exp of unit

  type unif (* unification data for internal data (in edges and vertices) *)
  val empty_unif : unif
  val compose_unif : unif -> unif -> unif
  val try_unif_vertex : Vertex.data -> Vertex.data -> unif -> unif option
  val try_unif_edge : Edge.data -> Edge.data -> unif -> unif option

  (* raises unif_exp *)
  val unif_vertex : Vertex.data -> Vertex.data -> unif -> unif
  val unif_edge : Edge.data -> Edge.data -> unif -> unif
  
  val subst_in_vertex : unif -> Vertex.data -> Vertex.data
  val subst_in_edge : unif -> Edge.data -> Edge.data

  val pretty_unif : unif -> Pretty.T
  val print_unif : unif -> unit

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
  

  (* syntax convenience for sharing types *)
  structure Sharing : GRAPH_PARAM_SHARING
    sharing Sharing.Vertex = Vertex.SharingWithData
    sharing Sharing.Edge = Edge.EdgeSharingWithData
    sharing type CRnmINames.renaming = Sharing.inames_renaming
    sharing type T = Sharing.T
    sharing type unif = Sharing.unif
end;



