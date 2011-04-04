(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graph Parameterisation. 
    The names that a graph depends on: Vertex names, Edge names, and any 
    names that might occur inside the data on vertices and edges.  *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

signature GRAPH_PARAM_SHARING
= sig 
  structure Vertex : NAME_AND_UNIFIABLE_DATA_SHARING
  structure Edge : NAME_AND_UNIFIABLE_DATA_SHARING
  type T
  type inames_renaming
  type subst 
  sharing type subst = Vertex.subst
  sharing type subst = Edge.subst
end;


(* Graph parameterisation consists of two parts: 
   1. substitutions, matching and unification on edges and vertex
   2. how internal variable names dependend on vertex and edge names 
*)
signature GRAPH_PARAM
= sig
  (* VERTEX AND EDGE DATA AND NAMES *)
  (* Names for vertices and edges *)
  structure Vertex : VERTEX (* defined in pregraph.ML *) 
  structure Edge : EDGE (* defined in pregraph.ML *) 

  (* SUBSTITUTION/MATCHING/UNIFICATION *)
  
  (* this is a user exception raised when things don't match *)
  exception match_exp of unit

  (* substitutions for internal variable names 
     (for data that lives in the edges and vertices) *)
  type subst 
  val empty_subst : subst (* all vars are unaffected *)
  val compose_subst : subst -> subst -> subst

  (* IMPROVE: should we add a function that given a subst, tells us
     which vertex names it affects? (could add dependency info to graph...) *)
  val subst_in_vertex : subst -> Vertex.data -> Vertex.data
  val subst_in_edge : subst -> Edge.data -> Edge.data

  (* invariant: subst instantiates vars in src before those in tgt: later code
     (isomorphism check of graphs) depends on this. *)
  val try_match_vertex : Vertex.data -> Vertex.data -> subst -> subst option
  val try_match_edge : Edge.data -> Edge.data -> subst -> subst option
  val match_vertex : Vertex.data -> Vertex.data 
        -> subst -> subst (* raises match_exp *)
  val match_edge : Edge.data -> Edge.data 
        -> subst -> subst (* raises match_exp *)
  
  (* pretty printing *)
  val pretty_subst : subst -> Pretty.T
  val print_subst : subst -> unit

  
  (* INTERNAL NAME DEPENDENCIES *)
  
  (* Dependencies between vertex and edge names and internal-names (in the 
     vertex/edge data) e.g. relating angle variable-names to vertex names in
     which that variable occurs.  *)
  type T
  
  (* no vertex names, no internal variable names *)
  val empty : T

  val add_vertex : Vertex.data -> Vertex.name -> T -> T 
  val add_edge : Edge.data -> Edge.name -> T -> T

  val of_vertex : Vertex.data -> Vertex.name -> T
  val of_edge : Edge.data -> Edge.name -> T

  val delete_vertex : Vertex.name -> T -> T
  val delete_edge : Edge.name -> T -> T

  (* raises Vertex.Rnm. *)
  val rename_vname : Vertex.name -> Vertex.name -> T -> T
  (* raises Edge.Rnm. *)
  val rename_ename : Edge.name -> Edge.name -> T -> T 

  val replace_vertex : Vertex.data -> Vertex.name -> T -> T
  val replace_edge : Edge.data -> Edge.name -> T -> T
  
  
  (* RENAMINGS  *)
  (* IMPROVE: should we add a compound renaming for renaming internal names in 
     a  subst? *)
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
    
    
  (* syntax convenience for sharing types *)
  structure Sharing : GRAPH_PARAM_SHARING
    sharing Sharing.Vertex = Vertex.SharingWithData
    sharing Sharing.Edge = Edge.EdgeSharingWithData
    sharing type CRnmINames.renaming = Sharing.inames_renaming
    sharing type T = Sharing.T
    sharing type subst = Sharing.subst
end;



