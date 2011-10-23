(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graph Parameterisation. 
    The names that a graph depends on: V.names, Edge names, and any 
    names that might occur inside the data on vertices and edges.  *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

signature GRAPH_PARAM_SHARING
= sig 
  structure VData : UNIFIABLE_DATA_SHARING
  structure EData : UNIFIABLE_DATA_SHARING
  type T
  type inames_renaming
  type subst 
  sharing type subst = VData.subst
  sharing type subst = EData.subst
end;


(* Graph parameterisation consists of two parts: 
   1. substitutions, matching and unification on edges and vertex
   2. how internal variable names dependend on vertex and edge names 
*)
signature GRAPH_PARAM
= sig
  val pretty_theory_name : Pretty.T

  (* VERTEX AND EDGE DATA AND NAME *)
  (* Names for vertices and edges *)
  structure VData : UNIFIABLE_DATA (* defined in pregraph.ML *) 
  structure EData : UNIFIABLE_DATA (* defined in pregraph.ML *) 

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
  val subst_in_vertex : subst -> VData.data -> VData.data
  val subst_in_edge : subst -> EData.data -> EData.data

  (* invariant: subst instantiates vars in src before those in tgt: later code
     (isomorphism check of graphs) depends on this. *)
  val try_match_vertex : VData.data -> VData.data -> subst -> subst option
  val try_match_edge : EData.data -> EData.data -> subst -> subst option
  val match_vertex : VData.data -> VData.data 
        -> subst -> subst (* raises match_exp *)
  val match_edge : EData.data -> EData.data 
        -> subst -> subst (* raises match_exp *)
  
  (* pretty printing *)
  val pretty_subst : subst -> Pretty.T
  val print_subst : subst -> unit
  
  (* INTERNAL NAME DEPENDENCIES *)
  
  (* Dependencies between vertex and edge names and internal-names (in the 
     vertex/edge data) e.g. relating angle variable-names to vertex names in
     which that variable occurs. Can be used to define vertices that are not commutative internal wiring of vertex can be dependent on edge names entereing the vertex. Actually: it would be better to change the vertex type to being a list of sets if we really want non-commutative matching. Maybe drop this? This also conflicts with the ability to do compound renaming in matches: matches don't have the internal data for a vertex. I guess it could be added, but it would be rather complicated. *)
  type T
  
  (* no vertex names, no internal variable names *)
  val empty : T

  val add_vertex : VData.data -> V.name -> T -> T 
  val add_edge : EData.data -> E.name -> T -> T

  val of_vertex : VData.data -> V.name -> T
  val of_edge : EData.data -> E.name -> T

  val delete_vertex : V.name -> T -> T
  val delete_edge : E.name -> T -> T

  (* raises V.Rnm. *)
  val rename_vname : V.name -> V.name -> T -> T
  (* raises E.Rnm. *)
  val rename_ename : E.name -> E.name -> T -> T 

  val replace_vertex : VData.data -> V.name -> T -> T
  val replace_edge : EData.data -> E.name -> T -> T
  
  
  (* RENAMINGS  *)
  (* IMPROVE: should we add a compound renaming for renaming internal names in 
     a  subst? *)
  (* rename internal names in T *)
  structure CRnmX : COMPOUND_RENAMING 
    sharing type CRnmX.obj = T
  (* rename internal names in Vertex Data *)
  structure VDataCRnmX : COMPOUND_RENAMING
    sharing type VDataCRnmX.obj = VData.data
    sharing type VDataCRnmX.renaming = CRnmX.renaming
  (* rename internal names in Edge Data *)
  structure EDataCRnmX : COMPOUND_RENAMING
    sharing type EDataCRnmX.obj = EData.data
    sharing type EDataCRnmX.renaming = CRnmX.renaming
  (* rename vertex and edge names in T *)
  structure CRnmV : COMPOUND_RENAMING 
    where type renaming = V.Rnm.T
    sharing type CRnmV.obj = T
  structure CRnmE : COMPOUND_RENAMING 
    where type renaming = E.Rnm.T
    sharing type CRnmE.obj = T 
    
  (* syntax convenience for sharing types *)
  structure Sharing : GRAPH_PARAM_SHARING
    sharing Sharing.VData = VData.Sharing
    sharing Sharing.EData = EData.Sharing
    sharing type CRnmX.renaming = Sharing.inames_renaming
    sharing type T = Sharing.T
    sharing type subst = Sharing.subst
end;


