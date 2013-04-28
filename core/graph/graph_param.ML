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
     which that variable occurs. Can be used to define vertices that are not commutative internal wiring of vertex can be dependent on edge names entereing the vertex. 
     
     Actually: it might be better to change the vertex type to being a list of sets if we really want non-commutative matching. Maybe drop this? This also conflicts with the ability to do compound renaming in matches: matches don't have the internal data for a vertex. I guess it could be added, but it would be rather complicated. *)
  type T
  
  (* get the internal variable names used *)
  val get_xnames : T -> X.NSet.T

  (* no vertex names, no internal variable names *)
  val empty : T

  (* pretty printing *)
  val pretty : T -> Pretty.T
  val print : T -> unit

  (*  *)
  val add_vertex : VData.data -> V.name -> T -> T 
  val add_edge : EData.data -> E.name -> T -> T

  val of_vertex : VData.data -> V.name -> T
  val of_edge : EData.data -> E.name -> T

  val delete_vname : V.name -> T -> T
  val delete_ename : E.name -> T -> T

  val dep_vnames_of_xname : T -> X.name -> V.NSet.T
  val dep_enames_of_xname : T -> X.name -> E.NSet.T 

  (* raises X.Rnm. *)
  val rename_xname : X.name -> X.name -> T -> T
  (* raises V.Rnm. *)
  val rename_vname : V.name -> V.name -> T -> T
  (* raises E.Rnm. *)
  val rename_ename : E.name -> E.name -> T -> T 

  val replace_vertex : VData.data -> V.name -> T -> T
  val replace_edge : EData.data -> E.name -> T -> T
  

  (* CONSIDER: move to unifiable data? *)
  val subst_xname_in_vdata : X.name -> X.name -> VData.data -> VData.data;
  val subst_xname_in_edata : X.name -> X.name -> EData.data -> EData.data;

  (* RENAMINGS  *)
  (* rename internal var names in T *)
  structure CRnmX : COMPOUND_RENAMING 
    where type renaming = X.Rnm.T
    sharing type CRnmX.obj = T
  (* rename vertex and edge names in T *)
  structure CRnmV : COMPOUND_RENAMING 
    where type renaming = V.Rnm.T
    sharing type CRnmV.obj = T
  structure CRnmE : COMPOUND_RENAMING 
    where type renaming = E.Rnm.T
    sharing type CRnmE.obj = T
  
  (* rename internal names in Vertex Data *)
  (* structure VDataCRnmX : COMPOUND_RENAMING
    where type renaming = X.Rnm.T
    sharing type VDataCRnmX.obj = VData.data *)
  (* rename internal names in Edge Data *)
  (* structure EDataCRnmX : COMPOUND_RENAMING
    where type renaming = X.Rnm.T
    sharing type EDataCRnmX.obj = EData.data *)
      
  (* syntax convenience for sharing types *)
  structure Sharing : GRAPH_PARAM_SHARING
    sharing Sharing.VData = VData.Sharing
    sharing Sharing.EData = EData.Sharing
    sharing type T = Sharing.T
    sharing type subst = Sharing.subst
end;

(* functor that takes away the work when the vertices and edges have no 
   internal data. *)
(* TODO: make this have a signature & fit into GRAPH_PARAM signature. *)
functor NoInternalDataGraphParam(type vdata type edata) = 
struct 
  (* INTERNAL NAME DEPENDENCIES *)
  (* internal name data *)
  type T = unit;
  
  (* pretty printing *)
  fun pretty () = Pretty.str "()";
  val print = Pretty.writeln o pretty;
  
  (* empty name-dependency information *)
  val empty = ();
  fun get_xnames _ = X.NSet.empty;
  fun subst_xname_in_vdata _ _ d = d
  fun subst_xname_in_edata _ _ d = d 
  fun add_vertex _ _ vdep = vdep;
  fun add_edge _ _ vdep = vdep;
  fun of_vertex _ _ = empty;
  fun of_edge _ _ = empty;
  fun delete_vname _ vdep = vdep;
  fun delete_ename _ vdep = vdep;
  fun rename_xname _ _ d = d;
  fun rename_vname _ _ d = d;
  fun rename_ename _ _ d = d;
  fun replace_vertex _ _ vdep = vdep; 
  fun replace_edge _ _ vdep = vdep;
  fun dep_vnames_of_xname () _ = V.NSet.empty;
  fun dep_enames_of_xname () _ = E.NSet.empty; 
  
  (* rename internal names in the dependency structure *)
  (* RENAMING *)
  (* [dummy] Renaming internal names within vertex data *)
  (* structure VDataCRnmX : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = X type obj = vdata); *)
  (* [dummy] Rename internal names within edge data *)
  (* structure EDataCRnmX : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = X type obj = edata); *)
    
  (* [dummy] Rename vertex-names in the dependency data *)
  structure CRnmV : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = V type obj = T);
  (* [dummy] Rename edge-names in the dependency data *)
  structure CRnmE : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = E type obj = T);
  (* [dummy] Rename internal names in T *)
  structure CRnmX : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = X type obj = T);

end;
