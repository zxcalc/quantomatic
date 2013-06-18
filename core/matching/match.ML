signature MATCH_SHARING =
sig
  type T
  structure Graph : OGRAPH_SHARING
end

signature MATCH =
sig
  type T
  structure Graph : OGRAPH
  
  (* sharing via sharing structure *)
  structure Sharing : MATCH_SHARING
    sharing type Sharing.T = T
    sharing Sharing.Graph = Graph.Sharing
  
  val get_pat             : T -> Graph.T
  val get_tgt             : T -> Graph.T
  val get_vmap            : T -> VVInj.T
  val get_emap            : T -> EEInj.T
  val get_subst           : T -> Graph.Data.subst

  val mk : Graph.T * Graph.T * VVInj.T * EEInj.T -> T
end

signature MATCH =
sig
  type T
  structure Graph : BANG_GRAPH
  
  (* sharing via sharing structure *)
  structure Sharing : MATCH_SHARING
    sharing type Sharing.T = T
    sharing Sharing.Graph = Graph.Sharing
  
  val get_pat             : T -> Graph.T
  val get_tgt             : T -> Graph.T
  val get_vmap            : T -> VVInj.T
  val get_emap            : T -> EEInj.T
  val get_subst           : T -> Graph.Data.subst

  val mk : Graph.T * Graph.T * VVInj.T * EEInj.T -> T
  (*val mk_with_bboxes : *)
end