signature OGRAPH_MATCH_STATE_SHARING
= sig
  type T
  structure G : OGRAPH_SHARING
end

signature OGRAPH_MATCH_STATE
= sig
  type T
  structure G : OGRAPH
  
  (* sharing via sharing structure *)
  structure Sharing : OGRAPH_MATCH_STATE_SHARING
    sharing type Sharing.T = T
    sharing Sharing.G = G.SharingOGraph
  
  val update_names : ((V.NSet.T * E.NSet.T) -> (V.NSet.T * E.NSet.T)) -> T -> T
  val update_vmap : (VInjEndo.T -> VInjEndo.T) -> T -> T
  
  val get_names : T -> (V.NSet.T * E.NSet.T)
  val get_vmap : T -> VInjEndo.T
  val get_vnames : T -> V.NSet.T
  val get_enames : T -> E.NSet.T
  
  val set_names : (V.NSet.T * E.NSet.T) -> T -> T
  val set_vmap : VInjEndo.T -> T -> T
end

functor OGraphMatchState(structure G : OGRAPH) : OGRAPH_MATCH_STATE
= struct
  structure G = G
  
  datatype T = MatchState of {
    (* names context for fresh names when copying bboxes in pat *)
    names : (V.NSet.T * E.NSet.T),
    (* Internal vertex mapping from pat to tgt *)
    vmap : VInjEndo.T
  }
  
  (* getters and setters *)
  
  fun update_names f (MatchState r) = MatchState {
    names = f (#names r),
    vmap = #vmap r
  }
  
  fun update_vmap f (MatchState r) = MatchState {
    names = #names r,
    vmap = f (#vmap r)
  }
  
  fun get_names (MatchState r) = #names r
  fun get_vmap (MatchState r) = #vmap r
  val get_vnames = #1 o get_names
  val get_enames = #2 o get_names
  
  val set_names = update_names o K
  val set_vmap = update_vmap o K
  
  structure Sharing : OGRAPH_MATCH_STATE_SHARING 
  = struct
    structure G = G.SharingOGraph
    type T = T;
  end;
end
