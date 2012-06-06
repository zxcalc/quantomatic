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
  
  val update_names        : ((V.NSet.T * E.NSet.T) -> (V.NSet.T * E.NSet.T)) -> T -> T
  val update_pat          : (G.T -> G.T) -> T -> T
  val update_tgt          : (G.T -> G.T) -> T -> T
  val update_vmap         : (VInjEndo.T -> VInjEndo.T) -> T -> T
  val update_u_circles    : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_u_nodeverts  : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_u_wireverts  : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_u_bare_wires : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_p_nodeverts  : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_ps_nodeverts : (V.NSet.T -> V.NSet.T) -> T -> T
  
  val get_names           : T -> (V.NSet.T * E.NSet.T)
  val get_pat             : T -> G.T
  val get_tgt             : T -> G.T
  val get_vmap            : T -> VInjEndo.T
  val get_u_circles       : T -> V.NSet.T
  val get_u_nodeverts     : T -> V.NSet.T
  val get_u_wireverts     : T -> V.NSet.T
  val get_p_nodeverts     : T -> V.NSet.T
  val get_ps_nodeverts    : T -> V.NSet.T
  
  val get_vnames          : T -> V.NSet.T
  val get_enames          : T -> E.NSet.T
  
  val set_names           : (V.NSet.T * E.NSet.T) -> T -> T
  val set_pat             : G.T -> T -> T
  val set_tgt             : G.T -> T -> T
  val set_vmap            : VInjEndo.T -> T -> T
  val set_u_circles       : V.NSet.T -> T -> T
  val set_u_nodeverts     : V.NSet.T -> T -> T
  val set_u_wireverts     : V.NSet.T -> T -> T
  val set_p_nodeverts     : V.NSet.T -> T -> T
  val set_ps_nodeverts    : V.NSet.T -> T -> T
  
  val empty : T
end

functor OGraphMatchState(structure G : OGRAPH) : OGRAPH_MATCH_STATE
= struct
  structure G = G
  
  datatype T = MatchState of {
    (* names context for fresh names when copying bboxes in pat *)
    names        : V.NSet.T * E.NSet.T,
    (* pattern and target graphs *)
    pat          : G.T,
    tgt          : G.T,
    (* internal vertex mapping from pat to tgt *)
    vmap         : VInjEndo.T,
    (* circles, node-vertices, and wire-vertices to be matched *)
    u_circles    : V.NSet.T,
    u_nodeverts  : V.NSet.T,
    u_wireverts  : V.NSet.T,
    u_bare_wires : V.NSet.T,
    (* partially matched node-vertices *)
    p_nodeverts  : V.NSet.T,
    (* partially matched node-vertices, scheduled for re-matching *)
    ps_nodeverts : V.NSet.T
  }
  
  val empty = MatchState {
    names        = (V.NSet.empty, E.NSet.empty),
    pat          = G.empty,
    tgt          = G.empty,
    vmap         = VInjEndo.empty,
    u_circles    = V.NSet.empty,
    u_nodeverts  = V.NSet.empty,
    u_wireverts  = V.NSet.empty,
    u_bare_wires = V.NSet.empty,
    p_nodeverts  = V.NSet.empty,
    ps_nodeverts = V.NSet.empty
  }
  
  (* getters and setters *)
  
  fun update_names f (MatchState r) = MatchState {
    names        = f(#names r),
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = #vmap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r
  }
  
  fun update_pat f (MatchState r) = MatchState {
    names        = #names r,
    pat          = f(#pat r),
    tgt          = #tgt r,
    vmap         = #vmap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r
  }
  
  fun update_tgt f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = f(#tgt r),
    vmap         = #vmap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r
  }
  
  fun update_vmap f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = f(#vmap r),
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r
  }
  
  fun update_u_circles f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = #vmap r,
    u_circles    = f(#u_circles r),
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r
  }
  
  fun update_u_nodeverts f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = #vmap r,
    u_circles    = #u_circles r,
    u_nodeverts  = f(#u_nodeverts r),
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r
  }
  
  fun update_u_wireverts f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = #vmap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = f(#u_wireverts r),
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r
  }
  
  fun update_u_bare_wires f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = #vmap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = f(#u_bare_wires r),
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r
  }
  
  fun update_p_nodeverts f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = #vmap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = f(#p_nodeverts r),
    ps_nodeverts = #ps_nodeverts r
  }
  
  fun update_ps_nodeverts f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = #vmap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = f(#ps_nodeverts r)
  }
  
  fun get_names        (MatchState r) = #names r
  fun get_pat          (MatchState r) = #pat r
  fun get_tgt          (MatchState r) = #tgt r
  fun get_vmap         (MatchState r) = #vmap r
  fun get_u_circles    (MatchState r) = #u_circles r
  fun get_u_nodeverts  (MatchState r) = #u_nodeverts r
  fun get_u_wireverts  (MatchState r) = #u_wireverts r
  fun get_u_bare_wires (MatchState r) = #u_bare_wires r
  fun get_p_nodeverts  (MatchState r) = #p_nodeverts r
  fun get_ps_nodeverts (MatchState r) = #ps_nodeverts r
  
  val get_vnames = #1 o get_names
  val get_enames = #2 o get_names
  
  val set_names        = update_names        o K
  val set_pat          = update_pat          o K
  val set_tgt          = update_tgt          o K
  val set_vmap         = update_vmap         o K
  val set_u_circles    = update_u_circles    o K
  val set_u_nodeverts  = update_u_nodeverts  o K
  val set_u_wireverts  = update_u_wireverts  o K
  val set_u_bare_wires = update_u_bare_wires o K
  val set_p_nodeverts  = update_p_nodeverts  o K
  val set_ps_nodeverts = update_ps_nodeverts o K
  
  
  structure Sharing : OGRAPH_MATCH_STATE_SHARING 
  = struct
    structure G = G.SharingOGraph
    type T = T;
  end;
end
