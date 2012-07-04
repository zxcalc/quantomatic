(* The match state for graph matching *)
(* see docs/matching_algo for details *)

(* The type-sharing struct for OGRAPH_MATCH_STATE *)
signature OGRAPH_MATCH_STATE_SHARING
= sig
  type T
  structure G : OGRAPH_SHARING
end

(* The match state for concrete graph matching *)
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
  val update_emap         : (EInjEndo.T -> EInjEndo.T) -> T -> T
  val update_u_circles    : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_u_nodeverts  : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_u_wireverts  : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_u_bare_wires : (E.NSet.T -> E.NSet.T) -> T -> T
  val update_p_nodeverts  : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_ps_nodeverts : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_match_subst  : (G.Param.subst -> G.Param.subst) -> T -> T
  val get_names           : T -> (V.NSet.T * E.NSet.T)
  val get_pat             : T -> G.T
  val get_tgt             : T -> G.T
  val get_vmap            : T -> VInjEndo.T
  val get_emap            : T -> EInjEndo.T
  val get_u_circles       : T -> V.NSet.T
  val get_u_nodeverts     : T -> V.NSet.T
  val get_u_wireverts     : T -> V.NSet.T
  val get_u_bare_wires    : T -> E.NSet.T
  val get_p_nodeverts     : T -> V.NSet.T
  val get_ps_nodeverts    : T -> V.NSet.T
  val get_match_subst     : T -> G.Param.subst
  val set_names           : (V.NSet.T * E.NSet.T) -> T -> T
  val set_pat             : G.T -> T -> T
  val set_tgt             : G.T -> T -> T
  val set_vmap            : VInjEndo.T -> T -> T
  val set_emap            : EInjEndo.T -> T -> T
  val set_u_circles       : V.NSet.T -> T -> T
  val set_u_nodeverts     : V.NSet.T -> T -> T
  val set_u_wireverts     : V.NSet.T -> T -> T
  val set_u_bare_wires    : E.NSet.T -> T -> T
  val set_p_nodeverts     : V.NSet.T -> T -> T
  val set_ps_nodeverts    : V.NSet.T -> T -> T
  val set_match_subst     : G.Param.subst -> T -> T
  
  val empty : T
  
  (* returns true if vmap and evap represent a total graph hm *)
  val is_total : T -> bool
  
  (* initialise MatchState and schedule all vertices in pattern to be matched *)
  val init_and_schedule_all : G.T -> G.T -> T
  
  (* Convenience functions for adding things to match. Convention is these do NOT remove 
     vertices or edges from the u_*, p_*, or ps_* sets. The caller should do that. *)
  
  
  (* adds given vertex to match *)
  val add_vertex_to_match : (V.name * V.name) -> T -> T
  
  (* adds edge and both endpoints to match *)
  val add_edge_to_match : (E.name * E.name) -> T -> T
  
  (* adds wire vertex and associated circle to match *)
  val add_circle_to_match : (V.name * V.name) -> T -> T
end

(* The match state for pattern graph matching *)
signature BANG_GRAPH_MATCH_STATE
= sig
  include OGRAPH_MATCH_STATE;
  (* NB: there is no BANG_GRAPH_SHARING signature *)
  structure BG : BANG_GRAPH;
  sharing type G.T = BG.T;
  sharing type G.Param.subst = BG.Param.subst;
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
    (* vertex mapping from pat to tgt *)
    vmap         : VInjEndo.T,
    (* edge mapping from pat to tgt *)
    emap         : EInjEndo.T,
    (* circles, node-vertices, and wire-vertices to be matched *)
    u_circles    : V.NSet.T,
    u_nodeverts  : V.NSet.T,
    u_wireverts  : V.NSet.T,
    u_bare_wires : E.NSet.T,
    (* partially matched node-vertices *)
    p_nodeverts  : V.NSet.T,
    (* partially matched node-vertices, scheduled for re-matching *)
    ps_nodeverts : V.NSet.T,
    match_subst  : G.Param.subst
  }
  
  val empty = MatchState {
    names        = (V.NSet.empty, E.NSet.empty),
    pat          = G.empty,
    tgt          = G.empty,
    vmap         = VInjEndo.empty,
    emap         = EInjEndo.empty,
    u_circles    = V.NSet.empty,
    u_nodeverts  = V.NSet.empty,
    u_wireverts  = V.NSet.empty,
    u_bare_wires = E.NSet.empty,
    p_nodeverts  = V.NSet.empty,
    ps_nodeverts = V.NSet.empty,
    match_subst  = G.Param.empty_subst
  }
  
  (* getters and setters *)
  
  fun update_names f (MatchState r) = MatchState {
    names        = f(#names r),
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r
  }

  fun update_pat f (MatchState r) = MatchState {
    names        = #names r,
    pat          = f(#pat r),
    tgt          = #tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r
  }

  fun update_tgt f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = f(#tgt r),
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r
  }

  fun update_vmap f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = f(#vmap r),
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r
  }

  fun update_emap f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = #vmap r,
    emap         = f(#emap r),
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r
  }

  fun update_u_circles f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = f(#u_circles r),
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r
  }

  fun update_u_nodeverts f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = f(#u_nodeverts r),
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r
  }

  fun update_u_wireverts f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = f(#u_wireverts r),
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r
  }

  fun update_u_bare_wires f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = f(#u_bare_wires r),
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r
  }

  fun update_p_nodeverts f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = f(#p_nodeverts r),
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r
  }

  fun update_ps_nodeverts f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = f(#ps_nodeverts r),
    match_subst  = #match_subst r
  }

  fun update_match_subst f (MatchState r) = MatchState {
    names        = #names r,
    pat          = #pat r,
    tgt          = #tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = f(#match_subst r)
  }

  fun get_names        (MatchState r) = #names r
  fun get_pat          (MatchState r) = #pat r
  fun get_tgt          (MatchState r) = #tgt r
  fun get_vmap         (MatchState r) = #vmap r
  fun get_emap         (MatchState r) = #emap r
  fun get_u_circles    (MatchState r) = #u_circles r
  fun get_u_nodeverts  (MatchState r) = #u_nodeverts r
  fun get_u_wireverts  (MatchState r) = #u_wireverts r
  fun get_u_bare_wires (MatchState r) = #u_bare_wires r
  fun get_p_nodeverts  (MatchState r) = #p_nodeverts r
  fun get_ps_nodeverts (MatchState r) = #ps_nodeverts r
  fun get_match_subst  (MatchState r) = #match_subst r

  val set_names           = update_names        o K
  val set_pat             = update_pat          o K
  val set_tgt             = update_tgt          o K
  val set_vmap            = update_vmap         o K
  val set_emap            = update_emap         o K
  val set_u_circles       = update_u_circles    o K
  val set_u_nodeverts     = update_u_nodeverts  o K
  val set_u_wireverts     = update_u_wireverts  o K
  val set_u_bare_wires    = update_u_bare_wires o K
  val set_p_nodeverts     = update_p_nodeverts  o K
  val set_ps_nodeverts    = update_ps_nodeverts o K
  val set_match_subst     = update_match_subst  o K
  
  fun add_edge_to_match (ep, et) ms = let
    val (_,(sp,tp)) = G.get_edge (get_pat ms) ep
    val (_,(st,tt)) = G.get_edge (get_tgt ms) et
  in ms |> update_vmap (VInjEndo.add sp st o VInjEndo.add tp tt)
        |> update_emap (EInjEndo.add ep et)
  end
  
  fun add_circle_to_match (cp, ct) ms = let
    val ep = (the o E.NSet.tryget_singleton) (G.get_in_edges (get_pat ms) cp)
    val et = (the o E.NSet.tryget_singleton) (G.get_in_edges (get_tgt ms) ct)
  in ms |> update_vmap (VInjEndo.add cp ct)
        |> update_emap (EInjEndo.add ep et)
  end
  
  fun add_vertex_to_match (vp, vt) ms = ms |> update_vmap (VInjEndo.add vp vt)
  
  fun is_total ms =
    V.NSet.eq (VInjEndo.get_domset (get_vmap ms)) (G.get_vnames (get_pat ms)) andalso
    E.NSet.eq (EInjEndo.get_domset (get_emap ms)) (G.get_enames (get_pat ms)) andalso
    V.NSet.is_empty (get_p_nodeverts ms)
  
  fun init_and_schedule_all pat tgt = let
    val wires = G.get_wire_list pat
    fun delete_wvs_in_wire (s,t,wv,_) set =
      set |> V.NSet.remove_set wv
          |> (if G.is_wire_vertex pat s then V.NSet.delete s else I)
          |> (if G.is_wire_vertex pat t then V.NSet.delete t else I)
    
    val circles = filter (fn (s,t,_,_) => V.name_eq (s,t) andalso G.is_wire_vertex pat s) wires
    val bare_wires = filter (fn (s,t,_,_) => not (V.name_eq (s,t)) andalso G.is_wire_vertex pat s andalso G.is_wire_vertex pat t) wires
    
    val wvs = fold delete_wvs_in_wire (circles @ bare_wires) (G.get_wire_vertices pat)
    val circle_wvs = fold (fn (s,_,_,_) => fn set => set |> V.NSet.add s) circles V.NSet.empty
    val bare_wire_es = fold (fn (_,_,_,es) => E.NSet.add ((the o E.NSet.get_min) es)) bare_wires E.NSet.empty  
  in empty
      |> set_pat pat
      |> set_tgt tgt
      |> set_u_circles circle_wvs
      |> set_u_nodeverts (G.get_node_vertices pat)
      |> set_u_wireverts wvs
      |> set_u_bare_wires bare_wire_es
  end
  
  
  structure Sharing : OGRAPH_MATCH_STATE_SHARING 
  = struct
    structure G = G.SharingOGraph
    type T = T;
  end;
end

(* type-coerced version of MatchState for pattern graphs *)
functor BangGraphMatchState(structure G : BANG_GRAPH) : BANG_GRAPH_MATCH_STATE
= struct
  structure BG = G;
  structure OGMS = OGraphMatchState(structure G = G);
  open OGMS;
end
