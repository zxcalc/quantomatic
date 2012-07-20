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
  
  (* the graph to match from *)
  val get_pat             : T -> G.T
  (* the graph to match onto *)
  val get_tgt             : T -> G.T
  (* the full graph that tgt is contained in *)
  (* this is carried around because of normalisation *)
  val get_full_tgt        : T -> G.T
  (* the vertex map so far (from pat verts to tgt verts *)
  val get_vmap            : T -> VInjEndo.T
  (* the edge map so far (from pat edges to tgt edges *)
  val get_emap            : T -> EInjEndo.T
  (* circles waiting to be matched *)
  (* contains a representative vertex from each circle *)
  val get_u_circles       : T -> V.NSet.T
  (* node vertices waiting to be matched *)
  val get_u_nodeverts     : T -> V.NSet.T
  (* wire vertices waiting to be matched *)
  val get_u_wireverts     : T -> V.NSet.T
  (* bare wires waiting to be matched *)
  val get_u_bare_wires    : T -> E.NSet.T
  (* partially-matched vertices - these are in vmap, but not all
   * the adjacent edges of their images are in the image of emap *)
  val get_p_nodeverts     : T -> V.NSet.T
  (* those vertices in p_nodeverts that may have new matchings
   * of adjacent wires *)
  val get_ps_nodeverts    : T -> V.NSet.T
  (* the vertex/edge data substitution for the matching *)
  val get_match_subst     : T -> G.Param.subst

  val update_pat          : (G.T -> G.T) -> T -> T
  val update_tgt          : (G.T -> G.T) -> T -> T
  val update_full_tgt     : (G.T -> G.T) -> T -> T
  val update_vmap         : (VInjEndo.T -> VInjEndo.T) -> T -> T
  val update_emap         : (EInjEndo.T -> EInjEndo.T) -> T -> T
  val update_u_circles    : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_u_nodeverts  : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_u_wireverts  : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_u_bare_wires : (E.NSet.T -> E.NSet.T) -> T -> T
  val update_p_nodeverts  : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_ps_nodeverts : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_match_subst  : (G.Param.subst -> G.Param.subst) -> T -> T

  val set_pat             : G.T -> T -> T
  val set_tgt             : G.T -> T -> T
  val set_full_tgt        : G.T -> T -> T
  val set_vmap            : VInjEndo.T -> T -> T
  val set_emap            : EInjEndo.T -> T -> T
  val set_u_circles       : V.NSet.T -> T -> T
  val set_u_nodeverts     : V.NSet.T -> T -> T
  val set_u_wireverts     : V.NSet.T -> T -> T
  val set_u_bare_wires    : E.NSet.T -> T -> T
  val set_p_nodeverts     : V.NSet.T -> T -> T
  val set_ps_nodeverts    : V.NSet.T -> T -> T
  val set_match_subst     : G.Param.subst -> T -> T

  val extend_u_circles    : V.NSet.T -> T -> T
  val extend_u_nodeverts  : V.NSet.T -> T -> T
  val extend_u_wireverts  : V.NSet.T -> T -> T
  val extend_u_bare_wires : E.NSet.T -> T -> T
  val extend_p_nodeverts  : V.NSet.T -> T -> T
  val extend_ps_nodeverts : V.NSet.T -> T -> T

  val empty : T

  (* initialise MatchState with nothing added *)
  val init : G.T -> (* pattern *)
             G.T -> (* target *)
             T

  (* initialise MatchState with nothing added *)
  val init_subgraph : G.T -> (* pattern *)
                      G.T -> (* target *)
                      V.NSet.T -> (* node verts of subgraph *)
                      T

  val get_complete_nodeverts : T -> V.NSet.T
  
  (* returns true if vmap and evap represent a total graph hm *)
  (* will only return true if p_nodeverts is empty (local isomorphism) *)
  val is_total : T -> bool
  
  (* initialise MatchState and schedule all vertices in pattern to be matched *)
  val init_and_schedule_all : G.T -> (* pattern *)
                              G.T -> (* target *)
                              T
  
  (* initialise MatchState and schedule all vertices in pattern to be matched *)
  val init_subgraph_and_schedule_all : G.T -> (* pattern *)
                                       G.T -> (* target *)
                                       V.NSet.T -> (* node verts of subgraph *)
                                       T
  
  (* schedule all vertices in pattern to be matched *)
  val schedule_all : T -> T
  
  (* Convenience functions for adding things to match. Convention is these do NOT remove 
     vertices or edges from the u_*, p_*, or ps_* sets. The caller should do that. *)
  
  (* adds given vertex to match *)
  val add_vertex_to_match : (V.name * V.name) -> T -> T
  
  (* adds edge and given endpoints to match *)
  val add_edge_to_match : (E.name * V.name) -> (E.name * V.name) -> T -> T
  
  (* adds wire vertex and associated circle to match *)
  val add_circle_to_match : (V.name * V.name) -> T -> T

  val pretty : T -> Pretty.T
  val print : T -> unit

  (* extra data for match state, for extending the datatype cos ML can't do
  it properly from the outside; this is basically a form of record
  extensibility, allowing hidden types, but will doing the lazy
  extension of data -- allows OOP style extension of signature/structure
  by refining data. *)
  type data
  val get_data : T -> data;
  val update_data : (data -> data) -> T -> T
end

functor OGraphMatchState(
  structure G : OGRAPH
  type data (* additional data *)
  val empty_data : data
) : OGRAPH_MATCH_STATE
= struct
  structure G = G
  type data = data
  
  datatype T = MatchState of {
    (* pattern and target graphs *)
    pat          : G.T,
    tgt          : G.T,
    full_tgt     : G.T,
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
    match_subst  : G.Param.subst,
    data         : data
  }
  
  val empty = MatchState {
    pat          = G.empty,
    tgt          = G.empty,
    full_tgt     = G.empty,
    vmap         = VInjEndo.empty,
    emap         = EInjEndo.empty,
    u_circles    = V.NSet.empty,
    u_nodeverts  = V.NSet.empty,
    u_wireverts  = V.NSet.empty,
    u_bare_wires = E.NSet.empty,
    p_nodeverts  = V.NSet.empty,
    ps_nodeverts = V.NSet.empty,
    match_subst  = G.Param.empty_subst,
    data         = empty_data
  }

  fun init pat tgt = MatchState {
    pat          = pat,
    tgt          = tgt,
    full_tgt     = tgt,
    vmap         = VInjEndo.empty,
    emap         = EInjEndo.empty,
    u_circles    = V.NSet.empty,
    u_nodeverts  = V.NSet.empty,
    u_wireverts  = V.NSet.empty,
    u_bare_wires = E.NSet.empty,
    p_nodeverts  = V.NSet.empty,
    ps_nodeverts = V.NSet.empty,
    match_subst  = G.Param.empty_subst,
    data         = empty_data
  }

  fun init_subgraph pat tgt tgt_verts = MatchState {
    pat          = pat,
    tgt          = tgt,
    full_tgt     = G.get_open_subgraph tgt_verts tgt,
    vmap         = VInjEndo.empty,
    emap         = EInjEndo.empty,
    u_circles    = V.NSet.empty,
    u_nodeverts  = V.NSet.empty,
    u_wireverts  = V.NSet.empty,
    u_bare_wires = E.NSet.empty,
    p_nodeverts  = V.NSet.empty,
    ps_nodeverts = V.NSet.empty,
    match_subst  = G.Param.empty_subst,
    data         = empty_data
  }
  
  (* getters and setters *)

  fun update_pat f (MatchState r) = MatchState {
    pat          = f(#pat r),
    tgt          = #tgt r,
    full_tgt     = #full_tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r,
    data         = #data r
  }

  fun update_tgt f (MatchState r) = MatchState {
    pat          = #pat r,
    tgt          = f(#tgt r),
    full_tgt     = #full_tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r,
    data         = #data r
  }

  fun update_full_tgt f (MatchState r) = MatchState {
    pat          = #pat r,
    tgt          = #tgt r,
    full_tgt     = f(#full_tgt r),
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r,
    data         = #data r
  }

  fun update_vmap f (MatchState r) = MatchState {
    pat          = #pat r,
    tgt          = #tgt r,
    full_tgt     = #full_tgt r,
    vmap         = f(#vmap r),
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r,
    data         = #data r
  }

  fun update_emap f (MatchState r) = MatchState {
    pat          = #pat r,
    tgt          = #tgt r,
    full_tgt     = #full_tgt r,
    vmap         = #vmap r,
    emap         = f(#emap r),
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r,
    data         = #data r
  }

  fun update_u_circles f (MatchState r) = MatchState {
    pat          = #pat r,
    tgt          = #tgt r,
    full_tgt     = #full_tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = f(#u_circles r),
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r,
    data         = #data r
  }

  fun update_u_nodeverts f (MatchState r) = MatchState {
    pat          = #pat r,
    tgt          = #tgt r,
    full_tgt     = #full_tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = f(#u_nodeverts r),
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r,
    data         = #data r
  }

  fun update_u_wireverts f (MatchState r) = MatchState {
    pat          = #pat r,
    tgt          = #tgt r,
    full_tgt     = #full_tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = f(#u_wireverts r),
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r,
    data         = #data r
  }

  fun update_u_bare_wires f (MatchState r) = MatchState {
    pat          = #pat r,
    tgt          = #tgt r,
    full_tgt     = #full_tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = f(#u_bare_wires r),
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r,
    data         = #data r
  }

  fun update_p_nodeverts f (MatchState r) = MatchState {
    pat          = #pat r,
    tgt          = #tgt r,
    full_tgt     = #full_tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = f(#p_nodeverts r),
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r,
    data         = #data r
  }

  fun update_ps_nodeverts f (MatchState r) = MatchState {
    pat          = #pat r,
    tgt          = #tgt r,
    full_tgt     = #full_tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = f(#ps_nodeverts r),
    match_subst  = #match_subst r,
    data         = #data r
  }

  fun update_match_subst f (MatchState r) = MatchState {
    pat          = #pat r,
    tgt          = #tgt r,
    full_tgt     = #full_tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = f(#match_subst r),
    data         = #data r
  }

  fun update_data f (MatchState r) = MatchState {
    pat          = #pat r,
    tgt          = #tgt r,
    full_tgt     = #full_tgt r,
    vmap         = #vmap r,
    emap         = #emap r,
    u_circles    = #u_circles r,
    u_nodeverts  = #u_nodeverts r,
    u_wireverts  = #u_wireverts r,
    u_bare_wires = #u_bare_wires r,
    p_nodeverts  = #p_nodeverts r,
    ps_nodeverts = #ps_nodeverts r,
    match_subst  = #match_subst r,
    data         = f(#data r)
  }

  fun get_pat          (MatchState r) = #pat r
  fun get_tgt          (MatchState r) = #tgt r
  fun get_full_tgt     (MatchState r) = #full_tgt r
  fun get_vmap         (MatchState r) = #vmap r
  fun get_emap         (MatchState r) = #emap r
  fun get_u_circles    (MatchState r) = #u_circles r
  fun get_u_nodeverts  (MatchState r) = #u_nodeverts r
  fun get_u_wireverts  (MatchState r) = #u_wireverts r
  fun get_u_bare_wires (MatchState r) = #u_bare_wires r
  fun get_p_nodeverts  (MatchState r) = #p_nodeverts r
  fun get_ps_nodeverts (MatchState r) = #ps_nodeverts r
  fun get_match_subst  (MatchState r) = #match_subst r
  fun get_data         (MatchState r) = #data r

  val set_pat             = update_pat          o K
  val set_tgt             = update_tgt          o K
  val set_full_tgt        = update_full_tgt     o K
  val set_vmap            = update_vmap         o K
  val set_emap            = update_emap         o K
  val set_u_circles       = update_u_circles    o K
  val set_u_nodeverts     = update_u_nodeverts  o K
  val set_u_wireverts     = update_u_wireverts  o K
  val set_u_bare_wires    = update_u_bare_wires o K
  val set_p_nodeverts     = update_p_nodeverts  o K
  val set_ps_nodeverts    = update_ps_nodeverts o K
  val set_match_subst     = update_match_subst  o K

  val extend_u_circles    = update_u_circles o V.NSet.union_merge
  val extend_u_nodeverts  = update_u_nodeverts o V.NSet.union_merge
  val extend_u_wireverts  = update_u_wireverts o V.NSet.union_merge
  val extend_u_bare_wires = update_u_bare_wires o E.NSet.union_merge
  val extend_p_nodeverts  = update_p_nodeverts o V.NSet.union_merge
  val extend_ps_nodeverts = update_ps_nodeverts o V.NSet.union_merge

  fun add_edge_to_match (ep, vp) (et, vt) ms = let
  in ms |> update_vmap (VInjEndo.add vp vt)
        |> update_emap (EInjEndo.add ep et)
  end
  
  fun add_circle_to_match (cp, ct) ms = let
    val ep = (the o E.NSet.tryget_singleton) (G.get_in_edges (get_pat ms) cp)
    val et = (the o E.NSet.tryget_singleton) (G.get_in_edges (get_tgt ms) ct)
  in ms |> update_vmap (VInjEndo.add cp ct)
        |> update_emap (EInjEndo.add ep et)
  end
  
  fun add_vertex_to_match (vp, vt) ms = ms |> update_vmap (VInjEndo.add vp vt)

  fun get_complete_nodeverts ms = let
    val node_vs = V.NSet.filter (G.is_node_vertex (get_pat ms))
                  (VInjEndo.get_domset (get_vmap ms))
  in
    V.NSet.subtract node_vs (get_p_nodeverts ms)
  end
  
  fun is_total ms =
    V.NSet.eq (VInjEndo.get_domset (get_vmap ms)) (G.get_vnames (get_pat ms)) andalso
    E.NSet.eq (EInjEndo.get_domset (get_emap ms)) (G.get_enames (get_pat ms)) andalso
    V.NSet.is_empty (get_p_nodeverts ms)
  
  fun schedule_all ms = let
    val pat = get_pat ms
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
  in
    ms |> set_u_circles circle_wvs
       |> set_u_nodeverts (G.get_node_vertices pat)
       |> set_u_wireverts wvs
       |> set_u_bare_wires bare_wire_es
  end

  val init_and_schedule_all = schedule_all oo init
  val init_subgraph_and_schedule_all = schedule_all ooo init_subgraph
  
  fun pretty ms = let
    fun pretty_vset prefix vs =
      if V.NSet.is_empty vs then []
      else [Pretty.block [
              Pretty.str (prefix^": "),
              V.NSet.pretty vs
           ]]
    fun pretty_eset prefix es =
      if E.NSet.is_empty es then []
      else [Pretty.block [
              Pretty.str (prefix^": "),
              E.NSet.pretty es
           ]]
  in
    Pretty.chunks [
      Pretty.str "Match {",
      Pretty.block [
        Pretty.str "  ", (* indent *)
        Pretty.chunks ([
          Pretty.block [
            Pretty.str "Pattern: ",
            G.pretty (get_pat ms)
          ],
          Pretty.block [
            Pretty.str "Target: ",
            G.pretty (get_tgt ms)
          ],
          Pretty.block [
            Pretty.str "VMap: ",
            VInjEndo.pretty (get_vmap ms)
          ],
          Pretty.block [
            Pretty.str "EMap: ",
            EInjEndo.pretty (get_emap ms)
          ]
        ]
        @ (pretty_vset "U_Cicles" (get_u_circles ms))
        @ (pretty_vset "U_NodeVerts" (get_u_nodeverts ms))
        @ (pretty_vset "U_WireVerts" (get_u_wireverts ms))
        @ (pretty_eset "U_BareWires" (get_u_bare_wires ms))
        @ (pretty_vset "P_NodeVerts" (get_p_nodeverts ms))
        @ (pretty_vset "Ps_NodeVerts" (get_ps_nodeverts ms)))
      ],
      Pretty.str "}"
    ]
  end;
  val print = Pretty.writeln o pretty;

  structure Sharing : OGRAPH_MATCH_STATE_SHARING 
  = struct
    structure G = G.SharingOGraph
    type T = T;
  end;
end
