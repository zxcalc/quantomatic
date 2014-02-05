(* The match state for graph matching *)
(* see docs/matching_algo for details *)

(* The type-sharing struct for OGRAPH_MATCH_STATE *)
signature OGRAPH_MATCH_STATE_SHARING
= sig
  type T
  structure Graph : OGRAPH_SHARING
end

(* The match state for concrete graph matching *)
signature OGRAPH_MATCH_STATE
= sig
  type T
  structure Graph : OGRAPH
  
  (* sharing via sharing structure *)
  structure Sharing : OGRAPH_MATCH_STATE_SHARING
    sharing type Sharing.T = T
    sharing Sharing.Graph = Graph.Sharing
  
  (* the graph to match from *)
  val get_pat             : T -> Graph.T
  (* the graph to match onto *)
  val get_tgt             : T -> Graph.T
  (* the vertex map so far (from pat verts to tgt verts *)
  val get_vmap            : T -> VVInj.T
  (* the edge map so far (from pat edges to tgt edges *)
  val get_emap            : T -> EEInj.T
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
  (* the vertices of tgt to be matched onto *)
  val get_tgt_verts       : T -> V.NSet.T
  (* the vertex/edge data substitution for the matching *)
  val get_match_psubst    : T -> Graph.psubst

  val update_pat          : (Graph.T -> Graph.T) -> T -> T
  val update_tgt          : (Graph.T -> Graph.T) -> T -> T
  val update_vmap         : (VVInj.T -> VVInj.T) -> T -> T
  val update_emap         : (EEInj.T -> EEInj.T) -> T -> T

  val update_u_circles    : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_u_nodeverts  : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_u_wireverts  : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_u_bare_wires : (E.NSet.T -> E.NSet.T) -> T -> T
  val update_p_nodeverts  : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_ps_nodeverts : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_tgt_verts    : (V.NSet.T -> V.NSet.T) -> T -> T
  val update_match_psubst : (Graph.psubst -> Graph.psubst) -> T -> T
  
  val set_pat             : Graph.T -> T -> T
  val set_tgt             : Graph.T -> T -> T
  val set_vmap            : VVInj.T -> T -> T
  val set_emap            : EEInj.T -> T -> T

  val set_u_circles       : V.NSet.T -> T -> T
  val set_u_nodeverts     : V.NSet.T -> T -> T
  val set_u_wireverts     : V.NSet.T -> T -> T
  val set_u_bare_wires    : E.NSet.T -> T -> T
  val set_p_nodeverts     : V.NSet.T -> T -> T
  val set_ps_nodeverts    : V.NSet.T -> T -> T
  val set_tgt_verts       : V.NSet.T -> T -> T
  val set_match_psubst    : Graph.psubst -> T -> T

  (* Add the given unmatched vertices, sorting as appropriate.
   * Two major assumptions: circles have only one wire-vertex and bare wires
   * have exactly two.  For bare wires, you should pass both the input and
   * the output; the edge connecting them will be added to u_bare_wires.  It is
   * important that you always pass either all or none of the wire-vertices on
   * any given wire.
   *)
  val extend_u            : V.NSet.T -> T -> T
  val extend_p_nodeverts  : V.NSet.T -> T -> T
  val extend_ps_nodeverts : V.NSet.T -> T -> T
  val extend_tgt_verts    : V.NSet.T -> T -> T

  (* initialise MatchState with nothing added *)
  val init : Graph.T -> (* pattern *)
             Graph.T -> (* target *)
             T

  val get_complete_nodeverts : T -> V.NSet.T
  
  (* returns true if vmap and evap represent a total graph hm *)
  (* will only return true if p_nodeverts is empty (local isomorphism) *)
  val is_total : T -> bool
  

  (* initialise MatchState and schedule all vertices in pattern and target to
   * be matched; arguments are *normalised* graphs *)
  val init_and_schedule_all : Graph.T -> (* pattern *)
                              Graph.T -> (* target *)
                              T
  
  (* initialise MatchState and schedule all vertices in pattern to be matched,
   * and the vertices from a subgraph of the target *)
  val init_and_schedule_for_subgraph : Graph.T -> (* pattern *)
                                       Graph.T -> (* target *)
                                       V.NSet.T -> (* node verts of target subgraph *)
                                       T
  
  (* schedule all vertices in pattern to be matched *)
  val schedule_all_pat : T -> T
  (* schedule all vertices in target to be matched *)
  val schedule_all_tgt : T -> T
  (* schedule the subgraph defined by the given node-vertices to be matched *)
  val schedule_tgt_subgraph : V.NSet.T -> T -> T
  
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
end

functor OGraphMatchState(Graph : OGRAPH) : OGRAPH_MATCH_STATE
= struct
  structure Graph = Graph
  
  datatype T = MatchState of {
    (* pattern and target graphs *)
    pat          : Graph.T,
    tgt          : Graph.T,
    (* vertex mapping from pat to tgt *)
    vmap         : VVInj.T,
    (* edge mapping from pat to tgt *)
    emap         : EEInj.T,
    (* circles, node-vertices, and wire-vertices to be matched *)
    u_circles    : V.NSet.T,
    u_nodeverts  : V.NSet.T,
    u_wireverts  : V.NSet.T,
    u_bare_wires : E.NSet.T,
    (* partially matched node-vertices *)
    p_nodeverts  : V.NSet.T,
    (* partially matched node-vertices, scheduled for re-matching *)
    ps_nodeverts : V.NSet.T,
    tgt_verts    : V.NSet.T,
    match_psubst : Graph.psubst
  }


  fun init pat tgt = MatchState {
    pat          = pat,
    tgt          = tgt,
    vmap         = VVInj.empty,
    emap         = EEInj.empty,
    u_circles    = V.NSet.empty,
    u_nodeverts  = V.NSet.empty,
    u_wireverts  = V.NSet.empty,
    u_bare_wires = E.NSet.empty,
    p_nodeverts  = V.NSet.empty,
    ps_nodeverts = V.NSet.empty,
    tgt_verts    = V.NSet.empty,
    match_psubst = Graph.init_psubst pat tgt
  }
  
  (* getters and setters *)
  fun update_pat f (MatchState r) = MatchState {pat=f(#pat r),tgt= #tgt r,vmap= #vmap r,emap= #emap r,u_circles= #u_circles r,u_nodeverts= #u_nodeverts r,u_wireverts= #u_wireverts r,u_bare_wires= #u_bare_wires r,p_nodeverts= #p_nodeverts r,ps_nodeverts= #ps_nodeverts r,tgt_verts= #tgt_verts r,match_psubst= #match_psubst r}
  fun update_tgt f (MatchState r) = MatchState {pat= #pat r,tgt=f(#tgt r),vmap= #vmap r,emap= #emap r,u_circles= #u_circles r,u_nodeverts= #u_nodeverts r,u_wireverts= #u_wireverts r,u_bare_wires= #u_bare_wires r,p_nodeverts= #p_nodeverts r,ps_nodeverts= #ps_nodeverts r,tgt_verts= #tgt_verts r,match_psubst= #match_psubst r}
  fun update_vmap f (MatchState r) = MatchState {pat= #pat r,tgt= #tgt r,vmap=f(#vmap r),emap= #emap r,u_circles= #u_circles r,u_nodeverts= #u_nodeverts r,u_wireverts= #u_wireverts r,u_bare_wires= #u_bare_wires r,p_nodeverts= #p_nodeverts r,ps_nodeverts= #ps_nodeverts r,tgt_verts= #tgt_verts r,match_psubst= #match_psubst r}
  fun update_emap f (MatchState r) = MatchState {pat= #pat r,tgt= #tgt r,vmap= #vmap r,emap=f(#emap r),u_circles= #u_circles r,u_nodeverts= #u_nodeverts r,u_wireverts= #u_wireverts r,u_bare_wires= #u_bare_wires r,p_nodeverts= #p_nodeverts r,ps_nodeverts= #ps_nodeverts r,tgt_verts= #tgt_verts r,match_psubst= #match_psubst r}
  fun update_u_circles f (MatchState r) = MatchState {pat= #pat r,tgt= #tgt r,vmap= #vmap r,emap= #emap r,u_circles=f(#u_circles r),u_nodeverts= #u_nodeverts r,u_wireverts= #u_wireverts r,u_bare_wires= #u_bare_wires r,p_nodeverts= #p_nodeverts r,ps_nodeverts= #ps_nodeverts r,tgt_verts= #tgt_verts r,match_psubst= #match_psubst r}
  fun update_u_nodeverts f (MatchState r) = MatchState {pat= #pat r,tgt= #tgt r,vmap= #vmap r,emap= #emap r,u_circles= #u_circles r,u_nodeverts=f(#u_nodeverts r),u_wireverts= #u_wireverts r,u_bare_wires= #u_bare_wires r,p_nodeverts= #p_nodeverts r,ps_nodeverts= #ps_nodeverts r,tgt_verts= #tgt_verts r,match_psubst= #match_psubst r}
  fun update_u_wireverts f (MatchState r) = MatchState {pat= #pat r,tgt= #tgt r,vmap= #vmap r,emap= #emap r,u_circles= #u_circles r,u_nodeverts= #u_nodeverts r,u_wireverts=f(#u_wireverts r),u_bare_wires= #u_bare_wires r,p_nodeverts= #p_nodeverts r,ps_nodeverts= #ps_nodeverts r,tgt_verts= #tgt_verts r,match_psubst= #match_psubst r}
  fun update_u_bare_wires f (MatchState r) = MatchState {pat= #pat r,tgt= #tgt r,vmap= #vmap r,emap= #emap r,u_circles= #u_circles r,u_nodeverts= #u_nodeverts r,u_wireverts= #u_wireverts r,u_bare_wires=f(#u_bare_wires r),p_nodeverts= #p_nodeverts r,ps_nodeverts= #ps_nodeverts r,tgt_verts= #tgt_verts r,match_psubst= #match_psubst r}
  fun update_p_nodeverts f (MatchState r) = MatchState {pat= #pat r,tgt= #tgt r,vmap= #vmap r,emap= #emap r,u_circles= #u_circles r,u_nodeverts= #u_nodeverts r,u_wireverts= #u_wireverts r,u_bare_wires= #u_bare_wires r,p_nodeverts=f(#p_nodeverts r),ps_nodeverts= #ps_nodeverts r,tgt_verts= #tgt_verts r,match_psubst= #match_psubst r}
  fun update_ps_nodeverts f (MatchState r) = MatchState {pat= #pat r,tgt= #tgt r,vmap= #vmap r,emap= #emap r,u_circles= #u_circles r,u_nodeverts= #u_nodeverts r,u_wireverts= #u_wireverts r,u_bare_wires= #u_bare_wires r,p_nodeverts= #p_nodeverts r,ps_nodeverts=f(#ps_nodeverts r),tgt_verts= #tgt_verts r,match_psubst= #match_psubst r}
  fun update_tgt_verts f (MatchState r) = MatchState {pat= #pat r,tgt= #tgt r,vmap= #vmap r,emap= #emap r,u_circles= #u_circles r,u_nodeverts= #u_nodeverts r,u_wireverts= #u_wireverts r,u_bare_wires= #u_bare_wires r,p_nodeverts= #p_nodeverts r,ps_nodeverts= #ps_nodeverts r,tgt_verts=f(#tgt_verts r),match_psubst= #match_psubst r}
  fun update_match_psubst f (MatchState r) = MatchState {pat= #pat r,tgt= #tgt r,vmap= #vmap r,emap= #emap r,u_circles= #u_circles r,u_nodeverts= #u_nodeverts r,u_wireverts= #u_wireverts r,u_bare_wires= #u_bare_wires r,p_nodeverts= #p_nodeverts r,ps_nodeverts= #ps_nodeverts r,tgt_verts= #tgt_verts r,match_psubst=f(#match_psubst r)}

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
  fun get_tgt_verts    (MatchState r) = #tgt_verts r
  fun get_match_psubst (MatchState r) = #match_psubst r

  fun set_pat          x  = update_pat          (fn _ => x)
  fun set_tgt          x  = update_tgt          (fn _ => x)
  fun set_vmap         x  = update_vmap         (fn _ => x)
  fun set_emap         x  = update_emap         (fn _ => x)
  fun set_u_circles    x  = update_u_circles    (fn _ => x)
  fun set_u_nodeverts  x  = update_u_nodeverts  (fn _ => x)
  fun set_u_wireverts  x  = update_u_wireverts  (fn _ => x)
  fun set_u_bare_wires x  = update_u_bare_wires (fn _ => x)
  fun set_p_nodeverts  x  = update_p_nodeverts  (fn _ => x)
  fun set_ps_nodeverts x  = update_ps_nodeverts (fn _ => x)
  fun set_tgt_verts    x  = update_tgt_verts    (fn _ => x)
  fun set_match_psubst x  = update_match_psubst (fn _ => x)



  val extend_u_circles    = update_u_circles o V.NSet.union_merge
  val extend_u_nodeverts  = update_u_nodeverts o V.NSet.union_merge
  val extend_u_wireverts  = update_u_wireverts o V.NSet.union_merge
  val extend_u_bare_wires = update_u_bare_wires o E.NSet.union_merge
  val extend_p_nodeverts  = update_p_nodeverts o V.NSet.union_merge
  val extend_ps_nodeverts = update_ps_nodeverts o V.NSet.union_merge
  val extend_tgt_verts    = update_tgt_verts o V.NSet.union_merge

  fun extend_u vs ms = let
    val pat = get_pat ms

    fun sort v (circle_wvs,nvs,wvs,bare_wires) =
      if Graph.is_node_vertex pat v
      then (circle_wvs,(V.NSet.ins_fresh v nvs),wvs,bare_wires)
      else
        let
          val in_es = Graph.get_in_edges pat v
          val out_es = Graph.get_out_edges pat v
          fun add_to_wvs () =
            (circle_wvs,nvs,(V.NSet.ins_fresh v wvs),bare_wires)
          fun add_to_wvs_or_bws e = let
            val v' = Graph.edge_get_other_vertex pat e v
          in
            if Graph.is_node_vertex pat v'
            then add_to_wvs ()
            else
              case Graph.wv_get_other_edge pat v' e
                of (SOME _) => add_to_wvs ()
                           (* will always happen twice for each bare wire: *)
                 | NONE => (circle_wvs,nvs,wvs,(E.NSet.add e bare_wires))
          end
        in
          case (E.NSet.tryget_singleton (in_es),
                E.NSet.tryget_singleton (out_es))
            of (SOME e1,SOME e2) =>
                 if E.name_eq (e1,e2)
                 then ((V.NSet.ins_fresh v circle_wvs),nvs,wvs,bare_wires)
                 else add_to_wvs ()
             | _ =>
                 (case E.NSet.list_of (E.NSet.union_merge in_es out_es)
                    of [_,_] => add_to_wvs ()
                     | [e] => add_to_wvs_or_bws e
                     | _ => (* bug *) raise Match)
        end

    val (circle_wvs,nvs,wvs,bare_wires) =
      V.NSet.fold sort vs (V.NSet.empty,V.NSet.empty,V.NSet.empty,E.NSet.empty)
  in
    ms |> extend_u_circles circle_wvs
       |> extend_u_nodeverts nvs
       |> extend_u_wireverts wvs
       |> extend_u_bare_wires bare_wires
  end

  fun add_edge_to_match (ep, vp) (et, vt) ms = let
    val ms' = update_emap (EEInj.add (ep, et)) ms
  in
    if VVInj.is_mapped (get_vmap ms) (vp,vt)
    then ms'
    else update_vmap (VVInj.add (vp, vt)) ms'
  end
  
  fun add_circle_to_match (cp, ct) ms = let
    val ep = (the o E.NSet.tryget_singleton) (Graph.get_in_edges (get_pat ms) cp)
    val et = (the o E.NSet.tryget_singleton) (Graph.get_in_edges (get_tgt ms) ct)
  in ms |> update_vmap (VVInj.add (cp, ct))
        |> update_emap (EEInj.add (ep, et))
  end
  
  fun add_vertex_to_match (vp, vt) ms = ms |> update_vmap (VVInj.add (vp, vt))

  fun get_complete_nodeverts ms = let
    val node_vs = V.NSet.filter (Graph.is_node_vertex (get_pat ms))
                  (VVInj.get_dom_set (get_vmap ms))
  in
    V.NSet.subtract node_vs (get_p_nodeverts ms)
  end
  
  fun is_total ms =
    V.NSet.eq (VVInj.get_dom_set (get_vmap ms)) (Graph.get_vertices (get_pat ms)) andalso
    E.NSet.eq (EEInj.get_dom_set (get_emap ms)) (Graph.get_edges (get_pat ms)) andalso
    V.NSet.is_empty (get_p_nodeverts ms)
  
  fun schedule_all_pat ms = let
    val pat = get_pat ms
    val wires = Graph.get_wire_list pat
    fun delete_wvs_in_wire (s,t,wv,_) set =
      set |> V.NSet.remove_set wv
          |> (if Graph.is_wire_vertex pat s then V.NSet.delete s else I)
          |> (if Graph.is_wire_vertex pat t then V.NSet.delete t else I)
    
    val circles = filter (fn (s,t,_,_) => V.name_eq (s,t) andalso Graph.is_wire_vertex pat s) wires
    val bare_wires = filter (fn (s,t,_,_) => not (V.name_eq (s,t)) andalso Graph.is_wire_vertex pat s andalso Graph.is_wire_vertex pat t) wires
    
    val wvs = fold delete_wvs_in_wire (circles @ bare_wires) (Graph.get_wire_vertices pat)
    val circle_wvs = fold (fn (s,_,_,_) => fn set => set |> V.NSet.add s) circles V.NSet.empty
    val bare_wire_es = fold (fn (_,_,_,es) => E.NSet.add ((the o E.NSet.get_min) es)) bare_wires E.NSet.empty  
  in
    ms |> set_u_circles circle_wvs
       |> set_u_nodeverts (Graph.get_node_vertices pat)
       |> set_u_wireverts wvs
       |> set_u_bare_wires bare_wire_es
  end

  fun schedule_all_tgt ms = set_tgt_verts (Graph.get_vertices (get_tgt ms)) ms;
  fun schedule_tgt_subgraph vs ms = let
    val tgt = get_tgt ms
    val tgt_subg = Graph.get_open_subgraph tgt vs
  in
    set_tgt_verts (Graph.get_vertices tgt_subg) ms
  end;

  val init_and_schedule_all = schedule_all_tgt o schedule_all_pat oo init;
  fun init_and_schedule_for_subgraph pat tgt vs =
    schedule_tgt_subgraph vs (schedule_all_pat (init pat tgt));
  
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
            Graph.pretty (get_pat ms)
          ],
          Pretty.block [
            Pretty.str "Target: ",
            Graph.pretty (get_tgt ms)
          ],
          Pretty.block [
            Pretty.str "VMap: ",
            VVInj.pretty (get_vmap ms)
          ],
          Pretty.block [
            Pretty.str "EMap: ",
            EEInj.pretty (get_emap ms)
          ]
        ]
        @ (pretty_vset "U_Cicles" (get_u_circles ms))
        @ (pretty_vset "U_NodeVerts" (get_u_nodeverts ms))
        @ (pretty_vset "U_WireVerts" (get_u_wireverts ms))
        @ (pretty_eset "U_BareWires" (get_u_bare_wires ms))
        @ (pretty_vset "P_NodeVerts" (get_p_nodeverts ms))
        @ (pretty_vset "Ps_NodeVerts" (get_ps_nodeverts ms))
        @ (pretty_vset "Tgt Verts" (get_tgt_verts ms)))
      ],
      Pretty.str "}"
    ]
  end;
  val print = Pretty.writeln o pretty;

  structure Sharing : OGRAPH_MATCH_STATE_SHARING 
  = struct
    structure Graph = Graph.Sharing
    type T = T
  end;
end
