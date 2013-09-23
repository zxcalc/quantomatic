(* A wrapper for matching bang graphs against concrete graphs
 * see docs/matching_algo for details
 *
 * This implements the "Lazy !-Box Expansion" algorithm
 *
 * This wrapper greedily matches the whole concrete part of the
 * graph, and then picks an arbitrary top-level !-box and tries
 * both expanding it and killing it.
 *)
functor GreedyMatchSearch(
  Graph : BANG_GRAPH
) : BG_MATCH_SEARCH =
struct

  structure Log : LOG = Log(val init_level = 0);
  structure Graph = Graph;
  structure InnerMatchState = OGraphMatchState(Graph);
  structure InnerMatchSearch = ProgressiveMatchSearch(InnerMatchState);
  structure InnerLog = InnerMatchSearch.Log;
  structure Match = BangGraphMatch(Graph);

  datatype State = MatchState of {
    init_pat     : Graph.T,
    bbox_ops     : bbox_op list,
    wild_bboxes  : B.NSet.T,
    inner_state  : InnerMatchState.T
  }

  fun update_init_pat f (MatchState r) = MatchState {init_pat=f(#init_pat r),bbox_ops= #bbox_ops r,wild_bboxes= #wild_bboxes r,inner_state= #inner_state r}
  fun update_bbox_ops f (MatchState r) = MatchState {init_pat= #init_pat r,bbox_ops=f(#bbox_ops r),wild_bboxes= #wild_bboxes r,inner_state= #inner_state r}
  fun update_wild_bboxes f (MatchState r) = MatchState {init_pat= #init_pat r,bbox_ops= #bbox_ops r,wild_bboxes=f(#wild_bboxes r),inner_state= #inner_state r}
  fun update_inner_state f (MatchState r) = MatchState {init_pat= #init_pat r,bbox_ops= #bbox_ops r,wild_bboxes= #wild_bboxes r,inner_state=f(#inner_state r)}

  fun get_init_pat    (MatchState r) = #init_pat r
  fun get_bbox_ops    (MatchState r) = #bbox_ops r
  fun get_wild_bboxes (MatchState r) = #wild_bboxes r
  fun get_inner_state (MatchState r) = #inner_state r

  fun set_init_pat    x  = update_init_pat    (fn _ => x)
  fun set_bbox_ops    x  = update_bbox_ops    (fn _ => x)
  fun set_wild_bboxes x  = update_wild_bboxes (fn _ => x)
  fun set_inner_state x  = update_inner_state (fn _ => x)

  (* the inner match state actually holds the canonical reference to the expanded pattern graph *)
  val get_pat = InnerMatchState.get_pat o get_inner_state
  val update_pat = update_inner_state o InnerMatchState.update_pat
  fun set_pat x = update_pat (fn _ => x)

  fun add_wild_bbox bb = update_wild_bboxes (B.NSet.add bb)

  fun log_p pretty_f lvl name =
    Log.logf lvl (fn g => Pretty.string_of
        (Pretty.chunks [Pretty.str (name^":"),(pretty_f g)]))
  val log_graph = log_p Graph.pretty
  val log_v_nset = log_p V.NSet.pretty

  fun init pat tgt = MatchState {
    init_pat = pat,
    bbox_ops = [],
    wild_bboxes = B.NSet.empty,
    inner_state = InnerMatchState.init pat tgt
  }

  fun schedule_concrete new_vs new_es bms = let
    val ms = get_inner_state bms
    val pat = InnerMatchState.get_pat ms

    val touched = Graph.get_adj_vertices_to_set pat new_vs
    (*val to_sched = V.NSet.intersect (InnerMatchState.get_p_nodeverts ms) touched*)
    
    (* re-schedule all partials *)
    (* TODO: this can be improved by only re-scheduling partials next to expanded or
     * killed bboxes. The code it replaces is incorrect, as it doesn't reschedule
     * for killed. *)
    val to_sched = InnerMatchState.get_p_nodeverts ms

    val unbboxed = V.NSet.subtract new_vs (Graph.get_bboxed pat)
    val wires = Graph.get_wires_for_edges pat new_es
    fun delete_wvs_in_wire (s,t,wv,_) set =
      set |> V.NSet.remove_set wv
          |> (if Graph.is_wire_vertex pat s then V.NSet.delete s else I)
          |> (if Graph.is_wire_vertex pat t then V.NSet.delete t else I)
    val circles = filter (fn (s,t,_,_) =>
                    V.name_eq (s,t) andalso
                    Graph.is_wire_vertex pat s andalso
                    V.NSet.contains unbboxed s
                  ) wires
    val bare_wires = filter (fn (s,t,_,_) =>
                       not (V.name_eq (s,t)) andalso
                       Graph.is_wire_vertex pat s andalso
                       Graph.is_wire_vertex pat t andalso
                       V.NSet.contains unbboxed s
                     ) wires
    val nvs = V.NSet.filter (Graph.is_node_vertex pat) new_vs
    val wvs' = V.NSet.filter (Graph.is_wire_vertex pat) new_vs
    val wvs = fold delete_wvs_in_wire (circles @ bare_wires) wvs'
    val circle_wvs = fold (fn (s,_,_,_) => fn set => set |> V.NSet.add s) circles V.NSet.empty
    val bare_wire_es = fold (fn (_,_,_,es) => E.NSet.add ((the o E.NSet.get_min) es)) bare_wires E.NSet.empty  
  in
    set_inner_state
    (ms |> InnerMatchState.extend_u_circles circle_wvs
        |> InnerMatchState.extend_u_nodeverts (V.NSet.intersect unbboxed nvs)
        |> InnerMatchState.extend_u_wireverts (V.NSet.intersect unbboxed wvs)
        |> InnerMatchState.extend_u_bare_wires bare_wire_es
        |> InnerMatchState.extend_ps_nodeverts to_sched)
    bms
  end

  fun schedule_new_concrete bms = let
    val ms = get_inner_state bms
    val pat = InnerMatchState.get_pat ms
    val new_vs = V.NSet.subtract (Graph.get_vertices pat)
                                 (VVInj.get_dom_set (InnerMatchState.get_vmap ms))
    val new_es = E.NSet.subtract (Graph.get_edges pat)
                                 (EEInj.get_dom_set (InnerMatchState.get_emap ms))
  in
    schedule_concrete new_vs new_es bms
  end
  fun schedule_concrete_pat bms = let
    val pat = InnerMatchState.get_pat (get_inner_state bms)
    val vs = Graph.get_vertices pat
    val es = Graph.get_edges pat
  in
    schedule_concrete vs es bms
  end

  (* TODO: it is probably worth optimising this some more *)
  fun doomedf ms pv = let
    val pat = InnerMatchState.get_pat ms
    val tgt = InnerMatchState.get_tgt ms
    val tv = VVInj.get (InnerMatchState.get_vmap ms) pv
    val p_arity = Graph.get_arity pat pv
    val t_arity = Graph.get_arity tgt tv
  in
    (
      (Arity.get_in p_arity < Arity.get_in t_arity) andalso
      not (V.NSet.exists (Graph.is_bboxed pat) (Graph.get_predecessor_vertices pat pv))
    )
    orelse
    (
      (Arity.get_out p_arity < Arity.get_out t_arity) andalso
      not (V.NSet.exists (Graph.is_bboxed pat) (Graph.get_successor_vertices pat pv))
    )
    orelse
    (
      (Arity.get_undir p_arity < Arity.get_undir t_arity) andalso
      not (V.NSet.exists (Graph.is_bboxed pat) (Graph.get_sibling_vertices pat pv))
    )
  end

  fun get_next_bbox g =
    B.NSet.get_exists (not o (Graph.bbox_has_parents g)) (Graph.get_bboxes g)

  fun is_total bms =
    if (InnerMatchState.is_total (get_inner_state bms))
    then (Log.log 2 "==== total match found ===="; true)
    else (Log.log 2 "==== match failed to be total ===="; false)

  val get_current_pat = InnerMatchState.get_pat o get_inner_state;
  fun match_bare_wires ms = let
    val ims = get_inner_state ms
  in
    Seq.map (fn ims => set_inner_state ims ms)
            (InnerMatchSearch.match_bare_wires ims)
  end;

  (* get the orbit of a single vertex under a series of expansions of a single bbox *)
  fun get_orbit (bbox_ops as (BBExpand {bbox=bbox,vmap=vmap,bbox_map=_} :: ops)) =
    (case V.NSet.get_local_bot (VSub.get_dom_set vmap)
       of SOME v =>
            let
              fun orb (BBExpand {bbox=bbox',vmap=vmap',bbox_map=_} :: ops) =
                    if B.name_eq (bbox,bbox')
                    then V.NSet.add (VSub.get vmap' v) (orb ops)
                    else V.NSet.empty
                | orb _ = V.NSet.empty
            in orb bbox_ops
            end
        | NONE => V.NSet.empty)
    | get_orbit _ = V.NSet.empty

  fun match_pending ms = let
    val ims = get_inner_state ms
  in
    Seq.map_filter (fn ims =>
      let
        val orbit = get_orbit (get_bbox_ops ms)
        val _ = Log.logf 2 (fn () => 
            "orbit: " ^ Pretty.string_of (V.NSet.pretty orbit)) ()
        val orbit_map = (InnerMatchState.get_vmap ims)
              |> VVInj.restrict_dom orbit
      in
        if VVInj.is_monotone orbit_map
        then SOME (ms |> set_inner_state ims)
        else NONE
      end) (InnerMatchSearch.match_pending doomedf ims)
  end;

  fun finish_match ms =
      Seq.filter is_total
        (match_bare_wires ms)

  fun expand_pat_bbox b ms = let
    val _ = Log.logf 2 (fn () => "Expanding !-box "^(B.string_of_name b)) ()
    val (bb_op, pat') = Graph.expand_bbox_op b (get_pat ms)
  in
    ms |> set_pat pat'
       |> update_bbox_ops (cons bb_op)
  end;

  fun kill_pat_bbox b ms = let
    val _ = Log.logf 2 (fn () => "Killing !-box "^(B.string_of_name b)) ()
    val (bb_op, pat') = Graph.kill_bbox_op b (get_pat ms)
  in
    ms |> set_pat pat'
       |> update_bbox_ops (cons bb_op)
  end;

  fun kill_bboxes_adj_to_complete_nvs bms = let
    val ms = get_inner_state bms
    val pat = InnerMatchState.get_pat ms
    val complete = InnerMatchState.get_complete_nodeverts ms
    val touched = Graph.get_adj_vertices_to_set pat complete
    fun kill_all_bboxes v bms = let
      val pat = InnerMatchState.get_pat (get_inner_state bms)
    in
      B.NSet.fold kill_pat_bbox (Graph.get_bboxes_containing_vertex pat v) bms
      handle Graph.no_such_vertex_exp _ => bms
    end
  in
    V.NSet.fold kill_all_bboxes touched bms
  end

  fun kill_wild_bboxes ms = let
    fun is_end_of_bare_wire g v =
      if Graph.is_input g v
      then
        case E.NSet.tryget_singleton (Graph.get_out_edges g v)
          of NONE => false (* this shouldn't happen, but isn't strictly impossible *)
           | SOME e => Graph.is_output g (Graph.get_edge_target g e)
      else
        if Graph.is_output g v
        then
          case E.NSet.tryget_singleton (Graph.get_in_edges g v)
            of NONE => false (* this shouldn't happen, but isn't strictly impossible *)
             | SOME e => Graph.is_input g (Graph.get_edge_source g e)
        else
          false
    fun is_wild g bbox =
      V.NSet.forall (is_end_of_bare_wire g) (Graph.get_vertices_in_bbox g bbox)
    fun kill_pat_bbox' bbox ms =
      ms |> kill_pat_bbox bbox
         |> add_wild_bbox bbox
    fun kill_if_wild bbox ms =
      (if is_wild (InnerMatchState.get_pat (get_inner_state ms)) bbox
       then kill_pat_bbox' bbox ms
       else ms)
      handle Graph.no_such_bbox_exp _ => ms
    val pat = InnerMatchState.get_pat (get_inner_state ms)
  in
    B.NSet.fold kill_if_wild (Graph.get_bboxes pat) ms
  end

  val kill_impossible_bboxes = kill_wild_bboxes
                             o kill_bboxes_adj_to_complete_nvs
  
  fun match_loop ms' = let
    fun kill_and_expand b ms = let
      val do_kill = choose_next_bbox o (kill_pat_bbox b)
      val do_copy = match_loop o schedule_new_concrete
                               o (expand_pat_bbox b)
    in
      Seq.append (Seq.maps do_kill (Seq.single ms))
                 (Seq.maps do_copy (Seq.single ms))
    end
    and choose_next_bbox ms =
          case get_next_bbox (get_current_pat ms)
            of NONE => finish_match ms
             | SOME b => kill_and_expand b ms
  in
    Seq.maps choose_next_bbox
      (Seq.map kill_impossible_bboxes
        (match_pending ms'))
  end

  fun matches_from_ms ms = let
    val ims = get_inner_state ms
    val (init_pat,pat,bbox_ops,tgt,vmap,emap,wild) = (
      get_init_pat ms,
      get_pat ms,
      get_bbox_ops ms,
      InnerMatchState.get_tgt ims,
      InnerMatchState.get_vmap ims,
      InnerMatchState.get_emap ims,
      get_wild_bboxes ms
    )
  in
    Seq.map (Match.mk init_pat pat bbox_ops tgt vmap emap BBInj.empty wild)
            (Graph.solve_psubst (InnerMatchState.get_match_psubst ims))
  end;

  fun find_matches ms = Seq.maps matches_from_ms (match_loop ms);

  fun match_normalised pat tgt = let
    val _ = log_graph 3 "GreedyMatchSearch: normalised pattern" pat
    val _ = log_graph 3 "GreedyMatchSearch: normalised target" tgt
    val ms = init pat tgt
           |> schedule_concrete_pat
           |> update_inner_state (InnerMatchState.schedule_all_tgt)
  in
    find_matches ms
  end

  fun match pat tgt =
    match_normalised (Graph.normalise pat) (Graph.normalise tgt)

  fun match_subgraph_normalised pat tgt tgt_verts = let
    val _ = log_graph 3 "GreedyMatchSearch: normalised pattern" pat
    val _ = log_graph 3 "GreedyMatchSearch: normalised target" tgt
    val _ = log_v_nset 4 "GreedyMatchSearch: target vertices" tgt_verts
    val ms = init pat tgt
           |> schedule_concrete_pat
           |> update_inner_state (InnerMatchState.schedule_tgt_subgraph tgt_verts)
  in
    find_matches ms
  end
  fun match_subgraph pat tgt =
    match_subgraph_normalised (Graph.normalise pat) (Graph.normalise tgt)

  structure Sharing =
  struct
    structure Graph = Graph.Sharing
    structure Match = Match.Sharing
  end
end
