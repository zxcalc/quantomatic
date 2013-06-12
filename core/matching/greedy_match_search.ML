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
  BBoxOperableGraph : BBOX_OPERABLE_GRAPH
) : BG_MATCH_SEARCH =
struct

  structure Log : LOG = Log(val init_level = 0);

  structure BBoxOperableGraph = BBoxOperableGraph;
  structure G = BBoxOperableGraph.G;
  structure InnerMatchState = OGraphMatchState(G);
  structure InnerMatchSearch = SimpleInnerMatchSearch(InnerMatchState);
  structure Match = BangGraphMatch(BBoxOperableGraph);

  datatype State = MatchState of {
    pat          : BBoxOperableGraph.T,
    wild_bboxes  : B.NSet.T,
    inner_state  : InnerMatchState.T
  };

  (* shorthand *)
  structure Oper = BBoxOperableGraph;
  structure Inner = InnerMatchState;

  fun log_p pretty_f lvl name =
    Log.logf lvl (fn g => Pretty.string_of
        (Pretty.chunks [Pretty.str (name^":"),(pretty_f g)]))
  val log_graph = log_p G.pretty
  val log_v_nset = log_p V.NSet.pretty

  fun init pat tgt = MatchState {
    pat = pat,
    wild_bboxes = B.NSet.empty,
    inner_state = Inner.init (Oper.get_current_graph pat) tgt
  };

  fun get_inner_state (MatchState state) = #inner_state state;
  fun update_inner_state f (MatchState state) = MatchState {
    pat          = #pat state,
    wild_bboxes  = #wild_bboxes state,
    inner_state  = f (#inner_state state)
  };
  val set_inner_state = update_inner_state o K;

  fun get_pat (MatchState state) = #pat state;
  fun update_pat f (MatchState state) = let
    val oper = f (#pat state)
    val pat = Oper.get_current_graph oper
  in
    MatchState {
      pat          = oper,
      wild_bboxes  = #wild_bboxes state,
      inner_state  = Inner.set_pat pat (#inner_state state)
    }
  end;
  val set_pat = update_pat o K;

  fun get_wild_bboxes (MatchState state) = #wild_bboxes state
  fun add_wild_bbox b (MatchState state) = MatchState {
    pat          = #pat state,
    wild_bboxes  = B.NSet.add b (#wild_bboxes state),
    inner_state  = #inner_state state
  };

  fun schedule_concrete new_vs new_es bms = let
    val ms = get_inner_state bms
    val pat = Inner.get_pat ms

    val touched = G.incident_vertices pat new_vs
    val to_sched = V.NSet.intersect (Inner.get_p_nodeverts ms) touched

    val unbboxed = V.NSet.subtract new_vs (G.get_bboxed pat)
    val wires = G.get_wires_for_edges pat new_es
    fun delete_wvs_in_wire (s,t,wv,_) set =
      set |> V.NSet.remove_set wv
          |> (if G.is_wire_vertex pat s then V.NSet.delete s else I)
          |> (if G.is_wire_vertex pat t then V.NSet.delete t else I)
    val circles = filter (fn (s,t,_,_) =>
                    V.name_eq (s,t) andalso
                    G.is_wire_vertex pat s andalso
                    V.NSet.contains unbboxed s
                  ) wires
    val bare_wires = filter (fn (s,t,_,_) =>
                       not (V.name_eq (s,t)) andalso
                       G.is_wire_vertex pat s andalso
                       G.is_wire_vertex pat t andalso
                       V.NSet.contains unbboxed s
                     ) wires
    val nvs = V.NSet.filter (G.is_node_vertex pat) new_vs
    val wvs' = V.NSet.filter (G.is_wire_vertex pat) new_vs
    val wvs = fold delete_wvs_in_wire (circles @ bare_wires) wvs'
    val circle_wvs = fold (fn (s,_,_,_) => fn set => set |> V.NSet.add s) circles V.NSet.empty
    val bare_wire_es = fold (fn (_,_,_,es) => E.NSet.add ((the o E.NSet.get_min) es)) bare_wires E.NSet.empty  
  in
    set_inner_state
    (ms |> Inner.extend_u_circles circle_wvs
        |> Inner.extend_u_nodeverts (V.NSet.intersect unbboxed nvs)
        |> Inner.extend_u_wireverts (V.NSet.intersect unbboxed wvs)
        |> Inner.extend_u_bare_wires bare_wire_es
        |> Inner.extend_ps_nodeverts to_sched)
    bms
  end

  fun schedule_new_concrete bms = let
    val ms = get_inner_state bms
    val pat = Inner.get_pat ms
    val new_vs = V.NSet.subtract (G.get_vnames pat)
                                 (VInjEndo.get_domset (Inner.get_vmap ms))
    val new_es = E.NSet.subtract (G.get_enames pat)
                                 (EInjEndo.get_domset (Inner.get_emap ms))
  in
    schedule_concrete new_vs new_es bms
  end
  fun schedule_concrete_pat bms = let
    val pat = Inner.get_pat (get_inner_state bms)
    val vs = G.get_vnames pat
    val es = G.get_enames pat
  in
    schedule_concrete vs es bms
  end

  (* TODO: it is probably worth optimising this some more *)
  fun doomedf ms pv = let
    val pat = InnerMatchState.get_pat ms
    val tgt = InnerMatchState.get_tgt ms
    val tv = VInjEndo.domf (InnerMatchState.get_vmap ms) pv
    val parity = G.get_arity pat pv
    val tarity = G.get_arity tgt tv
    fun has_bbox es = let
      fun other_end_in_bbox e =
        G.is_bboxed pat (G.edge_get_other_vertex pat e pv)
    in
      E.NSet.exists other_end_in_bbox es
    end
    val need_more_ins = Arity.get_in parity < Arity.get_in tarity
    val need_more_outs = Arity.get_out parity < Arity.get_out tarity
    val need_more_undir = Arity.get_undir parity < Arity.get_undir tarity
    fun chk_bbox (inctyp,v,(e,(dirtyp,_))) (nmins,nmouts,nmundir) =
      (* we can ignore selfloops, as the graph is normalised *)
      case (inctyp,dirtyp)
        of (G.SelfLoop,_) => raise ERROR ("Unexpected self-loop in normed graph ("^
                V.string_of_name pv^","^E.string_of_name e^")")
         | (_,G.UnDirected) => (nmins,nmouts,nmundir andalso not (G.is_bboxed pat v))
         | (G.Incoming,G.Directed) => (nmins andalso not (G.is_bboxed pat v),nmouts,nmundir)
         | (G.Outgoing,G.Directed) => (nmins,nmouts andalso not (G.is_bboxed pat v),nmundir)
  in
    (need_more_ins orelse need_more_outs orelse need_more_undir)
    andalso
    (let val (nmins,nmouts,nmundir) =
      G.fold_adj chk_bbox pat pv (need_more_ins,need_more_outs,need_more_undir)
     in nmins orelse nmouts orelse nmundir end)
  end

  fun get_next_bbox g =
    B.NSet.get_exists (not o (G.bbox_has_parent g)) (G.get_bboxes g)

  val is_total = InnerMatchState.is_total o get_inner_state;
  val get_current_pat = InnerMatchState.get_pat o get_inner_state;
  fun match_bare_wires ms = let
    val ims = get_inner_state ms
  in
    Seq.map (fn ims => set_inner_state ims ms)
            (InnerMatchSearch.match_bare_wires ims)
  end;
  fun match_pending ms = let
    val ims = get_inner_state ms
  in
    Seq.map (fn ims => set_inner_state ims ms)
            (InnerMatchSearch.match_pending doomedf ims)
  end;

  fun finish_match ms =
      Seq.filter is_total
        (match_bare_wires ms)

  fun expand_pat_bbox b ms = let
    val _ = Log.logf 2 (fn () => "Expanding !-box "^(B.string_of_name b)) ()
  in
    update_pat (BBoxOperableGraph.expand_bbox b) ms
  end;

  fun kill_pat_bbox b ms = let
    val _ = Log.logf 2 (fn () => "Killing !-box "^(B.string_of_name b)) ()
  in
    update_pat (BBoxOperableGraph.kill_bbox b) ms
  end;

  fun kill_bboxes_adj_to_complete_nvs bms = let
    val ms = get_inner_state bms
    val pat = Inner.get_pat ms
    val complete = Inner.get_complete_nodeverts ms
    val touched = G.incident_vertices pat complete
    fun kill_all_bboxes v bms = let
      val pat = Inner.get_pat (get_inner_state bms)
    in
      B.NSet.fold kill_pat_bbox (G.get_bboxes_of pat v) bms
      handle G.no_such_vname_exp _ => bms
    end
  in
    V.NSet.fold kill_all_bboxes touched bms
  end

  fun kill_wild_bboxes ms = let
    fun is_end_of_bare_wire g v =
      if G.is_input_vertex g v
      then
        case E.NSet.tryget_singleton (G.get_out_edges g v)
          of NONE => false (* this shouldn't happen, but isn't strictly impossible *)
           | SOME e => G.is_output_vertex g (G.get_edge_target g e)
      else
        if G.is_output_vertex g v
        then
          case E.NSet.tryget_singleton (G.get_in_edges g v)
            of NONE => false (* this shouldn't happen, but isn't strictly impossible *)
             | SOME e => G.is_input_vertex g (G.get_edge_source g e)
        else
          false
    fun is_wild g bbox =
      V.NSet.forall (is_end_of_bare_wire g) (G.get_bbox_contents g bbox)
    fun kill_pat_bbox' bbox ms =
      ms |> kill_pat_bbox bbox
         |> add_wild_bbox bbox
    fun kill_if_wild bbox ms =
      (if is_wild (Inner.get_pat (get_inner_state ms)) bbox
       then kill_pat_bbox' bbox ms
       else ms)
      handle G.no_such_bbox_exp _ => ms
    val pat = Inner.get_pat (get_inner_state ms)
  in
    B.NSet.fold kill_if_wild (G.get_bboxes pat) ms
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
      case get_next_bbox (get_current_pat ms) of
           NONE => finish_match ms
         | SOME b => kill_and_expand b ms
  in
    Seq.maps choose_next_bbox
      (Seq.map kill_impossible_bboxes
        (match_pending ms'))
  end

  fun match_from_ms ms = let
    val ims = get_inner_state ms
  in
    Match.mk (get_pat ms)
             (InnerMatchState.get_tgt ims)
             (InnerMatchState.get_vmap ims)
             (InnerMatchState.get_emap ims)
             (BInjEndo.empty)
             (InnerMatchState.get_match_subst ims)
             (get_wild_bboxes ms)
  end;

  fun find_matches ms = Seq.map match_from_ms (match_loop ms);

  fun match_normalised pat tgt = let
    val _ = log_graph 3 "GreedyMatchSearch: normalised pattern"
                        (BBoxOperableGraph.get_current_graph pat)
    val _ = log_graph 3 "GreedyMatchSearch: normalised target" tgt
    val ms = init pat tgt
           |> schedule_concrete_pat
           |> update_inner_state (InnerMatchState.schedule_all_tgt)
  in
    find_matches ms
  end
  fun match pat tgt =
    match_normalised (BBoxOperableGraph.normalise pat) (G.normalise tgt)

  fun match_subgraph_normalised pat tgt tgt_verts = let
    val _ = log_graph 3 "GreedyMatchSearch: normalised pattern"
                        (BBoxOperableGraph.get_current_graph pat)
    val _ = log_graph 3 "GreedyMatchSearch: normalised target" tgt
    val _ = log_v_nset 4 "GreedyMatchSearch: target vertices" tgt_verts
    val ms = init pat tgt
           |> schedule_concrete_pat
           |> update_inner_state (InnerMatchState.schedule_tgt_subgraph tgt_verts)
  in
    find_matches ms
  end
  fun match_subgraph pat tgt =
    match_subgraph_normalised (BBoxOperableGraph.normalise pat) (G.normalise tgt)

  structure Sharing =
  struct
    structure G = G.SharingOGraph
    structure Match = Match.Sharing
  end
end
