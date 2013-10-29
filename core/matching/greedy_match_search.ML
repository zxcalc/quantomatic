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

  structure Log : LOG = Log(val init_level = 0)
  structure Graph = Graph
  structure InnerMatchState = OGraphMatchState(Graph)
  structure InnerMatchSearch = ProgressiveMatchSearch(InnerMatchState)
  structure InnerLog = InnerMatchSearch.Log
  structure Match = BangGraphMatch(Graph)

  datatype State = MatchState of {
    init_pat        : Graph.T,
    bbox_ops        : bbox_op list,
    wild_bboxes     : B.NSet.T,
    bmap            : BBInj.T,
    pending_pat_bbs : B.NSet.T, (* unmatched, fixed !-boxes of pattern *)
    (* We split the operable (unfixed, top-level) !-boxes of the pattern into
     * two parts, in order to remove some duplication.  We wish to force all
     * copy operations to happen before any expands (for a given !-box), so
     * operable !-boxes start off in copyable_bbs, then move to expandable_bbs
     * and finally are removed entirely when they are killed *)
    copyable_bbs    : B.NSet.T,
    expandable_bbs  : B.NSet.T,
    pat_fix_tags    : BFFn.T,   (* fixity tags of pattern *)
    tgt_fix_tags    : BFFn.T,   (* fixity tags of target *)
    t_saved         : V.NSet.T, (* saved state of (#tgt_verts innerstate) *)
    tgt_pool        : V.NSet.T, (* all the vertices we are ever allowed to match *)
    inner_state     : InnerMatchState.T
  }

  fun update_init_pat f (MatchState r) = MatchState {
    init_pat        = f(#init_pat r),
    bbox_ops        = #bbox_ops r,
    wild_bboxes     = #wild_bboxes r,
    bmap            = #bmap r,
    pending_pat_bbs = #pending_pat_bbs r,
    copyable_bbs    = #copyable_bbs r,
    expandable_bbs  = #expandable_bbs r,
    pat_fix_tags    = #pat_fix_tags r,
    tgt_fix_tags    = #tgt_fix_tags r,
    t_saved         = #t_saved r,
    tgt_pool        = #tgt_pool r,
    inner_state     = #inner_state r
  }

  fun update_bbox_ops f (MatchState r) = MatchState {
    init_pat        = #init_pat r,
    bbox_ops        = f(#bbox_ops r),
    wild_bboxes     = #wild_bboxes r,
    bmap            = #bmap r,
    pending_pat_bbs = #pending_pat_bbs r,
    copyable_bbs    = #copyable_bbs r,
    expandable_bbs  = #expandable_bbs r,
    pat_fix_tags    = #pat_fix_tags r,
    tgt_fix_tags    = #tgt_fix_tags r,
    t_saved         = #t_saved r,
    tgt_pool        = #tgt_pool r,
    inner_state     = #inner_state r
  }

  fun update_wild_bboxes f (MatchState r) = MatchState {
    init_pat        = #init_pat r,
    bbox_ops        = #bbox_ops r,
    wild_bboxes     = f(#wild_bboxes r),
    bmap            = #bmap r,
    pending_pat_bbs = #pending_pat_bbs r,
    copyable_bbs    = #copyable_bbs r,
    expandable_bbs  = #expandable_bbs r,
    pat_fix_tags    = #pat_fix_tags r,
    tgt_fix_tags    = #tgt_fix_tags r,
    t_saved         = #t_saved r,
    tgt_pool        = #tgt_pool r,
    inner_state     = #inner_state r
  }

  fun update_bmap f (MatchState r) = MatchState {
    init_pat        = #init_pat r,
    bbox_ops        = #bbox_ops r,
    wild_bboxes     = #wild_bboxes r,
    bmap            = f(#bmap r),
    pending_pat_bbs = #pending_pat_bbs r,
    copyable_bbs    = #copyable_bbs r,
    expandable_bbs  = #expandable_bbs r,
    pat_fix_tags    = #pat_fix_tags r,
    tgt_fix_tags    = #tgt_fix_tags r,
    t_saved         = #t_saved r,
    tgt_pool        = #tgt_pool r,
    inner_state     = #inner_state r
  }

  fun update_pending_pat_bbs f (MatchState r) = MatchState {
    init_pat        = #init_pat r,
    bbox_ops        = #bbox_ops r,
    wild_bboxes     = #wild_bboxes r,
    bmap            = #bmap r,
    pending_pat_bbs = f(#pending_pat_bbs r),
    copyable_bbs    = #copyable_bbs r,
    expandable_bbs  = #expandable_bbs r,
    pat_fix_tags    = #pat_fix_tags r,
    tgt_fix_tags    = #tgt_fix_tags r,
    t_saved         = #t_saved r,
    tgt_pool        = #tgt_pool r,
    inner_state     = #inner_state r
  }

  fun update_copyable_bbs f (MatchState r) = MatchState {
    init_pat        = #init_pat r,
    bbox_ops        = #bbox_ops r,
    wild_bboxes     = #wild_bboxes r,
    bmap            = #bmap r,
    pending_pat_bbs = #pending_pat_bbs r,
    copyable_bbs    = f(#copyable_bbs r),
    expandable_bbs  = #expandable_bbs r,
    pat_fix_tags    = #pat_fix_tags r,
    tgt_fix_tags    = #tgt_fix_tags r,
    t_saved         = #t_saved r,
    tgt_pool        = #tgt_pool r,
    inner_state     = #inner_state r
  }

  fun update_expandable_bbs f (MatchState r) = MatchState {
    init_pat        = #init_pat r,
    bbox_ops        = #bbox_ops r,
    wild_bboxes     = #wild_bboxes r,
    bmap            = #bmap r,
    pending_pat_bbs = #pending_pat_bbs r,
    copyable_bbs    = #copyable_bbs r,
    expandable_bbs  = f(#expandable_bbs r),
    pat_fix_tags    = #pat_fix_tags r,
    tgt_fix_tags    = #tgt_fix_tags r,
    t_saved         = #t_saved r,
    tgt_pool        = #tgt_pool r,
    inner_state     = #inner_state r
  }

  fun update_pat_fixed f (MatchState r) = MatchState {
    init_pat        = #init_pat r,
    bbox_ops        = #bbox_ops r,
    wild_bboxes     = #wild_bboxes r,
    bmap            = #bmap r,
    pending_pat_bbs = #pending_pat_bbs r,
    copyable_bbs    = #copyable_bbs r,
    expandable_bbs  = #expandable_bbs r,
    pat_fix_tags    = #pat_fix_tags r,
    tgt_fix_tags    = #tgt_fix_tags r,
    t_saved         = #t_saved r,
    tgt_pool        = #tgt_pool r,
    inner_state     = #inner_state r
  }

  fun update_pat_fix_tags f (MatchState r) = MatchState {
    init_pat        = #init_pat r,
    bbox_ops        = #bbox_ops r,
    wild_bboxes     = #wild_bboxes r,
    bmap            = #bmap r,
    pending_pat_bbs = #pending_pat_bbs r,
    copyable_bbs    = #copyable_bbs r,
    expandable_bbs  = #expandable_bbs r,
    pat_fix_tags    = f(#pat_fix_tags r),
    tgt_fix_tags    = #tgt_fix_tags r,
    t_saved         = #t_saved r,
    tgt_pool        = #tgt_pool r,
    inner_state     = #inner_state r
  }

  fun update_tgt_fix_tags f (MatchState r) = MatchState {
    init_pat        = #init_pat r,
    bbox_ops        = #bbox_ops r,
    wild_bboxes     = #wild_bboxes r,
    bmap            = #bmap r,
    pending_pat_bbs = #pending_pat_bbs r,
    copyable_bbs    = #copyable_bbs r,
    expandable_bbs  = #expandable_bbs r,
    pat_fix_tags    = #pat_fix_tags r,
    tgt_fix_tags    = f(#tgt_fix_tags r),
    t_saved         = #t_saved r,
    tgt_pool        = #tgt_pool r,
    inner_state     = #inner_state r
  }

  fun update_t_saved f (MatchState r) = MatchState {
    init_pat        = #init_pat r,
    bbox_ops        = #bbox_ops r,
    wild_bboxes     = #wild_bboxes r,
    bmap            = #bmap r,
    pending_pat_bbs = #pending_pat_bbs r,
    copyable_bbs    = #copyable_bbs r,
    expandable_bbs  = #expandable_bbs r,
    pat_fix_tags    = #pat_fix_tags r,
    tgt_fix_tags    = #tgt_fix_tags r,
    t_saved         = f(#t_saved r),
    tgt_pool        = #tgt_pool r,
    inner_state     = #inner_state r
  }

  fun update_tgt_pool f (MatchState r) = MatchState {
    init_pat        = #init_pat r,
    bbox_ops        = #bbox_ops r,
    wild_bboxes     = #wild_bboxes r,
    bmap            = #bmap r,
    pending_pat_bbs = #pending_pat_bbs r,
    copyable_bbs    = #copyable_bbs r,
    expandable_bbs  = #expandable_bbs r,
    pat_fix_tags    = #pat_fix_tags r,
    tgt_fix_tags    = #tgt_fix_tags r,
    t_saved         = #t_saved r,
    tgt_pool        = f(#tgt_pool r),
    inner_state     = #inner_state r
  }

  fun update_inner_state f (MatchState r) = MatchState {
    init_pat        = #init_pat r,
    bbox_ops        = #bbox_ops r,
    wild_bboxes     = #wild_bboxes r,
    bmap            = #bmap r,
    pending_pat_bbs = #pending_pat_bbs r,
    copyable_bbs    = #copyable_bbs r,
    expandable_bbs  = #expandable_bbs r,
    pat_fix_tags    = #pat_fix_tags r,
    tgt_fix_tags    = #tgt_fix_tags r,
    t_saved         = #t_saved r,
    tgt_pool        = #tgt_pool r,
    inner_state     = f(#inner_state r)
  }

  fun get_init_pat        (MatchState r) = #init_pat r
  fun get_bbox_ops        (MatchState r) = #bbox_ops r
  fun get_wild_bboxes     (MatchState r) = #wild_bboxes r
  fun get_bmap            (MatchState r) = #bmap r
  fun get_pending_pat_bbs (MatchState r) = #pending_pat_bbs r
  fun get_copyable_bbs    (MatchState r) = #copyable_bbs r
  fun get_expandable_bbs  (MatchState r) = #expandable_bbs r
  fun get_pat_fix_tags    (MatchState r) = #pat_fix_tags r
  fun get_tgt_fix_tags    (MatchState r) = #tgt_fix_tags r
  fun get_t_saved         (MatchState r) = #t_saved r
  fun get_tgt_pool        (MatchState r) = #tgt_pool r
  fun get_inner_state     (MatchState r) = #inner_state r

  fun set_init_pat        x = update_init_pat        (fn _ => x)
  fun set_bbox_ops        x = update_bbox_ops        (fn _ => x)
  fun set_wild_bboxes     x = update_wild_bboxes     (fn _ => x)
  fun set_bmap            x = update_bmap            (fn _ => x)
  fun set_pending_pat_bbs x = update_pending_pat_bbs (fn _ => x)
  fun set_copyable_bbs    x = update_copyable_bbs    (fn _ => x)
  fun set_expandable_bbs  x = update_expandable_bbs  (fn _ => x)
  fun set_pat_fixed       x = update_pat_fixed       (fn _ => x)
  fun set_pat_fix_tags    x = update_pat_fix_tags    (fn _ => x)
  fun set_tgt_fix_tags    x = update_tgt_fix_tags    (fn _ => x)
  fun set_t_saved         x = update_t_saved         (fn _ => x)
  fun set_inner_state     x = update_inner_state     (fn _ => x)

  (* the inner match state actually holds the canonical reference to the expanded pattern graph *)
  val get_pat = InnerMatchState.get_pat o get_inner_state
  val update_pat = update_inner_state o InnerMatchState.update_pat
  fun set_pat x = update_pat (fn _ => x)

  val get_tgt = InnerMatchState.get_tgt o get_inner_state

  fun add_wild_bbox bb = update_wild_bboxes (B.NSet.add bb)

  fun log_p pretty_f lvl name =
    Log.logf lvl (fn g => Pretty.string_of
        (Pretty.chunks [Pretty.str (name^":"),(pretty_f g)]))
  val log_graph = log_p Graph.pretty
  val log_v_nset = log_p V.NSet.pretty
  val log_e_nset = log_p E.NSet.pretty
  val log_b_nset = log_p B.NSet.pretty

  fun init_with_fixed pat tgt tgt_pool pat_fix_tags tgt_fix_tags = let
    val toplevel_bbs = B.NSet.filter (not o (Graph.bbox_has_parents pat))
                                     (Graph.get_bboxes pat)
    val all_tags = F.NSet.union_merge (BFFn.get_cod_set pat_fix_tags)
                                      (BFFn.get_cod_set tgt_fix_tags)
    val pat_fixed = BFFn.get_dom_set pat_fix_tags
  in
    MatchState {
      init_pat        = pat,
      bbox_ops        = [],
      wild_bboxes     = B.NSet.empty,
      bmap            = BBInj.empty,
      pending_pat_bbs = pat_fixed,
      copyable_bbs    = B.NSet.subtract toplevel_bbs pat_fixed,
      expandable_bbs  = B.NSet.empty,
      pat_fix_tags    = pat_fix_tags,
      tgt_fix_tags    = tgt_fix_tags,
      t_saved         = V.NSet.subtract tgt_pool (Graph.get_bboxed tgt),
      tgt_pool        = tgt_pool,
      inner_state     = InnerMatchState.init pat tgt
    }
  end

  fun init pat tgt tgt_pool = init_with_fixed pat tgt tgt_pool BFFn.empty BFFn.empty

  fun schedule_new_concrete bms = let
    val ms = get_inner_state bms
    val pat = InnerMatchState.get_pat ms
    val new_vs = V.NSet.subtract (Graph.get_vertices pat)
                                 (VVInj.get_dom_set (InnerMatchState.get_vmap ms))
    val unbboxed = V.NSet.filter (not o (Graph.is_bboxed pat)) new_vs
    val touched = Graph.get_adj_vertices_to_set pat unbboxed
    (* only need to schedule p verts adjacent to new unbboxed verts;
     * p verts adjacent to killed verts are scheduled in kill_pat_bbox *)
    fun schedule_p_nvs ims =
        InnerMatchState.extend_ps_nodeverts
          (V.NSet.intersect (InnerMatchState.get_p_nodeverts ims) touched)
          ims
  in
    update_inner_state
      (InnerMatchState.extend_u unbboxed
       o (InnerMatchState.set_tgt_verts (get_t_saved bms))
       o schedule_p_nvs)
      bms
  end

  fun schedule_all_concrete bms = let
    val pat = InnerMatchState.get_pat (get_inner_state bms)
    val vs = Graph.get_vertices pat
    val unbboxed = V.NSet.filter (not o (Graph.is_bboxed pat)) vs
  in
    update_inner_state
      (InnerMatchState.extend_u unbboxed
       o (InnerMatchState.set_tgt_verts (get_t_saved bms)))
      bms
  end

  (* TODO: it is probably worth optimising this some more *)
  (* TODO: account for fixed/matched !-boxes? *)
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

  fun is_total bms =
    if (InnerMatchState.is_total (get_inner_state bms))
    then (Log.log 2 "==== total match found ===="; true)
    else (Log.log 2 "==== match failed to be total ===="; false)

  fun match_bare_wires ms = let
    val bmap = get_bmap ms
    fun bare_wire_prevent_match ims (p_s,_,p_t) (t_s,_,t_t) = let
      val pat = InnerMatchState.get_pat ims
      val tgt = InnerMatchState.get_tgt ims
      val p_wv = if Graph.is_wire_vertex pat p_s then p_s else p_t
      val t_wv = if Graph.is_wire_vertex tgt t_s then t_s else t_t
      val p_bs = Graph.get_bboxes_containing_vertex pat p_wv
      val t_bs = Graph.get_bboxes_containing_vertex tgt t_wv
      val p_bs' = BBInj.img_of_set bmap p_bs
    in
      not (B.NSet.eq t_bs p_bs')
    end
    val ims = get_inner_state ms
  in
    Seq.map (fn ims => set_inner_state ims ms)
            (InnerMatchSearch.match_bare_wires bare_wire_prevent_match ims)
  end
  fun match_pending ms = let
    fun update_ms ims =
          ms |> set_inner_state ims
             |> set_t_saved (InnerMatchState.get_tgt_verts ims)
    val ims = get_inner_state ms
  in
    Seq.map update_ms (InnerMatchSearch.match_pending doomedf ims)
  end

  fun finish_match ms =
      Seq.filter is_total
        (match_bare_wires ms)

  fun expand_pat_bbox b ms = let
    val _ = Log.logf 2 (fn () => "Expanding !-box "^(B.string_of_name b)) ()
    val pat = get_pat ms
    val (bb_op, pat') = Graph.expand_bbox_op b pat
    val new_toplevel = B.NSet.filter (not o (Graph.bbox_has_parents pat'))
                                     (bbox_op_added_bboxes bb_op)
    val _ = log_b_nset 4 "  new top-level !-boxes" new_toplevel
  in
    ms |> set_pat pat'
       |> update_copyable_bbs (B.NSet.union_merge new_toplevel)
       |> update_bbox_ops (cons bb_op)
  end

  fun kill_pat_bbox b ms = let
    val _ = Log.logf 2 (fn () => "Killing !-box "^(B.string_of_name b)) ()
    val pat = get_pat ms
    val bb_vs = Graph.get_vertices_in_bbox pat b
    val adj_vs = Graph.get_adj_vertices_to_set pat bb_vs
    val (bb_op, pat') = Graph.kill_bbox_op b pat
    fun sched_adj ims =
        InnerMatchState.extend_ps_nodeverts
          (V.NSet.intersect (InnerMatchState.get_p_nodeverts ims) adj_vs)
          ims
  in
    ms |> set_pat pat'
       |> update_copyable_bbs (B.NSet.delete b)
       |> update_expandable_bbs (B.NSet.delete b)
       |> update_bbox_ops (cons bb_op)
       |> update_inner_state sched_adj
  end

  fun kill_bboxes_adj_to_complete_nvs bms = let
    val ms = get_inner_state bms
    val pat = InnerMatchState.get_pat ms
    val complete = InnerMatchState.get_complete_nodeverts ms
    val touched = Graph.get_adj_vertices_to_set pat complete
    fun kill_all_bboxes v bms = let
      val pat = InnerMatchState.get_pat (get_inner_state bms)
      val bbs = Graph.get_bboxes_containing_vertex pat v
      val operable_bbs = B.NSet.union_merge (get_copyable_bbs bms)
                                            (get_expandable_bbs bms)
      val adj_operable_bbs = B.NSet.intersect bbs operable_bbs
    in
      B.NSet.fold kill_pat_bbox adj_operable_bbs bms
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

  fun cfix_pat_bbox b ms = let
    val _ = Log.logf 2 (fn () => "Copy-fixing !-box "^(B.string_of_name b)) ()
    val pat = get_pat ms
    val (bb_op, pat') = Graph.copy_bbox_op b pat
    val b' = bbox_op_get_bbox_copy bb_op b
    val b'_singleton = B.NSet.single b'
    val new_toplevel = B.NSet.filter
          (fn c => B.NSet.eq (Graph.get_bbox_parents pat' c) b'_singleton)
          (bbox_op_added_bboxes bb_op)
    val _ = log_b_nset 4 "  new top-level !-boxes" new_toplevel
  in
    ms |> set_pat pat'
       |> update_copyable_bbs (B.NSet.union_merge new_toplevel)
       |> update_pending_pat_bbs (B.NSet.ins_fresh b')
       |> update_bbox_ops (cons bb_op)
  end

  fun match_pending_bbs bms =
    case B.NSet.pull_local_bot (get_pending_pat_bbs bms)
      of NONE => Seq.single bms
       | SOME (b,bbs') =>
           let
             val pat = get_pat bms
             val tgt = get_tgt bms
             val bmap = get_bmap bms
             val pat_parents = Graph.get_bbox_parents pat b
             val tgt_parents = BBInj.img_of_set bmap pat_parents
             val b_fix_tag = BFFn.get_opt (get_pat_fix_tags bms) b
             fun is_tgt_cand b' =
                 (not (BBInj.cod_contains bmap b'))
               andalso
                 (case (b_fix_tag,BFFn.get_opt (get_tgt_fix_tags bms) b')
                    of (NONE,_) => true
                     | (SOME x,SOME y) => F.name_eq (x,y)
                     | (SOME _,_) => false)
               andalso
                 B.NSet.eq (Graph.get_bbox_parents tgt b') tgt_parents
             val candidates = B.NSet.filter is_tgt_cand (Graph.get_bboxes tgt)
             val bms' = set_pending_pat_bbs bbs' bms
             (* schedule matchable parts of B(b) *)
             (* set inner.tgt_verts to matchable part of B(b') *)
             fun do_bb_match b' = let
               val bmap' = BBInj.add (b,b') bmap
               fun pat_is_matchable v = let
                 val bbs = Graph.get_bboxes_containing_vertex pat v
               in
                   B.NSet.sub_set bbs (BBInj.get_dom_set bmap')
               end
               val bbox_vs = Graph.get_vertices_in_bbox pat b
               val pat_matchable = V.NSet.filter pat_is_matchable bbox_vs
               fun tgt_is_matchable v = let
                 val bbs = Graph.get_bboxes_containing_vertex tgt v
               in
                   B.NSet.contains bbs b'
                 andalso
                   B.NSet.sub_set bbs (BBInj.get_cod_set bmap')
               end
               val tgt_matchable = V.NSet.filter
                    tgt_is_matchable
                    (get_tgt_pool bms)
               val touched = Graph.get_adj_vertices_to_set pat bbox_vs
               fun schedule_p_nvs ims =
                 InnerMatchState.extend_ps_nodeverts
                   (V.NSet.intersect (InnerMatchState.get_p_nodeverts ims) touched)
                   ims
               val ims = get_inner_state bms'
                       |> InnerMatchState.set_tgt_verts tgt_matchable
                       |> InnerMatchState.extend_u pat_matchable
                       |> schedule_p_nvs
               (* TODO: doomed *)
               fun doomedf _ _ = false
               val bms' =
                 bms' |> set_bmap bmap'
             in
               Seq.map (fn ims => set_inner_state ims bms')
                       (InnerMatchSearch.match_pending doomedf ims)
             end
           in
             Seq.maps do_bb_match (Seq.of_list (B.NSet.list_of candidates))
           end

  fun bbox_expand_branch b ms =
    let
      fun do_kill ms =
        ms |> kill_pat_bbox b
           |> choose_next_bbox
      fun do_expand ms =
        ms |> expand_pat_bbox b
           |> schedule_new_concrete
           |> match_loop
    in
      Seq.append (Seq.maps do_kill (Seq.single ms))
                 (Seq.maps do_expand (Seq.single ms))
    end
  and bbox_copy_branch b ms =
    let
      fun start_expanding ms = 
        (Log.logf 3 (fn () => "!-box "^(B.string_of_name b)^" -> expandable") ();
         ms |> update_copyable_bbs (B.NSet.delete b)
            |> update_expandable_bbs (B.NSet.ins_fresh b)
            |> bbox_expand_branch b)
      fun do_cfix ms =
        ms |> cfix_pat_bbox b
           |> match_pending_bbs
           |> Seq.maps (choose_next_bbox o kill_impossible_bboxes)
    in
      Seq.append (Seq.maps start_expanding (Seq.single ms))
                 (Seq.maps do_cfix (Seq.single ms))
    end
  and choose_next_bbox ms =
    case B.NSet.get_local_bot (get_expandable_bbs ms)
      of SOME b => bbox_expand_branch b ms
       | NONE => (case B.NSet.get_local_bot (get_copyable_bbs ms)
                    of SOME b => bbox_copy_branch b ms
                     | NONE => finish_match ms)
  and match_loop ms' =
    Seq.maps (choose_next_bbox o kill_impossible_bboxes)
      (Seq.maps match_pending_bbs (match_pending ms'))

  fun matches_from_ms ms = let
    val ims = get_inner_state ms
    val (init_pat,pat,bbox_ops,tgt,vmap,emap,bmap,wild) = (
      get_init_pat ms,
      get_pat ms,
      get_bbox_ops ms,
      InnerMatchState.get_tgt ims,
      InnerMatchState.get_vmap ims,
      InnerMatchState.get_emap ims,
      get_bmap ms,
      get_wild_bboxes ms
    )
  in
    Seq.map (Match.mk init_pat pat bbox_ops tgt vmap emap bmap wild)
            (Graph.solve_psubst (InnerMatchState.get_match_psubst ims))
  end

  fun find_matches ms = Seq.maps matches_from_ms (match_loop ms)

  fun match_normalised pat tgt = let
    val _ = log_graph 3 "GreedyMatchSearch: normalised pattern" pat
    val _ = log_graph 3 "GreedyMatchSearch: normalised target" tgt
    val ms = init pat tgt (Graph.get_vertices tgt)
  in
    find_matches (schedule_all_concrete ms)
  end

  fun match pat tgt =
    match_normalised (Graph.normalise pat) (Graph.normalise tgt)

  fun match_subgraph_normalised pat tgt tgt_verts = let
    val _ = log_graph 3 "GreedyMatchSearch: normalised pattern" pat
    val _ = log_graph 3 "GreedyMatchSearch: normalised target" tgt
    val _ = log_v_nset 4 "GreedyMatchSearch: target vertices" tgt_verts
    val tgt_subgraph = Graph.get_open_subgraph tgt tgt_verts
    val ms = init pat tgt (Graph.get_vertices tgt_subgraph)
  in
    find_matches (schedule_all_concrete ms)
  end
  fun match_subgraph pat tgt =
    match_subgraph_normalised (Graph.normalise pat) (Graph.normalise tgt)

  structure Sharing =
  struct
    structure Graph = Graph.Sharing
    structure Match = Match.Sharing
  end
end
