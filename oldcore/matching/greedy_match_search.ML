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
  fun get_inner_state     (MatchState r) = #inner_state r

  fun set_init_pat        x = update_init_pat        (fn _ => x)
  fun set_bbox_ops        x = update_bbox_ops        (fn _ => x)
  fun set_wild_bboxes     x = update_wild_bboxes     (fn _ => x)
  fun set_bmap            x = update_bmap            (fn _ => x)
  fun set_pending_pat_bbs x = update_pending_pat_bbs (fn _ => x)
  fun set_copyable_bbs    x = update_copyable_bbs    (fn _ => x)
  fun set_expandable_bbs  x = update_expandable_bbs  (fn _ => x)
  fun set_pat_fix_tags    x = update_pat_fix_tags    (fn _ => x)
  fun set_tgt_fix_tags    x = update_tgt_fix_tags    (fn _ => x)
  fun set_inner_state     x = update_inner_state     (fn _ => x)

  structure IMSHooks : PROGRESSIVE_MATCH_SEARCH_HOOKS =
  struct
    type Context = State
    structure MatchState = InnerMatchState

    (* TODO: it is probably worth optimising this some more *)
    (* TODO: account for fixed/matched !-boxes? *)
    fun p_vertex_may_be_completed _ ms pv =
      let
        val pat = InnerMatchState.get_pat ms
        val tgt = InnerMatchState.get_tgt ms
        val tv = VVInj.get (InnerMatchState.get_vmap ms) pv
        val p_arity = Graph.get_arity pat pv
        val t_arity = Graph.get_arity tgt tv
      in
        (
          not (Arity.get_in p_arity < Arity.get_in t_arity) orelse
          (V.NSet.exists (Graph.is_bboxed pat) (Graph.get_predecessor_vertices pat pv))
        )
        andalso
        (
          not (Arity.get_out p_arity < Arity.get_out t_arity) orelse
          (V.NSet.exists (Graph.is_bboxed pat) (Graph.get_successor_vertices pat pv))
        )
        andalso
        (
          not (Arity.get_undir p_arity < Arity.get_undir t_arity) orelse
          (V.NSet.exists (Graph.is_bboxed pat) (Graph.get_sibling_vertices pat pv))
        )
      end

    fun vertex_match_is_allowed ms ims pv tv =
      let
        val pat = InnerMatchState.get_pat ims
        val tgt = InnerMatchState.get_tgt ims
        val p_bs = Graph.get_bboxes_containing_vertex pat pv
        val t_bs = Graph.get_bboxes_containing_vertex tgt tv
        val p_bs' = BBInj.img_of_set (get_bmap ms) p_bs
      in
        B.NSet.eq t_bs p_bs'
      end

    fun bare_wire_match_is_allowed ms ims (p_s,_,p_t) (t_s,_,t_t) =
      let
        val pat = InnerMatchState.get_pat ims
        val tgt = InnerMatchState.get_tgt ims
        val p_wv = if Graph.is_wire_vertex pat p_s then p_s else p_t
        val t_wv = if Graph.is_wire_vertex tgt t_s then t_s else t_t
        val p_bs = Graph.get_bboxes_containing_vertex pat p_wv
        val t_bs = Graph.get_bboxes_containing_vertex tgt t_wv
        val p_bs' = BBInj.img_of_set (get_bmap ms) p_bs
      in
        B.NSet.eq t_bs p_bs'
      end
  end
  structure InnerMatchSearch = ProgressiveMatchSearch(IMSHooks)
  structure InnerLog = InnerMatchSearch.Log

  (* the inner match state actually holds the canonical reference to the expanded pattern graph *)
  val get_pat = InnerMatchState.get_pat o get_inner_state
  val update_pat = update_inner_state o InnerMatchState.update_pat
  fun set_pat x = update_pat (fn _ => x)

  val get_tgt = InnerMatchState.get_tgt o get_inner_state

  fun add_wild_bbox bb = update_wild_bboxes (B.NSet.add bb)

  fun i_cap n = if n > 15 then 15 else n
  fun indent 0 s = s
    | indent n s = "  "^(indent ((i_cap n)-1) s)
  fun log_p pretty_f lvl ind name =
    Log.logf lvl (fn g => Pretty.string_of (Pretty.indent (2*(i_cap ind))
        (Pretty.chunks [Pretty.str (name^":"),(pretty_f g)])))
  val log_graph = log_p Graph.pretty
  val log_v_nset = log_p V.NSet.pretty
  val log_e_nset = log_p E.NSet.pretty
  val log_b_nset = log_p B.NSet.pretty

  fun init_with_fixed pat tgt pat_fix_tags tgt_fix_tags = let
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
      inner_state     = InnerMatchState.init pat tgt
    }
  end

  fun init pat tgt = init_with_fixed pat tgt BFFn.empty BFFn.empty

  fun schedule_new lvl bms = let
    val ms = get_inner_state bms
    val pat = InnerMatchState.get_pat ms
    val new_vs = V.NSet.subtract (Graph.get_vertices pat)
                                 (VVInj.get_dom_set (InnerMatchState.get_vmap ms))
    val matched_bbs = BBInj.get_dom_set (get_bmap bms)
    val _ = log_b_nset 4 lvl "Matched !-boxes in pattern so far" matched_bbs
    val get_bboxes_for_v = Graph.get_bboxes_containing_vertex pat
    fun in_match_surface v = B.NSet.sub_set (get_bboxes_for_v v) matched_bbs
    val matchable = V.NSet.filter in_match_surface new_vs
    val touched = Graph.get_adj_vertices_to_set pat matchable
    (* only need to schedule p verts adjacent to new unbboxed verts;
     * p verts adjacent to killed verts are scheduled in kill_pat_bbox *)
    fun schedule_p_nvs ims =
        InnerMatchState.extend_ps_nodeverts
          (V.NSet.intersect (InnerMatchState.get_p_nodeverts ims) touched)
          ims
    val _ = log_v_nset 3 lvl "Scheduling new U" matchable
  in
    update_inner_state
      (InnerMatchState.extend_u matchable
       o schedule_p_nvs)
      bms
  end

  fun schedule_all_concrete tgt_pool bms = let
    val pat = get_pat bms
    val tgt = get_tgt bms
    val pat_unbboxed = V.NSet.filter (not o (Graph.is_bboxed pat))
                                     (Graph.get_vertices pat)
    val tgt_unbboxed = V.NSet.filter (not o (Graph.is_bboxed tgt))
                                     tgt_pool
    fun schedule ims =
      ims |> InnerMatchState.extend_u pat_unbboxed
          |> InnerMatchState.set_tgt_verts tgt_unbboxed
  in
    update_inner_state schedule bms
  end

  (* TODO: it is probably worth optimising this some more *)
  fun is_total (lvl,bms) =
    if (InnerMatchState.is_total (get_inner_state bms))
    then (Log.logf 2 (fn() => indent lvl "==== total match found ====") (); true)
    else (Log.logf 2 (fn() => indent lvl "==== match failed to be total ====") (); false)

  fun match_bare_wires (lvl,ms) = let
    val ims = get_inner_state ms
  in
    Seq.map (fn ims => (lvl+1,set_inner_state ims ms))
            (InnerMatchSearch.match_bare_wires ms ims)
  end

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

  fun match_pending (lvl,ms) = let
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
        then SOME (lvl+1, ms |> set_inner_state ims)
        else NONE
      end) (InnerMatchSearch.match_pending ms ims)
  end

  (*fun match_pending (lvl,ms) = let
    fun update_ms ims =
          (lvl+1,set_inner_state ims ms)
    val ims = get_inner_state ms
  in
    Seq.map update_ms (InnerMatchSearch.match_pending ms ims)
  end*)


  fun finish_match ms =
      Seq.filter is_total
        (match_bare_wires ms)

  fun expand_pat_bbox lvl b ms = let
    val _ = Log.logf 2 (fn () => indent lvl ("Expanding !-box "^(B.string_of_name b))) ()
    val pat = get_pat ms
    val (bb_op, pat') = Graph.expand_bbox_op b pat
    val new_toplevel = B.NSet.filter (not o (Graph.bbox_has_parents pat'))
                                     (bbox_op_added_bboxes bb_op)
    val _ = log_b_nset 4 lvl ">> new top-level !-boxes" new_toplevel
  in
    ms |> set_pat pat'
       |> update_copyable_bbs (B.NSet.union_merge new_toplevel)
       |> update_bbox_ops (cons bb_op)
  end

  fun kill_pat_bbox lvl b ms = let
    val _ = Log.logf 2 (fn () => indent lvl ("Killing !-box "^(B.string_of_name b))) ()
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

  fun kill_bboxes_adj_to_complete_nvs (lvl,bms) = let
    val ms = get_inner_state bms
    val pat = InnerMatchState.get_pat ms
    val complete = InnerMatchState.get_complete_nodeverts ms
    val touched = Graph.get_adj_vertices_to_set pat complete
    fun kill_all_bboxes v bms = let
      val pat = InnerMatchState.get_pat (get_inner_state bms)
      (* if two vertices border the same bbox, this vertex might have
       * already been removed *)
      val bbs = if Graph.has_vertex pat v
                then Graph.get_bboxes_containing_vertex pat v
                else B.NSet.empty
      val operable_bbs = B.NSet.union_merge (get_copyable_bbs bms)
                                            (get_expandable_bbs bms)
      val adj_operable_bbs = B.NSet.intersect bbs operable_bbs
    in
      B.NSet.fold (kill_pat_bbox lvl) adj_operable_bbs bms
      handle Graph.no_such_vertex_exp _ => bms
    end
  in
    (lvl,V.NSet.fold kill_all_bboxes touched bms)
  end

  fun kill_wild_bboxes (lvl,ms) = let
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
      ms |> kill_pat_bbox lvl bbox
         |> add_wild_bbox bbox
    fun kill_if_wild bbox ms =
      (if is_wild (InnerMatchState.get_pat (get_inner_state ms)) bbox
       then kill_pat_bbox' bbox ms
       else ms)
      handle Graph.no_such_bbox_exp _ => ms
    val pat = InnerMatchState.get_pat (get_inner_state ms)
  in
    (lvl,B.NSet.fold kill_if_wild (Graph.get_bboxes pat) ms)
  end

  val kill_impossible_bboxes = kill_wild_bboxes
                             o kill_bboxes_adj_to_complete_nvs

  fun cfix_pat_bbox lvl b ms = let
    val _ = Log.logf 2 (fn () => indent lvl ("Copy-fixing !-box "^(B.string_of_name b))) ()
    val pat = get_pat ms
    val (bb_op, pat') = Graph.copy_bbox_op b pat
    val b' = bbox_op_get_bbox_copy bb_op b
    val b'_singleton = B.NSet.single b'
    val new_toplevel = B.NSet.filter
          (fn c => B.NSet.eq (Graph.get_bbox_parents pat' c) b'_singleton)
          (bbox_op_added_bboxes bb_op)
    val _ = log_b_nset 4 lvl ">> new top-level !-boxes" new_toplevel
  in
    ms |> set_pat pat'
       |> update_copyable_bbs (B.NSet.union_merge new_toplevel)
       |> update_pending_pat_bbs (B.NSet.ins_fresh b')
       |> update_bbox_ops (cons bb_op)
  end

  fun match_pending_bbs (lvl,bms) =
    case B.NSet.pull_local_bot (get_pending_pat_bbs bms)
      of NONE => Seq.single (lvl,bms)
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
               fun extend_tgt_verts ims =
                 let
                   val vs = Graph.get_vertices_in_bbox tgt b'
                   val matched_bbs = BBInj.get_cod_set bmap'
                   fun add_if_in_match_surface v set =
                     let
                       val bbs = Graph.get_bboxes_containing_vertex tgt v
                     in
                       if B.NSet.sub_set bbs matched_bbs
                       then V.NSet.add v set
                       else set
                     end
                   val tgt_verts = InnerMatchState.get_tgt_verts ims
                 in
                   InnerMatchState.set_tgt_verts
                     (V.NSet.fold add_if_in_match_surface vs tgt_verts)
                     ims
                 end
               val ims = extend_tgt_verts (get_inner_state bms')
               val bms' = set_bmap bmap' bms'
             in
               Seq.map (fn ims => (lvl+1,set_inner_state ims bms'))
                       (InnerMatchSearch.match_pending bms' ims)
             end
           in
             Seq.maps do_bb_match (Seq.of_list (B.NSet.list_of candidates))
           end

  fun bbox_expand_branch b (lvl,ms) =
    let
      fun do_kill ms =
        choose_next_bbox (lvl+1,kill_pat_bbox lvl b ms)
      fun do_expand ms =
        (* need to match non-concrete verts as well *)
        match_loop' (lvl+1,ms |> expand_pat_bbox lvl b
                              |> schedule_new lvl)
    in
      Seq.append (Seq.maps do_kill (Seq.single ms))
                 (Seq.maps do_expand (Seq.single ms))
    end
  and bbox_copy_branch b (lvl,ms) =
    let
      fun start_expanding ms = 
        (Log.logf 3 (fn () => indent lvl ("!-box "^(B.string_of_name b)^" -> expandable")) ();
         bbox_expand_branch b (lvl+1,
            ms |> update_copyable_bbs (B.NSet.delete b)
               |> update_expandable_bbs (B.NSet.ins_fresh b)))
      fun do_cfix ms =
        Seq.maps (match_loop' o (fn (l,s) => (l,schedule_new l s)))
                 (match_pending_bbs (lvl+1,cfix_pat_bbox lvl b ms))
    in
      Seq.append (Seq.maps start_expanding (Seq.single ms))
                 (Seq.maps do_cfix (Seq.single ms))
    end
  and choose_next_bbox (lvl,ms) =
    case B.NSet.get_local_bot (get_expandable_bbs ms)
      of SOME b => bbox_expand_branch b (lvl,ms)
       | NONE => (case B.NSet.get_local_bot (get_copyable_bbs ms)
                    of SOME b => bbox_copy_branch b (lvl,ms)
                     | NONE => finish_match (lvl,ms))
  and match_loop' x =
    Seq.maps (choose_next_bbox o kill_impossible_bboxes)
      (Seq.maps match_pending_bbs (match_pending x))

  fun match_loop ms = match_loop' (0,ms)


  fun matches_from_ms (_,ms) = let
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
    val _ = log_graph 3 0 "GreedyMatchSearch: normalised pattern" pat
    val _ = log_graph 3 0 "GreedyMatchSearch: normalised target" tgt
    val ms = init pat tgt
  in
    find_matches (schedule_all_concrete (Graph.get_vertices tgt) ms)
  end

  fun match pat tgt =
    match_normalised (Graph.normalise pat) (Graph.normalise tgt)

  fun match_with_initial_nodes pat tgt nmap = match pat tgt

  fun match_subgraph_normalised pat tgt tgt_verts = let
    val _ = log_graph 3 0 "GreedyMatchSearch: normalised pattern" pat
    val _ = log_graph 3 0 "GreedyMatchSearch: normalised target" tgt
    val _ = log_v_nset 4 0 "GreedyMatchSearch: target vertices" tgt_verts
    val tgt_subgraph = Graph.get_open_subgraph tgt tgt_verts
    val ms = init pat tgt
  in
    find_matches (schedule_all_concrete (Graph.get_vertices tgt_subgraph) ms)
  end
  fun match_subgraph pat tgt =
    match_subgraph_normalised (Graph.normalise pat) (Graph.normalise tgt)

  structure Sharing =
  struct
    structure Graph = Graph.Sharing
    structure Match = Match.Sharing
  end
end
