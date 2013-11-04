(* A piece-by-piece matcher for use by other matching code *)
(* see docs/matching_algo for details *)
signature PROGRESSIVE_MATCH_SEARCH_SHARING =
sig
  structure MatchState : OGRAPH_MATCH_STATE_SHARING
end

signature PROGRESSIVE_MATCH_SEARCH_HOOKS =
sig
  type Context
  structure MatchState : OGRAPH_MATCH_STATE

  (* Returns false if the given partially-matched vertex (in the
   * pattern graph) is guaranteed to never be completed; and true
   * otherwise.
   *
   * If in doubt, return true.
   *)
  val p_vertex_may_be_completed : Context -> MatchState.T -> V.name -> bool

  (* Allows additional constraints to be placed on the match.  A potential
   * match will be tested using this function, and discarded if it returns
   * false.
   *
   * This is *not* used by the bare-wire matching (see
   * bare_wire_match_is_allowed).
   *)
  val vertex_match_is_allowed : Context -> MatchState.T -> V.name -> V.name -> bool

  (* Allows additional constraints to be placed on bare-wire matching.  A
   * potential match will be tested using this function, and discarded if it
   * returns false.
   *
   * The wire segment arguments are (s,e,t) and (s',e',t'), where e is the edge
   * in a bare wire of the pattern graph, e' is an edge in the target graph and
   * s, t, s' and t' are the corresponding sources and targets.
   *
   * s, e and t are all unmatched, and s and t are wire-vertices (specifically,
   * an input and an output of the pattern).
   *
   * e' is unmatched, but one or both of s' and t' may be matched.  Further,
   * while at least one of s' and t' is a wire-vertex, the other may be a
   * node-vertex.  The wire segment will be expanded as necessary if the match
   * proceeds.
   *)
  val bare_wire_match_is_allowed : Context
        -> MatchState.T
        -> (V.name*E.name*V.name) (* bare wire in the pattern *)
        -> (V.name*E.name*V.name) (* wire segment in the target graph *)
        -> bool
end

functor PermissiveHooks(MatchState : OGRAPH_MATCH_STATE)
  : PROGRESSIVE_MATCH_SEARCH_HOOKS =
struct
  type Context = unit
  structure MatchState = MatchState
  fun p_vertex_may_be_completed _ _ _ = true
  fun vertex_match_is_allowed _ _ _ _ = true
  fun bare_wire_match_is_allowed _ _ _ _ = true
end

signature PROGRESSIVE_MATCH_SEARCH =
sig
  structure Log : LOG

  structure MatchState : OGRAPH_MATCH_STATE
  structure Hooks : PROGRESSIVE_MATCH_SEARCH_HOOKS

  (* Match all queued unmatched circles and node vertices, and extend the
   * matchings of any scheduled partially-matched node vertices as much as
   * possible using queued unmatched wire vertices.
   *
   * Returns a lazy list of updated match states.
   *
   * Note that this will _not_ match bare wires.
   *)
  val match_pending : Hooks.Context
                      -> MatchState.T
                      -> MatchState.T Seq.seq

  (* Match all the bare wires in u_bare_wires, expanding edge points where
   * necessary.
   *)
  val match_bare_wires : Hooks.Context
                         -> MatchState.T
                         -> MatchState.T Seq.seq

  structure Sharing : PROGRESSIVE_MATCH_SEARCH_SHARING
    sharing Sharing.MatchState = MatchState.Sharing
    sharing Hooks.MatchState.Sharing = MatchState.Sharing
end

functor ProgressiveMatchSearch(
    Hooks : PROGRESSIVE_MATCH_SEARCH_HOOKS
) : PROGRESSIVE_MATCH_SEARCH
= struct
  structure Log : LOG = Log(val init_level = 0)

  structure Hooks = Hooks
  structure MatchState = Hooks.MatchState
  structure Graph = MatchState.Graph

  fun log_p pretty_f lvl name =
    Log.logf lvl (fn g => Pretty.string_of
        (Pretty.chunks [Pretty.str (name^":"),(pretty_f g)]))
  val log_graph = log_p Graph.pretty
  val log_vset = log_p V.NSet.pretty
  val log_vmap = log_p VVInj.pretty
  val log_emap = log_p EEInj.pretty

  fun ind 0 str = str
    | ind n str = ind (n-1) (" "^str)
  
  fun add_edge_to_match lvl (ep,vp) (et,vt) ms = let
    val _ = Log.logf 2 (fn () => ind lvl
      ">> Edge "^(E.string_of_name ep)^" -> "^(E.string_of_name et)) ()
    val _ = Log.logf 2 (fn () => ind lvl
      ">> Node "^(V.string_of_name vp)^" -> "^(V.string_of_name vt)) ()
  in
    MatchState.add_edge_to_match (ep,vp) (et,vt) ms
  end
  fun add_circle_to_match lvl (cp,ct) ms = let
    val _ = Log.logf 2 (fn () => ind lvl
      ">> Circle "^(V.string_of_name cp)^" -> "^(V.string_of_name ct)) ()
  in
    MatchState.add_circle_to_match (cp,ct) ms
  end
  fun add_vertex_to_match lvl (vp,vt) ms = let
    val _ = Log.logf 2 (fn () => ind lvl
      ">> Node "^(V.string_of_name vp)^" -> "^(V.string_of_name vt)) ()
  in
    MatchState.add_vertex_to_match (vp,vt) ms
  end
  fun log_match_fail_v1 lvl msg v =
    Log.log 2 (ind lvl "!! "^msg^" ("^(V.string_of_name v)^")")
  fun log_match_fail_v2 lvl msg v1 v2 =
    Log.log 2 (ind lvl "!! "^msg^" ("^(V.string_of_name v1)^" -> "^(V.string_of_name v2)^")")
  fun log_match_fail_e2 lvl msg e1 e2 =
    Log.log 2 (ind lvl "!! "^msg^" ("^(E.string_of_name e1)^" -> "^(E.string_of_name e2)^")")
  (* this version is for things that failed straight away *)
  (* (ie: when branches aren't killed, but are never created) *)
  fun log_match_fail_e2' lvl msg e1 e2 =
    Log.log 4 (ind lvl "!! "^msg^" ("^(E.string_of_name e1)^" -> "^(E.string_of_name e2)^")")
  
  (*****************************************)
  (************ CIRCLE MATCHING ************)
  (*****************************************)
  

  fun u_circles_in_tgt ms = let
    val tgt = MatchState.get_tgt ms
    fun is_circle v =
      Graph.is_wire_vertex tgt v andalso
      (let val (ins,outs) = (Graph.get_in_edges tgt v, Graph.get_out_edges tgt v)
       in
        E.NSet.is_singleton ins andalso
        E.NSet.is_singleton outs andalso
        E.NSet.eq ins outs end);
    fun is_matched v = VVInj.cod_contains (MatchState.get_vmap ms) v;
    fun add_if_circle v =
      if is_circle v andalso not (is_matched v)
      then V.NSet.add v
      else I;
  in
    V.NSet.fold add_if_circle (MatchState.get_tgt_verts ms) V.NSet.empty
  end;

  fun match_circles' lvl ms = let
    val _ = Log.log 2 (ind lvl "== Matching next circle")
  in
    case V.NSet.get_min (MatchState.get_u_circles ms)
      of SOME cp => let
           val (pat,tgt) = (MatchState.get_pat ms, MatchState.get_tgt ms)
           val cpdata = Graph.get_vertex_data pat cp
           val ep = (the o E.NSet.tryget_singleton) (Graph.get_in_edges pat cp)
           val (epdir,epdata) = Graph.get_edge_dir_and_data pat ep
           val subst = MatchState.get_match_psubst ms
           fun m_circ ct = let
             val ctdata = Graph.get_vertex_data tgt ct
             val et = (the o E.NSet.tryget_singleton) (Graph.get_in_edges tgt ct)
             val _ = Log.logf 4 (fn () => ind lvl "Attempting to match circle ("^
                (V.string_of_name cp)^ ","^(E.string_of_name ep)^") against ("^
                (V.string_of_name ct)^","^(E.string_of_name et)^")") ()
              val (etdir,etdata) = Graph.get_edge_dir_and_data tgt et
           in case (epdir = etdir,
                    Option.composePartial
                      (Graph.match_vdata (cpdata, ctdata),
                       Graph.match_edata (epdata, etdata))
                      subst)
                of (true, SOME subst') =>
                      match_circles' (lvl+1)
                          (ms |> MatchState.update_u_circles (V.NSet.delete cp)
                              |> MatchState.update_tgt_verts (V.NSet.delete ct)
                              |> add_circle_to_match lvl (cp,ct)
                              |> MatchState.set_match_psubst subst')
                 | (false, _) =>
                     (log_match_fail_e2' lvl "Circle directedness mismatch" ep et;
                      Seq.empty)
                 | (_, NONE) =>
                     (log_match_fail_e2' lvl "Circle edge data mismatch" ep et;
                      Seq.empty)
           end
         in Seq.maps m_circ (V.NSet.fold Seq.cons (u_circles_in_tgt ms) Seq.empty)
         end
       | NONE => (* no circles left to match, return match state *)
                 (Log.log 2 "== All circles matched"; Seq.single ms)
  end
  val match_circles = match_circles' 0
  
  
  (*****************************************)
  (************* MAIN MATCHING *************)
  (*****************************************)
  
  
  (* Try to recursively add wire to matching, starting with the given head
   * vertex and edge. Return NONE on failure.
   *
   * vp: already-matched vertex
   * ep: unmatched edge incident to vp (other end must be in P, Uw or Un)
   * vt: target of vp
   * et: unmatched edge incident to vt
   *)
  fun tryadd_wire lvl ms (vp,ep) (vt,et) = let
    val _ = Log.logf 4 (fn () => ind lvl "Attempting to match ("^
                       (V.string_of_name vp)^ ","^(E.string_of_name ep)^
                       ") against ("^
                       (V.string_of_name vt)^","^(E.string_of_name et)^")") ()
    val (pat, tgt) = (MatchState.get_pat ms, MatchState.get_tgt ms)
    val e_subst_maybe =
      let (* check edge compatibilty and update subst *)
        val (dir_p,data_p) = Graph.get_edge_dir_and_data pat ep
        val (dir_t,data_t) = Graph.get_edge_dir_and_data tgt et
        val pat_inedge = V.name_eq (Graph.get_edge_target pat ep, vp)
        val tgt_inedge = V.name_eq (Graph.get_edge_target tgt et, vt)
      in
        if dir_p = dir_t andalso (            (* directedness must match and... *)
             dir_p = Undirected orelse      (* undirected or... *)
             (pat_inedge = tgt_inedge))       (* directions match *)
        then Graph.match_edata (data_p, data_t) (MatchState.get_match_psubst ms)
        else NONE
      end
  in
    case e_subst_maybe
      of NONE => (log_match_fail_e2 lvl "Edge data does not match" ep et; NONE)
       | SOME e_subst =>
          let
            val new_vp = Graph.edge_get_other_vertex pat ep vp
            val new_vt = Graph.edge_get_other_vertex tgt et vt
            val vmap = MatchState.get_vmap ms
          in
            if V.NSet.contains (MatchState.get_p_nodeverts ms) new_vp
            then
              if VVInj.is_mapped vmap (new_vp, new_vt)
              then
                SOME (ms |> add_edge_to_match lvl (ep,new_vp) (et,new_vt)
                         |> MatchState.update_ps_nodeverts (V.NSet.add new_vp)
                         |> MatchState.set_match_psubst e_subst)
              else
                (log_match_fail_v2 lvl "Match conflict" new_vp new_vt; NONE)
            else
              if not (V.NSet.contains (MatchState.get_tgt_verts ms) new_vt)
              then
                (log_match_fail_v2 lvl "Vertex not in T" new_vp new_vt; NONE)
              else
                case Graph.match_vdata (Graph.get_vertex_data pat new_vp,
                                        Graph.get_vertex_data tgt new_vt)
                                       e_subst
                  of NONE =>
                      (log_match_fail_v2 lvl "Vertex data does not match"
                                         new_vp new_vt;
                       NONE)
                   | SOME v_subst =>
                      (* since data matched, new_vp is a node- or wire-vertex iff new_vt is *)
                      if Graph.is_wire_vertex pat new_vp
                      then
                        case (Graph.wv_get_other_edge pat new_vp ep, Graph.wv_get_other_edge tgt new_vt et)
                          of (SOME new_ep, SOME new_et) =>
                              (* pat and tgt are internal. add edge to match and recurse *)
                              tryadd_wire lvl
                                (ms |> add_edge_to_match lvl (ep,new_vp) (et,new_vt)
                                    |> MatchState.update_tgt_verts (V.NSet.delete new_vt)
                                    |> MatchState.update_u_wireverts (V.NSet.delete new_vp)
                                    |> MatchState.set_match_psubst v_subst)
                                (new_vp,new_ep) (new_vt,new_et)
                           | (SOME _, NONE) =>
                              (* pat is internal, tgt is bdry. fail *)
                              (log_match_fail_v2 lvl "Tgt wire too short" new_vp new_vt; NONE)
                           | (NONE, _) =>
                              (* pat is boundary. add edge to match and finish *)
                              SOME (ms |> add_edge_to_match lvl (ep,new_vp) (et,new_vt)
                                       |> MatchState.update_tgt_verts (V.NSet.delete new_vt)
                                       |> MatchState.update_u_wireverts (V.NSet.delete new_vp)
                                       |> MatchState.set_match_psubst v_subst)
                      else
                        if V.NSet.contains (MatchState.get_u_nodeverts ms) new_vp
                        then
                          SOME (ms |> add_edge_to_match lvl (ep,new_vp) (et,new_vt)
                                   |> MatchState.update_tgt_verts (V.NSet.delete new_vt)
                                   |> MatchState.update_u_nodeverts (V.NSet.delete new_vp)
                                   |> MatchState.update_p_nodeverts (V.NSet.add new_vp)
                                   |> MatchState.update_ps_nodeverts (V.NSet.add new_vp)
                                   |> MatchState.set_match_psubst v_subst)
                        else
                          (* if this node-vert isn't scheduled for matching, leave its wire alone *)
                          (* NB: this shouldn't happen if Uw, Un set up correctly? *)
                          (log_match_fail_v1 lvl "Vertex not in Un" new_vp; NONE)
          end
  end
  
  
    
  fun match_pending context match_state = let
    val _ = Log.log 2 "== Starting inner match"
    val _ = log_graph 4 "Pattern" (MatchState.get_pat match_state)
    val _ = log_graph 4 "Target" (MatchState.get_tgt match_state)
    val p_vertex_may_be_completed = Hooks.p_vertex_may_be_completed context
  
    (* main matching loop *)
    fun match_main lvl ms = let
      val _ = Log.logf 3 (fn () => ind lvl "== Entering main loop") ()
      val _ = log_vset 4 "Un" (MatchState.get_u_nodeverts ms)
      val _ = log_vset 4 "Uw" (MatchState.get_u_wireverts ms)
      val _ = log_vset 4 "P" (MatchState.get_p_nodeverts ms)
      val _ = log_vset 4 "Ps" (MatchState.get_ps_nodeverts ms)
      val _ = log_vset 4 "T" (MatchState.get_tgt_verts ms)
      val _ = log_vmap 4 "Mv" (MatchState.get_vmap ms)
      val _ = log_emap 4 "Me" (MatchState.get_emap ms)
      
      (* match all scheduled wires in the neighbourhood of the given node-vertex *)
      fun match_nhd lvl ms nvp = let
        val _ = Log.logf 3 (fn () => ind lvl
              "== Matching nhd of "^(V.string_of_name nvp)) ()
        
        val nvt = VVInj.get (MatchState.get_vmap ms) nvp
        val (pat, tgt) = (MatchState.get_pat ms, MatchState.get_tgt ms)
        
        (* We pick one wire-vertex in Uw adjacent to nvp, and branch for each
         * wire-vertex in T adjacent to nvt.  On each branch, we try to match
         * the entire wire, and recurse back to match_main if successful.
         *
         * Since we care about edge-data, we actually grab the incident edges,
         * rather than just the vertex at the other end.
         *)
        
        fun edge_end_in_Uw e =
              V.NSet.contains (MatchState.get_u_wireverts ms)
                              (Graph.edge_get_other_vertex pat e nvp)
        fun edge_end_in_T e =
              V.NSet.contains (MatchState.get_tgt_verts ms)
                              (Graph.edge_get_other_vertex tgt e nvt)
        
        fun add_branch_if_wire_matches ep et =
              case tryadd_wire lvl ms (nvp,ep) (nvt,et)
                of SOME ms' => Seq.append (match_nhd (lvl+1) ms' nvp)
                 | NONE     => I
        
        fun remove_nvp_from_P_if_complete ms =
          let
            val edge_matched = EEInj.cod_contains (MatchState.get_emap ms)
          in
            if E.NSet.forall edge_matched (Graph.get_adj_edges tgt nvt)
            then MatchState.update_p_nodeverts (V.NSet.delete nvp) ms
            else ms
          end
      in
        case E.NSet.get_exists edge_end_in_Uw (Graph.get_adj_edges pat nvp)
          of SOME ep =>
               let
                 val candidate_tgt_edges =
                      E.NSet.filter edge_end_in_T
                                    (Graph.get_adj_edges tgt nvt)
               in
                 E.NSet.fold (add_branch_if_wire_matches ep)
                             candidate_tgt_edges
                             Seq.empty
               end
           | NONE => match_main (lvl+1) (remove_nvp_from_P_if_complete ms)
      end (* match_nhd *)
      
      
      (* continue matching from the given partially-matched node-vertex *)
      fun continue_matching_from nvp =
        if not (p_vertex_may_be_completed ms nvp)
        then (log_match_fail_v1 lvl "Doomed" nvp; Seq.empty)
        else match_nhd lvl
              (ms |> MatchState.update_ps_nodeverts (V.NSet.delete nvp))
              nvp
      
      (* Start matching a graph component
       *
       * nv is removed from Un and, for every matching vertex tnv in T,
       * a branch is created matching nv against tnv
       *)
      fun match_and_schedule_new nv = let
        val _ = Log.logf 3 (fn () => ind lvl
              "== Next unmatched: "^(V.string_of_name nv)) ()
        val ms' = ms |> MatchState.update_u_nodeverts  (V.NSet.delete nv)
                     |> MatchState.update_p_nodeverts  (V.NSet.add nv)
                     |> MatchState.update_ps_nodeverts (V.NSet.add nv)
        val nv_data = Graph.get_vertex_data (MatchState.get_pat ms') nv
        fun match_nv_and_continue tnv =
          case Graph.match_vdata (nv_data,
                                  Graph.get_vertex_data (MatchState.get_tgt ms') tnv)
                                 (MatchState.get_match_psubst ms')
            of SOME subst =>
                 match_main (lvl+1)
                            (ms' |> MatchState.set_match_psubst subst
                                 |> add_vertex_to_match lvl (nv, tnv)
                                 |> MatchState.update_tgt_verts (V.NSet.delete tnv))
             | NONE => Seq.empty
      in
        Seq.maps match_nv_and_continue
                 (V.NSet.fold Seq.cons (MatchState.get_tgt_verts ms) Seq.empty)
      end (* match_and_schedule_new *)
    in
      (* process next node-vertex in Ps *)
      case V.NSet.get_min (MatchState.get_ps_nodeverts ms)
        of SOME next_v => continue_matching_from next_v
         | NONE =>
             (
               (* Ps empty; look in Un *)
               case V.NSet.get_min (MatchState.get_u_nodeverts ms)
                 of SOME next_v => match_and_schedule_new next_v
                  | NONE =>
                      (Log.log 2 (ind lvl "==? All scheduled vertices matched");
                       Seq.single ms)
             )
    end (* match_main *)
  in
    (* match all circles, then enter main matching routine *)
    Seq.maps (match_main 0) (match_circles match_state)
  end
  
  
  
  (*****************************************)
  (********** BARE WIRE MATCHING ***********)
  (*****************************************)
  
  (* Splits an edge into two or three edges
   *
   * Input: an edge not in cod emap, whose source and target are in either T or
   * cod vmap, and at least one of them is a wire-vertex
   *
   * Result: ((s,e,t),g), where g is the updated graph, e is one of the edges
   * resulting from splitting the input edge, s and t are the source and target
   * of e, respectively, and are in T (and not in cod vmap), and the "other
   * edges" of s and t satisfy the input requirements of this function.
   *)
  fun grow_tgt_edge lvl ms e = let
    val tgt = MatchState.get_tgt ms
    val (s,t) = (Graph.get_edge_source tgt e, Graph.get_edge_target tgt e)
    (* hackish way to avoid renaming issues: make sure new vnames
     * are not in pattern *)
    val patvs = Graph.get_vertices (MatchState.get_pat ms)
    val avoidvs = V.NSet.union_merge patvs (Graph.get_vertices tgt);
    val ((e1,v1,e2),tgt') = Graph.split_edge e tgt
    val (v1',avoidvs') = V.NSet.add_new v1 avoidvs
    val tgt' = if V.name_eq (v1,v1') then tgt'
               else Graph.rename_vertex v1 v1' tgt'
  in
    if V.name_eq (s,t) then
      (* circles get special treatment; s/t cannot be in cod vmap if e is not in
       * cod emap, and so it must be in T, and we only need to split once to get
       * the result we want
       *)
      (Log.logf 2 (fn () => ind lvl
        ">> Split edge "^(E.string_of_name e)^" into ("^
        (E.string_of_name e1)^","^(V.string_of_name v1')^","^
        (E.string_of_name e2)^")") ();
       log_graph 4 "New tgt" tgt';
      ((v1',e2,t),
        ms |> MatchState.set_tgt tgt'
           |> MatchState.update_tgt_verts (V.NSet.add v1')))
    else
      let
        val ((e2,v2,e3),tgt') = Graph.split_edge e2 tgt'
        val (v2',_) = V.NSet.add_new v2 avoidvs'
        val tgt' = if V.name_eq (v2,v2') then tgt'
                   else Graph.rename_vertex v2 v2' tgt'
        val _ = Log.logf 2 (fn () => ind lvl
          ">> Split edge "^(E.string_of_name e)^" into ("^
            (E.string_of_name e1)^","^(V.string_of_name v1')^","^
            (E.string_of_name e2)^","^(V.string_of_name v2')^","^
            (E.string_of_name e3)^")") ()
        val _ = log_graph 4 "New tgt" tgt';
      in
        ((v1',e2,v2'),
         ms |> MatchState.set_tgt tgt'
            |> MatchState.update_tgt_verts (V.NSet.add v1')
            |> MatchState.update_tgt_verts (V.NSet.add v2'))
      end
  end
  
  
  (* match the given set of bare wires, expanding wire-vertices where necessary *)
  fun match_bare_wires' lvl context ms = let
    val _ = Log.log 2 (ind lvl "== Starting bare wire match")
  in
    if E.NSet.is_empty (MatchState.get_u_bare_wires ms) then Seq.single ms
    else let
      val bare_wire_match_is_allowed = Hooks.bare_wire_match_is_allowed context
      val (pat, tgt) = (MatchState.get_pat ms, MatchState.get_tgt ms)
      val tgt_verts = MatchState.get_tgt_verts ms
      val (vmap, emap) = (MatchState.get_vmap ms, MatchState.get_emap ms)
      (* for a normalised graph, this will always be the source end *)
      val bw = (the o E.NSet.get_min) (MatchState.get_u_bare_wires ms)
      val ((bw_dir, bw_data), (bw_s, bw_t)) = Graph.get_edge_info pat bw
      val (bw_sdata, bw_tdata) = (Graph.get_vertex_data pat bw_s, Graph.get_vertex_data pat bw_t)
      
      (* candidate edges for matching are:
           - the unique edge in a circle, bare wire, or boundary wire
           - the middle edge in an interior wire *)
      fun is_candidate_for_bw e = let
        val ((d,_),(s,t)) = Graph.get_edge_info tgt e
        fun vertex_matchable v =
          (V.NSet.contains tgt_verts v) orelse (VVInj.cod_contains vmap v)
      in
        (bare_wire_match_is_allowed ms (bw_s,bw,bw_t) (s,e,t)) andalso
        not (EEInj.cod_contains (MatchState.get_emap ms) e) andalso
        d = bw_dir andalso vertex_matchable s andalso vertex_matchable t andalso
        (Graph.is_boundary tgt s orelse Graph.is_boundary tgt t orelse    (* bare and boundary wires *)
         (Graph.is_wire_vertex tgt s andalso Graph.is_wire_vertex tgt t)) (* circles + middle edges *)
      end
      
      (* sequence of all the edges of the right kind with matching dir_or_undir *)
      val edges = E.NSet.fold_rev
        (fn e => if is_candidate_for_bw e then Seq.cons e else I)
        (Graph.get_edges tgt) Seq.empty
      fun match_bw e = let
        (* grow edge and grab relevant data *)
        val ((s,mid_e,t), ms') = grow_tgt_edge lvl ms e
        val tgt' = MatchState.get_tgt ms'
        val et_data = Graph.get_edge_data tgt' mid_e
        val (tgt_sdata,tgt_tdata) = (Graph.get_vertex_data tgt' s, Graph.get_vertex_data tgt' t)
        
        (* match data for edge, source, and target *)
        val fmatch_d = Option.composePartial
                          (Graph.match_edata (bw_data, et_data),
                           Option.composePartial
                              (Graph.match_vdata (bw_sdata, tgt_sdata),
                               Graph.match_vdata (bw_tdata, tgt_tdata)));
        fun add_bw new_subst = let
          val ms' = ms' |> MatchState.set_match_psubst new_subst
                        |> MatchState.update_tgt_verts (V.NSet.delete s)
                        |> MatchState.update_tgt_verts (V.NSet.delete t)
                        |> MatchState.update_u_bare_wires (E.NSet.delete bw)
        in (fn (es,et) =>
             ms' |> add_vertex_to_match lvl (bw_s,es)
                 |> add_edge_to_match lvl (bw,bw_t) (mid_e,et)
           )
        end
      in
        case fmatch_d (MatchState.get_match_psubst ms')
          of SOME new_subst =>
               let val add_bw' = add_bw new_subst in
                  case bw_dir
                    of Directed => match_bare_wires' (lvl+1) context (add_bw' (s,t))
                     | Undirected =>
                         Seq.append (match_bare_wires' (lvl+1) context (add_bw' (s,t)))
                                    (match_bare_wires' (lvl+1) context (add_bw' (t,s)))
               end
           | NONE => Seq.empty
      end
    in Seq.maps match_bw edges
    end
  end
  val match_bare_wires = match_bare_wires' 0
  
  structure Sharing =
  struct
    structure Graph = Graph.Sharing
    structure MatchState = MatchState.Sharing
  end
end
