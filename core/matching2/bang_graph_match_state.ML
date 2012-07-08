(* The match state for graph matching with !-graphs *)
(* see docs/matching_algo for details *)

(* The match state for pattern graph matching *)
signature BANG_GRAPH_MATCH_STATE
= sig
  include OGRAPH_MATCH_STATE;
  (* NB: there is no BANG_GRAPH_SHARING signature *)
  structure BG : BANG_GRAPH;
  sharing G.SharingOGraph = BG.SharingOGraph;

  (* initialise MatchState and schedule all concrete vertices in pattern
   * to be matched
   * args: pattern graph, target graph *)
  val init_and_schedule_concrete : G.T -> G.T -> T
  (* schedules all the concrete vertices that have not been seen before
   *)
  val schedule_new_concrete : T -> T

  val expand_pat_bbox : B.name -> T -> T
  val kill_pat_bbox : B.name -> T -> T

  (* Kills any !-boxes that are adjacent to a completely-matched vertex *)
  val kill_impossible_bboxes : T -> T
end

(* type-coerced version of MatchState for pattern graphs *)
functor BangGraphMatchState(
  structure G : BANG_GRAPH
  type data
  val empty_data : data
) : BANG_GRAPH_MATCH_STATE
= struct
  structure BG = G;
  structure OGMS = OGraphMatchState(
    structure G = G
    type data = data
    val empty_data = empty_data
  );
  open OGMS;

  fun schedule_concrete new_vs new_es ms = let
    val pat = get_pat ms

    val touched = G.incident_vertices pat new_vs
    val to_sched = V.NSet.intersect (get_p_nodeverts ms) touched

    val unbboxed = V.NSet.subtract new_vs (BG.get_bboxed pat)
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
    ms |> extend_u_circles circle_wvs
       |> extend_u_nodeverts (V.NSet.intersect unbboxed nvs)
       |> extend_u_wireverts (V.NSet.intersect unbboxed wvs)
       |> extend_u_bare_wires bare_wire_es
       |> extend_ps_nodeverts to_sched
  end

  (* NB: undefined behaviour if !-boxes not open subgraphs or graphs not
   * normalised *)
  (* FIXME: make more efficient? *)
  fun init_and_schedule_concrete pat tgt =
    schedule_concrete (G.get_vnames pat) (G.get_enames pat) (init pat tgt)

  fun schedule_new_concrete ms = let
    val new_vs = V.NSet.subtract (G.get_vnames (get_pat ms))
                                 (VInjEndo.get_domset (get_vmap ms))
    val new_es = E.NSet.subtract (G.get_enames (get_pat ms))
                                 (EInjEndo.get_domset (get_emap ms))
  in
    schedule_concrete new_vs new_es ms
  end

  (* FIXME: record this in an appropriate format *)
  fun expand_pat_bbox b ms = let
    fun expand_bbox g = let
      val (v_avoid,e_avoid) = get_names ms
      val v_avoid = v_avoid |> V.NSet.union_merge (BG.get_vnames g)
      val e_avoid = e_avoid |> E.NSet.union_merge (BG.get_enames g)
      val vrn = V.Rnm.mk_from_avoids v_avoid
      val ern = E.Rnm.mk_from_avoids e_avoid
      val brn = B.Rnm.mk_from_avoids (BG.get_bboxes g)
      val (_,g') = BG.expand1_bbox b ((vrn,ern,brn),g)
    in g' end
  in
    ms |> update_pat expand_bbox
  end
  (* FIXME: record this in an appropriate format *)
  fun kill_pat_bbox b = update_pat (BG.kill_bbox b)

  fun kill_impossible_bboxes ms = let
    val pat = get_pat ms
    val complete = get_complete_nodeverts ms
    val touched = G.incident_vertices pat complete
    fun kill_all_bboxes v =
      B.NSet.fold BG.kill_bbox (BG.get_bboxes_of pat v)
    val pat' = V.NSet.fold kill_all_bboxes touched pat
  in
    set_pat pat' ms
  end
end

