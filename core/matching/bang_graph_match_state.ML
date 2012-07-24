(* The match state for graph matching with !-graphs *)
(* see docs/matching_algo for details *)

(* The match state for pattern graph matching *)
signature BANG_GRAPH_MATCH_STATE
= sig
  include OGRAPH_MATCH_STATE;
  (* NB: there is no BANG_GRAPH_SHARING signature *)
  structure BG : BANG_GRAPH;
  sharing G.SharingOGraph = BG.SharingOGraph;

  (* Issues encountered during matching *)
  datatype Warning = (* A !-box with no node-vertices was killed to stop
                      * it running wild; matching will therefore be
                      * incomplete
                      *)
                     WildBBoxKilled of B.name;

  (* Vertex names to avoid when copying bboxes in pat
   *
   * (see replay_pat_bbox_ops)
   *)
  val get_pat_avoids    : T -> V.NSet.T
  val update_pat_avoids : (V.NSet.T -> V.NSet.T) -> T -> T
  val set_pat_avoids    : V.NSet.T -> T -> T

  (* Vertex names to keep track of when copying bboxes in pat
   *
   * Typically the boundary of the graph.
   *
   * (see replay_pat_bbox_ops)
   *)
  val get_pat_tracked    : T -> V.NSet.T
  val update_pat_tracked : (V.NSet.T -> V.NSet.T) -> T -> T
  val set_pat_tracked    : V.NSet.T -> T -> T

  (* Issues encountered during matching.
   *
   * (see Warning datatype)
   *)
  val get_warnings : T -> Warning list
  val add_warning  : Warning -> T -> T

  (* initialise MatchState and schedule all concrete vertices in pattern
   * to be matched
   * args: pattern graph, target graph *)
  val init_and_schedule_concrete : G.T -> G.T -> T
  val init_subgraph_and_schedule_concrete : G.T -> G.T -> V.NSet.T -> T
  (* schedules all the concrete vertices that have not been seen before
   *)
  val schedule_new_concrete : T -> T

  val expand_pat_bbox : B.name -> T -> T
  val kill_pat_bbox : B.name -> T -> T

  (* Kills any !-boxes that are adjacent to a completely-matched vertex *)
  val kill_impossible_bboxes : T -> T

  (* Replays the pattern graph's !-box operations on another graph.
   *
   * The graph that they will be replayed on must have exactly the same
   * !-box structure (including names) as the pattern graph for this to
   * work.  The vertices in pat_tracked should also be in both graphs,
   * and be contained in exactly the same !-boxes in both graphs.  This
   * function will then ensure that the same names are given to copies
   * of those vertices as in the pattern graph.
   *
   * It is also required that any vertex names in the graph were part
   * of pat_avoids, otherwise a name clash could occur.
   *)
  val replay_pat_bbox_ops : T -> G.T -> G.T
end

(* type-coerced version of MatchState for pattern graphs *)
functor BangGraphMatchState(
  structure G : BANG_GRAPH
  type data
  val empty_data : data
) : BANG_GRAPH_MATCH_STATE
= struct
  type data2 = data

  datatype BBoxOp = BBKill of B.name
                  | BBCopy of {
                      bbox : B.name,
                      vmap : V.name V.NTab.T,
                      bmap : B.name B.NTab.T
                    };

  datatype Warning = WildBBoxKilled of B.name;

  type bdata = {
    (* names context for fresh names when copying bboxes in pat *)
    pat_avoids   : V.NSet.T,
    (* names context for fresh names when copying bboxes in pat *)
    pat_tracked  : V.NSet.T,
    (* !-box operation sequence *)
    pat_bbox_ops : BBoxOp list,
    (* Issues encountered during matching *)
    warnings     : Warning list,
    (* type-extensibility *)
    data         : data
  };
  fun get_data_of_bdata (bdata:bdata) : data = #data bdata;
  fun update_data_of_bdata f (bdata:bdata) = {
    pat_avoids   = #pat_avoids bdata,
    pat_tracked  = #pat_tracked bdata,
    pat_bbox_ops = #pat_bbox_ops bdata,
    warnings     = #warnings bdata,
    data         = f (#data bdata)
  };
  fun get_pat_avoids_of_bdata (bdata:bdata) = #pat_avoids bdata
  fun update_pat_avoids_of_bdata f (bdata:bdata) = {
    pat_avoids   = f (#pat_avoids bdata),
    pat_tracked  = #pat_tracked bdata,
    pat_bbox_ops = #pat_bbox_ops bdata,
    warnings     = #warnings bdata,
    data         = #data bdata
  };
  fun get_pat_tracked_of_bdata (bdata:bdata) = #pat_tracked bdata
  fun update_pat_tracked_of_bdata f (bdata:bdata) = {
    pat_avoids   = #pat_avoids bdata,
    pat_tracked  = f (#pat_tracked bdata),
    pat_bbox_ops = #pat_bbox_ops bdata,
    warnings     = #warnings bdata,
    data         = #data bdata
  };
  fun get_pat_bbox_ops_of_bdata (bdata:bdata) = #pat_bbox_ops bdata
  fun update_pat_bbox_ops_of_bdata f (bdata:bdata) = {
    pat_avoids   = #pat_avoids bdata,
    pat_tracked  = #pat_tracked bdata,
    pat_bbox_ops = f (#pat_bbox_ops bdata),
    warnings     = #warnings bdata,
    data         = #data bdata
  };
  fun get_warnings_of_bdata (bdata:bdata) = #warnings bdata
  fun add_warning_of_bdata w (bdata:bdata) = {
    pat_avoids   = #pat_avoids bdata,
    pat_tracked  = #pat_tracked bdata,
    pat_bbox_ops = #pat_bbox_ops bdata,
    warnings     = w::(#warnings bdata),
    data         = #data bdata
  };

  structure BG = G;
  structure OGMS = OGraphMatchState(
    structure G = G
    type data = bdata
    val empty_data = {
      pat_avoids   = V.NSet.empty,
      pat_tracked   = V.NSet.empty,
      pat_bbox_ops = [],
      warnings     = [],
      data         = empty_data
    }
  );
  open OGMS;

  (* override data things from OGraphMatchState *)
  type data = data2
  val get_data = get_data_of_bdata o OGMS.get_data;
  val update_data = OGMS.update_data o update_data_of_bdata;

  (* new getters/setters *)
  val get_pat_avoids = get_pat_avoids_of_bdata o OGMS.get_data;
  val update_pat_avoids = OGMS.update_data o update_pat_avoids_of_bdata;
  val set_pat_avoids = update_pat_avoids o K;

  val get_pat_tracked = get_pat_tracked_of_bdata o OGMS.get_data;
  val update_pat_tracked = OGMS.update_data o update_pat_tracked_of_bdata;
  val set_pat_tracked = update_pat_tracked o K;

  val get_pat_bbox_ops = get_pat_bbox_ops_of_bdata o OGMS.get_data;
  val update_pat_bbox_ops = OGMS.update_data o update_pat_bbox_ops_of_bdata;

  val get_warnings = get_warnings_of_bdata o OGMS.get_data;
  val add_warning = OGMS.update_data o add_warning_of_bdata;

  (* FIXME: make more efficient? *)
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
  fun init_and_schedule_concrete pat tgt =
    schedule_concrete (G.get_vnames pat) (G.get_enames pat) (init pat tgt)

  (* NB: undefined behaviour if !-boxes not open subgraphs or graphs not
   * normalised *)
  fun init_subgraph_and_schedule_concrete pat tgt tgt_verts =
    schedule_concrete (G.get_vnames pat)
                      (G.get_enames pat)
                      (init_subgraph pat tgt tgt_verts)

  fun schedule_new_concrete ms = let
    val new_vs = V.NSet.subtract (G.get_vnames (get_pat ms))
                                 (VInjEndo.get_domset (get_vmap ms))
    val new_es = E.NSet.subtract (G.get_enames (get_pat ms))
                                 (EInjEndo.get_domset (get_emap ms))
  in
    schedule_concrete new_vs new_es ms
  end

  fun replay_bbox_op _ (BBKill b) g = BG.kill_bbox b g
    | replay_bbox_op v_avoid (BBCopy {bbox,vmap,bmap}) g = let
        val v_avoid = v_avoid |> V.NSet.union_merge (BG.get_vnames g)
        val vrn = V.Rnm.mk V.NSet.empty v_avoid vmap
        val ern = E.Rnm.mk_from_avoids (BG.get_enames g)
        val brn = B.Rnm.mk B.NSet.empty (BG.get_bboxes g) bmap
        val (_,g') = BG.expand1_bbox bbox ((vrn,ern,brn),g)
      in g' end

  fun replay_pat_bbox_ops ms = let
    val bbops   = get_pat_bbox_ops ms
    val avoids' = get_pat_avoids ms
    val pat     = get_pat ms
    val avoids  = avoids' |> V.NSet.union_merge (BG.get_vnames pat)
  in
    fold_rev (replay_bbox_op avoids) bbops
  end

  fun expand_pat_bbox b ms = let
    val g = get_pat ms
    val v_avoid = get_pat_avoids ms
    val v_avoid = v_avoid |> V.NSet.union_merge (BG.get_vnames g)
    val vrn = V.Rnm.mk_from_avoids v_avoid
    val ern = E.Rnm.mk_from_avoids (BG.get_enames g)
    val brn = B.Rnm.mk_from_avoids (BG.get_bboxes g)
    val ((vrn',_,brn'),g') = BG.expand1_bbox b ((vrn,ern,brn),g)
    val vmap = V.Rnm.get_nmap vrn'
    val tracked = get_pat_tracked ms
    (* only keep track of the names of the vertices we care about *)
    val vmap' = V.NSet.fold (fn v =>
        case V.NTab.lookup vmap v of
             NONE => (fn x => x)
           | SOME v' => V.NTab.ins (v,v')) tracked V.NTab.empty
    val bbop = BBCopy {
        bbox = b,
        vmap = vmap',
        bmap = B.Rnm.get_nmap brn'
      }
  in
    ms |> set_pat g'
       |> update_pat_bbox_ops (cons bbop)
       |> update_pat_tracked (V.NTab.fold (V.NSet.add o snd) vmap')
  end

  fun kill_pat_bbox b ms = let
    val pat' = BG.kill_bbox b (get_pat ms)
  in
    ms |> set_pat pat'
       |> update_pat_bbox_ops (cons (BBKill b))
    handle BG.no_such_bbox_exp _ => ms
  end

  fun kill_bboxes_adj_to_complete_nvs ms = let
    val pat = get_pat ms
    val complete = get_complete_nodeverts ms
    val touched = G.incident_vertices pat complete
    fun kill_all_bboxes v ms = let
      val pat = get_pat ms
    in
      B.NSet.fold kill_pat_bbox (BG.get_bboxes_of pat v) ms
      handle G.no_such_vname_exp _ => ms
    end
  in
    V.NSet.fold kill_all_bboxes touched ms
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
      V.NSet.forall (is_end_of_bare_wire g) (BG.get_bbox_contents g bbox)
    fun kill_pat_bbox' bbox ms =
      ms |> kill_pat_bbox bbox
         |> add_warning (WildBBoxKilled bbox)
    fun kill_if_wild bbox ms =
      (if is_wild (get_pat ms) bbox
       then kill_pat_bbox' bbox ms
       else ms)
      handle BG.no_such_bbox_exp _ => ms
  in
    B.NSet.fold kill_if_wild (BG.get_bboxes (get_pat ms)) ms
  end

  val kill_impossible_bboxes = kill_wild_bboxes
                             o kill_bboxes_adj_to_complete_nvs
end

