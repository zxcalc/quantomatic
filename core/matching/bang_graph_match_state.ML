(* The match state for graph matching with !-graphs *)
(* see docs/matching_algo for details *)

(* The type-sharing struct for OGRAPH_MATCH_STATE *)
signature BANG_GRAPH_MATCH_STATE_SHARING
= sig
  structure G : OGRAPH_SHARING
  structure BBoxOperableGraph : BBOX_OPERABLE_GRAPH_SHARING
    sharing G = BBoxOperableGraph.G
  type T;
end

(* The match state for pattern graph matching *)
signature BANG_GRAPH_MATCH_STATE
= sig
  type T;

  (* NB: there is no BANG_GRAPH_SHARING signature *)
  structure G : BANG_GRAPH;
  structure BBoxOperableGraph : BBOX_OPERABLE_GRAPH
  structure InnerMatchState : OGRAPH_MATCH_STATE;
  structure Sharing : BANG_GRAPH_MATCH_STATE_SHARING
    sharing type Sharing.T = T
    sharing Sharing.G = G.SharingOGraph
    sharing Sharing.BBoxOperableGraph = BBoxOperableGraph.Sharing
    sharing Sharing.G = InnerMatchState.Sharing.G

  val get_pat : T -> BBoxOperableGraph.T;
  val update_pat : (BBoxOperableGraph.T -> BBoxOperableGraph.T) -> T -> T;
  val set_pat : BBoxOperableGraph.T -> T -> T;

  val get_inner_state : T -> InnerMatchState.T;
  val update_inner_state : (InnerMatchState.T -> InnerMatchState.T) -> T -> T;
  val set_inner_state : InnerMatchState.T -> T -> T;

  (* Wild !-boxes that were killed *)
  val get_wild_bboxes : T -> B.NSet.T
  val add_wild_bbox   : B.name -> T -> T

  (* schedules all the concrete vertices that have not been seen before
   *)
  val schedule_new_concrete : T -> T
  (* schedules all the concrete vertices in the pattern *)
  val schedule_concrete_pat : T -> T

  val expand_pat_bbox : B.name -> T -> T
  val kill_pat_bbox : B.name -> T -> T

  (* Kills any !-boxes that are adjacent to a completely-matched vertex *)
  val kill_impossible_bboxes : T -> T

  val init : BBoxOperableGraph.T -> G.T -> T;
end

(* type-coerced version of MatchState for pattern graphs *)
functor BangGraphMatchState(
  structure InnerMatchState : OGRAPH_MATCH_STATE;
  structure BBoxOperableGraph : BBOX_OPERABLE_GRAPH
  sharing InnerMatchState.Sharing.G = BBoxOperableGraph.Sharing.G
) : BANG_GRAPH_MATCH_STATE
= struct
  structure G = BBoxOperableGraph.G;
  structure BBoxOperableGraph = BBoxOperableGraph;
  structure InnerMatchState = InnerMatchState;

  (* shorthand *)
  structure Oper = BBoxOperableGraph;
  structure Inner = InnerMatchState;

  datatype T = MatchState of {
    pat          : BBoxOperableGraph.T,
    wild_bboxes  : B.NSet.T,
    inner_state  : InnerMatchState.T
  };

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

  fun pretty ms = let
    fun pretty_bset prefix bs =
      if B.NSet.is_empty bs then []
      else [Pretty.block [
              Pretty.str (prefix^": "),
              B.NSet.pretty bs
           ]]
  in Pretty.chunks(
    [Pretty.str "BangGraphMatchState{",
     Pretty.block [
       Pretty.str ("bbox_oper: "),
       Oper.pretty (get_pat ms)
     ],
     Inner.pretty (get_inner_state ms)]
    @ (pretty_bset "wild !-boxes" (get_wild_bboxes ms))
    @ [Pretty.str "}"])
  end;
  val print = Pretty.writeln o pretty;

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

  val expand_pat_bbox = update_pat o BBoxOperableGraph.expand_bbox
  val kill_pat_bbox = update_pat o BBoxOperableGraph.kill_bbox

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

  structure Sharing =
  struct
    type T = T;
    structure G = G.SharingOGraph;
    structure BBoxOperableGraph = BBoxOperableGraph.Sharing;
  end;
end

