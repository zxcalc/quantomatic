(* The match state for graph matching with !-graphs *)
(* see docs/matching_algo for details *)

(* The type-sharing struct for OGRAPH_MATCH_STATE *)
signature BANG_GRAPH_MATCH_STATE_SHARING
= sig
  include OGRAPH_MATCH_STATE_SHARING
  structure BBoxOperableGraph : BBOX_OPERABLE_GRAPH_SHARING
    sharing G = BBoxOperableGraph.G
end

(* The match state for pattern graph matching *)
signature BANG_GRAPH_MATCH_STATE
= sig
  include OGRAPH_MATCH_STATE;
  (* NB: there is no BANG_GRAPH_SHARING signature *)
  structure BG : BANG_GRAPH;
  structure BBoxOperableGraph : BBOX_OPERABLE_GRAPH
  structure BGSharing : BANG_GRAPH_MATCH_STATE_SHARING
    sharing BGSharing = Sharing
    sharing BG.SharingOGraph = BGSharing.G
    sharing BBoxOperableGraph.Sharing = BGSharing.BBoxOperableGraph

  (* Issues encountered during matching *)
  datatype Warning = (* A !-box with no node-vertices was killed to stop
                      * it running wild; matching will therefore be
                      * incomplete
                      *)
                     WildBBoxKilled of B.name;

  val get_bbox_oper : T -> BBoxOperableGraph.T;
  val update_bbox_oper : (BBoxOperableGraph.T -> BBoxOperableGraph.T) -> T -> T;
  val set_bbox_oper : BBoxOperableGraph.T -> T -> T;

  (* Issues encountered during matching.
   *
   * (see Warning datatype)
   *)
  val get_warnings : T -> Warning list
  val add_warning  : Warning -> T -> T

  (* schedules all the concrete vertices that have not been seen before
   *)
  val schedule_new_concrete : T -> T
  (* schedules all the concrete vertices in the pattern *)
  val schedule_concrete_pat : T -> T

  val expand_pat_bbox : B.name -> T -> T
  val kill_pat_bbox : B.name -> T -> T

  (* Kills any !-boxes that are adjacent to a completely-matched vertex *)
  val kill_impossible_bboxes : T -> T
end

(* type-coerced version of MatchState for pattern graphs *)
functor BangGraphMatchState(
  structure G : BANG_GRAPH
  structure BBoxOperableGraph : BBOX_OPERABLE_GRAPH
  sharing G.SharingOGraph = BBoxOperableGraph.G.SharingOGraph;
  type data
  val empty_data : data
) : BANG_GRAPH_MATCH_STATE
= struct
  type data2 = data

  structure BBoxOperableGraph = BBoxOperableGraph;

  datatype Warning = WildBBoxKilled of B.name;

  type bdata = {
    (* !-box operation sequence *)
    bbox_oper    : BBoxOperableGraph.T,
    (* Issues encountered during matching *)
    warnings     : Warning list,
    (* type-extensibility *)
    data         : data
  };
  fun get_data_of_bdata (bdata:bdata) : data = #data bdata;
  fun update_data_of_bdata f (bdata:bdata) = {
    bbox_oper    = #bbox_oper bdata,
    warnings     = #warnings bdata,
    data         = f (#data bdata)
  };
  fun get_bbox_oper_of_bdata (bdata:bdata) = #bbox_oper bdata
  fun set_bbox_oper_of_bdata oper (bdata:bdata) = {
    bbox_oper    = oper,
    warnings     = #warnings bdata,
    data         = #data bdata
  };
  fun get_warnings_of_bdata (bdata:bdata) = #warnings bdata
  fun add_warning_of_bdata w (bdata:bdata) = {
    bbox_oper    = #bbox_oper bdata,
    warnings     = w::(#warnings bdata),
    data         = #data bdata
  };

  structure BG = G;
  structure OGMS = OGraphMatchState(
    structure G = G
    type data = bdata
    val empty_data = {
      bbox_oper    = BBoxOperableGraph.empty,
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
  val get_bbox_oper = get_bbox_oper_of_bdata o OGMS.get_data;
  fun set_bbox_oper new_oper ms =
    ms |> OGMS.set_pat (BBoxOperableGraph.get_current_graph new_oper)
       |> OGMS.update_data (set_bbox_oper_of_bdata new_oper);
  fun update_bbox_oper f ms = let
    val old_oper = get_bbox_oper ms
    val new_oper = f old_oper
  in
    set_bbox_oper new_oper ms
  end;

  val get_warnings = get_warnings_of_bdata o OGMS.get_data;
  val add_warning = OGMS.update_data o add_warning_of_bdata;
  
  fun pretty ms = let
    fun pretty_vset prefix vs =
      if V.NSet.is_empty vs then []
      else [Pretty.block [
              Pretty.str (prefix^": "),
              V.NSet.pretty vs
           ]]
  in Pretty.chunks(
    [Pretty.str "BangGraphMatchState{",
     OGMS.pretty ms,
     Pretty.str "}"])
  end

  val print = Pretty.writeln o pretty

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

  fun schedule_new_concrete ms = let
    val new_vs = V.NSet.subtract (G.get_vnames (get_pat ms))
                                 (VInjEndo.get_domset (get_vmap ms))
    val new_es = E.NSet.subtract (G.get_enames (get_pat ms))
                                 (EInjEndo.get_domset (get_emap ms))
  in
    schedule_concrete new_vs new_es ms
  end
  fun schedule_concrete_pat ms = let
    val vs = G.get_vnames (get_pat ms)
    val es = G.get_enames (get_pat ms)
  in
    schedule_concrete vs es ms
  end

  val expand_pat_bbox = update_bbox_oper o BBoxOperableGraph.expand_bbox
  val kill_pat_bbox = update_bbox_oper o BBoxOperableGraph.kill_bbox

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

  structure Sharing =
  struct
    type T = T;
    structure G = BG.SharingOGraph;
  end;

  structure BGSharing =
  struct
    open Sharing;
    structure BBoxOperableGraph = BBoxOperableGraph.Sharing;
  end;
end

