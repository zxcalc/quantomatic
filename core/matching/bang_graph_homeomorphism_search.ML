(**
  * Finds wire-homeomorphisms between !-graphs
  *
  * This is essentially a particularly strict match search where
  * the matches must be valid in both directions and no !-box
  * operations are permitted.  It is essentially an isomorphism
  * up to the number of wire-vertices on a wire.
  *
  * This essentially checks that the normalised forms of the two
  * graphs are the same up to vertex, edge and !-box names.  Be
  * warned that it makes use of Graph.vdata_eq and Graph.edata_eq,
  * so alpha-conversion in element data is not supported: the names
  * must be identical.
  *)

signature BANG_GRAPH_HOMEOMORPHISM_SHARING
= sig
  type T
  structure Graph : OGRAPH_SHARING
end

(* A homeomorphism from a !-graph to a !-graph *)
signature BANG_GRAPH_HOMEOMORPHISM
= sig
  type T
  structure Graph : BANG_GRAPH
  
  (* sharing via sharing structure *)
  structure Sharing : BANG_GRAPH_HOMEOMORPHISM_SHARING
    sharing type Sharing.T = T
    sharing Sharing.Graph = Graph.Sharing
  
  (* the graph to match from *)
  val get_pat   : T -> Graph.T
  (* the graph to match onto *)
  val get_tgt   : T -> Graph.T
  (* the vertex map (from pat verts to tgt verts *)
  val get_vmap  : T -> VVInj.T
  (* the edge map (from pat edges to tgt edges *)
  val get_emap  : T -> EEInj.T
  (* the bbox map (from pat !-boxes to tgt !-boxes *)
  val get_bmap  : T -> BBInj.T

  exception bad_homeomorphism_exp of string
                                   * Graph.T
                                   * Graph.T
                                   * VVInj.T
                                   * EEInj.T
                                   * BBInj.T

  (* create a match; checks for correctness *)
  (* raises bad_match_exp if not a valid match *)
  val mk : Graph.T
        -> Graph.T
        -> VVInj.T
        -> EEInj.T
        -> BBInj.T
        -> T

  (* create a match; checks for correctness *)
  (* returns NONE if not a valid match *)
  val mk_opt : Graph.T
            -> Graph.T
            -> VVInj.T
            -> EEInj.T
            -> BBInj.T
            -> T option

  val print : T -> unit
  val pretty : T -> Pretty.T
end

signature BANG_GRAPH_HOMEOMORPHISM_SEARCH_SHARING
= sig
  structure Graph : OGRAPH_SHARING
  structure Homeomorphism : BANG_GRAPH_HOMEOMORPHISM_SHARING
  sharing Graph = Homeomorphism.Graph
end


signature BANG_GRAPH_HOMEOMORPHISM_SEARCH
= sig
  structure Graph : BANG_GRAPH
  structure Homeomorphism : BANG_GRAPH_HOMEOMORPHISM
  structure Sharing : BANG_GRAPH_HOMEOMORPHISM_SEARCH_SHARING
    sharing Sharing.Graph = Graph.Sharing
    sharing Sharing.Homeomorphism = Homeomorphism.Sharing

  val find_homeomorphisms : Graph.T -> Graph.T -> Homeomorphism.T Seq.seq
  val is_homeomorphic : Graph.T -> Graph.T -> bool
end



functor BangGraphHomeomorphism(Graph : BANG_GRAPH) : BANG_GRAPH_HOMEOMORPHISM
= struct
  structure Graph = Graph
  
  datatype T = Match of {
    (* pattern and target graphs *)
    pat      : Graph.T,
    tgt      : Graph.T,
    (* vertex mapping from pat to tgt *)
    vmap     : VVInj.T,
    (* edge mapping from pat to tgt *)
    emap     : EEInj.T,
    (* !-box mapping from pat to tgt *)
    bmap     : BBInj.T
  }

  fun get_pat         (Match r) = #pat r
  fun get_tgt         (Match r) = #tgt r
  fun get_vmap        (Match r) = #vmap r
  fun get_emap        (Match r) = #emap r
  fun get_bmap        (Match r) = #bmap r

  exception bad_homeomorphism_exp of string
                                   * Graph.T
                                   * Graph.T
                                   * VVInj.T
                                   * EEInj.T
                                   * BBInj.T

  fun find_match_error pat tgt vmap emap bmap = let
    val vmapf = VVInj.get vmap
    val emapf = EEInj.get emap
    val bmapf = BBInj.get bmap

    fun edge_coherent e = let
      val ((dir,_),(s,t)) = Graph.get_edge_info pat e
      val e' = emapf e
      val exp_s' = vmapf s
      val exp_t' = vmapf t
      val ((dir',_),(real_s',real_t')) = Graph.get_edge_info tgt e'
    in
      case (dir,dir')
        of (Directed,Directed) =>
            (V.name_eq (exp_s',real_s')) andalso
            (V.name_eq (exp_t',real_t'))
         | (Undirected,Undirected) =>
            (V.NSet.eq (V.NSet.of_list [exp_s',exp_t'])
                       (V.NSet.of_list [real_s',real_t']))
         | (Directed,Undirected) => false
         | (Undirected,Directed) => false
    end

    fun bbox_coherent b = let
      val b' = bmapf b
      val children = Graph.get_bbox_children pat b
      val children' = Graph.get_bbox_children tgt b'
      val contents = Graph.get_vertices_in_bbox pat b
      val contents' = Graph.get_vertices_in_bbox tgt b'
    in
      B.NSet.eq children' (BBInj.img_of_set bmap children)
      andalso
      V.NSet.eq contents' (VVInj.img_of_set vmap contents)
    end

    (* FIXME: alpha conversion? *)
    fun vdata_eq v = let
      val v' = vmapf v
      val pat_vdata = Graph.get_vertex_data pat v
      val tgt_vdata = Graph.get_vertex_data tgt v'
    in
      Graph.vdata_eq (pat_vdata, tgt_vdata)
    end

    fun edata_eq e = let
      val e' = emapf e
      val pat_edata = Graph.get_edge_data pat e
      val tgt_edata = Graph.get_edge_data tgt e'
    in
      Graph.edata_eq (pat_edata, tgt_edata)
    end
  in
    if not (V.NSet.eq (VVInj.get_dom_set vmap) (Graph.get_vertices pat))
    then SOME "vmap is not total"
    else if not (E.NSet.eq (EEInj.get_dom_set emap) (Graph.get_edges pat))
    then SOME "emap is not total"
    else if not (B.NSet.eq (BBInj.get_dom_set bmap) (Graph.get_bboxes pat))
    then SOME "bmap is not total"
    else if not (V.NSet.eq (VVInj.get_cod_set vmap) (Graph.get_vertices tgt))
    then SOME "vmap is not onto"
    else if not (E.NSet.eq (EEInj.get_cod_set emap) (Graph.get_edges tgt))
    then SOME "emap is not onto"
    else if not (B.NSet.eq (BBInj.get_cod_set bmap) (Graph.get_bboxes tgt))
    then SOME "bmap is not onto"
    else if not (E.NSet.forall edge_coherent (EEInj.get_dom_set emap))
    then SOME "vmap and emap are not coherent"
    else if not (V.NSet.forall vdata_eq (VVInj.get_dom_set vmap))
    then SOME "vmap does not respect vertex data"
    else if not (E.NSet.forall edata_eq (EEInj.get_dom_set emap))
    then SOME "emap does not respect vertex data"
    else if not (B.NSet.forall bbox_coherent (BBInj.get_dom_set bmap))
    then SOME "bmap does not respect contents and children"
    else NONE
  end

  fun mk_opt pat tgt vmap emap bmap =
    case find_match_error pat tgt vmap emap bmap
      of SOME _ => NONE
       | NONE => SOME (Match {
                   pat      = pat,
                   tgt      = tgt,
                   vmap     = vmap,
                   emap     = emap,
                   bmap     = bmap
                 })

  fun mk pat tgt vmap emap bmap =
    case find_match_error pat tgt vmap emap bmap
      of SOME e => raise bad_homeomorphism_exp (e,pat,tgt,vmap,emap,bmap)
       | NONE => Match {
                   pat      = pat,
                   tgt      = tgt,
                   vmap     = vmap,
                   emap     = emap,
                   bmap     = bmap
                 }

  fun pretty ms =
    Pretty.chunks [
      Pretty.str "Match {",
      Pretty.block [
        Pretty.str "  ", (* indent *)
        Pretty.chunks [
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
          ],
          Pretty.block [
            Pretty.str "BMap: ",
            BBInj.pretty (get_bmap ms)
          ]
        ]
      ],
      Pretty.str "}"
    ]
  val print = Pretty.writeln o pretty

  structure Sharing : BANG_GRAPH_HOMEOMORPHISM_SHARING
  = struct
    structure Graph = Graph.Sharing
    type T = T
  end
end


functor BangGraphHomeomorphismSearcher(Graph : BANG_GRAPH)
  : BANG_GRAPH_HOMEOMORPHISM_SEARCH
= struct
  structure Graph = Graph
  structure Homeomorphism = BangGraphHomeomorphism(Graph)

  structure MatchState = OGraphMatchState(Graph)
  structure InnerMatchSearch = ProgressiveMatchSearch(MatchState)

  fun concrete_doomedf ms v = let
    val pat = MatchState.get_pat ms
    val tgt = MatchState.get_tgt ms
    val v' = VVInj.get (MatchState.get_vmap ms) v
    val vdata = Graph.get_vertex_data pat v
    val vdata' = Graph.get_vertex_data tgt v'
  in
    (* FIXME: alpha conversion? *)
    not (Graph.vdata_eq (vdata,vdata'))
    orelse
    B.NSet.cardinality (Graph.get_bboxes_containing_vertex pat v)
    <>
    B.NSet.cardinality (Graph.get_bboxes_containing_vertex tgt v')
  end

  fun homeo_from_ms (bmap,ms) =
    Homeomorphism.mk_opt (MatchState.get_pat ms)
                         (MatchState.get_tgt ms)
                         (MatchState.get_vmap ms)
                         (MatchState.get_emap ms)
                         bmap

  (* match the given set of bare wires against other bare wires *)
  fun match_bare_wires ms =
    if E.NSet.is_empty (MatchState.get_u_bare_wires ms) then Seq.single ms
    else let
      val (pat, tgt) = (MatchState.get_pat ms, MatchState.get_tgt ms)
      val (vmap, emap) = (MatchState.get_vmap ms, MatchState.get_emap ms)
      val tgt_verts = MatchState.get_tgt_verts ms
      (* for a normalised graph, this will always be the source end *)
      val bw = (the o E.NSet.get_min) (MatchState.get_u_bare_wires ms)
      val ((bw_dir, bw_data), (bw_s, bw_t)) = Graph.get_edge_info pat bw
      val (bw_sdata, bw_tdata) = (Graph.get_vertex_data pat bw_s, Graph.get_vertex_data pat bw_t)
      
      fun match_bw e = let
        val ((dir,data),(s,t)) = Graph.get_edge_info tgt e
      in
        if not (EEInj.cod_contains emap e)
           andalso dir = bw_dir
           andalso Graph.edata_eq (data,bw_data)
           andalso V.NSet.contains tgt_verts s
           andalso V.NSet.contains tgt_verts t
           andalso Graph.is_boundary tgt s
           andalso Graph.is_boundary tgt t
        then
          let
            val tgt_sdata = Graph.get_vertex_data tgt s
            val tgt_tdata = Graph.get_vertex_data tgt t
        
            (* match data for edge, source, and target *)
            val fmatch_d = Option.composePartial
                              (Graph.match_edata (bw_data, data),
                               Option.composePartial
                                  (Graph.match_vdata (bw_sdata, tgt_sdata),
                                   Graph.match_vdata (bw_tdata, tgt_tdata)))
            fun add_bw new_subst = let
              val ms = ms |> MatchState.set_match_psubst new_subst
                          |> MatchState.update_tgt_verts (V.NSet.delete s)
                          |> MatchState.update_tgt_verts (V.NSet.delete t)
                          |> MatchState.update_u_bare_wires (E.NSet.delete bw)
            in (fn (es,et) =>
                 ms |> MatchState.update_vmap (VVInj.add (bw_s, s))
                    |> MatchState.update_vmap (VVInj.add (bw_t, t))
                    |> MatchState.update_emap (EEInj.add (bw, e))
               )
            end
          in
            case fmatch_d (MatchState.get_match_psubst ms)
              of SOME new_subst =>
                   let val add_bw' = add_bw new_subst in
                      case bw_dir
                        of Directed => match_bare_wires (add_bw' (s,t))
                         | Undirected =>
                             Seq.append (match_bare_wires (add_bw' (s,t)))
                                        (match_bare_wires (add_bw' (t,s)))
                   end
               | NONE => Seq.empty
          end
        else Seq.empty
      end
    in
      Seq.maps match_bw (Seq.of_list (E.NSet.list_of (Graph.get_edges tgt)))
    end

  fun match_bboxes_recursive (bmap,ms) pat_bboxes tgt_bboxes =
    case B.NSet.get_local_bot pat_bboxes
      of NONE => if B.NSet.is_empty tgt_bboxes
                 then Seq.single (bmap,ms)
                 else Seq.empty
       | SOME b =>
          let
            val pat = MatchState.get_pat ms
            val tgt = MatchState.get_tgt ms
            val vmap = MatchState.get_vmap ms
            val b_children = Graph.get_bbox_children pat b
            val no_b_children = B.NSet.cardinality b_children
            val b_contents_img = VVInj.img_of_set vmap
                  (Graph.get_vertices_in_bbox pat b)
            val get_tgt_children = Graph.get_bbox_children tgt
            val get_tgt_contents = Graph.get_vertices_in_bbox tgt
            val pat_bboxes' = B.NSet.delete b pat_bboxes
            fun try_match_bbox b' = let
              val b'_children = get_tgt_children b'
              val no_b'_children = B.NSet.cardinality b'_children
              val tgt_bboxes' = B.NSet.delete b' tgt_bboxes
              val bmap' = BBInj.add (b,b') bmap
            in
              if no_b'_children <> no_b_children
              then Seq.empty
              else if not (V.NSet.eq b_contents_img (get_tgt_contents b'))
              then Seq.empty
              else
                (* recurse down into children of b,b', then move onto next
                 * sibling pattern !-box *)
                Seq.maps (fn (bmap',ms') =>
                    match_bboxes_recursive (bmap',ms') pat_bboxes' tgt_bboxes')
                    (match_bboxes_recursive (BBInj.add (b,b') bmap,ms)
                                            b_children
                                            b'_children)
            end
          in
            Seq.maps try_match_bbox (Seq.of_list (B.NSet.list_of tgt_bboxes))
          end

  fun match_bboxes ms = let
    val pat = MatchState.get_pat ms
    val tgt = MatchState.get_tgt ms
    fun get_toplevel_bboxes gr =
          B.NSet.filter (not o (Graph.bbox_has_parents gr))
                        (Graph.get_bboxes gr)
  in
    match_bboxes_recursive (BBInj.empty,ms)
      (get_toplevel_bboxes pat)
      (get_toplevel_bboxes tgt)
  end

  fun find_homeomorphisms g1 g2 = let
    val pat = Graph.normalise g1
    val tgt = Graph.normalise g2
  in
    if V.NSet.cardinality (Graph.get_vertices pat) =
         V.NSet.cardinality (Graph.get_vertices tgt) andalso
       E.NSet.cardinality (Graph.get_edges pat) =
         E.NSet.cardinality (Graph.get_edges tgt) andalso
       B.NSet.cardinality (Graph.get_bboxes pat) =
         B.NSet.cardinality (Graph.get_bboxes tgt)
    then
      MatchState.init_and_schedule_all pat tgt
        |> InnerMatchSearch.match_pending concrete_doomedf
        |> Seq.maps match_bare_wires
        |> Seq.filter MatchState.is_total
        |> Seq.maps match_bboxes
        |> Seq.map_filter homeo_from_ms
    else
      Seq.empty
  end

  val is_homeomorphic = is_some o Seq.pull oo find_homeomorphisms

  structure Sharing =
  struct
    structure Graph = Graph.Sharing
    structure Homeomorphism = Homeomorphism.Sharing
  end
end
