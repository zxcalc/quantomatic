(* The type-sharing struct for BANG_GRAPH_MATCH *)
signature BANG_GRAPH_MATCH_SHARING
= sig
  type T;
  structure G : OGRAPH_SHARING;
  structure BBoxOperableGraph : BBOX_OPERABLE_GRAPH_SHARING;
  sharing G = BBoxOperableGraph.G;
end;

(* A match from a !-graph to a !-graph *)
signature BANG_GRAPH_MATCH
= sig
  type T;
  structure G : BANG_GRAPH;
  structure BBoxOperableGraph : BBOX_OPERABLE_GRAPH;
  
  (* sharing via sharing structure *)
  structure Sharing : BANG_GRAPH_MATCH_SHARING
    sharing type Sharing.T = T
    sharing Sharing.G = G.SharingOGraph;
    sharing Sharing.BBoxOperableGraph = BBoxOperableGraph.Sharing;
  
  (* the graph to match from *)
  val get_pat   : T -> BBoxOperableGraph.T;
  (* the graph to match onto *)
  val get_tgt   : T -> G.T;
  (* the vertex map (from pat verts to tgt verts *)
  val get_vmap  : T -> VInjEndo.T;
  (* the edge map (from pat edges to tgt edges *)
  val get_emap  : T -> EInjEndo.T;
  (* the bbox map (from pat !-boxes to tgt !-boxes *)
  val get_bmap  : T -> BInjEndo.T;
  (* the vertex/edge data substitution for the matching *)
  val get_subst : T -> G.Param.subst;
  (* any prematurely-killed !-boxes (these may not exist in the initial graph *)
  val get_wild_bboxes  : T -> B.NSet.T;

  exception bad_match_exp of string
                           * BBoxOperableGraph.T
                           * G.T
                           * VInjEndo.T
                           * EInjEndo.T
                           * BInjEndo.T
                           * G.Param.subst
                           * B.NSet.T;

  (* create a match; checks for correctness *)
  (* raises bad_match_exp if not a valid match *)
  val mk : BBoxOperableGraph.T
        -> G.T
        -> VInjEndo.T
        -> EInjEndo.T
        -> BInjEndo.T
        -> G.Param.subst
        -> B.NSet.T
        -> T;

  (* create a match; checks for correctness *)
  (* returns NONE if not a valid match *)
  val mk_opt : BBoxOperableGraph.T
            -> G.T
            -> VInjEndo.T
            -> EInjEndo.T
            -> BInjEndo.T
            -> G.Param.subst
            -> B.NSet.T
            -> T option;

  val print : T -> unit;
  val pretty : T -> Pretty.T;
end;

functor BangGraphMatch(BBoxOperableGraph : BBOX_OPERABLE_GRAPH) : BANG_GRAPH_MATCH
= struct
  structure G = BBoxOperableGraph.G
  structure BBoxOperableGraph = BBoxOperableGraph
  
  datatype T = Match of {
    (* pattern and target graphs *)
    pat      : BBoxOperableGraph.T,
    tgt      : G.T,
    (* vertex mapping from pat to tgt *)
    vmap     : VInjEndo.T,
    (* edge mapping from pat to tgt *)
    emap     : EInjEndo.T,
    bmap     : BInjEndo.T,
    subst    : G.Param.subst,
    wild_bbs : B.NSet.T
  };

  fun get_pat         (Match r) = #pat r;
  fun get_tgt         (Match r) = #tgt r;
  fun get_vmap        (Match r) = #vmap r;
  fun get_emap        (Match r) = #emap r;
  fun get_bmap        (Match r) = #bmap r;
  fun get_subst       (Match r) = #subst r;
  fun get_wild_bboxes (Match r) = #wild_bbs r;

  exception bad_match_exp of string
                           * BBoxOperableGraph.T
                           * G.T
                           * VInjEndo.T
                           * EInjEndo.T
                           * BInjEndo.T
                           * G.Param.subst
                           * B.NSet.T;

  fun find_match_error pat tgt vmap emap bmap subst = let
    val vmapf = VInjEndo.domf vmap
    val emapf = EInjEndo.domf emap
    val pat_g = BBoxOperableGraph.get_current_graph pat

    fun edge_coherent e = let
      val ((dir,_),(s,t)) = G.get_edge pat_g e
      val e' = emapf e
      val exp_s' = vmapf s
      val exp_t' = vmapf t
      val ((dir',_),(real_s',real_t')) = G.get_edge tgt e'
    in
      case (dir,dir')
        of (G.Directed,G.Directed) =>
            (V.name_eq (exp_s',real_s')) andalso
            (V.name_eq (exp_t',real_t'))
         | (G.UnDirected,G.UnDirected) =>
            (V.NSet.eq (V.NSet.of_list [exp_s',exp_t'])
                       (V.NSet.of_list [real_s',real_t']))
         | (G.Directed,G.UnDirected) => false
         | (G.UnDirected,G.Directed) => false
    end;

    fun vdata_matches v = let
      val v' = vmapf v
      val pat_vdata = G.get_vertex_data pat_g v
      val tgt_vdata = G.get_vertex_data tgt v'
    in
      G.Param.VData.data_eq (tgt_vdata,
                             G.Param.subst_in_vertex subst pat_vdata)
    end;

    fun edata_matches e = let
      val e' = emapf e
      val pat_edata = G.get_edge_data pat_g e
      val tgt_edata = G.get_edge_data tgt e'
    in
      G.Param.EData.data_eq (tgt_edata,
                             G.Param.subst_in_edge subst pat_edata)
    end;
  in
    if not (V.NSet.eq (G.get_vnames pat_g) (VInjEndo.get_domset vmap))
    then SOME "vmap is not total"
    else if not (E.NSet.eq (G.get_enames pat_g) (EInjEndo.get_domset emap))
    then SOME "emap is not total"
    else if not (V.NSet.sub_set (VInjEndo.get_codset vmap) (G.get_vnames tgt))
    then SOME "vmap image is not in the target graph"
    else if not (E.NSet.sub_set (EInjEndo.get_codset emap) (G.get_enames tgt))
    then SOME "emap image is not in the target graph"
    else if not (E.NSet.forall edge_coherent (EInjEndo.get_domset emap))
    then SOME "vmap and emap are not coherent"
    else if not (V.NSet.forall vdata_matches (VInjEndo.get_domset vmap))
    then SOME "vmap and subst do not give correct data"
    else if not (E.NSet.forall edata_matches (EInjEndo.get_domset emap))
    then SOME "emap and subst do not give correct data"
    else NONE
  end;

  fun mk_opt pat tgt vmap emap bmap subst wild =
    case find_match_error pat tgt vmap emap bmap subst
      of SOME _ => NONE
       | NONE => SOME (Match {
                   pat      = pat,
                   tgt      = tgt,
                   vmap     = vmap,
                   emap     = emap,
                   bmap     = bmap,
                   subst    = subst,
                   wild_bbs = wild
                 });

  fun mk pat tgt vmap emap bmap subst wild =
    case find_match_error pat tgt vmap emap bmap subst
      of SOME e => raise bad_match_exp (e,pat,tgt,vmap,emap,bmap,subst,wild)
       | NONE => Match {
                   pat      = pat,
                   tgt      = tgt,
                   vmap     = vmap,
                   emap     = emap,
                   bmap     = bmap,
                   subst    = subst,
                   wild_bbs = wild
                 };

  fun pretty ms = let
    fun pretty_bset prefix bs =
      if B.NSet.is_empty bs then []
      else [Pretty.block [
              Pretty.str (prefix^": "),
              B.NSet.pretty bs
           ]]
  in
    Pretty.chunks [
      Pretty.str "Match {",
      Pretty.block [
        Pretty.str "  ", (* indent *)
        Pretty.chunks [
          Pretty.block [
            Pretty.str "Pattern: ",
            BBoxOperableGraph.pretty (get_pat ms)
          ],
          Pretty.block [
            Pretty.str "Target: ",
            G.pretty (get_tgt ms)
          ],
          Pretty.block [
            Pretty.str "VMap: ",
            VInjEndo.pretty (get_vmap ms)
          ],
          Pretty.block [
            Pretty.str "EMap: ",
            EInjEndo.pretty (get_emap ms)
          ],
          Pretty.block [
            Pretty.str "BMap: ",
            BInjEndo.pretty (get_bmap ms)
          ],
          Pretty.block [
            Pretty.str "Wild !-boxes: ",
            B.NSet.pretty (get_wild_bboxes ms)
          ]
        ]
      ],
      Pretty.str "}"
    ]
  end;
  val print = Pretty.writeln o pretty;

  structure Sharing : BANG_GRAPH_MATCH_SHARING
  = struct
    structure G = G.SharingOGraph
    structure BBoxOperableGraph = BBoxOperableGraph.Sharing
    type T = T;
  end;
end;




