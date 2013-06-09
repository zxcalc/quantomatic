(* The type-sharing struct for OGRAPH_MATCH *)
signature OGRAPH_MATCH_SHARING
= sig
  type T
  structure G : OGRAPH_SHARING
end;

(* A match from a string graph to a string graph *)
signature OGRAPH_MATCH
= sig
  type T;
  structure G : OGRAPH;
  
  (* sharing via sharing structure *)
  structure Sharing : OGRAPH_MATCH_SHARING
    sharing type Sharing.T = T
    sharing Sharing.G = G.SharingOGraph;
  
  (* the pattern graph (domain) *)
  val get_pat   : T -> G.T;
  (* the target graph (codomain) *)
  val get_tgt   : T -> G.T;
  (* the vertex map (from pat verts to tgt verts *)
  val get_vmap  : T -> VInjEndo.T;
  (* the edge map (from pat edges to tgt edges *)
  val get_emap  : T -> EInjEndo.T;
  (* the vertex/edge data substitution for the matching *)
  val get_subst : T -> G.Param.subst;

  exception bad_match_exp of string
                           * G.T
                           * G.T
                           * VInjEndo.T
                           * EInjEndo.T
                           * G.Param.subst;

  (* create a match; checks for correctness *)
  (* raises bad_match_exp if not a valid match *)
  val mk : G.T -> G.T -> VInjEndo.T -> EInjEndo.T -> G.Param.subst -> T;

  (* create a match; checks for correctness *)
  (* returns NONE if not a valid match *)
  val mk_opt : G.T -> G.T -> VInjEndo.T -> EInjEndo.T -> G.Param.subst
            -> T option;

  val print : T -> unit;
  val pretty : T -> Pretty.T;
end;

functor OGraphMatch(G : OGRAPH) : OGRAPH_MATCH
= struct
  structure G = G
  
  datatype T = Match of {
    (* pattern and target graphs *)
    pat   : G.T,
    tgt   : G.T,
    (* vertex mapping from pat to tgt *)
    vmap  : VInjEndo.T,
    (* edge mapping from pat to tgt *)
    emap  : EInjEndo.T,
    subst : G.Param.subst
  };

  fun get_pat   (Match r) = #pat r;
  fun get_tgt   (Match r) = #tgt r;
  fun get_vmap  (Match r) = #vmap r;
  fun get_emap  (Match r) = #emap r;
  fun get_subst (Match r) = #subst r;

  exception bad_match_exp of string
                           * G.T
                           * G.T
                           * VInjEndo.T
                           * EInjEndo.T
                           * G.Param.subst;

  fun find_match_error pat tgt vmap emap subst = let
    val vmapf = VInjEndo.domf vmap
    val emapf = EInjEndo.domf emap

    fun edge_coherent e = let
      val ((dir,_),(s,t)) = G.get_edge pat e
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
      val pat_vdata = G.get_vertex_data pat v
      val tgt_vdata = G.get_vertex_data tgt v'
    in
      G.Param.VData.data_eq (tgt_vdata,
                             G.Param.subst_in_vertex subst pat_vdata)
    end;

    fun edata_matches e = let
      val e' = emapf e
      val pat_edata = G.get_edge_data pat e
      val tgt_edata = G.get_edge_data tgt e'
    in
      G.Param.EData.data_eq (tgt_edata,
                             G.Param.subst_in_edge subst pat_edata)
    end;
  in
    if not (V.NSet.eq (G.get_vnames pat) (VInjEndo.get_domset vmap))
    then SOME "vmap is not total"
    else if not (E.NSet.eq (G.get_enames pat) (EInjEndo.get_domset emap))
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

  fun mk_opt pat tgt vmap emap subst =
    case find_match_error pat tgt vmap emap subst
      of SOME _ => NONE
       | NONE => SOME (Match {
                   pat   = pat,
                   tgt   = tgt,
                   vmap  = vmap,
                   emap  = emap,
                   subst = subst
                 });

  fun mk pat tgt vmap emap subst =
    case find_match_error pat tgt vmap emap subst
      of SOME e => raise bad_match_exp (e,pat,tgt,vmap,emap,subst)
       | NONE => Match {
                   pat   = pat,
                   tgt   = tgt,
                   vmap  = vmap,
                   emap  = emap,
                   subst = subst
                 };

  fun pretty ms =
    Pretty.chunks [
      Pretty.str "Match {",
      Pretty.block [
        Pretty.str "  ", (* indent *)
        Pretty.chunks [
          Pretty.block [
            Pretty.str "Pattern: ",
            G.pretty (get_pat ms)
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
          ]
        ]
      ],
      Pretty.str "}"
    ];
  val print = Pretty.writeln o pretty;

  structure Sharing : OGRAPH_MATCH_SHARING 
  = struct
    structure G = G.SharingOGraph
    type T = T;
  end;
end;



