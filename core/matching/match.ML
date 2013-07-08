(* The type-sharing struct for OGRAPH_MATCH *)
signature OGRAPH_MATCH_SHARING
= sig
  type T
  structure Graph : OGRAPH_SHARING
end;

(* A match from a string graph to a string graph *)
signature OGRAPH_MATCH
= sig
  type T;
  structure Graph : OGRAPH;
  
  (* sharing via sharing structure *)
  structure Sharing : OGRAPH_MATCH_SHARING
    sharing type Sharing.T = T
    sharing Sharing.Graph = Graph.Sharing
  
  (* the pattern graph (domain) *)
  val get_pat   : T -> Graph.T;
  (* the target graph (codomain) *)
  val get_tgt   : T -> Graph.T;
  (* the vertex map (from pat verts to tgt verts *)
  val get_vmap  : T -> VVInj.T;
  (* the edge map (from pat edges to tgt edges *)
  val get_emap  : T -> EEInj.T;
  (* the vertex/edge data substitution for the matching *)
  val get_subst : T -> Graph.subst;

  exception bad_match_exp of string
                           * Graph.T
                           * Graph.T
                           * VVInj.T
                           * EEInj.T
                           * Graph.subst;

  (* create a match; checks for correctness *)
  (* raises bad_match_exp if not a valid match *)
  val mk : Graph.T -> Graph.T -> VVInj.T -> EEInj.T -> Graph.subst -> T;

  (* create a match; checks for correctness *)
  (* returns NONE if not a valid match *)
  val mk_opt : Graph.T -> Graph.T -> VVInj.T -> EEInj.T -> Graph.subst
            -> T option;

  val print : T -> unit;
  val pretty : T -> Pretty.T;
end;

functor OGraphMatch(Graph : OGRAPH) : OGRAPH_MATCH
= struct
  structure Graph = Graph
  
  datatype T = Match of {
    (* pattern and target graphs *)
    pat   : Graph.T,
    tgt   : Graph.T,
    (* vertex mapping from pat to tgt *)
    vmap  : VVInj.T,
    (* edge mapping from pat to tgt *)
    emap  : EEInj.T,
    subst : Graph.subst
  };

  fun get_pat   (Match r) = #pat r;
  fun get_tgt   (Match r) = #tgt r;
  fun get_vmap  (Match r) = #vmap r;
  fun get_emap  (Match r) = #emap r;
  fun get_subst (Match r) = #subst r;

  exception bad_match_exp of string
                           * Graph.T
                           * Graph.T
                           * VVInj.T
                           * EEInj.T
                           * Graph.subst;

  fun find_match_error pat tgt vmap emap subst = let
    val vmapf = VVInj.get vmap
    val emapf = EEInj.get emap

    fun edge_coherent e = let
      val dir = Graph.get_edge_dir pat e
      val (s,t) = (Graph.get_edge_source pat e, Graph.get_edge_target pat e)
      val e' = emapf e
      val exp_s' = vmapf s
      val exp_t' = vmapf t
      val dir' = Graph.get_edge_dir tgt e'
      val (real_s',real_t') = (Graph.get_edge_source tgt e', Graph.get_edge_target tgt e')
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
    end;

    fun vdata_matches v = let
      val v' = vmapf v
      val pat_vdata = Graph.get_vertex_data pat v
      val tgt_vdata = Graph.get_vertex_data tgt v'
    in
      Graph.vdata_eq (tgt_vdata, snd (Graph.subst_in_vdata subst pat_vdata))
    end;

    fun edata_matches e = let
      val e' = emapf e
      val pat_edata = Graph.get_edge_data pat e
      val tgt_edata = Graph.get_edge_data tgt e'
    in
      Graph.edata_eq (tgt_edata, snd (Graph.subst_in_edata subst pat_edata))
    end;
  in
    if not (V.NSet.eq (Graph.get_vertices pat) (VVInj.get_dom_set vmap))
    then SOME "vmap is not total"
    else if not (E.NSet.eq (Graph.get_edges pat) (EEInj.get_dom_set emap))
    then SOME "emap is not total"
    else if not (V.NSet.sub_set (VVInj.get_cod_set vmap) (Graph.get_vertices tgt))
    then SOME "vmap image is not in the target graph"
    else if not (E.NSet.sub_set (EEInj.get_cod_set emap) (Graph.get_edges tgt))
    then SOME "emap image is not in the target graph"
    else if not (E.NSet.forall edge_coherent (EEInj.get_dom_set emap))
    then SOME "vmap and emap are not coherent"
    else if not (V.NSet.forall vdata_matches (VVInj.get_dom_set vmap))
    then SOME "vmap and subst do not give correct data"
    else if not (E.NSet.forall edata_matches (EEInj.get_dom_set emap))
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
          ]
        ]
      ],
      Pretty.str "}"
    ];
  val print = Pretty.writeln o pretty;

  structure Sharing : OGRAPH_MATCH_SHARING 
  = struct
    structure Graph = Graph.Sharing
    type T = T;
  end;
end
