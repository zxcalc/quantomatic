(**
 * An implementation of double-pushout rewriting for string graphs
 *
 * Delegates to a MATCH_SEARCH implementation to find matches, then cuts
 * out the matched subgraph (except for the boundary) and pastes in the
 * RHS of the rule.
 *)

signature REWRITER =
sig
  structure Log : LOG

  (* NB: we don't require the Rule.Graph and Graph to be the same *)
  structure Rule : RULE;
  structure Graph : OGRAPH;
  type match;

  (**
   * Find all matches of a rule's LHS in a graph
   *
   * As well as the matches, a prepared version of the rule is returned.
   * This is the version that must be used for rewriting.
   *)
  val find_matches : Rule.T
                  -> Graph.T
                  -> (Rule.T * match Seq.seq);

  (**
   * Find all matches of a rule's LHS in a subgraph of a graph
   *
   * As well as the matches, a prepared version of the rule is returned.
   * This is the version that must be used for rewriting.
   *
   * The subgraph is defined by the vertex list given in the third argument.
   *)
  val find_matches_in_subgraph : Rule.T
                              -> Graph.T
                              -> V.NSet.T
                              -> (Rule.T * match Seq.seq);

  (**
   * Find all rewrites for a rule in a graph
   *)
  val find_rewrites : Rule.T
                   -> Graph.T
                   -> (Rule.T * Graph.T) Seq.seq;

  (**
   * Find all rewrites for a rule in a subgraph of a graph
   *
   * The subgraph is defined by the vertex list given in the third argument.
   *)
  val find_rewrites_in_subgraph : Rule.T
                               -> Graph.T
                               -> V.NSet.T
                               -> (Rule.T * Graph.T) Seq.seq;

  val rewrite_at : Rule.T
                -> match
                -> (Rule.T * Graph.T);

  (* a couple of helpful fns because match is opaque *)
  val print_match : match -> unit;
  val pretty_match : match -> Pretty.T;
end;

(**
 * Rewriter for !-graphs.
 *
 * Note that, although the rewriter signature does not require the rule graph
 * type to be the actual graph type, our use of BG_MATCH_SEARCH does force them
 * to agree.
 *)
functor BangGraphRewriter(
  structure Rule : BANG_GRAPH_RULE
  structure Matcher : BG_MATCH_SEARCH
  structure BBoxOperableGraph : REPLAYABLE_BBOX_OPERABLE_GRAPH
  sharing Matcher.Sharing.G = Rule.Sharing.Graph
  sharing Matcher.Match.BBoxOperableGraph.Sharing = BBoxOperableGraph.Sharing
) : REWRITER =
struct
  structure Log : LOG = Log(val init_level = 0)

  structure Rule = Rule;
  structure Graph = Rule.BangGraph;
  type match = Matcher.Match.T;

  structure G = Graph;
  structure Match = Matcher.Match;
  structure BBOp = BBoxOperableGraph;

  fun log_p pretty_f lvl name =
    Log.logf lvl (fn g => Pretty.string_of
        (Pretty.chunks [Pretty.str (name^":"),(pretty_f g)]))

  val log_graph = log_p G.pretty
  val log_vrnm = log_p V.Rnm.pretty
  val log_ernm = log_p E.Rnm.pretty

  fun prepare_rule rule target =
      let
        (* (Boundary) vertices and variables must be renamed
         * away from those in g, otherwise the rewriting will
         * fail (a quirk of the implementations of LinRatExpr
         * and V.CRnm) *)
        val g_norm = G.normalise target
        val rnms = (X.Rnm.mk_from_avoids (G.get_xnames g_norm),
                    V.Rnm.mk_from_avoids (G.get_vnames g_norm),
                    E.Rnm.empty, B.Rnm.empty)
        val (_,rule) = Rule.rename4 rnms rule;
        val lhs = G.normalise (Rule.get_lhs rule);
        val rhs = G.normalise (Rule.get_rhs rule);
        val avoid = V.NSet.union_merge (G.get_vnames g_norm) (G.get_vnames rhs);
        val track = Rule.get_boundary_vertices rule;
      in
        (Rule.mk (lhs,rhs),
         BBOp.init lhs avoid track,
         g_norm)
      end;

  fun find_matches r g =
      let
        val (rule,oper,graph) = prepare_rule r g;
      in
        (rule,
         Matcher.match_normalised oper graph)
      end;

  fun find_matches_in_subgraph r g vset =
      let
        val (rule,oper,graph) = prepare_rule r g;
      in
        (rule,
         Matcher.match_subgraph_normalised oper graph vset)
      end;

  fun sg_rewrite_at m rhs =
    let
      val g = Match.get_tgt m
      val pat_oper = Match.get_pat m
      val pat = BBoxOperableGraph.get_current_graph pat_oper
      val subst_data = G.apply_data_subst (Match.get_subst m)
      val shared_boundary = G.get_boundary pat;
      val vmap = Match.get_vmap m
      val vmapf = VInjEndo.domf vmap
      val matched = VInjEndo.get_codset vmap;
          (* remove the matched part *)
      val interior = V.NSet.fold (V.NSet.delete o vmapf)
                                 shared_boundary
                                 matched
      val interior_edges = EInjEndo.get_codset (Match.get_emap m);
      (* h is g with LHS cut out; note that we need to delete edges explicitly,
         in case of bare wire matches *)
      val h = g |> E.NSet.fold G.delete_edge interior_edges
                |> V.NSet.fold G.delete_vertex interior
                   (* apply the data substitution to what remains *)
                |> subst_data;
      val _ = log_graph 4 "OGraphSubst.rewrite: pattern graph" pat
      val _ = log_graph 3 "OGraphSubst.rewrite: G" g
      val _ = log_graph 3 "OGraphSubst.rewrite: G-pat" h

      (* Rename the RHS verts for merging.
       *
       * The aim is to have the boundary verts match where the
       * LHS was cut out of g, and make all the other verts have
       * names distinct from any verts in h.  Then
       * G.merge_by_vertices will do the right thing.
       *
       * We actually make it avoid anything in g, so clients can
       * tell what was replaced.
       *
       * It also does the data substitution.
       *)
      val g_pat_verts = V.NSet.union_merge (G.get_vnames g) (G.get_vnames pat);
      val e_rnm = E.Rnm.mk_from_avoids (G.get_enames g);
      (* first v rename makes all names except boundary verts fresh *)
      (* this avoid clashes when doing the second rename *)
      val v_rnm1 = V.Rnm.mk shared_boundary g_pat_verts V.NTab.empty;
      (* second v rename sets the boundary verts to match the
       * corresponding verts in h. *)
      val v_rnm2 = V.Rnm.mk V.NSet.empty
                            g_pat_verts
                            (VInjEndo.get_domtab vmap);
      val _ = log_vrnm 4 "OGraphSubst.rewrite: v_rnm1" v_rnm1
      val _ = log_vrnm 4 "OGraphSubst.rewrite: v_rnm2" v_rnm2
      val _ = log_ernm 4 "OGraphSubst.rewrite: e_rnm" e_rnm

      val prepare = snd o G.VertexCRnm.rename v_rnm2
                  o snd o G.VertexCRnm.rename v_rnm1
                  o snd o G.EdgeCRnm.rename e_rnm
                  o subst_data

      val _ = log_graph 4 "OGraphSubst.rewrite: RHS" rhs
      val rhs' = prepare rhs
      val _ = log_graph 4 "OGraphSubst.rewrite: RHS (prepared)" rhs'
      val pat' = prepare pat
      val _ = log_graph 4 "OGraphSubst.rewrite: LHS (prepared)" pat'

      in
        (Rule.mk (G.minimise pat',G.minimise rhs'),
         G.merge_by_vertices rhs' h)
      end;

  fun rewrite_at rule m = let
    val rhs = Rule.get_rhs rule
    val oper = Match.get_pat m
    val sg_rhs = BBOp.replay_bbox_ops oper rhs
    val (final_rule,g') = sg_rewrite_at m sg_rhs
  in
    (final_rule, G.minimise g')
  end

  fun find_rewrites r g = let
    val (r',mseq) = find_matches r g
  in
    Seq.map (rewrite_at r') mseq
  end

  fun find_rewrites_in_subgraph r g vset = let
    val (r',mseq) = find_matches_in_subgraph r g vset
  in
    Seq.map (rewrite_at r') mseq
  end

  val print_match = Matcher.Match.print;
  val pretty_match = Matcher.Match.pretty;
end;

