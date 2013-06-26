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
  structure Rule : OGRAPH_RULE;
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
  sharing Matcher.Sharing.Graph = Rule.Sharing.Graph
  sharing Matcher.Match.BBoxOperableGraph.Sharing = BBoxOperableGraph.Sharing
) : REWRITER =
struct
  structure Log : LOG = Log(val init_level = 0)

  structure Rule = Rule;
  structure Graph = Rule.Graph;
  type match = Matcher.Match.T;
  structure Match = Matcher.Match;
  structure BBOp = BBoxOperableGraph;

  fun log_p pretty_f lvl name =
    Log.logf lvl (fn g => Pretty.string_of
        (Pretty.chunks [Pretty.str (name^":"),(pretty_f g)]))

  val log_graph = log_p Graph.pretty
  val log_vrnm = log_p V.Rnm.pretty
  val log_ernm = log_p E.Rnm.pretty

  fun prepare_rule rule target =
      let
        (* (Boundary) vertices and variables must be renamed
         * away from those in g, otherwise the rewriting will
         * fail (a quirk of the implementations of LinRatExpr
         * and V.CRnm) *)
        val g_norm = Graph.normalise target
        val rnms = (
          VSub.empty |> VSub.avoid_set_in_cod (Graph.get_vertices g_norm)
                     |> VSub.extend_fresh (Rule.get_vertices rule),
          ESub.empty, BSub.empty)
        val (_,rule) = Rule.rename_bang_graph_rule rnms rule;
        val lhs = Graph.normalise (Rule.get_lhs rule);
        val rhs = Graph.normalise (Rule.get_rhs rule);
        val avoid = V.NSet.union_merge (Graph.get_vertices g_norm) (Graph.get_vertices rhs);
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

  fun sg_rewrite_at m rhs = let
    val g = Match.get_tgt m
    val pat = BBoxOperableGraph.get_current_graph (Match.get_pat m)
    val (vmap,emap) = (Match.get_vmap m, Match.get_emap m)
    val rhs' = rhs |> Graph.apply_data_subst (Match.get_subst m)

    val shared_boundary = Graph.get_boundary pat
    val lhs_interior = V.NSet.subtract (Graph.get_vertices pat) shared_boundary

    val shared_boundary_img = VVInj.img_of_set vmap shared_boundary

    val h = g |> E.NSet.fold (Graph.delete_edge o EEInj.get emap) (Graph.get_edges pat)
              |> V.NSet.fold (Graph.delete_vertex o VVInj.get vmap) lhs_interior

    val _ = log_graph 4 "OGraphSubst.rewrite: pattern graph" pat
    val _ = log_graph 3 "OGraphSubst.rewrite: G" g
    val _ = log_graph 3 "OGraphSubst.rewrite: G-pat" h
    val _ = log_graph 4 "OGraphSubst.rewrite: RHS" rhs'

    val rule = Rule.mk (Graph.minimise pat, Graph.minimise rhs')
    val vrn = VSub.empty
      |> V.NSet.fold (fn b => VSub.add (b, VVInj.get vmap b)) shared_boundary
      |> VSub.avoid_set_in_cod (V.NSet.subtract (Graph.get_vertices h) shared_boundary_img)
      |> VSub.extend_fresh (Rule.get_vertices rule)
    val ern = ESub.mk_from_avoids (Graph.get_edges h) |> ESub.extend_fresh (Rule.get_edges rule)
    val (_,rule') = rule |> Rule.rename_bang_graph_rule (vrn,ern,BSub.empty)

    val _ = log_graph 4 "OGraphSubst.rewrite: RHS (prepared)" (Rule.get_rhs rule')
    val _ = log_graph 4 "OGraphSubst.rewrite: LHS (prepared)" (Rule.get_lhs rule')

  in
    (rule', Graph.merge_by_vertices (Rule.get_rhs rule') h)
  end;

  fun rewrite_at rule m = let
    val rhs = Rule.get_rhs rule
    val oper = Match.get_pat m
    val sg_rhs = BBOp.replay_bbox_ops oper rhs
    val (final_rule,g') = sg_rewrite_at m sg_rhs
  in
    (final_rule, Graph.minimise g')
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

