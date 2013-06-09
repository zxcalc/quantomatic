signature BBOX_OPERABLE_GRAPH_SHARING =
sig
  type T;
  structure G : OGRAPH_SHARING;
end;

(* Used by !-graph matching to apply !-box operations to the pattern.
 * This allows the sequence of !-box operations to be tracked, for example to
 * record it or to apply it to a rule or another graph.
 *)
signature BBOX_OPERABLE_GRAPH =
sig
  type T;
  structure G : BANG_GRAPH;
  structure Sharing : BBOX_OPERABLE_GRAPH_SHARING
    sharing type Sharing.T = T
    sharing Sharing.G = G.SharingOGraph

  val normalise : T -> T;
  val expand_bbox : B.name -> T -> T;
  val kill_bbox : B.name -> T -> T;

  (* the graph so far, with all the operations applied *)
  val get_current_graph : T -> G.T;

  val empty : T;

  val print : T -> unit;
  val pretty : T -> Pretty.T;
end;

(* This is an operator that records the !-box operations that are performed
 * and allows them to be replayed on another graph.
 *
 * It is possible to specify a set of vertex names to avoid when doing
 * expansions, as well as a set of vertex names to track, so that the same
 * renamings will be performed when replaying the operations.  If the graph
 * passed to init shares vertex names with another graph that the operations
 * will be replayed on, you will usually want to track those shared names.
 *)
signature REPLAYABLE_BBOX_OPERABLE_GRAPH =
sig
  include BBOX_OPERABLE_GRAPH;

  (* First arg: the original graph
   * Second arg: a set of vertex names to avoid when copying bboxes
   * Third arg: a set of vertex names to keep track of when copying bboxes;
   *            typically the boundary of the graph
   *)
  val init : G.T -> V.NSet.T -> V.NSet.T -> T;

  (* Replays the !-box operations on another graph.
   *
   * The graph that they will be replayed on must have exactly the same
   * !-box structure (including names) as the original graph (passed to init)
   * for this to work.  The tracked should also be in both graphs, and be
   * contained in exactly the same !-boxes in both graphs.  This function will
   * then ensure that the same names are given to copies of those vertices as in
   * the original graph.
   *
   * It is also required that any vertex names in the graph were part
   * of the avoids set, otherwise a name clash could occur.
   *)
  val replay_bbox_ops : T -> G.T -> G.T
end;

functor ReplayableBBoxOperableGraph(G: BANG_GRAPH) : REPLAYABLE_BBOX_OPERABLE_GRAPH =
struct
  datatype BBoxOp = BBKill of B.name
                  | BBCopy of {
                      bbox : B.name,
                      vmap : V.name V.NTab.T,
                      bmap : B.name B.NTab.T
                    };

  structure G = G;
  type T = {
    (* current graph *)
    graph    : G.T,
    (* names context for fresh names when copying bboxes *)
    avoids   : V.NSet.T,
    (* names context for fresh names when copying bboxes *)
    tracked  : V.NSet.T,
    (* !-box operation sequence *)
    bbox_ops : BBoxOp list
  };

  val empty = {
    graph = G.empty,
    avoids = V.NSet.empty,
    tracked = V.NSet.empty,
    bbox_ops = []
  };

  fun init g avoids track = {
        graph = g,
        avoids = avoids,
        tracked = track,
        bbox_ops = []
      };

  fun get_current_graph (oper:T) = #graph oper;
  fun get_avoids (oper:T) = #avoids oper;

  fun normalise (oper:T) =
    {
      graph = G.normalise (get_current_graph oper),
      avoids = #avoids oper,
      tracked = #tracked oper,
      bbox_ops = #bbox_ops oper
    };

  fun expand_bbox b (oper:T) = let
    val g = get_current_graph oper
    val v_avoid = get_avoids oper
    val v_avoid = v_avoid |> V.NSet.union_merge (G.get_vnames g)
    val vrn = V.Rnm.mk_from_avoids v_avoid
    val ern = E.Rnm.mk_from_avoids (G.get_enames g)
    val brn = B.Rnm.mk_from_avoids (G.get_bboxes g)
    val ((vrn',_,brn'),g') = G.expand1_bbox b ((vrn,ern,brn),g)
    val vmap = V.Rnm.get_nmap vrn'
    val tracked = #tracked oper
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
    {
      graph = g',
      avoids = #avoids oper,
      tracked = #tracked oper,
      bbox_ops = bbop::(#bbox_ops oper)
    }
  end;

  fun kill_bbox b (oper:T) =
    {
      graph = G.kill_bbox b (get_current_graph oper),
      avoids = #avoids oper,
      tracked = #tracked oper,
      bbox_ops = (BBKill b)::(#bbox_ops oper)
    };

  fun replay_bbox_op _ (BBKill b) g = G.kill_bbox b g
    | replay_bbox_op v_avoid (BBCopy {bbox,vmap,bmap}) g = let
        val v_avoid = v_avoid |> V.NSet.union_merge (G.get_vnames g)
        val vrn = V.Rnm.mk V.NSet.empty v_avoid vmap
        val ern = E.Rnm.mk_from_avoids (G.get_enames g)
        val brn = B.Rnm.mk B.NSet.empty (G.get_bboxes g) bmap
        val (_,g') = G.expand1_bbox bbox ((vrn,ern,brn),g)
      in g' end;

  fun replay_bbox_ops (oper:T) = let
    val bbops  = #bbox_ops oper
    val avoids = #avoids oper
    val g      = #graph oper
    val avoids = V.NSet.union_merge (G.get_vnames g)
                                    (#avoids oper)
  in
    fold_rev (replay_bbox_op avoids) bbops
  end;

  fun pretty oper =
    Pretty.chunks [
      Pretty.str "ReplayableBBoxOperableGraph {",
      Pretty.block [
        Pretty.str "  ", (* indent *)
        Pretty.chunks [
          Pretty.block [
            Pretty.str "Current Graph: ",
            G.pretty (get_current_graph oper)
          ]
        ]
      ],
      Pretty.str "}"
    ];
  val print = Pretty.writeln o pretty;

  structure Sharing =
  struct
    type T = T
    structure G = G.SharingOGraph
  end;
end;
