signature BBOX_OPERABLE_GRAPH_SHARING =
sig
  type T;
  structure Graph : OGRAPH_SHARING;
end;

(* Used by !-graph matching to apply !-box operations to the pattern.
 * This allows the sequence of !-box operations to be tracked, for example to
 * record it or to apply it to a rule or another graph.
 *)
signature BBOX_OPERABLE_GRAPH =
sig
  type T;
  structure Graph : BANG_GRAPH;
  structure Sharing : BBOX_OPERABLE_GRAPH_SHARING
    sharing type Sharing.T = T
    sharing Sharing.Graph = Graph.Sharing

  val normalise : T -> T;
  val expand_bbox : B.name -> T -> T;
  val kill_bbox : B.name -> T -> T;

  (* the graph so far, with all the operations applied *)
  val get_current_graph : T -> Graph.T;

  val empty : T;

  val print : T -> unit;
  val pretty : T -> Pretty.T;
end;

(* This is a dumb wrapper that just passes function calls on to Graph *)
signature SIMPLE_BBOX_OPERABLE_GRAPH =
sig
  include BBOX_OPERABLE_GRAPH

  val init : Graph.T -> T
end

functor DumbBBoxOperableGraph(Graph: BANG_GRAPH)
: SIMPLE_BBOX_OPERABLE_GRAPH where type T = Graph.T =
struct
  type T = Graph.T
  structure Graph = Graph

  val init = I
  val normalise = Graph.normalise
  val expand_bbox = Graph.fresh_expand_bbox_anon
  val kill_bbox = Graph.kill_bbox
  val get_current_graph = I
  val empty = Graph.empty
  val print = Graph.print
  val pretty = Graph.pretty

  structure Sharing =
  struct
    type T = T
    structure Graph = Graph.Sharing
  end
end

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
  val init : Graph.T -> V.NSet.T -> V.NSet.T -> T;

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
  val replay_bbox_ops : T -> Graph.T -> Graph.T
end;

datatype bbox_op = BBKill of B.name
                 | BBCopy of {
                      bbox : B.name,
                      vmap : VSub.T,
                      bmap : BSub.T }

functor ReplayableBBoxOperableGraph(structure Graph: BANG_GRAPH) : REPLAYABLE_BBOX_OPERABLE_GRAPH =
struct


  structure Graph = Graph;
  type T = {
    (* current graph *)
    graph    : Graph.T,
    (* names context for fresh names when copying bboxes *)
    avoids   : V.NSet.T,
    (* names context for fresh names when copying bboxes *)
    tracked  : V.NSet.T,
    (* !-box operation sequence *)
    bbox_ops : bbox_op list
  };

  val empty = {
    graph = Graph.empty,
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
      graph = Graph.normalise (get_current_graph oper),
      avoids = #avoids oper,
      tracked = #tracked oper,
      bbox_ops = #bbox_ops oper
    };

  fun expand_bbox b (oper:T) = let
    val g = get_current_graph oper
    val v_avoid = get_avoids oper
    val v_avoid = v_avoid |> V.NSet.union_merge (Graph.get_vertices g)
    val vrn = VSub.mk_from_avoids v_avoid
    val ern = ESub.mk_from_avoids (Graph.get_edges g)
    val brn = BSub.mk_from_avoids (Graph.get_bboxes g)
    val ((vrn',_,brn'),g') = Graph.expand_bbox b ((vrn,ern,brn),g)
    val tracked = #tracked oper
    (* only keep track of the names of the vertices we care about *)
    val vmap = V.NSet.fold (fn v =>
        case VSub.get_opt vrn' v of
             NONE => (fn x => x)
           | SOME v' => VSub.add (v,v')) tracked VSub.empty
    val bmap = brn' |> BSub.clear_avoids
    val bbop = BBCopy {
        bbox = b,
        vmap = vmap,
        bmap = bmap
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
      graph = Graph.kill_bbox b (get_current_graph oper),
      avoids = #avoids oper,
      tracked = #tracked oper,
      bbox_ops = (BBKill b)::(#bbox_ops oper)
    };

  fun replay_bbox_op _ (BBKill b) g = Graph.kill_bbox b g
    | replay_bbox_op v_avoid (BBCopy {bbox,vmap,bmap}) g = let
        val v_avoid = v_avoid |> V.NSet.union_merge (Graph.get_vertices g)
        val vrn = vmap |> VSub.avoid_set_in_cod (V.NSet.subtract v_avoid (VSub.get_cod_set vmap))
        val ern = ESub.mk_from_avoids (Graph.get_edges g)
        val brn = bmap |> BSub.avoid_set_in_cod (Graph.get_bboxes g)
        val (_,g') = Graph.expand_bbox bbox ((vrn,ern,brn),g)
      in g' end;

  fun replay_bbox_ops (oper:T) = let
    val bbops  = #bbox_ops oper
    val avoids = #avoids oper
    val g      = #graph oper
    val avoids = V.NSet.union_merge (Graph.get_vertices g)
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
            Graph.pretty (get_current_graph oper)
          ]
        ]
      ],
      Pretty.str "}"
    ];
  val print = Pretty.writeln o pretty;

  structure Sharing =
  struct
    type T = T
    structure Graph = Graph.Sharing
  end;
end;
