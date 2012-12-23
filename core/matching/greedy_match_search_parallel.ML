(* A wrapper for matching bang graphs against concrete graphs
 * see docs/matching_algo for details
 *
 * This implements the "Lazy !-Box Expansion" algorithm
 *
 * This wrapper greedily matches the whole concrete part of the
 * graph, and then picks an arbitrary top-level !-box and tries
 * both expanding it and killing it.
 *)
functor MJB_GreedyMatchSearchParallel(
    structure InnerMatchSearch : INNER_MATCH_SEARCH
    (* This arg is required to make typing work sanely. *)
    (* It must be the same structure as the one in InnerMatchSearch *)
    structure BGMatchState : BANG_GRAPH_MATCH_STATE
    sharing InnerMatchSearch.MatchState.Sharing = BGMatchState.Sharing
) : BG_MATCH_SEARCH =
struct
  local
  (*For all of the future calls that I need!*)
  open MJB_Simple_Future;
  in
  structure Log : LOG = Log(val init_level = 0);

  structure BGMatchState = BGMatchState
  structure BG = BGMatchState.BG
  structure MatchState = InnerMatchSearch.MatchState
  structure G = InnerMatchSearch.G
  
  (*For ease of use*)
  structure Future = MJB_Simple_Future;
  structure Par_Seq = MJB_Safe_Parallel_Seq;
  structure Par_Tree = MJB_Par_Tree;
  structure Stopwatch = MJB_Stopwatch;
  
  (*
  PARALLELISATION DETAILS:
  
  There was a fairly serious problem when trying to parallelise this algorithm. The algorithm
  has two significant branch points (labelled BRANCH POINT if you want to search). These do offer
  a number of branches, but forking separate threads off on each branch is not enough.
  
  Each branch forms a new lazy sequence. Since the sequence is lazy, the actual computation
  of this sequence is extremely quick. Evaluating the entire branch would, of course, be expensive,
  but this is not done in reality at the point when the branch occurs. If it were fully evaluated,
  then the matching algorithm would no longer be lazy, which is naturally extremely undesirable.
  
  Forking with regular futures and the parallel lazy sequence at the BRANCH POINTs found was
  very inefficient for the reason abrove. Very quickly, the produced threads would finish and return
  a lazy sequence which took no time to compute at all.
  
  If you imagine the branches forming a tree structure for the process, you might wonder
  if it would be possible to return functions which would be ready to compute
  the leaves. Unfortunately, this is not possible either. The nodes of the tree are actually
  nearly as expensive as the leaves themselves in general to compute (unless there are many bare wires),
  and so the leaves cannot be found without doing the majority of the work. In other words, it is not
  efficient to produce a sequence of functions to finish off the leaves.
  
  Since bare wires may pose a serious threat to every solution, it may still be worth returning
  a sequence of functions to perform the bare wire matching, and then compacting and parallelising
  on those functions at the end. (May try this if the tree appears to be bad at doing this
  automatically)
  
  The main issue then is how to reach the leaves while parallelising, if we know we have to compute
  the nodes and that the nodes hold much of the workload. The solution is to use the parallel
  tree. This will slice down the left hand side of the tree first (where the first leaves are
  assumed to come from) parallelising across rows of nodes to make use of the cores, while
  maintaining some laziness.
  
  The parallel tree expects a function which it can apply to any node to produce the next
  list of children. This is genuinely a list, so it requires full evaluation of the immediate
  children! Theoretically, this could be avoided and a lazy sequence of children should be possible
  to use, at least for the last node consumed by the parallel tree, but due to the complexity
  of the algorithm and some time efficiency issues, it is expecting a list at the moment.
  For example, it is desirable for the function applied to the node to take time to run,
  and so allowing it to return a lazy sequence would make the tree considerably less efficient.
  
  Clearly, both the leaves and the nodes hold the same type of value - a match state.
  Here is the code that contains what will form the functions to apply to the nodes:
  
  fun match_loop data ms' = let
    fun kill_and_expand fd' b ms = let
      fun do_kill fd = (choose_next_bbox fd) o (kill_pat_bbox fd b)
      fun do_copy fd = (match_loop fd) o BGMatchState.schedule_new_concrete
                               o (expand_pat_bbox fd b)
    in
      Seq.append (Seq.maps (do_kill data) (Seq.single ms))
                 (Seq.maps (do_copy data) (Seq.single ms))
    end
    and choose_next_bbox fd ms =
      case get_next_bbox (BGMatchState.get_pat ms) of
           NONE => finish_match fd ms
         | SOME b => kill_and_expand fd b ms
  in
        Seq.maps (choose_next_bbox data)
    (Seq.map BGMatchState.kill_impossible_bboxes
        (InnerMatchSearch.match_pending doomedf ms'))
  end
  
  So, match loop itself is a function which applies the inner match search to the given match
  state (the value in the node). Given the children of this after applying "kill_impossible_bboxes",
  it will then apply "choose_next_bbox".
  
  "choose_next_bbox" is a little harder to split up. It too is applied to a single
  match state, and will produce more match states. Specifically, it can produce a leaf
  formed by "finish_match" which applies the bare wire matching, or it can produce more
  nodes by applying "kill_and_expand".
  
  In the case of kill and expand, the functions which should be applied to the children
  are a bit more interesting. It essentially produces two nodes, each with the same match
  state as it was fed originally.
  
  1.) The first is kill. It kills the pattern bbox, and then wishes for the "choose_next_bbox"
      to be reapplied.
  2.) The second is copy. This expands the bbox and schedules the expanded graph, and then
      wishes for the "match_loop" to be reapplied.
      
  Hence, all nodes will have one of these two functions applied to them to produce the next children:
  
  1.) "match_loop"
  2.) "choose_next_bbox"
  
  This implies that the nodes should not just hold the match state, but also the function
  that should be applied to them. The function actually applied to all of the nodes should
  just take this function from the node and apply it to the match state. Hence, nodes
  will contain values of this type (abstractly):
  
  Node (match_state,match_state -> match_state Tree_Elem List.list)
  Leaf (match_state)
  
  (where Tree_Elem is either a node or a leaf)
  
  The function to be applied to each node is simply:
  fun (ms,ms_func) = ms_func ms
  
  When "finish_match" is applied, for now (unless producing functions to prepare for this is more
  desirable) we will evaluate all of the resulting matches and return them as leaves.
  
  Note: the node function can create no children, so the algorithms ability to produce
  empty sequences is not a problem.
    
  So, that's the plan. Let's do this!
  *)
  
  (*Log the future data*)
  fun log_future_data data lvl =
    Log.logf lvl future_data_to_string data
  
  (*Use these to print in the right order...*)
  fun logf data lvl =
    (
    log_future_data data lvl;
    Log.logf lvl
    )
  (*All log functions adjusted to log the future data first*)
  fun log_p data pretty_f lvl name =
    (
    log_future_data data lvl;
    Log.logf lvl (fn g => Pretty.string_of
        (Pretty.chunks [Pretty.str (name^":"),(pretty_f g)]))
    )
  fun log_graph data lvl =
    (
    log_future_data data lvl;
    log_p data G.pretty lvl
    )
  
  fun log_v_nset data = log_p data V.NSet.pretty
  (* TODO: it is probably worth optimising this some more *)
  fun doomedf ms pv = let
    val pat = BGMatchState.get_pat ms
    val tgt = BGMatchState.get_tgt ms
    val tv = VInjEndo.domf (BGMatchState.get_vmap ms) pv
    val parity = BG.get_arity pat pv
    val tarity = BG.get_arity tgt tv
    fun has_bbox es = let
      fun other_end_in_bbox e =
        BG.is_bboxed pat (BG.edge_get_other_vertex pat e pv)
    in
      E.NSet.exists other_end_in_bbox es
    end
    val need_more_ins = Arity.get_in parity < Arity.get_in tarity
    val need_more_outs = Arity.get_out parity < Arity.get_out tarity
    val need_more_undir = Arity.get_undir parity < Arity.get_undir tarity
    fun chk_bbox (inctyp,v,(e,(dirtyp,_))) (nmins,nmouts,nmundir) =
      (* we can ignore selfloops, as the graph is normalised *)
      case (inctyp,dirtyp)
        of (BG.SelfLoop,_) => raise ERROR ("Unexpected self-loop in normed graph ("^
                V.string_of_name pv^","^E.string_of_name e^")")
         | (_,BG.UnDirected) => (nmins,nmouts,nmundir andalso not (BG.is_bboxed pat v))
         | (BG.Incoming,BG.Directed) => (nmins andalso not (BG.is_bboxed pat v),nmouts,nmundir)
         | (BG.Outgoing,BG.Directed) => (nmins,nmouts andalso not (BG.is_bboxed pat v),nmundir)
  in
    (need_more_ins orelse need_more_outs orelse need_more_undir)
    andalso
    (let val (nmins,nmouts,nmundir) =
      BG.fold_adj chk_bbox pat pv (need_more_ins,need_more_outs,need_more_undir)
     in nmins orelse nmouts orelse nmundir end)
  end

  fun get_next_bbox g =
    B.NSet.get_exists (not o (BG.bbox_has_parent g)) (BG.get_bboxes g)

  fun finish_match _ ms =
      Seq.filter BGMatchState.is_total
        (InnerMatchSearch.match_bare_wires ms)

  fun kill_pat_bbox data b ms = let
    val _ = logf data 2 (fn () => "Killing !-box "^(B.string_of_name b)) ()
  in
    BGMatchState.kill_pat_bbox b ms
  end
  fun expand_pat_bbox data b ms = let
    val _ = logf data 2 (fn () => "Expanding !-box "^(B.string_of_name b)) ()
  in
    BGMatchState.expand_pat_bbox b ms
  end
  
  (*These are the functions used within the nodes of the tree*)
  datatype node_functions = Choose_Next_BBox of Future.future_data
                          | Match_Loop of Future.future_data
  
  (*Apply the inner match algorithm*)
  fun match_loop data ms =
    (*Produce the next nodes in the tree!*)
    List.map (fn ms' => Par_Tree.Node (ms',Choose_Next_BBox data)) (
    Seq.list_of (
    Seq.map BGMatchState.kill_impossible_bboxes (
    InnerMatchSearch.match_pending doomedf ms)))
  
  (*Kill or expand the pattern bang box*)
  fun choose_next_bbox data ms =
    case get_next_bbox (BGMatchState.get_pat ms) of
        NONE => (*Leaves here!*)
                let
                  val sw = Stopwatch.start Stopwatch.new;
                  val res = List.map (fn ms' => Par_Tree.Leaf ms') (
                            Seq.list_of (finish_match data ms)) (*Computed at the moment*)
                  val sw = Time.toMicroseconds (Stopwatch.check sw);
                  val _ = PolyML.print("Finishing the match took " ^ (LargeInt.toString(sw)));
                in
                  res
                end
      | SOME b => (*Exactly two children nodes*)
                (Par_Tree.Node (BGMatchState.schedule_new_concrete (expand_pat_bbox data b ms),Match_Loop data))::
                (choose_next_bbox data (kill_pat_bbox data b ms))
                (*
                [(*Kill the bbox*)
                 Par_Tree.Node (kill_pat_bbox data b ms,Choose_Next_BBox data),
                 (*Copy the bbox*)
                 Par_Tree.Node (BGMatchState.schedule_new_concrete (expand_pat_bbox data b ms),Match_Loop data)
                ]*)
  
  (*The function which is actually applied to the above nodes*)
  fun apply_to_node (ms,ms_func) = case ms_func of
      (Match_Loop fd) =>
        let
          val sw = Stopwatch.start Stopwatch.new;
          val res = match_loop fd ms;
          val t = Time.toMicroseconds(Stopwatch.check sw);
          val _ = PolyML.print("Match_Loop took: " ^ (LargeInt.toString(t)));
        in
          res
        end
        (*match_loop fd ms*)
    | (Choose_Next_BBox fd) =>
        let
          val sw = Stopwatch.start Stopwatch.new;
          val res = choose_next_bbox fd ms;
          val t = Time.toMicroseconds(Stopwatch.check sw);
          val _ = PolyML.print("Choose_Next_BBox took: " ^ (LargeInt.toString(t)));
        in
          res
        end
        (*choose_next_bbox fd ms*)

  fun match_normalised_avoiding_tracking (avoid,track) pat tgt = let
    val data = (Future.new_future_data (SOME "match_norm"));
    val _ = log_graph data 3 "GreedyMatchSearch: normalised pattern" pat
    val _ = log_graph data 3 "GreedyMatchSearch: normalised target" tgt
    val _ = log_v_nset data 3 "GreedyMatchSearch: avoids" avoid
    val _ = log_v_nset data 3 "GreedyMatchSearch: tracked" track
    val ms = BGMatchState.init_and_schedule_concrete pat tgt
              |> BGMatchState.set_pat_avoids avoid
              |> BGMatchState.set_pat_tracked track
    (*val _ = PolyML.print("starting -1");*)
    (*The parallel tree to use*)
    val par_tree = Par_Tree.new data;
    (*The root node*)
    val root = Par_Tree.Node (ms,Match_Loop data);
  in
    (*Run the parallel tree! Returns the sequence of matches*)
    Par_Tree.compute_tree par_tree root apply_to_node
  end
  fun match_avoiding_tracking at pat tgt = let
    val pat' = G.normalise pat
    val tgt' = G.normalise tgt
    (*val _ = PolyML.print("starting 0");*)
  in match_normalised_avoiding_tracking at pat' tgt' end

  fun match_subgraph_normalised_avoiding_tracking (avoid,track) pat tgt tgt_verts = let
    val pat = G.normalise pat
    val tgt = G.normalise tgt
    val data = Future.new_future_data (SOME "match_sub_norm");
    val _ = log_graph data 3 "GreedyMatchSearch: normalised pattern" pat
    val _ = log_graph data 3 "GreedyMatchSearch: normalised target" tgt
    val _ = log_v_nset data 3 "GreedyMatchSearch: avoids" avoid
    val _ = log_v_nset data 3 "GreedyMatchSearch: tracked" track
    val _ = log_v_nset data 4 "GreedyMatchSearch: target vertices" tgt_verts
    val ms = BGMatchState.init_subgraph_and_schedule_concrete pat tgt tgt_verts
              |> BGMatchState.set_pat_avoids avoid
              |> BGMatchState.set_pat_tracked track
    (*val _ = PolyML.print("starting 1");*)
    (*The parallel tree to use*)
    val par_tree = Par_Tree.new data;
    (*The root node*)
    val root = Par_Tree.Node (ms,Match_Loop data);
  in
    (*Run the parallel tree! Returns the sequence of matches*)
    Par_Tree.compute_tree par_tree root apply_to_node
  end
  fun match_subgraph_avoiding_tracking at pat tgt = let
    val pat' = G.normalise pat
    val tgt' = G.normalise tgt
    (*val _ = PolyML.print("starting 2");*)
  in match_subgraph_normalised_avoiding_tracking at pat' tgt' end

  val match = let
    (*val _ = PolyML.print("starting 3");*)in
    match_avoiding_tracking (V.NSet.empty, V.NSet.empty)
    end
  val match_subgraph = let
    (*val _ = PolyML.print("starting 4");*)in
    match_subgraph_avoiding_tracking (V.NSet.empty, V.NSet.empty)
    end
  val match_normalised = let
    (*val _ = PolyML.print("starting 5");*)in
    match_normalised_avoiding_tracking (V.NSet.empty, V.NSet.empty)
    end
  val match_subgraph_normalised = let
    (*val _ = PolyML.print("starting 6");*)in
    match_subgraph_normalised_avoiding_tracking (V.NSet.empty, V.NSet.empty)
    end

  structure Sharing =
  struct
    structure G = G.SharingOGraph
    structure MatchState = MatchState.Sharing
  end
  end
end
